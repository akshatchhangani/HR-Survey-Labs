# =============================================================
# HR Survey Labs — survey mapping
# Works out whether an uploaded file belongs to the selected survey type
# by what its questions MEAN, not by exact column names.
#
#   1. Concept library: for each of the 16 survey types, the concepts it
#      measures and many ways organisations phrase them.
#   2. Column roles: respondent ID, respondent detail (department, tenure…),
#      free-text comment, or survey question.
#   3. Relevance: every question is compared with all 16 survey types; the
#      file is accepted, rejected as a clear mismatch, or held for review.
#   4. Scales: detected from the ANSWERS (1–5, 1–7, 0–10, "Strongly agree"…),
#      and answers outside the scale are flagged, never changed.
#
# Matching rule: a phrasing such as "beyond expect" matches a heading when
# EVERY word in it is the start of some word in the heading. So "manag"
# matches manager / managers / management, and word order does not matter.
# =============================================================


# ---- 1. Concept library -----------------------------------------

concept <- function(label, ...) list(label = label, patterns = c(...))

SURVEY_CONCEPTS <- list(

  employee_engagement = list(
    concept("Pride",                "proud", "pride"),
    concept("Advocacy",             "recommend", "refer friend", "great place work", "enps", "nps",
                                    "speak highly", "ambassador"),
    concept("Intent to stay",       "see myself", "stay", "plan stay", "intend stay", "intention stay",
                                    "stay organis", "stay compan", "work here year",
                                    "look job", "look another", "think leav", "thought leav",
                                    "leav organis", "leav compan", "retention"),
    concept("Discretionary effort", "beyond expect", "beyond what", "extra effort",
                                    "extra mile", "discretionary"),
    concept("Motivation",           "motiv", "inspir", "energis", "enthusias", "passion"),
    concept("Meaningful work",      "accomplish", "meaningful", "purpose", "fulfil"),
    concept("Commitment",           "commit", "loyal", "care future", "emotional attach",
                                    "attached organis"),
    concept("Engagement (overall)", "engag")
  ),

  # Satisfaction surveys phrase most items as "I am satisfied with my X", so
  # each item also matches "Overall satisfaction"; pick_concept() then prefers
  # the specific area (X) unless the item says "overall" or "in general".
  employee_satisfaction = list(
    concept("Overall satisfaction", "satisf", "happy job", "happy work", "content job"),
    concept("The work itself",      "enjoy work", "enjoy job", "like job", "like work",
                                    "interesting", "variety", "type work", "nature work",
                                    "work i do", "tasks", "job content"),
    concept("Manager and supervision", "supervis", "manag", "boss", "team lead", "reporting"),
    concept("Pay",                  "pay", "paid", "salar", "compensat", "wage", "ctc", "remunerat"),
    concept("Benefits",             "benefit", "insurance", "medical", "perk", "allowance",
                                    "pension", "retire", "leave polic"),
    concept("Recognition",          "reward", "recogn", "appreciat", "acknowledg"),
    concept("Growth and development", "career", "growth", "promot", "advancement", "develop",
                                    "training", "learn", "skill"),
    concept("Colleagues and teamwork", "colleag", "coworker", "co worker", "peer", "team member",
                                    "teamwork", "team work"),
    concept("Work-life balance",    "work life", "life balance", "flexib", "hours", "workload"),
    concept("Working conditions",   "work condition", "work environment", "workplace", "facilit",
                                    "office", "equipment", "tools"),
    concept("Job security",         "job secur", "secure", "stabil", "stable"),
    concept("Communication",        "communicat", "kept informed", "inform"),
    concept("Policies",             "polic", "procedur", "rules", "process")
  ),

  # Pulse surveys usually reuse engagement / satisfaction items; those are
  # also accepted when Pulse is selected (see assess_relevance()).
  pulse_survey = list(
    concept("Current sentiment",    "this week", "this month", "past week", "past month",
                                    "last week", "last month", "today", "right now", "mood",
                                    "how feeling", "pulse"),
    concept("Wellbeing",            "wellbeing", "well being", "stress", "burnout", "burn out",
                                    "exhaust", "overwhelm", "mental health"),
    concept("Workload",             "workload", "manageable", "too much work", "capacity"),
    concept("Support",              "feel support", "get support", "support need", "help need")
  ),

  onboarding = list(
    concept("Onboarding (overall)", "onboard", "induction", "orientation", "new hire",
                                    "new joiner", "new employee", "joining", "joined"),
    concept("Early experience",     "first day", "first week", "first month", "first few",
                                    "welcom"),
    concept("Role clarity",         "role clar", "clear role", "understand role",
                                    "understand what expect", "clear what expect",
                                    "expectations clear", "responsibilit clear",
                                    "job description"),
    concept("Readiness",            "ready", "prepar", "equipped", "confident perform",
                                    "laptop", "equipment", "system access", "access system",
                                    "tools need"),
    concept("Support and buddy",    "buddy", "mentor", "introduc team", "introduc colleag",
                                    "integrat team", "settle"),
    concept("Joining process",      "paperwork", "documentation", "offer letter", "before joining")
  ),

  training_needs_analysis = list(
    concept("Skill gaps",           "skill gap", "gap", "lack skill", "improve skill",
                                    "skill improv"),
    concept("Training needs",       "training need", "need train", "need training",
                                    "require train", "training require", "would like train",
                                    "training would help", "priority train"),
    concept("Current proficiency",  "proficien", "competenc", "capabilit", "current level",
                                    "skill level", "rate skill", "rate your", "confident using",
                                    "knowledge of", "expertise"),
    concept("Importance to role",   "importan role", "importan job", "required level",
                                    "critical role", "needed role", "required role"),
    concept("Learning interests",   "want learn", "like learn", "interest learn",
                                    "develop area", "areas develop", "learning interest",
                                    "upskill", "reskill")
  ),

  training_effectiveness = list(
    concept("Training reaction",    "training", "course", "session", "workshop", "program",
                                    "module", "webinar"),
    concept("Trainer quality",      "trainer", "facilitat", "instructor", "presenter"),
    concept("Content and materials","content", "material", "relevan", "objectiv met",
                                    "well organis", "pace"),
    concept("Learning gained",      "learn", "knowledge gain", "new knowledge", "understand better",
                                    "pre test", "post test", "before train", "after train",
                                    "skills gain"),
    concept("Application on the job","apply", "applied", "use what", "using what", "on job",
                                    "back work", "transfer"),
    concept("Impact",               "impact", "improv perform", "perform better", "worth",
                                    "useful")
  ),

  performance_management = list(
    concept("Goal clarity",         "goal", "objectiv", "kpi", "target", "key result", "okr"),
    concept("Feedback quality",     "feedback", "check in", "one on one", "conversation perform"),
    concept("Appraisal fairness",   "appraisal", "rating fair", "fair rating", "rated fair",
                                    "fair assess", "fair evaluat", "assess fair", "evaluat fair",
                                    "rating reflect", "bias rating", "calibrat"),
    concept("Review process",       "perform review", "review process", "performance process",
                                    "review meeting", "mid year", "year end", "annual review",
                                    "evaluat"),
    concept("Performance and development","perform", "development plan", "improvement plan",
                                    "career conversation", "growth plan"),
    concept("Recognition of performance","recogn perform", "reward perform", "link reward",
                                    "pay perform", "merit")
  ),

  leadership = list(
    concept("Leadership (overall)", "leader", "senior manag", "management team", "executive"),
    concept("Manager support",      "manag", "supervis", "boss", "reporting manag"),
    concept("Trust",                "trust", "confiden leader", "confiden manag", "credib",
                                    "honest", "integrit"),
    concept("Communication",        "communicat", "inform", "explain", "listen", "approachable",
                                    "open door", "transparen"),
    concept("Coaching and development","coach", "support develop", "develop me",
                                    "help grow", "career develop", "mentor"),
    concept("Decision-making",      "decision", "direction", "vision", "strategy", "priorit"),
    concept("Role modelling",       "role model", "lead example", "act line", "walk talk")
  ),

  dei_inclusion = list(
    concept("Inclusion",            "inclu", "accept", "authentic", "be myself", "be yourself",
                                    "valued who", "valu who"),
    concept("Belonging",            "belong", "fit in", "part of team"),
    concept("Diversity",            "divers", "different background", "representation"),
    concept("Equal opportunity",    "equal opportun", "equal access", "equit", "regardless",
                                    "same opportun", "fair chance", "merit regardless"),
    concept("Respect and fairness", "respect", "treated fair", "treat fair", "dignity",
                                    "fair treatment"),
    concept("Discrimination and bias","discriminat", "bias", "harass", "prejudic",
                                    "stereotyp", "microaggress")
  ),

  exit_survey = list(
    concept("Reason for leaving",   "reason leav", "why leav", "leav", "resign", "quit",
                                    "exit", "departure", "decid leave", "decision leave"),
    concept("Would return",         "return", "rejoin", "come back", "rehire",
                                    "work again"),
    concept("Career opportunities", "career", "growth", "promot", "advancement"),
    concept("Manager relationship", "manag", "supervis", "boss"),
    concept("Pay",                  "pay", "paid", "salar", "compensat"),
    concept("Work-life and workload","work life", "workload", "life balance", "hours"),
    concept("New role",             "new employer", "new job", "new role", "new company",
                                    "joining", "better offer", "offer"),
    concept("Retention signals",    "kept you", "made you stay", "could have done", "retain",
                                    "prevent leav", "stay if"),
    concept("Experience here",      "time here", "experience here", "overall experience",
                                    "exit process", "notice")
  ),

  compensation_benefits = list(
    concept("Pay satisfaction",     "pay", "paid", "salar", "compensat", "wage", "ctc",
                                    "remunerat", "earn"),
    concept("Pay equity",           "fair pay", "pay fair", "paid fair", "equit pay",
                                    "pay equit", "equal pay", "pay gap", "paid equal"),
    concept("Market competitiveness","market", "competitive", "industry standard",
                                    "compared other", "peers other"),
    concept("Bonus and incentives", "bonus", "incentiv", "variable", "commission",
                                    "increment", "hike", "raise"),
    concept("Benefits",             "benefit", "insurance", "medical", "health cover",
                                    "pension", "retire", "provident", "gratuity", "allowance",
                                    "perk", "leave polic", "wellness"),
    concept("Benefits awareness",   "aware benefit", "understand benefit", "know benefit",
                                    "benefit explained", "benefit communicat"),
    concept("Total rewards",        "reward", "total reward", "recogn")
  ),

  workplace_culture = list(
    concept("Values",               "valu", "mission", "ethic", "integrit", "principles"),
    concept("Collaboration",        "collaborat", "teamwork", "team work", "work together",
                                    "cooperat", "cross team", "cross functional", "silo",
                                    "help each"),
    concept("Trust and openness",   "trust colleag", "trust each", "open communicat",
                                    "transparen", "honest"),
    concept("Behaviours",           "behavio", "conduct", "accountab", "ownership"),
    concept("Work environment",     "culture", "environment", "atmospher", "positive place",
                                    "friendly", "toxic"),
    concept("Innovation",           "innovat", "new ideas", "creativ", "experiment",
                                    "continuous improv")
  ),

  psychological_safety = list(
    concept("Speaking up",          "speak up", "speak openly", "voice", "raise concern",
                                    "raise issue", "raise problem", "share opinion",
                                    "express opinion", "honest opinion"),
    concept("Mistakes",             "mistake", "error", "fail", "held against", "blame",
                                    "punish"),
    concept("Asking for help",      "ask question", "ask help", "asking help", "seek help",
                                    "admit dont know", "admit not know"),
    concept("Challenging ideas",    "challeng", "disagree", "different view", "different opinion",
                                    "push back", "question decision"),
    concept("Risk-taking and fear", "risk", "fear", "afraid", "safe", "comfortable",
                                    "without consequence", "repercuss")
  ),

  candidate_experience = list(
    concept("Application process",  "appl process", "applicat", "apply", "job post",
                                    "job ad", "career site", "portal"),
    concept("Interview experience", "interview", "assessment", "test round", "panel"),
    concept("Recruiter communication","recruit", "hiring team", "talent acquisit", "hr contact",
                                    "kept informed", "updates", "status"),
    concept("Transparency",         "transparen", "clear process", "process clear",
                                    "expectations", "job description", "salary discuss"),
    concept("Hiring process",       "hiring process", "selection", "candidate", "recruitment",
                                    "offer", "timel", "speed", "turnaround")
  ),

  hr_services = list(
    concept("HR (overall)",         "hr", "human resource", "people team", "people operations"),
    concept("Responsiveness",       "respon", "timel", "turnaround", "quick", "prompt",
                                    "wait", "reach"),
    concept("Resolution",           "resolv", "resolution", "query", "queries", "ticket",
                                    "request", "issue handled", "problem solved"),
    concept("Service quality",      "service", "helpful", "courteous", "knowledgeable",
                                    "professional", "helpdesk", "help desk"),
    concept("Policy clarity",       "polic", "procedur", "process clear", "guideline",
                                    "handbook", "portal", "self service", "payroll")
  ),

  employer_branding = list(
    concept("Employer reputation",  "reputation", "employer", "brand", "image", "known",
                                    "perception", "public"),
    concept("Attractiveness",       "attract", "employer choice", "best place", "great place",
                                    "dream", "prefer work"),
    concept("Advocacy",             "recommend", "refer", "proud tell", "tell others",
                                    "speak positive", "social media", "glassdoor", "linkedin"),
    concept("Value proposition",    "evp", "value proposition", "offers employees",
                                    "unique", "stand out", "differen")
  )
)

# Survey types never suggested as "a better match" — Pulse is a format
# (short, repeated checks), not a separate topic.
NOT_SUGGESTED <- c("pulse_survey")

# Broad surveys: many engagement and satisfaction surveys ask a few core
# questions (e.g. "Engagement score", "Would recommend") plus DRIVER
# questions about management, growth, recognition, workload and so on.
# For these types, questions from the listed areas count as drivers.
BROAD_DRIVERS <- list(
  employee_engagement = c("employee_satisfaction", "leadership", "dei_inclusion",
                          "workplace_culture", "psychological_safety",
                          "compensation_benefits", "performance_management",
                          "training_effectiveness", "pulse_survey", "exit_survey"),
  employee_satisfaction = c("employee_engagement", "leadership", "dei_inclusion",
                            "workplace_culture", "compensation_benefits",
                            "performance_management", "pulse_survey", "exit_survey")
)

# A broad match also needs at least one of the survey's own CORE questions
# (for satisfaction: an overall-satisfaction question, not just "manager" or "pay")
BROAD_CORE <- list(
  employee_engagement = vapply(SURVEY_CONCEPTS$employee_engagement, `[[`, character(1), "label"),
  employee_satisfaction = "Overall satisfaction"
)

# Where to look for a label when a question is a driver, in order
DRIVER_LABEL_ORDER <- c("leadership", "employee_satisfaction", "dei_inclusion",
                        "workplace_culture", "psychological_safety", "compensation_benefits",
                        "performance_management", "training_effectiveness", "exit_survey",
                        "pulse_survey", "employee_engagement")

# Pre-split every phrasing into word prefixes once, at startup
CONCEPT_INDEX <- lapply(SURVEY_CONCEPTS, function(concepts) {
  lapply(concepts, function(cn) {
    list(label = cn$label,
         patterns = lapply(cn$patterns, function(p) strsplit(p, " ", fixed = TRUE)[[1]]))
  })
})


# ---- 2. Reading a column heading --------------------------------

# Words that carry no meaning in a heading
IGNORE_TOKENS <- c("q", "qn", "question", "item", "statement", "rating", "score",
                   "col", "column", "var", "variable", "field", "x", "num", "number")

# Common abbreviations in HRIS and survey-platform column codes
ABBREVIATIONS <- c(mgr = "manager", mngr = "manager", wlb = "work life balance",
                   eng = "engagement", engmt = "engagement", recog = "recognition",
                   dev = "development", comms = "communication",
                   sat = "satisfaction", satis = "satisfaction", org = "organisation",
                   perf = "performance", ben = "benefits", psych = "psychological",
                   ldr = "leadership", lead = "leadership", trng = "training",
                   cult = "culture", incl = "inclusion", div = "diversity",
                   emp = "employee", dept = "department", loc = "location")

header_tokens <- function(header) {
  x <- gsub("([a-z])([A-Z])", "\\1 \\2", header)        # camelCase -> camel Case
  x <- tolower(x)
  x <- gsub("iz", "is", x, fixed = TRUE)                # organization -> organisation
  x <- gsub("[^a-z0-9]+", " ", x)
  toks <- strsplit(trimws(x), "\\s+")[[1]]
  toks <- toks[toks != ""]
  toks <- toks[!grepl("^q?[0-9]+[a-z]?$", toks)]        # Q1, 12, q3a
  toks <- toks[!toks %in% IGNORE_TOKENS]
  # Expand abbreviations into full words
  if (length(toks) > 0 && any(toks %in% names(ABBREVIATIONS))) {
    toks <- unlist(lapply(toks, function(t) {
      if (t %in% names(ABBREVIATIONS)) strsplit(ABBREVIATIONS[[t]], " ")[[1]] else t
    }))
  }
  toks
}

# When a heading matches several concepts, prefer the specific one over an
# "overall" one, unless the heading itself says "overall" or "in general"
pick_concept <- function(hits, tokens) {
  if (length(hits) == 0) return(NA_character_)
  if (length(hits) == 1) return(hits[1])
  is_overall <- grepl("overall", hits, ignore.case = TRUE)
  asks_overall <- any(tokens %in% c("overall", "general", "considered"))
  if (asks_overall && any(is_overall)) return(hits[is_overall][1])
  if (any(!is_overall)) hits[!is_overall][1] else hits[1]
}

phrase_matches <- function(tokens, prefixes) {
  all(vapply(prefixes, function(p) any(startsWith(tokens, p)), logical(1)))
}

# Labels of the concepts in one survey type that a heading matches
matched_concepts <- function(tokens, survey_key) {
  if (length(tokens) == 0) return(character(0))
  hits <- vapply(CONCEPT_INDEX[[survey_key]], function(cn) {
    any(vapply(cn$patterns, function(p) phrase_matches(tokens, p), logical(1)))
  }, logical(1))
  vapply(CONCEPT_INDEX[[survey_key]][hits], `[[`, character(1), "label")
}


# ---- 3. Reading a column's answers ------------------------------

# Checked in this order. 1–5 comes first because it is by far the most common;
# a 1–4 survey still fits inside it, and the user confirms the scale later.
STANDARD_SCALES <- list(c(1, 5), c(0, 5), c(1, 7), c(1, 10), c(0, 10))

# Text answer scales, each placed on a 1-7 ladder (4 = the neutral middle).
# Shared with analytics_common.R, which scores answers with these values.
TEXT_LADDERS <- list(
  agreement = c(
    "strongly disagree" = 1, "disagree strongly" = 1, "completely disagree" = 1,
    "totally disagree" = 1, "disagree" = 2, "mostly disagree" = 2,
    "somewhat disagree" = 3, "slightly disagree" = 3, "tend to disagree" = 3,
    "disagree somewhat" = 3, "disagree slightly" = 3,
    "neither agree nor disagree" = 4, "neither agree or disagree" = 4,
    "neither disagree nor agree" = 4, "neutral" = 4, "neither" = 4, "undecided" = 4,
    "somewhat agree" = 5, "slightly agree" = 5, "tend to agree" = 5,
    "agree somewhat" = 5, "agree slightly" = 5, "agree" = 6, "mostly agree" = 6,
    "strongly agree" = 7, "agree strongly" = 7, "completely agree" = 7, "totally agree" = 7),
  satisfaction = c(
    "very dissatisfied" = 1, "extremely dissatisfied" = 1, "highly dissatisfied" = 1,
    "dissatisfied" = 2, "not satisfied" = 2, "somewhat dissatisfied" = 3,
    "slightly dissatisfied" = 3, "neither satisfied nor dissatisfied" = 4,
    "neither satisfied or dissatisfied" = 4, "neutral" = 4, "somewhat satisfied" = 5,
    "slightly satisfied" = 5, "fairly satisfied" = 5, "moderately satisfied" = 5,
    "satisfied" = 6, "very satisfied" = 7, "extremely satisfied" = 7, "highly satisfied" = 7),
  likelihood = c(
    "very unlikely" = 1, "extremely unlikely" = 1, "unlikely" = 2, "somewhat unlikely" = 3,
    "neither likely nor unlikely" = 4, "neutral" = 4, "not sure" = 4, "somewhat likely" = 5,
    "likely" = 6, "very likely" = 7, "extremely likely" = 7),
  frequency = c(
    "never" = 1, "none of the time" = 1, "almost never" = 2, "rarely" = 2, "seldom" = 2,
    "hardly ever" = 2, "occasionally" = 3, "sometimes" = 4, "some of the time" = 4,
    "often" = 6, "frequently" = 6, "usually" = 6, "very often" = 6, "most of the time" = 6,
    "almost always" = 7, "always" = 7, "all of the time" = 7),
  quality = c(
    "very poor" = 1, "very bad" = 1, "poor" = 2, "bad" = 2, "needs improvement" = 2,
    "unsatisfactory" = 2, "below average" = 3, "fair" = 3, "average" = 4,
    "satisfactory" = 4, "good" = 5, "above average" = 5, "very good" = 6,
    "excellent" = 7, "outstanding" = 7),
  extent = c(
    "not at all" = 1, "to a very small extent" = 2, "to a small extent" = 2,
    "a little" = 3, "slightly" = 3, "somewhat" = 4, "to some extent" = 4,
    "to a moderate extent" = 4, "moderately" = 4, "to a large extent" = 6,
    "to a great extent" = 6, "very much" = 6, "a lot" = 6, "to a very large extent" = 7,
    "to a very great extent" = 7, "completely" = 7, "extremely" = 7),
  importance = c(
    "not at all important" = 1, "not important" = 2, "slightly important" = 3,
    "somewhat important" = 4, "moderately important" = 4, "important" = 5,
    "very important" = 6, "extremely important" = 7)
)

TEXT_SCALES <- c(
  lapply(TEXT_LADDERS, names),
  list(yes_no = c("yes", "no", "maybe", "not sure", "unsure", "don't know", "dont know"))
)

TEXT_SCALE_LABELS <- c(agreement = "Agree / disagree", satisfaction = "Satisfied / dissatisfied",
                       frequency = "Never \u2013 always", likelihood = "Unlikely / likely",
                       quality = "Poor \u2013 excellent", extent = "Not at all \u2013 completely",
                       importance = "Not important \u2013 very important", yes_no = "Yes / no")

# Tidy an answer label so variants compare equal:
# "5 - Strongly Agree", "(5) Strongly agree", "Strongly agree (5)" -> "strongly agree"
norm_label <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("\u2019", "'", x)
  x <- gsub("^\\(?[0-9]+\\)?\\s*[-=:.)]?\\s*(?=[a-z])", "", x, perl = TRUE)
  x <- gsub("(?<=[a-z])\\s*[-=(:]?\\s*[0-9]+\\s*\\)?$", "", x, perl = TRUE)
  x <- gsub("[.!]+$", "", x)
  gsub("\\s+", " ", trimws(x))
}

# Smallest standard scale that holds at least `share` of the values
fit_scale <- function(v, share = 0.95) {
  for (s in STANDARD_SCALES) {
    if (mean(v >= s[1] & v <= s[2]) >= share) return(s)
  }
  NULL
}

# Describe what kind of answers a column holds
profile_answers <- function(x) {
  v <- x[!is.na(x)]
  if (length(v) == 0) return(list(type = "empty"))

  if (is.numeric(v)) {
    if (all(abs(v - round(v)) < 1e-9)) {
      s <- fit_scale(v)
      if (!is.null(s)) return(list(type = "rating", scale = s))
    }
    return(list(type = "number"))
  }

  low <- norm_label(v)
  shares <- vapply(TEXT_SCALES, function(lbls) mean(low %in% lbls), numeric(1))
  if (max(shares) >= 0.9) {
    kind <- names(shares)[which.max(shares)]
    return(list(type = "text_scale", kind = kind, n_options = length(unique(low))))
  }

  if (length(unique(v)) <= 25 && mean(nchar(v)) <= 40) return(list(type = "category"))
  list(type = "free_text")
}


# ---- 4. Column roles --------------------------------------------

DETAIL_TOKENS <- c("department", "dept", "team", "function", "division", "unit",
                   "business", "vertical", "tenure", "service", "experience", "gender",
                   "sex", "age", "location", "city", "region", "branch", "site", "office",
                   "country", "state", "zone", "grade", "level", "band", "role",
                   "designation", "position", "job", "title", "date", "month", "year",
                   "years", "quarter", "wave", "period", "week", "timestamp", "time",
                   "cohort", "batch", "shift", "manager", "supervisor", "reporting",
                   "name", "email", "employee", "type", "category", "status", "joining",
                   "exit", "submitted", "start", "end", "mode", "arrangement", "education",
                   "qualification", "degree", "employment", "contract", "generation",
                   "ethnicity", "race", "religion", "marital", "nationality", "language",
                   "group", "segment", "squad", "project", "client", "plant", "store",
                   "channel", "entity", "company")

COMMENT_PATTERN <- "^((any|additional|other|general|open|final|further) )*(comment|comments|remark|remarks|suggestion|suggestions|note|notes|feedback|thoughts)$"

TIME_TOKENS <- c("date", "month", "year", "quarter", "wave", "period", "week",
                 "timestamp", "time", "submitted")

# Columns survey tools add on their own (Qualtrics, SurveyMonkey, Microsoft
# and Google Forms). They describe the response, not the respondent.
SYSTEM_PATTERN <- paste0(
  "^(start_?date|end_?date|start_?time|end_?time|completion_?time|recorded_?date|",
  "last_?modified(_time)?|timestamp|status|ip_?address|progress|duration.*|finished|",
  "response_?id|recipient.*|external_?(data_?)?reference|location_?(latitude|longitude)|",
  "distribution_?channel|user_?language|collector_?id|custom_?data.*|respondent_?id_?2|",
  "q_?recaptcha.*|q_?relevant.*)$")

# HR-system records often sit next to survey answers in the same export:
# absence days, training hours, promotions, performance ratings, salary...
# They are facts about the employee, not answers, so they are read as
# respondent details (and can be used for group comparisons).
HR_RECORD_PATTERN <- paste0(
  "(absen|sick|leaves?_taken|days_(off|absent|leave)|overtime|hours|",
  "training_(hours|days|count|completed|attended)|trainings|courses|certification|",
  "promotion|increment|hike|bonus|salary|ctc|compensation_band|pay_(band|grade)|",
  "grade_level|performance_(rating|score|band|grade)|perf_(rating|score)|",
  "appraisal_(rating|score)|kpi_(score|achievement)|target_achievement|",
  "commute|distance|dependents|num_|number_of|no_of|_count$|^count_|",
  "_last_(year|month|quarter|[0-9]+_(years|months))|per_(month|week|year))")

# Words that show a heading is a survey question about the topic, not a record
# ("Salary satisfaction", "My performance rating was fair")
OPINION_PATTERN <- "satisf|fair|happy|agree|feel|opinion|perceiv|perception|confiden|trust|understand|aware|clear|enough|adequate"

# Record columns have short, code-like names ("Absence_Days_Last_Year",
# "Bonus received"); a full sentence ("The bonus scheme is transparent") is a question
is_hr_record <- function(clean_name, profile, tokens, original = "") {
  code_like <- !grepl(" ", trimws(original)) || length(tokens) <= 4
  profile$type %in% c("rating", "number") &&
    code_like && length(tokens) <= 6 &&
    grepl(HR_RECORD_PATTERN, clean_name) &&
    !grepl(OPINION_PATTERN, clean_name)
}

column_role <- function(clean_name, original, profile) {
  tokens <- header_tokens(original)
  if (grepl(ID_PATTERN, clean_name)) return("id")
  if (grepl(SYSTEM_PATTERN, clean_name)) {
    if (grepl("date|time|timestamp", clean_name)) return("time")
    return("system")
  }
  if (profile$type == "empty") return("empty")
  if (is_hr_record(clean_name, profile, tokens, original)) return("record")

  is_rating <- profile$type %in% c("rating", "text_scale")
  asks_detail <- profile$type == "category" && length(tokens) <= 12 &&
    (any(tokens %in% DETAIL_TOKENS) || grepl("how long", tolower(original)))
  if (!is_rating && (asks_detail || (length(tokens) <= 4 && any(tokens %in% DETAIL_TOKENS)))) {
    if (any(tokens %in% TIME_TOKENS)) return("time")
    return("detail")
  }
  if (profile$type == "number" && length(tokens) <= 4) return("detail")
  if (profile$type == "free_text" ||
      grepl(COMMENT_PATTERN, paste(tokens, collapse = " "))) return("comment")
  "question"
}


# ---- 5. Relevance decision --------------------------------------

assess_relevance <- function(columns, selected) {
  q <- columns[columns$role == "question", , drop = FALSE]
  keys <- names(SURVEY_CONCEPTS)
  n_q <- nrow(q)

  if (n_q == 0) {
    return(list(decision = "no_questions", coverage = 0, suggested = NULL,
                n_matched = 0, n_questions = 0))
  }

  # match[i, s] = TRUE when question i fits any concept of survey s
  match <- matrix(FALSE, nrow = n_q, ncol = length(keys), dimnames = list(NULL, keys))
  for (i in seq_len(n_q)) {
    toks <- header_tokens(q$original[i])
    for (s in keys) match[i, s] <- length(matched_concepts(toks, s)) > 0
  }

  # Pulse surveys may reuse engagement items and overall-satisfaction items
  # (not every satisfaction topic: "satisfied with my pay" is not a pulse item)
  if (selected == "pulse_survey") {
    sat_overall <- vapply(q$original, function(h) {
      toks <- header_tokens(h)
      identical(pick_concept(matched_concepts(toks, "employee_satisfaction"), toks), "Overall satisfaction")
    }, logical(1))
    match[, "pulse_survey"] <- match[, "pulse_survey"] | match[, "employee_engagement"] | sat_overall
  }

  coverage <- colMeans(match)
  # Specificity: a question shared by several survey types counts less for each
  share <- pmax(1, rowSums(match))
  specificity <- colSums(match / share) / n_q

  others <- setdiff(keys, c(selected, NOT_SUGGESTED))
  best_other <- others[which.max(specificity[others])]
  sel_cov <- coverage[[selected]]
  sel_spec <- specificity[[selected]]
  other_spec <- specificity[[best_other]]

  coded <- mean(vapply(q$original, function(h) length(header_tokens(h)) == 0, logical(1)))

  # Broad survey: at least one core question, most of the rest are drivers,
  # and no single other survey type explains the file better
  n_drivers <- 0
  broad <- FALSE
  has_core <- selected %in% names(BROAD_CORE) &&
    any(vapply(q$original, function(h) any(matched_concepts(header_tokens(h), selected) %in% BROAD_CORE[[selected]]),
               logical(1)))
  if (selected %in% names(BROAD_DRIVERS) && has_core) {
    drv <- BROAD_DRIVERS[[selected]]
    is_driver <- !match[, selected] & apply(match[, drv, drop = FALSE], 1, any)
    broad_cov <- mean(match[, selected] | is_driver)
    # Engagement and satisfaction overlap by nature, so neither counts against the other here
    max_single <- max(coverage[setdiff(keys, c(selected, NOT_SUGGESTED, names(BROAD_DRIVERS)))])
    if (sel_cov < 0.5 && broad_cov >= 0.6 && max_single < 0.6) {
      broad <- TRUE
      n_drivers <- sum(is_driver)
    }
  }

  # Another survey type that clearly covers more of the questions (and is at
  # least as specific) means the file is not simply the selected type
  widest <- others[which.max(coverage[others])]
  outcovered <- coverage[[widest]] > sel_cov + 0.05 && specificity[[widest]] > sel_spec

  decision <-
    if (broad) "match"
    else if (sel_cov >= 0.5 && sel_spec >= 0.75 * other_spec && !outcovered) "match"
    else if (other_spec >= 0.35 && other_spec >= 2 * sel_spec) "mismatch"
    else if (coded >= 0.5) "coded_headers"
    else if (sel_cov >= 0.25) "unclear"
    else "unrecognised"

  list(decision = decision,
       broad = broad,
       n_drivers = n_drivers,
       coverage = sel_cov,
       n_matched = sum(match[, selected]),
       n_questions = n_q,
       suggested = if (decision == "mismatch") best_other else NULL,
       n_suggested_matched = sum(match[, best_other]),
       scores = data.frame(survey = keys, coverage = round(coverage, 2),
                           specificity = round(specificity, 2), row.names = NULL))
}


# ---- 6. Scale check ---------------------------------------------

scale_label <- function(p) {
  switch(p$type,
         rating     = sprintf("%g–%g", p$scale[1], p$scale[2]),
         text_scale = TEXT_SCALE_LABELS[[p$kind]],
         category   = "Categories",
         free_text  = "Free text",
         number     = "Number",
         empty      = "No answers",
         "")
}

# A scale written in the heading, e.g. "Recommend_0_10", "Score (1-5)", "1 to 7"
header_scale_hint <- function(original) {
  x <- tolower(original)
  m <- regmatches(x, regexec("(^|[^0-9])(0|1)[ _-]+(to[ _-]+)?(5|7|10)[^0-9]*$", x))[[1]]
  if (length(m) == 0) return(NULL)
  c(as.numeric(m[3]), as.numeric(m[5]))
}

# Net-promoter style questions are conventionally 0-10
looks_like_nps <- function(original) {
  grepl("nps|recommend|likely to refer|likelihood to refer", tolower(original))
}

scale_key <- function(s) paste(s, collapse = "-")

# Settle the scale of each rating question and flag answers outside it.
#   1. a scale written in the heading wins, if the answers fit it
#   2. a 0/1-10 "recommend" question is read as 0-10 (the NPS convention)
#   3. otherwise the question uses the scale most questions in the file use,
#      unless its answers clearly need a wider one
check_scales <- function(df, columns) {
  rating_cols <- columns$clean[columns$role == "question" & columns$type == "rating"]
  out_of_range <- data.frame(column = character(0), row = integer(0), value = numeric(0))
  if (length(rating_cols) == 0) return(list(columns = columns, out_of_range = out_of_range))

  fits <- function(v, s) mean(v >= s[1] & v <= s[2]) >= 0.95
  own <- list()
  fixed <- character(0)
  for (col in rating_cols) {
    i <- which(columns$clean == col)
    v <- df[[col]][!is.na(df[[col]])]
    hint <- header_scale_hint(columns$original[i])
    if (!is.null(hint) && fits(v, hint)) {
      own[[col]] <- hint
      fixed <- c(fixed, col)
    } else if (looks_like_nps(columns$original[i]) && max(v) <= 10 && max(v) > 7) {
      own[[col]] <- c(0, 10)
      fixed <- c(fixed, col)
    } else {
      own[[col]] <- columns$scale[[i]]
    }
  }

  # The scale most questions use (ignoring questions with a written scale)
  pool <- setdiff(rating_cols, fixed)
  common <- NULL
  if (length(pool) > 0) {
    keys <- vapply(own[pool], scale_key, character(1))
    top <- names(sort(table(keys), decreasing = TRUE))[1]
    common <- own[[pool[which(keys == top)[1]]]]
  }

  for (col in rating_cols) {
    v <- df[[col]]
    i <- which(columns$clean == col)
    present <- v[!is.na(v)]
    s <- own[[col]]
    if (!col %in% fixed && !is.null(common) && fits(present, common)) s <- common
    columns$scale[[i]] <- s
    columns$scale_text[i] <- sprintf("%g\u2013%g", s[1], s[2])
    bad <- which(!is.na(v) & (v < s[1] | v > s[2]))
    if (length(bad) > 0) {
      out_of_range <- rbind(out_of_range, data.frame(column = col, row = bad, value = v[bad]))
    }
    columns$n_out_of_range[i] <- length(bad)
  }
  list(columns = columns, out_of_range = out_of_range)
}


# ---- Negatively worded questions --------------------------------
# For "I often think about leaving", agreeing is UNfavourable, so the scoring
# must be reversed. A question is treated as negatively worded when it has a
# negative cue OR a negation, but not both (double negatives such as
# "Mistakes are not held against me" are positive).

NEGATIVE_CUES <- c("leav", "quit", "resign", "another job", "other job", "new job",
                   "look job", "looking for job", "job search", "burn", "stress",
                   "exhaust", "overwhelm", "frustrat", "unfair", "discriminat", "harass",
                   "bully", "bullied", "toxic", "micromanag", "too much", "excessive",
                   "overload", "overwork", "afraid", "fear", "worri", "worry", "anxious",
                   "unsafe", "held against", "blame", "ignored", "undervalued",
                   "unappreciated", "unclear", "confus", "lack", "difficult", "dread",
                   "favourit", "favorit", "politic", "gossip")

# "leave" that is about time off, not leaving the job
LEAVE_ALLOWANCE <- c("polic", "parental", "annual", "sick", "maternity", "paternity",
                     "benefit", "entitle", "holiday", "vacation", "casual")

NEGATION_PATTERN <- "\\b(not|never|rarely|seldom|no|without|hardly|cannot|nobody|none)\\b|n't\\b|\\bdont\\b"

is_negatively_worded <- function(original) {
  toks <- header_tokens(original)
  if (length(toks) == 0) return(FALSE)
  cue_hits <- vapply(NEGATIVE_CUES, function(cue) {
    phrase_matches(toks, strsplit(cue, " ", fixed = TRUE)[[1]])
  }, logical(1))
  if (cue_hits[["leav"]] && any(vapply(LEAVE_ALLOWANCE, function(w) any(startsWith(toks, w)),
                                       logical(1)))) {
    cue_hits[["leav"]] <- FALSE
  }
  has_cue <- any(cue_hits)
  has_negation <- grepl(NEGATION_PATTERN, tolower(original), perl = TRUE)
  xor(has_cue, has_negation)
}


# ---- Main entry point -------------------------------------------
# Adds a survey-match verdict, a column map and scale checks to a
# result from process_file(). Data are never altered.
assess_file <- function(result, selected) {
  if (result$status == "error" || is.null(result$data)) return(result)

  df <- result$data
  cmap <- result$column_map
  s_label <- get_survey(selected)$label

  profiles <- lapply(cmap$clean, function(c) profile_answers(df[[c]]))
  columns <- data.frame(
    clean = cmap$clean,
    original = cmap$original,
    type = vapply(profiles, `[[`, character(1), "type"),
    stringsAsFactors = FALSE
  )
  columns$role <- mapply(column_role, columns$clean, columns$original, profiles,
                         USE.NAMES = FALSE)
  columns$scale <- lapply(profiles, function(p) if (!is.null(p$scale)) p$scale else NA)
  columns$scale_text <- vapply(profiles, scale_label, character(1))
  columns$n_out_of_range <- 0L
  columns$concept <- vapply(seq_len(nrow(columns)), function(i) {
    if (columns$role[i] != "question") return(NA_character_)
    hits <- matched_concepts(header_tokens(columns$original[i]), selected)
    if (length(hits) == 0 && selected == "pulse_survey") {
      toks <- header_tokens(columns$original[i])
      hits <- c(matched_concepts(toks, "employee_engagement"),
                matched_concepts(toks, "employee_satisfaction"))
    }
    pick_concept(hits, header_tokens(columns$original[i]))
  }, character(1))
  columns$driver <- FALSE

  # Questions that belong to another area keep that area's label (e.g.
  # "Manager support"), so broad surveys read sensibly
  label_from_other <- function(i) {
    toks <- header_tokens(columns$original[i])
    for (s in setdiff(DRIVER_LABEL_ORDER, selected)) {
      h <- matched_concepts(toks, s)
      if (length(h) > 0) return(pick_concept(h, toks))
    }
    NA_character_
  }

  sc <- check_scales(df, columns)
  columns <- sc$columns

  # A short numeric column that means nothing in any survey type and whose
  # numbers do not fit the file's answer scale is an HR record, not a question
  # (e.g. "Absence_Days" holding 0-20 in a 1-5 survey)
  q_scales <- columns$scale_text[columns$role == "question" & columns$type == "rating"]
  main_scale <- if (length(q_scales) > 0) names(sort(table(q_scales), decreasing = TRUE))[1] else NA
  any_meaning <- function(h) {
    toks <- header_tokens(h)
    any(vapply(names(SURVEY_CONCEPTS), function(sv) length(matched_concepts(toks, sv)) > 0, logical(1)))
  }
  moved <- which(columns$role == "question" & columns$type == "rating" &
                   lengths(lapply(columns$original, header_tokens)) <= 5 &
                   (columns$n_out_of_range > 0 | columns$scale_text != main_scale) &
                   !vapply(columns$original, any_meaning, logical(1)) &
                   !vapply(columns$original, looks_like_nps, logical(1)))
  if (length(moved) > 0) {
    columns$role[moved] <- "record"
    columns$n_out_of_range[moved] <- 0L
    sc <- check_scales(df, columns)
    columns <- sc$columns
  }
  records <- columns$role == "record"
  columns$scale_text[records] <- "Number"

  rel <- assess_relevance(columns, selected)

  # Negatively worded rating questions are reverse-scored (the user can undo this)
  columns$reversed <- vapply(seq_len(nrow(columns)), function(i) {
    columns$role[i] == "question" && columns$type[i] %in% c("rating", "text_scale") &&
      !(looks_like_nps(columns$original[i]) && identical(columns$scale_text[i], "0\u201310")) &&
      is_negatively_worded(columns$original[i])
  }, logical(1))

  # Questions that belong to another area keep that area's label (e.g.
  # "Manager support"); in broad surveys they are the drivers
  if (rel$decision == "match") {
    for (i in which(columns$role == "question" & is.na(columns$concept))) {
      lbl <- label_from_other(i)
      if (!is.na(lbl)) {
        columns$concept[i] <- lbl
        columns$driver[i] <- isTRUE(rel$broad)
      }
    }
  }

  issues <- result$issues

  # --- Survey match messages
  if (rel$decision == "match" && isTRUE(rel$broad)) {
    issues <- add_issue(issues, "info", sprintf(
      "Matches %s: %d core question%s plus %d driver question%s on related areas (such as management, growth or recognition), which are analysed as drivers.",
      s_label, rel$n_matched, if (rel$n_matched == 1) "" else "s",
      rel$n_drivers, if (rel$n_drivers == 1) "" else "s"))
  } else if (rel$decision == "match") {
    issues <- add_issue(issues, "info", sprintf(
      "Matches %s: %d of %d questions recognised.", s_label, rel$n_matched, rel$n_questions))
  } else if (rel$decision == "mismatch") {
    other <- get_survey(rel$suggested)$label
    issues <- add_issue(issues, "error", sprintf(
      "These look like %s questions, not %s: %d of %d questions fit %s and %d fit %s. Choose %s instead, or check the file.",
      other, s_label, rel$n_suggested_matched, rel$n_questions, other,
      rel$n_matched, s_label, other))

    # Show what each question fits in the survey type it resembles
    for (i in which(columns$role == "question")) {
      hits <- matched_concepts(header_tokens(columns$original[i]), rel$suggested)
      if (length(hits) > 0) columns$concept[i] <- paste0(other, ": ", pick_concept(hits, header_tokens(columns$original[i])))
    }
  } else if (rel$decision == "coded_headers") {
    issues <- add_issue(issues, "warning",
      "The column headings are codes (e.g. Q1, Q2) rather than question wording, so the survey type cannot be confirmed. You will be able to label the questions in the next step.")
  } else if (rel$decision == "unclear") {
    issues <- add_issue(issues, "warning", sprintf(
      "Only %d of %d questions clearly relate to %s. You will be asked to confirm before analysis.",
      rel$n_matched, rel$n_questions, s_label))
  } else if (rel$decision == "unrecognised") {
    issues <- add_issue(issues, "warning", sprintf(
      "None of the column headings could be clearly linked to %s or another survey type. You will be asked to confirm before analysis.",
      s_label))
  } else if (rel$decision == "no_questions") {
    issues <- add_issue(issues, "error",
      "No survey questions were found — only respondent details, IDs or comments.")
  }

  # --- Scale messages
  rq <- columns[columns$role == "question", , drop = FALSE]
  if (nrow(rq) > 0) {
    tab <- sort(table(rq$scale_text), decreasing = TRUE)
    issues <- add_issue(issues, "info", paste0(
      "Answer scale: ",
      paste(sprintf("%s (%d question%s)", names(tab), as.integer(tab),
                    ifelse(as.integer(tab) == 1, "", "s")), collapse = "; "), "."))
  }

  rec <- columns$original[columns$role == "record"]
  if (length(rec) > 0) {
    issues <- add_issue(issues, "info", sprintf(
      "%d column%s %s employee record%s rather than survey answer%s, so %s used to compare groups instead of being scored: %s.",
      length(rec), if (length(rec) == 1) "" else "s", if (length(rec) == 1) "looks like an" else "look like",
      if (length(rec) == 1) "" else "s", if (length(rec) == 1) "" else "s",
      if (length(rec) == 1) "it is" else "they are",
      paste(vapply(rec, quote_q, character(1)), collapse = ", ")))
  }

  rev_q <- columns$original[columns$reversed]
  if (length(rev_q) > 0) {
    issues <- add_issue(issues, "info", sprintf(
      "%d question%s %s worded negatively, so agreeing is unfavourable and the scoring is reversed: %s. You can undo this before the analysis.",
      length(rev_q), if (length(rev_q) == 1) "" else "s", if (length(rev_q) == 1) "is" else "are",
      paste(vapply(utils::head(rev_q, 5), quote_q, character(1)), collapse = "; ")))
  }

  # Questions whose answers are words the app cannot place on a scale
  unscored <- rq[rq$type == "category", , drop = FALSE]
  if (nrow(unscored) > 0) {
    examples <- vapply(seq_len(min(3, nrow(unscored))), function(k) {
      vals <- unique(stats::na.omit(df[[unscored$clean[k]]]))
      sprintf("%s (answers such as %s)", quote_q(unscored$original[k]),
              paste(sprintf("\u201C%s\u201D", utils::head(vals, 3)), collapse = ", "))
    }, character(1))
    issues <- add_issue(issues, "info", sprintf(
      "%d question%s %s answered with labels that are not a rating scale, so %s shown in the data but not scored: %s%s.",
      nrow(unscored), if (nrow(unscored) == 1) "" else "s",
      if (nrow(unscored) == 1) "is" else "are", if (nrow(unscored) == 1) "it is" else "they are",
      paste(examples, collapse = "; "), if (nrow(unscored) > 3) "; ..." else ""))
  }

  oor <- sc$out_of_range
  if (nrow(oor) > 0) {
    detail <- vapply(seq_len(min(nrow(oor), 6)), function(k) {
      i <- which(columns$clean == oor$column[k])
      sprintf("\"%s\" = %g in row %d (scale %s)", columns$original[i], oor$value[k],
              result$src_rows[oor$row[k]], columns$scale_text[i])
    }, character(1))
    issues <- add_issue(issues, "warning", sprintf(
      "%d answer%s fall%s outside the answer scale: %s%s. They are left unchanged and will be excluded from scores.",
      nrow(oor), if (nrow(oor) == 1) "" else "s", if (nrow(oor) == 1) "s" else "",
      paste(detail, collapse = "; "), if (nrow(oor) > 6) "; ..." else ""))
  }

  # --- Final status
  status <- if (any(issues$level == "error")) "error"
            else if (rel$decision %in% c("unclear", "unrecognised", "coded_headers")) "review"
            else if (any(issues$level == "warning")) "warning"
            else "ok"

  result$issues <- issues
  result$status <- status
  result$columns <- columns
  result$relevance <- rel
  result$out_of_range <- oor
  result$n_questions <- rel$n_questions
  result$scale_summary <- if (nrow(rq) > 0) paste(unique(rq$scale_text), collapse = ", ") else NA
  result
}


# =============================================================
# 7. Combining files into analysis datasets (Step 6b)
#
# Files are combined only when they have the SAME questions on the SAME
# answer scale. Anything else needs the user's decision:
#   * same questions, different scale  -> analyse separately, or leave out
#   * different questions              -> analyse separately, or leave out
#   * file marked "Needs review"       -> include, or leave out
# Questions are matched by their heading. Files whose questions are worded
# differently are kept apart rather than assumed to be equivalent.
# =============================================================

# A file's "fingerprint": which questions it has, and the scale of each
file_signature <- function(r) {
  q <- r$columns[r$columns$role == "question", , drop = FALSE]
  q <- q[order(q$clean), , drop = FALSE]
  list(
    key        = paste(paste(q$clean, q$scale_text, sep = "="), collapse = "|"),
    questions  = q$clean,
    scale_text = paste(unique(q$scale_text), collapse = ", ")
  )
}

# Put files with identical fingerprints together, largest group first
group_files <- function(results) {
  if (length(results) == 0) return(list())
  sigs <- lapply(results, file_signature)
  keys <- vapply(sigs, `[[`, character(1), "key")
  groups <- lapply(unique(keys), function(k) {
    members <- results[keys == k]
    sig <- sigs[[which(keys == k)[1]]]
    list(key = k,
         questions = sig$questions,
         scale_text = sig$scale_text,
         files = vapply(members, `[[`, character(1), "name"),
         n_rows = sum(vapply(members, function(m) as.numeric(m$n_rows), numeric(1))))
  })
  groups[order(-vapply(groups, `[[`, numeric(1), "n_rows"))]
}

# Name a list of files in a sentence: "a.csv", "a.csv and b.csv", ...
file_phrase <- function(files) {
  files <- paste0("“", files, "”")
  if (length(files) == 1) return(files)
  paste(paste(files[-length(files)], collapse = ", "), "and", files[length(files)])
}

# Work out which datasets can be formed and which decisions the user must make
plan_datasets <- function(processed, selected) {
  label <- get_survey(selected)$label
  usable <- Filter(function(r) r$status %in% c("ok", "warning"), processed)
  review <- Filter(function(r) r$status == "review", processed)

  groups <- group_files(usable)
  decisions <- list()
  add_decision <- function(kind, files, title, message, choices) {
    decisions[[length(decisions) + 1]] <<- list(
      id = paste0("d", length(decisions) + 1), kind = kind, files = files,
      title = title, message = message, choices = choices)
  }

  separate_choices <- c("Analyse as a separate dataset" = "separate",
                        "Leave out of the analysis" = "exclude")

  if (length(groups) > 1) {
    main <- groups[[1]]
    for (g in groups[-1]) {
      verb_s <- if (length(g$files) == 1) "s" else ""
      if (identical(sort(g$questions), sort(main$questions))) {
        add_decision("scale", g$files,
          title = paste("Different answer scale:", paste(g$files, collapse = ", ")),
          message = sprintf(
            "%s use%s a %s scale, but %s use%s %s. Answers on different scales mean different things, so they will not be combined.",
            file_phrase(g$files), verb_s, g$scale_text, file_phrase(main$files),
            if (length(main$files) == 1) "s" else "", main$scale_text),
          choices = separate_choices)
      } else {
        shared <- length(intersect(g$questions, main$questions))
        add_decision("questions", g$files,
          title = paste("Different questions:", paste(g$files, collapse = ", ")),
          message = sprintf(
            "%s ha%s a different set of questions from %s (%d of %d questions in common), so the files will not be combined.",
            file_phrase(g$files), if (length(g$files) == 1) "s" else "ve",
            file_phrase(main$files), shared, length(g$questions)),
          choices = separate_choices)
      }
    }
  }

  for (r in review) {
    rel <- r$relevance
    reason <- switch(rel$decision,
      unclear = sprintf("Only %d of its %d questions clearly relate to %s.",
                        rel$n_matched, rel$n_questions, label),
      coded_headers = "Its column headings are codes (e.g. Q1, Q2), so the questions cannot be checked.",
      sprintf("Its column headings could not be linked to %s.", label))
    add_decision(if (rel$decision == "coded_headers") "label" else "review", r$name,
      title = paste("Confirm survey type:", r$name),
      message = paste(reason, sprintf("Is this file really %s responses?", label)),
      choices = setNames(c("include", "exclude"),
                         c(sprintf("Yes \u2014 include it as %s", label),
                           "No \u2014 leave it out")))
    if (rel$decision == "coded_headers") {
      q <- r$columns[r$columns$role == "question", c("clean", "original"), drop = FALSE]
      decisions[[length(decisions)]]$questions <- q
      decisions[[length(decisions)]]$label_choices <- label_choices_for(selected)
    }
  }

  # Negatively worded questions: confirm the reversal (all ticked = reversed)
  candidates <- c(usable, review)
  rev_cols <- unlist(lapply(candidates, function(r) r$columns$clean[r$columns$reversed]))
  rev_text <- unlist(lapply(candidates, function(r) r$columns$original[r$columns$reversed]))
  if (length(rev_cols) > 0) {
    keep <- !duplicated(rev_cols)
    add_decision("reverse", character(0),
      title = "Negatively worded questions",
      message = "For these questions, agreeing is a bad sign (for example \u201CI often think about leaving\u201D), so the app reverses their scoring. Untick any question where agreeing is actually positive.",
      choices = setNames(rev_cols[keep], rev_text[keep]))
  }

  list(groups = groups, decisions = decisions)
}

# Areas a user can pick for a coded question: the survey's own areas first,
# then common driver areas, then "General" and "Ignore"
label_choices_for <- function(selected) {
  own <- vapply(SURVEY_CONCEPTS[[selected]], `[[`, character(1), "label")
  drivers <- c("Manager support", "Leadership (overall)", "Communication",
               "Career opportunities", "Pay and recognition", "Working conditions",
               "Workload", "Belonging", "Inclusion", "Collaboration", "Wellbeing")
  drivers <- setdiff(drivers, own)
  c(stats::setNames("General", "General / not sure"),
    stats::setNames(own, own),
    stats::setNames(drivers, paste0(drivers, " (driver)")),
    stats::setNames("Ignore", "Not a survey question \u2014 ignore"))
}

# Stack the working copies of several compatible files
combine_files <- function(results) {
  parts <- lapply(results, function(r) {
    df <- r$data
    # Answers outside the scale are excluded from scores (originals untouched)
    oor <- r$out_of_range
    if (!is.null(oor) && nrow(oor) > 0) {
      for (k in seq_len(nrow(oor))) df[oor$row[k], oor$column[k]] <- NA
    }
    # Respondent details may be stored differently in different files
    for (cn in r$columns$clean[r$columns$role != "question"]) df[[cn]] <- as.character(df[[cn]])
    df$source_file <- r$name
    df
  })
  # A column that is a number in one file and text in another becomes text
  for (cn in unique(unlist(lapply(parts, names)))) {
    types <- unique(unlist(lapply(parts, function(p) if (cn %in% names(p)) class(p[[cn]])[1])))
    if (length(types) > 1) {
      parts <- lapply(parts, function(p) {
        if (cn %in% names(p)) p[[cn]] <- as.character(p[[cn]])
        p
      })
    }
  }
  as.data.frame(dplyr::bind_rows(parts), stringsAsFactors = FALSE)
}

# Turn the plan plus the user's answers into analysis datasets
build_datasets <- function(processed, plan, answers = list()) {
  answer_for <- function(d) {
    a <- answers[[d$id]]
    if (is.null(a)) "exclude" else a
  }

  keep_groups <- if (length(plan$groups) > 0) plan$groups[1] else list()
  left_out <- character(0)

  for (d in plan$decisions) {
    choice <- answer_for(d)
    if (d$kind %in% c("scale", "questions")) {
      g <- Filter(function(g) identical(g$files, d$files), plan$groups)[[1]]
      if (choice == "separate") keep_groups[[length(keep_groups) + 1]] <- g
      else left_out <- c(left_out, d$files)
    }
  }

  # Coded questions the user labelled
  for (d in plan$decisions) {
    if (d$kind == "label" && answer_for(d) == "include") {
      labs <- answers[[paste0(d$id, "_labels")]]
      cols <- processed[[d$files]]$columns
      for (cn in names(labs)) {
        i <- which(cols$clean == cn)
        if (labs[[cn]] == "Ignore") {
          cols$role[i] <- "system"
        } else if (labs[[cn]] != "General") {
          cols$concept[i] <- labs[[cn]]
          cols$original[i] <- paste0(cols$original[i], " \u2014 ", labs[[cn]])
        }
      }
      processed[[d$files]]$columns <- cols
    }
  }

  # Files the user confirmed: join a matching group, or form their own
  confirmed <- character(0)
  for (d in plan$decisions) {
    if (d$kind %in% c("review", "label")) {
      if (answer_for(d) == "include") confirmed <- c(confirmed, d$files)
      else left_out <- c(left_out, d$files)
    }
  }
  for (name in confirmed) {
    key <- file_signature(processed[[name]])$key
    hit <- which(vapply(keep_groups, function(g) identical(g$key, key), logical(1)))
    if (length(hit) > 0) {
      keep_groups[[hit[1]]]$files <- c(keep_groups[[hit[1]]]$files, name)
    } else {
      sig <- file_signature(processed[[name]])
      keep_groups[[length(keep_groups) + 1]] <- list(
        key = key, questions = sig$questions, scale_text = sig$scale_text,
        files = name, n_rows = as.numeric(processed[[name]]$n_rows))
    }
  }

  # Which negatively worded questions stay reversed (default: all of them)
  rev_decision <- Filter(function(d) d$kind == "reverse", plan$decisions)
  kept_reversed <- if (length(rev_decision) == 0) character(0) else {
    a <- answers[[rev_decision[[1]]$id]]
    if (is.null(a) && !rev_decision[[1]]$id %in% names(answers)) unname(rev_decision[[1]]$choices) else a
  }

  datasets <- lapply(seq_along(keep_groups), function(k) {
    g <- keep_groups[[k]]
    members <- processed[g$files]
    data <- combine_files(members)
    qcols <- members[[1]]$columns
    qcols <- qcols[qcols$role == "question", , drop = FALSE]
    qcols$reversed <- qcols$clean %in% kept_reversed
    answered <- vapply(qcols$clean, function(cn) sum(!is.na(data[[cn]])), integer(1))
    list(
      id = k,
      files = g$files,
      data = data,
      questions = data.frame(clean = qcols$clean, question = qcols$original,
                             concept = qcols$concept, scale = qcols$scale_text,
                             reversed = qcols$reversed,
                             answered = unname(answered), stringsAsFactors = FALSE),
      scale_text = g$scale_text,
      records = members[[1]]$columns$clean[members[[1]]$columns$role == "record"],
      n_responses = nrow(data),
      n_excluded = sum(vapply(members, function(r) nrow(r$out_of_range), integer(1))),
      n_repeats = sum(vapply(members, function(r) as.integer(r$n_duplicates), integer(1))),
      pct_missing = round(100 * mean(is.na(as.matrix(data[, qcols$clean, drop = FALSE]))), 1)
    )
  })

  list(datasets = datasets, left_out = left_out)
}
