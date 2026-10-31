# =============================================================
# HR Survey Labs — survey registry
# The single source of truth for the 16 survey types.
# The home-screen cards and the module routing are both built from this list.
#
# Module functions are stored as NAMES (strings) and looked up only when
# needed, so the app still starts if a module is unfinished.
#
# Each survey has a line icon (SURVEY_ICONS), a category for grouping on
# the home screen, and a `ready` flag for whether its full analysis exists.
# =============================================================

# Line icons (24 x 24, drawn with strokes in the current text colour)
SURVEY_ICONS <- list(
  heart     = '<path d="M19 14c1.49-1.46 3-3.21 3-5.5A5.5 5.5 0 0 0 16.5 3c-1.76 0-3 .5-4.5 2-1.5-1.5-2.74-2-4.5-2A5.5 5.5 0 0 0 2 8.5c0 2.3 1.5 4.05 3 5.5l7 7Z"/>',
  smile     = '<circle cx="12" cy="12" r="10"/><path d="M8 14s1.5 2 4 2 4-2 4-2"/><path d="M9 9h.01"/><path d="M15 9h.01"/>',
  activity  = '<path d="M22 12h-4l-3 9L9 3l-3 9H2"/>',
  user_plus = '<path d="M16 21v-2a4 4 0 0 0-4-4H6a4 4 0 0 0-4 4v2"/><circle cx="9" cy="7" r="4"/><path d="M19 8v6"/><path d="M22 11h-6"/>',
  target    = '<circle cx="12" cy="12" r="10"/><circle cx="12" cy="12" r="6"/><circle cx="12" cy="12" r="2"/>',
  cap       = '<path d="M22 10 12 5 2 10l10 5 10-5Z"/><path d="M6 12v5c3 3 9 3 12 0v-5"/><path d="M22 10v6"/>',
  trend     = '<path d="M22 7 13.5 15.5 8.5 10.5 2 17"/><path d="M16 7h6v6"/>',
  compass   = '<circle cx="12" cy="12" r="10"/><path d="m16.24 7.76-2.12 6.36-6.36 2.12 2.12-6.36 6.36-2.12Z"/>',
  users     = '<path d="M16 21v-2a4 4 0 0 0-4-4H6a4 4 0 0 0-4 4v2"/><circle cx="9" cy="7" r="4"/><path d="M22 21v-2a4 4 0 0 0-3-3.87"/><path d="M16 3.13a4 4 0 0 1 0 7.75"/>',
  log_out   = '<path d="M9 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h4"/><path d="m16 17 5-5-5-5"/><path d="M21 12H9"/>',
  banknote  = '<rect x="2" y="6" width="20" height="12" rx="2"/><circle cx="12" cy="12" r="2"/><path d="M6 12h.01"/><path d="M18 12h.01"/>',
  layers    = '<path d="m12 2 10 5-10 5L2 7l10-5Z"/><path d="m2 17 10 5 10-5"/><path d="m2 12 10 5 10-5"/>',
  shield    = '<path d="M12 22s8-4 8-10V5l-8-3-8 3v7c0 6 8 10 8 10Z"/><path d="m9 12 2 2 4-4"/>',
  briefcase = '<rect x="2" y="7" width="20" height="14" rx="2"/><path d="M16 21V5a2 2 0 0 0-2-2h-4a2 2 0 0 0-2 2v16"/>',
  message   = '<path d="M21 15a2 2 0 0 1-2 2H7l-4 4V5a2 2 0 0 1 2-2h14a2 2 0 0 1 2 2Z"/>',
  award     = '<circle cx="12" cy="8" r="6"/><path d="M15.48 12.89 17 22l-5-3-5 3 1.52-9.11"/>',
  check     = '<path d="M20 6 9 17l-5-5"/>'
)

survey_icon <- function(name, size = 22) {
  HTML(sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">%s</svg>',
    size, size, SURVEY_ICONS[[name]]))
}

# The 16 survey types, in the order they appear on the home screen.
# `ready` = the full analysis module is built (the others show data checks only).
survey_registry <- list(
  list(key = "employee_engagement",     label = "Employee Engagement",          icon = "heart",
       category = "Engagement & experience", ready = TRUE,
       focus = "Engagement, advocacy, commitment, motivation and intent to stay.",
       fn = "survey_module_employee_engagement"),
  list(key = "employee_satisfaction",   label = "Employee Satisfaction",        icon = "smile",
       category = "Engagement & experience", ready = FALSE,
       focus = "Satisfaction with work, management, rewards, policies and the employee experience.",
       fn = "survey_module_employee_satisfaction"),
  list(key = "pulse_survey",            label = "Pulse Survey",                 icon = "activity",
       category = "Engagement & experience", ready = FALSE,
       focus = "Short, frequent checks on employee sentiment and changes over time.",
       fn = "survey_module_pulse_survey"),
  list(key = "workplace_culture",       label = "Workplace Culture",            icon = "layers",
       category = "Engagement & experience", ready = FALSE,
       focus = "Values, collaboration, behaviours, trust and everyday work environment.",
       fn = "survey_module_workplace_culture"),
  list(key = "candidate_experience",    label = "Candidate Experience",         icon = "briefcase",
       category = "Employee lifecycle", ready = FALSE,
       focus = "Recruitment communication, interview experience, transparency and hiring-process satisfaction.",
       fn = "survey_module_candidate_experience"),
  list(key = "onboarding",              label = "Onboarding",                   icon = "user_plus",
       category = "Employee lifecycle", ready = FALSE,
       focus = "New-hire experience, role clarity, training, support and readiness.",
       fn = "survey_module_onboarding"),
  list(key = "exit_survey",             label = "Exit Survey",                  icon = "log_out",
       category = "Employee lifecycle", ready = FALSE,
       focus = "Reasons for leaving, career opportunities, management issues and retention signals.",
       fn = "survey_module_exit_survey"),
  list(key = "employer_branding",       label = "Employer Branding",            icon = "award",
       category = "Employee lifecycle", ready = FALSE,
       focus = "Employer reputation, attractiveness, employee advocacy and perceived employer value proposition.",
       fn = "survey_module_employer_branding"),
  list(key = "training_needs_analysis", label = "Training Needs Analysis",      icon = "target",
       category = "Learning & performance", ready = FALSE,
       focus = "Skill gaps, capability requirements and priority training needs.",
       fn = "survey_module_training_needs_analysis"),
  list(key = "training_effectiveness",  label = "Training Effectiveness",       icon = "cap",
       category = "Learning & performance", ready = FALSE,
       focus = "Learning satisfaction, knowledge gains, application and perceived impact.",
       fn = "survey_module_training_effectiveness"),
  list(key = "performance_management",  label = "Performance Management",       icon = "trend",
       category = "Learning & performance", ready = FALSE,
       focus = "Goal clarity, feedback quality, appraisal fairness and performance review effectiveness.",
       fn = "survey_module_performance_management"),
  list(key = "leadership",              label = "Leadership",                   icon = "compass",
       category = "Learning & performance", ready = FALSE,
       focus = "Leadership trust, communication, coaching, support and decision-making.",
       fn = "survey_module_leadership"),
  list(key = "dei_inclusion",           label = "DEI and Inclusion",            icon = "users",
       category = "Inclusion, wellbeing & rewards", ready = FALSE,
       focus = "Belonging, inclusion, equal opportunity, respect and perceived fairness.",
       fn = "survey_module_dei_inclusion"),
  list(key = "psychological_safety",    label = "Psychological Safety",         icon = "shield",
       category = "Inclusion, wellbeing & rewards", ready = FALSE,
       focus = "Comfort speaking up, admitting mistakes, asking questions and challenging ideas.",
       fn = "survey_module_psychological_safety"),
  list(key = "compensation_benefits",   label = "Compensation & Benefits",      icon = "banknote",
       category = "Inclusion, wellbeing & rewards", ready = FALSE,
       focus = "Pay satisfaction, benefits awareness, perceived equity and reward competitiveness.",
       fn = "survey_module_compensation_benefits"),
  list(key = "hr_services",             label = "HR Services",                  icon = "message",
       category = "Inclusion, wellbeing & rewards", ready = FALSE,
       focus = "HR responsiveness, service quality, resolution experience and policy clarity.",
       fn = "survey_module_hr_services")
)

# Name each entry by its key so it can be looked up as survey_registry[["leadership"]]
names(survey_registry) <- vapply(survey_registry, `[[`, character(1), "key")

# ---- Helpers ----

# Return one survey's metadata, or NULL for an unknown key
get_survey <- function(key) {
  if (is.null(key) || !key %in% names(survey_registry)) return(NULL)
  survey_registry[[key]]
}

# TRUE if the survey's module function has been loaded
module_available <- function(key) {
  s <- get_survey(key)
  !is.null(s) && !is.null(get0(s$fn, mode = "function"))
}

# Run the selected survey's module on prepared data
run_survey_module <- function(key, data, ...) {
  s <- get_survey(key)
  if (is.null(s)) stop("Unknown survey type: ", key)
  fn <- get0(s$fn, mode = "function")
  if (is.null(fn)) stop("The ", s$label, " module has not been loaded yet.")
  fn(data, ...)
}

# ---- Startup check: runs once when app.R sources this file ----
local({
  if (length(survey_registry) != 16) {
    stop("survey_registry must contain exactly 16 survey types, found ",
         length(survey_registry))
  }
  missing <- names(survey_registry)[!vapply(names(survey_registry), module_available, logical(1))]
  if (length(missing) > 0) {
    message("[HR Survey Labs] Modules not loaded yet: ", paste(missing, collapse = ", "))
  } else {
    message("[HR Survey Labs] All 16 survey modules loaded.")
  }
})
