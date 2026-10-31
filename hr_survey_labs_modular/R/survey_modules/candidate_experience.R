# =============================================================
# HR Labs — Candidate Experience module
# Every question is part of the index. When there is no single outcome
# question, each area is linked with the average of all the other questions
# to show which areas move most with the overall result.
#
# The analysis itself is the shared engine build_survey_analysis() in
# analytics_common.R; this file holds what is specific to this survey.
# Explanations are hypotheses to check, not conclusions.
# =============================================================

CANDIDATE_EXPERIENCE_GUIDE <- list(
  "Application process" = list(
    explain = "the application may be long, repetitive or hard to complete on a phone.",
    action  = "Shorten the application, allow CV upload instead of retyping, and test it on a phone.",
    owner   = "Talent acquisition"),
  "Interview experience" = list(
    explain = "interviewers may be unprepared, inconsistent, or not show the organisation at its best.",
    action  = "Brief and train interviewers, use structured interviews, and ask candidates for feedback on each round.",
    owner   = "Talent acquisition + hiring managers"),
  "Recruiter communication" = list(
    explain = "candidates may not hear back for long periods or may not know what happens next.",
    action  = "Set a maximum time to update candidates at each stage and send a clear next-steps message after every round.",
    owner   = "Talent acquisition"),
  "Transparency" = list(
    explain = "candidates may not get clear information about the role, the process or the salary range.",
    action  = "Share the process, timeline and salary range early, and make the job description match the real role.",
    owner   = "Talent acquisition + hiring managers"),
  "Hiring process" = list(
    explain = "the process may be slow, with long gaps between rounds or a late offer.",
    action  = "Track time between stages, remove unnecessary rounds, and set a target time from first interview to offer.",
    owner   = "Talent acquisition")
)

CANDIDATE_EXPERIENCE_CONFIG <- list(
  noun = "candidate experience", Noun = "Candidate experience", index_label = "Candidate experience index",
  people = c(fav = "satisfied", neutral = "neutral", unfav = "dissatisfied"),
  core_labels = vapply(SURVEY_CONCEPTS$candidate_experience, `[[`, character(1), "label"),
  outcomes = list(),
  no_outcome_text = "there are fewer than four questions to compare",
  item_rest = TRUE,
  guide = make_guide(CANDIDATE_EXPERIENCE_GUIDE)
)

survey_module_candidate_experience <- function(ds, ...) {
  build_survey_analysis(ds, CANDIDATE_EXPERIENCE_CONFIG)
}
