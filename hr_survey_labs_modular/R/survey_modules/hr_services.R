# =============================================================
# HR Labs — HR Services module
# Every question is part of the index. When there is no single outcome
# question, each area is linked with the average of all the other questions
# to show which areas move most with the overall result.
#
# The analysis itself is the shared engine build_survey_analysis() in
# analytics_common.R; this file holds what is specific to this survey.
# Explanations are hypotheses to check, not conclusions.
# =============================================================

HR_SERVICES_GUIDE <- list(
  "HR (overall)" = list(
    explain = "views of HR usually reflect speed, accuracy and how easy it is to get an answer.",
    action  = "Share the results with the HR team and agree two or three service improvements with clear targets.",
    owner   = "Head of HR"),
  "Responsiveness" = list(
    explain = "queries may sit unanswered, or people may not know who to ask or when to expect a reply.",
    action  = "Set and publish response times for common requests, and track them every month.",
    owner   = "HR operations"),
  "Resolution" = list(
    explain = "requests may need several contacts, or issues may be closed without being fully solved.",
    action  = "Track first-contact resolution and review repeat requests to fix their root causes.",
    owner   = "HR operations"),
  "Service quality" = list(
    explain = "people may find HR staff hard to reach, unclear or inconsistent in their answers.",
    action  = "Train the HR team on the most common queries and give consistent written answers for them.",
    owner   = "HR operations"),
  "Policy clarity" = list(
    explain = "policies may be hard to find, written in complex language, or applied differently by different people.",
    action  = "Rewrite the most-used policies in plain language and put them in one easy-to-search place.",
    owner   = "HR policy owner")
)

HR_SERVICES_CONFIG <- list(
  noun = "HR service", Noun = "HR service", index_label = "HR service index",
  people = c(fav = "satisfied", neutral = "neutral", unfav = "dissatisfied"),
  core_labels = vapply(SURVEY_CONCEPTS$hr_services, `[[`, character(1), "label"),
  outcomes = list(),
  no_outcome_text = "there are fewer than four questions to compare",
  item_rest = TRUE,
  guide = make_guide(HR_SERVICES_GUIDE)
)

survey_module_hr_services <- function(ds, ...) {
  build_survey_analysis(ds, HR_SERVICES_CONFIG)
}
