# =============================================================
# HR Labs — Onboarding module
# Every question is part of the index. When there is no single outcome
# question, each area is linked with the average of all the other questions
# to show which areas move most with the overall result.
# Compare new-joiner groups by tenure or joining month where the file has them.
#
# The analysis itself is the shared engine build_survey_analysis() in
# analytics_common.R; this file holds what is specific to this survey.
# Explanations are hypotheses to check, not conclusions.
# =============================================================

ONBOARDING_GUIDE <- list(
  "Onboarding (overall)" = list(
    explain = "the onboarding experience usually reflects preparation before day one, a clear role and support in the first weeks.",
    action  = "Map the onboarding journey and fix the stage with the lowest scores first.",
    owner   = "HR onboarding owner"),
  "Early experience" = list(
    explain = "the first days or weeks may feel unplanned, with new joiners left waiting.",
    action  = "Give every new joiner a planned first week with named people, tasks and check-ins.",
    owner   = "Hiring managers + HR"),
  "Role clarity" = list(
    explain = "expectations may not be explained, or the role may differ from what was described when hiring.",
    action  = "Agree 30-60-90-day goals with every new joiner in their first week.",
    owner   = "Hiring managers"),
  "Readiness" = list(
    explain = "equipment, system access or training may not be ready on day one.",
    action  = "Use a pre-joining checklist so laptop, access and training are ready before the first day.",
    owner   = "IT + HR onboarding owner"),
  "Support and buddy" = list(
    explain = "new joiners may lack a go-to person or struggle to build relationships.",
    action  = "Assign a trained buddy to every new joiner and schedule introductions to key colleagues.",
    owner   = "Hiring managers"),
  "Joining process" = list(
    explain = "paperwork and joining formalities may be slow, repetitive or confusing.",
    action  = "Move joining paperwork online, ask for each document only once, and finish it before day one.",
    owner   = "HR operations")
)

ONBOARDING_CONFIG <- list(
  noun = "onboarding", Noun = "Onboarding", index_label = "Onboarding index",
  people = c(fav = "positive", neutral = "neutral", unfav = "negative"),
  core_labels = vapply(SURVEY_CONCEPTS$onboarding, `[[`, character(1), "label"),
  outcomes = list(),
  no_outcome_text = "there are fewer than four questions to compare",
  item_rest = TRUE,
  guide = make_guide(ONBOARDING_GUIDE)
)

survey_module_onboarding <- function(ds, ...) {
  build_survey_analysis(ds, ONBOARDING_CONFIG)
}
