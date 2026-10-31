# =============================================================
# HR Labs — Workplace Culture module
# Every question is part of the index. When there is no single outcome
# question, each area is linked with the average of all the other questions
# to show which areas move most with the overall result.
#
# The analysis itself is the shared engine build_survey_analysis() in
# analytics_common.R; this file holds what is specific to this survey.
# Explanations are hypotheses to check, not conclusions.
# =============================================================

WORKPLACE_CULTURE_GUIDE <- list(
  "Values" = list(
    explain = "people may see a gap between the stated values and the decisions that are actually made and rewarded.",
    action  = "Link each value to two or three concrete behaviours, and recognise people who show them.",
    owner   = "Leadership team + HR"),
  "Collaboration" = list(
    explain = "teams may work in silos, with competing goals or unclear ownership of shared work.",
    action  = "Set shared goals across teams and make it easy to ask other teams for help.",
    owner   = "Department heads"),
  "Trust and openness" = list(
    explain = "information may be shared late or selectively, or past openness may not have been met well.",
    action  = "Share decisions and their reasons openly, and respond constructively when people raise concerns.",
    owner   = "Leadership team"),
  "Behaviours" = list(
    explain = "poor behaviour may go unaddressed, or accountability may be applied unevenly across levels.",
    action  = "Agree clear behavioural standards and apply them consistently, including to senior people.",
    owner   = "HR + line managers"),
  "Work environment" = list(
    explain = "day-to-day pressure, conflict or a lack of respect may be shaping how people feel at work.",
    action  = "Ask the lowest-scoring teams what most affects their day-to-day environment, and fix the top issue first.",
    owner   = "HR business partners + team managers"),
  "Innovation" = list(
    explain = "new ideas may be slow to get approval, or failure may be seen as risky.",
    action  = "Create a simple route for new ideas, fund a few small experiments, and celebrate what is learned.",
    owner   = "Leadership team")
)

WORKPLACE_CULTURE_CONFIG <- list(
  noun = "culture", Noun = "Culture", index_label = "Culture index",
  people = c(fav = "positive", neutral = "neutral", unfav = "negative"),
  core_labels = vapply(SURVEY_CONCEPTS$workplace_culture, `[[`, character(1), "label"),
  outcomes = list(),
  no_outcome_text = "there are fewer than four questions to compare",
  item_rest = TRUE,
  guide = make_guide(WORKPLACE_CULTURE_GUIDE)
)

survey_module_workplace_culture <- function(ds, ...) {
  build_survey_analysis(ds, WORKPLACE_CULTURE_CONFIG)
}
