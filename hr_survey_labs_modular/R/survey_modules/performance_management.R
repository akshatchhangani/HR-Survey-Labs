# =============================================================
# HR Labs — Performance Management module
# Every question is part of the index. When there is no single outcome
# question, each area is linked with the average of all the other questions
# to show which areas move most with the overall result.
#
# The analysis itself is the shared engine build_survey_analysis() in
# analytics_common.R; this file holds what is specific to this survey.
# Explanations are hypotheses to check, not conclusions.
# =============================================================

PERFORMANCE_MANAGEMENT_GUIDE <- list(
  "Goal clarity" = list(
    explain = "goals may be set late, change often, or not be linked to team and company priorities.",
    action  = "Set goals at the start of each cycle and show how each goal links to team and company priorities.",
    owner   = "Line managers + HR"),
  "Feedback quality" = list(
    explain = "feedback may be rare, vague, or saved for the annual review.",
    action  = "Hold regular one-to-ones with specific, timely feedback, and train managers to give it well.",
    owner   = "Line managers"),
  "Appraisal fairness" = list(
    explain = "ratings may be seen as inconsistent between managers, biased, or not linked to results.",
    action  = "Calibrate ratings across teams, explain the rating criteria, and offer a way to discuss a rating.",
    owner   = "HR + line managers"),
  "Review process" = list(
    explain = "the review process may feel like paperwork rather than a useful conversation.",
    action  = "Simplify the review form and focus the conversation on progress, development and next steps.",
    owner   = "HR"),
  "Performance and development" = list(
    explain = "reviews may focus on ratings, with little discussion of development or growth.",
    action  = "Make a development plan part of every review and follow up on it during the year.",
    owner   = "Line managers + L&D"),
  "Recognition of performance" = list(
    explain = "good performance may not be visibly recognised or rewarded.",
    action  = "Link rewards and recognition clearly to performance, and recognise strong work as it happens.",
    owner   = "Line managers + Compensation & Benefits")
)

PERFORMANCE_MANAGEMENT_CONFIG <- list(
  noun = "performance management", Noun = "Performance management", index_label = "Performance management index",
  people = c(fav = "positive", neutral = "neutral", unfav = "negative"),
  core_labels = vapply(SURVEY_CONCEPTS$performance_management, `[[`, character(1), "label"),
  outcomes = list(),
  no_outcome_text = "there are fewer than four questions to compare",
  item_rest = TRUE,
  guide = make_guide(PERFORMANCE_MANAGEMENT_GUIDE)
)

survey_module_performance_management <- function(ds, ...) {
  build_survey_analysis(ds, PERFORMANCE_MANAGEMENT_CONFIG)
}
