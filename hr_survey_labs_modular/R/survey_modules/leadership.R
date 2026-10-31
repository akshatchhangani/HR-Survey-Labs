# =============================================================
# HR Labs — Leadership module
# Every question is part of the index. When there is no single outcome
# question, each area is linked with the average of all the other questions
# to show which areas move most with the overall result.
#
# The analysis itself is the shared engine build_survey_analysis() in
# analytics_common.R; this file holds what is specific to this survey.
# Explanations are hypotheses to check, not conclusions.
# =============================================================

LEADERSHIP_GUIDE <- list(
  "Decision-making" = list(
    explain = "decisions may feel slow, inconsistent or unexplained, or people may not see the direction behind them.",
    action  = "Explain the reasoning and the direction behind major decisions, and say when the next update will come.",
    owner   = "Senior leadership team"),
  "Role modelling" = list(
    explain = "people may see leaders saying one thing and doing another.",
    action  = "Have leaders agree the two or three behaviours they will model visibly, and ask teams for feedback on them.",
    owner   = "Senior leadership team")
)

LEADERSHIP_CONFIG <- list(
  noun = "leadership", Noun = "Leadership", index_label = "Leadership index",
  people = c(fav = "positive", neutral = "neutral", unfav = "negative"),
  core_labels = vapply(SURVEY_CONCEPTS$leadership, `[[`, character(1), "label"),
  outcomes = list(),
  no_outcome_text = "there are fewer than four questions to compare",
  item_rest = TRUE,
  guide = make_guide(LEADERSHIP_GUIDE)
)

survey_module_leadership <- function(ds, ...) {
  build_survey_analysis(ds, LEADERSHIP_CONFIG)
}
