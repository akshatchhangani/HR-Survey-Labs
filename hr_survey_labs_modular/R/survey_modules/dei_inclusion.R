# =============================================================
# HR Labs — DEI and Inclusion module
# Every question is part of the index. When there is no single outcome
# question, each area is linked with the average of all the other questions
# to show which areas move most with the overall result.
# Group comparisons are especially important here; groups under 5 are hidden.
#
# The analysis itself is the shared engine build_survey_analysis() in
# analytics_common.R; this file holds what is specific to this survey.
# Explanations are hypotheses to check, not conclusions.
# =============================================================

DEI_INCLUSION_GUIDE <- list(
  "Diversity" = list(
    explain = "people may see little diversity in teams or leadership, or feel diverse views are not used.",
    action  = "Review representation by level, set realistic goals, and make sure diverse perspectives are sought in decisions.",
    owner   = "DEI lead + leadership team"),
  "Equal opportunity" = list(
    explain = "promotions, projects or development may be seen as going to the same people, or criteria may be unclear.",
    action  = "Make the criteria for promotions and key projects transparent, and check outcomes by group.",
    owner   = "HR + talent team"),
  "Discrimination and bias" = list(
    explain = "some people may have experienced or seen unfair treatment, or not trust that reporting it will help.",
    action  = "Make reporting safe and simple, act visibly on reports, and train managers on bias in everyday decisions.",
    owner   = "HR + DEI lead")
)

DEI_INCLUSION_CONFIG <- list(
  noun = "inclusion", Noun = "Inclusion", index_label = "Inclusion index",
  people = c(fav = "positive", neutral = "neutral", unfav = "negative"),
  core_labels = vapply(SURVEY_CONCEPTS$dei_inclusion, `[[`, character(1), "label"),
  outcomes = list(),
  no_outcome_text = "there are fewer than four questions to compare",
  item_rest = TRUE,
  guide = make_guide(DEI_INCLUSION_GUIDE)
)

survey_module_dei_inclusion <- function(ds, ...) {
  build_survey_analysis(ds, DEI_INCLUSION_CONFIG)
}
