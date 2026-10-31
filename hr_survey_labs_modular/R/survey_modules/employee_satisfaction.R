# =============================================================
# HR Labs — Employee Satisfaction module
#
# Works for two common survey designs:
#   * one overall question ("Overall, I am satisfied with my job") plus
#     questions on specific areas (pay, manager, growth, colleagues ...):
#     the areas are analysed as drivers of overall satisfaction
#   * only area questions: the satisfaction index is the average across them,
#     and each area is linked with the average of the other questions
#
# The analysis itself is the shared engine build_survey_analysis() in
# analytics_common.R; this file holds what is specific to satisfaction.
# Guidance for the areas (pay, benefits, recognition, growth, manager,
# colleagues, work-life balance, job security, policies, the work itself)
# is in COMMON_GUIDE, because the same areas appear in other surveys.
# =============================================================

SATISFACTION_GUIDE <- list(
  "Overall satisfaction" = list(
    explain = "overall satisfaction usually reflects the work itself, the manager, pay and working conditions together; the driver analysis shows which matter most here.",
    action  = "Share results openly with every team, and act first on the areas most closely linked with overall satisfaction."),
  "Working conditions" = list(
    explain = "the physical workplace, equipment or tools may be getting in the way of doing the job well.",
    action  = "Ask the lowest-scoring teams which conditions or tools get in their way, and fix the quick wins first."),
  "Communication" = list(
    explain = "people may hear about decisions late or second-hand, or without the reasons behind them.",
    action  = "Set a regular rhythm of team and company updates, and give people a simple way to ask questions.")
)

SATISFACTION_CONFIG <- list(
  noun = "satisfaction", Noun = "Satisfaction", index_label = "Satisfaction index",
  people = c(fav = "satisfied", neutral = "neutral", unfav = "dissatisfied"),
  core_labels = c("Overall satisfaction"),
  outcomes = list(list(concept = "Overall satisfaction", label = "overall satisfaction")),
  no_outcome_text = "no overall satisfaction question was recognised and there are fewer than four questions",
  item_rest = TRUE,
  guide = make_guide(SATISFACTION_GUIDE)
)

survey_module_employee_satisfaction <- function(ds, ...) {
  build_survey_analysis(ds, SATISFACTION_CONFIG)
}
