# =============================================================
# HR Labs — Compensation & Benefits module
# Every question is part of the index. When there is no single outcome
# question, each area is linked with the average of all the other questions
# to show which areas move most with the overall result.
#
# The analysis itself is the shared engine build_survey_analysis() in
# analytics_common.R; this file holds what is specific to this survey.
# Explanations are hypotheses to check, not conclusions.
# =============================================================

COMPENSATION_BENEFITS_GUIDE <- list(
  "Pay satisfaction" = list(
    explain = "pay may be felt to lag the market, or not to reflect effort and contribution.",
    action  = "Benchmark pay for the lowest-scoring roles and explain how pay decisions are made.",
    owner   = "Compensation & Benefits"),
  "Pay equity" = list(
    explain = "people may see differences in pay for similar work that are not explained.",
    action  = "Run a pay equity review across similar roles, correct unexplained gaps, and explain the principles used.",
    owner   = "Compensation & Benefits + HR leadership"),
  "Market competitiveness" = list(
    explain = "pay or benefits may have fallen behind what similar employers offer.",
    action  = "Benchmark the full package against the market for key roles and plan adjustments where it lags.",
    owner   = "Compensation & Benefits"),
  "Bonus and incentives" = list(
    explain = "the link between performance and bonus may be unclear, or targets may feel out of reach.",
    action  = "Explain how bonuses are calculated with worked examples, and check that targets are achievable.",
    owner   = "Compensation & Benefits + managers"),
  "Benefits awareness" = list(
    explain = "people may not know which benefits exist or how to use them.",
    action  = "Run a short benefits awareness campaign and give each employee a one-page summary of their benefits.",
    owner   = "Compensation & Benefits")
)

COMPENSATION_BENEFITS_CONFIG <- list(
  noun = "pay and benefits", Noun = "Pay and benefits", index_label = "Pay & benefits index",
  people = c(fav = "satisfied", neutral = "neutral", unfav = "dissatisfied"),
  core_labels = vapply(SURVEY_CONCEPTS$compensation_benefits, `[[`, character(1), "label"),
  outcomes = list(),
  no_outcome_text = "there are fewer than four questions to compare",
  item_rest = TRUE,
  guide = make_guide(COMPENSATION_BENEFITS_GUIDE)
)

survey_module_compensation_benefits <- function(ds, ...) {
  build_survey_analysis(ds, COMPENSATION_BENEFITS_CONFIG)
}
