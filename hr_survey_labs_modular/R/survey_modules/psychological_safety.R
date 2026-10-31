# =============================================================
# HR Labs — Psychological Safety module
# Every question is part of the index. When there is no single outcome
# question, each area is linked with the average of all the other questions
# to show which areas move most with the overall result.
#
# The analysis itself is the shared engine build_survey_analysis() in
# analytics_common.R; this file holds what is specific to this survey.
# Explanations are hypotheses to check, not conclusions.
# =============================================================

PSYCHOLOGICAL_SAFETY_GUIDE <- list(
  "Speaking up" = list(
    explain = "people may have seen others ignored or penalised for speaking up, or meetings may leave little room for other views.",
    action  = "Have managers invite views explicitly, thank people who raise issues, and show what changed as a result.",
    owner   = "Line managers"),
  "Mistakes" = list(
    explain = "mistakes may be met with blame rather than learning, so people hide them.",
    action  = "Run blame-free reviews of mistakes and have leaders share their own mistakes and what they learned.",
    owner   = "Line managers + HR"),
  "Asking for help" = list(
    explain = "asking for help may be seen as weakness, or people may be too stretched to help each other.",
    action  = "Make asking for help normal: managers can model it, and teams can set aside time to support each other.",
    owner   = "Team managers"),
  "Challenging ideas" = list(
    explain = "disagreeing with a manager or senior person may feel risky.",
    action  = "Use structured ways to challenge ideas, such as a devil's advocate role or anonymous input, and reward constructive disagreement.",
    owner   = "Line managers"),
  "Risk-taking and fear" = list(
    explain = "people may fear the consequences of taking a sensible risk, or remember past punishment.",
    action  = "Agree what a sensible risk looks like, protect people who take one, and address any fear of repercussions directly.",
    owner   = "Leadership team + HR")
)

PSYCHOLOGICAL_SAFETY_CONFIG <- list(
  noun = "psychological safety", Noun = "Psychological safety", index_label = "Psychological safety index",
  people = c(fav = "comfortable", neutral = "neutral", unfav = "uncomfortable"),
  core_labels = vapply(SURVEY_CONCEPTS$psychological_safety, `[[`, character(1), "label"),
  outcomes = list(),
  no_outcome_text = "there are fewer than four questions to compare",
  item_rest = TRUE,
  guide = make_guide(PSYCHOLOGICAL_SAFETY_GUIDE)
)

survey_module_psychological_safety <- function(ds, ...) {
  build_survey_analysis(ds, PSYCHOLOGICAL_SAFETY_CONFIG)
}
