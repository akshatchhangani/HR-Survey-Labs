# =============================================================
# HR Labs — Employee Engagement module
#
# Works for two common survey designs:
#   * classic: several engagement statements (pride, advocacy, intent to stay ...)
#   * driver-based: one or two engagement outcomes (e.g. "Engagement score",
#     "Recommend employer 0-10") plus driver questions (manager support,
#     growth, recognition, workload ...)
#
# The analysis itself is the shared engine build_survey_analysis() in
# analytics_common.R; this file holds what is specific to engagement:
# its words, its outcome questions and the guidance for its own areas.
# =============================================================

# What each area usually reflects, and a first action.
# Explanations are hypotheses to check, not conclusions.
# Guidance for the engagement outcome areas (common driver areas such as
# manager support or workload are in COMMON_GUIDE, analytics_common.R).
# Explanations are hypotheses to check, not conclusions.
ENGAGEMENT_GUIDE <- list(
  # --- engagement outcomes
  "Pride" = list(
    explain = "low pride often follows limited visibility of the organisation's achievements, a recent setback, or a gap between stated values and everyday experience.",
    action  = "Share wins, customer impact and recognition stories regularly, and ask teams what would make them prouder to work here."),
  "Advocacy" = list(
    explain = "people hold back recommendations when day-to-day experience (workload, management, pay or growth) does not match what the organisation says about itself.",
    action  = "Run short focus groups with the lowest-scoring teams to find what stops people recommending the organisation, then fix the top two issues they raise."),
  "Intent to stay" = list(
    explain = "low intent to stay is commonly linked to limited career growth, pay concerns or the relationship with the line manager. Exit-survey and pay data can confirm which applies.",
    action  = "Hold stay interviews with at-risk groups, make career paths and internal moves visible, and review pay where it lags the market."),
  "Discretionary effort" = list(
    explain = "extra effort drops when it goes unrecognised, when workload is already stretched, or when people see little link between effort and reward.",
    action  = "Review workload in the affected teams and make recognition for extra effort timely and specific."),
  "Motivation" = list(
    explain = "motivation tends to fall with unclear goals, little autonomy, repetitive work or lack of recognition.",
    action  = "Have managers discuss with each team member what energises them, and adjust goals, autonomy and recognition accordingly."),
  "Meaningful work" = list(
    explain = "people may not see how their role contributes to the organisation's purpose or to customers.",
    action  = "Connect each role to the organisation's purpose: show the impact of the team's work and involve people in setting priorities."),
  "Commitment" = list(
    explain = "commitment weakens when direction is unclear, when change is poorly explained, or when earlier survey feedback was not acted on.",
    action  = "Communicate where the organisation is heading and how teams fit in, and show visible action on this survey within 90 days."),
  "Engagement (overall)" = list(
    explain = "overall engagement usually reflects a combination of management, growth, recognition and workload. The driver analysis shows which of these matter most here.",
    action  = "Share results openly with every team and agree two or three team-level actions.")
)

ENGAGEMENT_CONFIG <- list(
  noun = "engagement", Noun = "Engagement", index_label = "Engagement index",
  people = c(fav = "engaged", neutral = "neutral", unfav = "disengaged"),
  core_labels = vapply(SURVEY_CONCEPTS$employee_engagement, `[[`, character(1), "label"),
  outcomes = list(list(concept = "Engagement (overall)", label = "engagement"),
                  list(concept = "Intent to stay", label = "intent to stay")),
  no_outcome_text = "no overall engagement or intent-to-stay question was recognised",
  guide = make_guide(ENGAGEMENT_GUIDE)
)

survey_module_employee_engagement <- function(ds, ...) {
  build_survey_analysis(ds, ENGAGEMENT_CONFIG)
}
