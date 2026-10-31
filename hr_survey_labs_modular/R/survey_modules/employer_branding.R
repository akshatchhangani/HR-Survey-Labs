# =============================================================
# HR Labs — Employer Branding module
# Advocacy (would recommend) is used as the outcome when the survey has it;
# otherwise each area is linked with the average of the other questions.
#
# The analysis itself is the shared engine build_survey_analysis() in
# analytics_common.R; this file holds what is specific to this survey.
# Explanations are hypotheses to check, not conclusions.
# =============================================================

EMPLOYER_BRANDING_GUIDE <- list(
  "Employer reputation" = list(
    explain = "the organisation may be little known, or what people hear about it may not match the inside view.",
    action  = "Share real employee stories about the work and culture on the channels candidates use.",
    owner   = "Employer branding + talent acquisition"),
  "Attractiveness" = list(
    explain = "what the organisation offers may not stand out against competitors for the roles that matter most.",
    action  = "Find out what the best candidates value most, and strengthen and communicate those parts of the offer.",
    owner   = "Talent acquisition + HR"),
  "Advocacy" = list(
    explain = "people hold back recommendations when day-to-day experience does not match what the organisation says about itself.",
    action  = "Ask employees what stops them recommending the organisation, fix the top issues, and then launch a referral programme.",
    owner   = "HR + employer branding"),
  "Value proposition" = list(
    explain = "the employee value proposition may be unclear, generic, or not delivered in practice.",
    action  = "Define the value proposition in a few concrete promises and check each year that employees experience them.",
    owner   = "HR leadership")
)

EMPLOYER_BRANDING_CONFIG <- list(
  noun = "employer brand", Noun = "Employer brand", index_label = "Employer brand index",
  people = c(fav = "positive", neutral = "neutral", unfav = "negative"),
  core_labels = vapply(SURVEY_CONCEPTS$employer_branding, `[[`, character(1), "label"),
  outcomes = list(list(concept = "Advocacy", label = "advocacy")),
  no_outcome_text = "there is no advocacy question and fewer than four questions to compare",
  item_rest = TRUE,
  guide = make_guide(EMPLOYER_BRANDING_GUIDE)
)

survey_module_employer_branding <- function(ds, ...) {
  build_survey_analysis(ds, EMPLOYER_BRANDING_CONFIG)
}
