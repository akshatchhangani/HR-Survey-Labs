# =============================================================
# HR Survey Labs — shared insight helpers
# Every chart carries three short insights:
#   1. What does the chart show?        (facts from the data)
#   2. What might explain it?           (hypotheses, clearly labelled)
#   3. What should HR do next?          (a practical action)
# =============================================================

make_insight <- function(shows, explains, next_step) {
  list(shows = shows, explains = explains, next_step = next_step)
}

insight_ui <- function(ins) {
  row <- function(cls, label, text) {
    div(class = paste("insight-row", cls),
        span(class = "insight-label", label),
        p(class = "insight-text", text))
  }
  div(
    class = "insight",
    row("insight-shows",   "What it shows",        ins$shows),
    row("insight-explains","What might explain it", ins$explains),
    row("insight-next",    "What HR could do next", ins$next_step)
  )
}

# ---- Number and wording helpers ----

fmt_pct <- function(x) sprintf("%.0f%%", x)

# Plain-language level for a % favourable score
level_word <- function(pct) {
  if (is.na(pct)) return("unknown")
  if (pct >= 75) "strong" else if (pct >= 60) "moderate" else if (pct >= 45) "mixed" else "weak"
}

# Quote a question for use inside a sentence
quote_q <- function(x) paste0("“", x, "”")

# "a", "a and b", "a, b and c"
and_list <- function(x) {
  if (length(x) <= 1) return(paste(x, collapse = ""))
  paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])
}

# Strength of a correlation in words
link_word <- function(r) {
  a <- abs(r)
  if (is.na(a)) "unclear" else if (a >= 0.5) "strong" else if (a >= 0.3) "moderate" else "weak"
}
