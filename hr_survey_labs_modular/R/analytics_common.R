# =============================================================
# HR Survey Labs — shared analytics
# Reusable scoring, comparison and chart helpers for all 16 modules.
#
# Favourable / neutral / unfavourable on any scale:
#   answers ABOVE the scale midpoint are favourable, AT the midpoint
#   neutral, BELOW it unfavourable.
#   1–5: 4–5 fav, 3 neutral, 1–2 unfav    1–7: 5–7 / 4 / 1–3
#   0–10: 6–10 / 5 / 0–4
# Text scales ("Strongly agree" ...) are placed on a 1–7 ladder first.
# =============================================================

# ---- Chart colours ----------------------------------------------
# Favourable / unfavourable is a diverging pair (blue <-> orange) with a
# grey neutral midpoint. Checked for colour-blind separation.
CHART_COLORS <- list(
  fav     = "#2B5C9E",
  neutral = "#D5D9DF",
  unfav   = "#C2571A",
  single  = "#2E4372",   # one-series bars
  ink     = "#1F2937",
  muted   = "#6B7280",
  grid    = "#E3E7ED"
)

# Groups smaller than this are never shown, to protect anonymity
MIN_GROUP_N <- 5

# Driver analysis needs at least this many complete responses
MIN_DRIVER_N <- 30


# ---- Turning answers into numbers -------------------------------

# Text answer ladders live in survey_mapping.R (TEXT_LADDERS) so detection
# and scoring always use the same labels.
TEXT_SCALE_LADDER <- TEXT_LADDERS

# Scale label (as shown in the app) -> text ladder name
ladder_for_label <- function(scale_text) {
  hit <- names(TEXT_SCALE_LABELS)[TEXT_SCALE_LABELS == scale_text]
  if (length(hit) == 0 || !hit %in% names(TEXT_SCALE_LADDER)) NULL else hit
}

# Returns list(values, min, max) or NULL when the question cannot be scored.
# Negatively worded questions are flipped (5 -> 1 on a 1-5 scale) so that a
# higher value always means a more favourable answer.
score_answers <- function(x, scale_text, reversed = FALSE) {
  m <- regmatches(scale_text, regexec("^(-?[0-9.]+)\u2013(-?[0-9.]+)$", scale_text))[[1]]
  if (length(m) == 3) {
    out <- list(values = suppressWarnings(as.numeric(x)),
                min = as.numeric(m[2]), max = as.numeric(m[3]))
  } else {
    ladder <- ladder_for_label(scale_text)
    if (is.null(ladder)) return(NULL)
    lad <- TEXT_SCALE_LADDER[[ladder]]
    out <- list(values = unname(lad[norm_label(x)]), min = 1, max = 7)
  }
  if (isTRUE(reversed)) out$values <- out$min + out$max - out$values
  out
}

is_reversed <- function(q, i) "reversed" %in% names(q) && isTRUE(q$reversed[i])

# Classify answers: "fav", "neutral", "unfav" (NA stays NA)
favourability <- function(values, min, max) {
  mid <- (min + max) / 2
  out <- ifelse(values > mid, "fav", ifelse(values < mid, "unfav", "neutral"))
  out[is.na(values)] <- NA
  out
}

# 0–100 version of a score, so different scales can be averaged
normalise <- function(values, min, max) 100 * (values - min) / (max - min)


# ---- Question-level summary -------------------------------------

# One row per scorable question: n, mean, % fav / neutral / unfav
score_questions <- function(ds) {
  q <- ds$questions
  rows <- lapply(seq_len(nrow(q)), function(i) {
    sc <- score_answers(ds$data[[q$clean[i]]], q$scale[i], is_reversed(q, i))
    if (is.null(sc)) return(NULL)
    v <- sc$values[!is.na(sc$values)]
    if (length(v) == 0) return(NULL)
    f <- favourability(v, sc$min, sc$max)
    data.frame(
      clean = q$clean[i],
      question = paste0(display_question(q$question[i]),
                        if (is_reversed(q, i)) " (reverse-scored)" else ""),
      original = q$question[i],
      is_nps = sc$min == 0 && sc$max == 10 && looks_like_nps(q$question[i]),
      concept = ifelse(is.na(q$concept[i]), "Other", q$concept[i]),
      n = length(v), mean = mean(v), scale_min = sc$min, scale_max = sc$max,
      mean_norm = mean(normalise(v, sc$min, sc$max)),
      pct_fav = 100 * mean(f == "fav"),
      pct_neutral = 100 * mean(f == "neutral"),
      pct_unfav = 100 * mean(f == "unfav"),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
}

# Respondent x question matrix of 0–100 scores (NA where unanswered)
normalised_matrix <- function(ds, cols) {
  q <- ds$questions[match(cols, ds$questions$clean), ]
  m <- vapply(seq_along(cols), function(i) {
    sc <- score_answers(ds$data[[cols[i]]], q$scale[i], is_reversed(q, i))
    normalise(sc$values, sc$min, sc$max)
  }, numeric(nrow(ds$data)))
  matrix(m, ncol = length(cols), dimnames = list(NULL, cols))
}

# Respondent x question matrix of "fav" / "neutral" / "unfav"
favourability_matrix <- function(ds, cols) {
  q <- ds$questions[match(cols, ds$questions$clean), ]
  m <- vapply(seq_along(cols), function(i) {
    sc <- score_answers(ds$data[[cols[i]]], q$scale[i], is_reversed(q, i))
    favourability(sc$values, sc$min, sc$max)
  }, character(nrow(ds$data)))
  matrix(m, ncol = length(cols), dimnames = list(NULL, cols))
}

# Respondent-level profile: each person's average answer (0–100).
# Someone is "favourable" when their average reaches the first favourable
# answer of the scale (4 on 1–5, 5 on 1–7), "unfavourable" when it is at or
# below the last unfavourable answer (2 on 1–5, 3 on 1–7), neutral otherwise.
# Needs at least half the questions answered.
respondent_profile <- function(ds, cols, scale_min, scale_max) {
  nm <- normalised_matrix(ds, cols)
  answered <- rowSums(!is.na(nm))
  avg <- ifelse(answered >= ceiling(length(cols) / 2), rowMeans(nm, na.rm = TRUE), NA)
  mid <- (scale_min + scale_max) / 2
  fav_cut   <- normalise(floor(mid) + 1, scale_min, scale_max)
  unfav_cut <- normalise(ceiling(mid) - 1, scale_min, scale_max)
  group <- ifelse(avg >= fav_cut, "fav", ifelse(avg <= unfav_cut, "unfav", "neutral"))
  data.frame(avg = avg, group = group, stringsAsFactors = FALSE)
}


# ---- Group comparisons ------------------------------------------

pretty_name <- function(clean_name) {
  tools::toTitleCase(gsub("_", " ", clean_name))
}

# Which respondent details to compare by, most useful first
DIMENSION_ORDER <- c("department", "dept", "function", "division", "business", "team", "unit",
                     "location", "region", "site", "branch", "office", "city",
                     "job_level", "level", "grade", "band", "tenure", "service", "experience",
                     "performance", "perf", "appraisal", "absen", "sick", "promotion",
                     "training", "overtime", "work_mode", "mode", "employment", "contract",
                     "age", "gender")

band_tenure <- function(v) {
  as.character(cut(v, c(-Inf, 1, 3, 5, 10, Inf), right = FALSE,
                   labels = c("Under 1 year", "1\u20133 years", "3\u20135 years",
                              "5\u201310 years", "10+ years")))
}

band_age <- function(v) {
  as.character(cut(v, c(-Inf, 25, 35, 45, 55, Inf), right = FALSE,
                   labels = c("Under 25", "25\u201334", "35\u201344", "45\u201354", "55+")))
}

# Band any numeric HR record into 3-4 groups of similar size.
# Few distinct values (e.g. performance rating 1-5, promotions 0-3) are kept as they are.
band_numeric <- function(v) {
  present <- v[!is.na(v)]
  if (length(present) == 0) return(as.character(v))
  vals <- sort(unique(present))
  if (length(vals) <= 6) return(ifelse(is.na(v), NA, format(v, trim = TRUE, drop0trailing = TRUE)))
  br <- unique(stats::quantile(present, c(0, 0.25, 0.5, 0.75, 1), names = FALSE, type = 1))
  if (length(br) < 3) br <- unique(c(min(present), stats::median(present), max(present)))
  idx <- findInterval(v, br[-length(br)], rightmost.closed = TRUE)
  idx[!is.na(v) & v == max(br)] <- length(br) - 1
  fmt <- function(x) format(round(x, 1), trim = TRUE, drop0trailing = TRUE)
  lows <- br[-length(br)]
  highs <- c(br[-c(1, length(br))], br[length(br)])
  labels <- vapply(seq_along(lows), function(k) {
    hi_vals <- present[present >= lows[k] & (if (k < length(lows)) present < lows[k + 1] else TRUE)]
    if (length(hi_vals) == 0) return(NA_character_)
    a <- min(hi_vals); b <- max(hi_vals)
    if (a == b) fmt(a) else paste0(fmt(a), "\u2013", fmt(b))
  }, character(1))
  out <- labels[idx]
  out[is.na(v)] <- NA
  out
}

# Respondent details worth comparing. Text details with 2-15 groups are used
# as they are; numeric tenure and age are put into bands.
group_dimensions <- function(ds, max_n = 4) {
  cand <- setdiff(names(ds$data), c(ds$questions$clean, "source_file"))
  cand <- cand[!grepl(ID_PATTERN, cand)]
  dims <- list()
  for (cn in cand) {
    x <- ds$data[[cn]]
    present <- x[!is.na(x)]
    if (length(present) == 0) next
    num <- suppressWarnings(as.numeric(as.character(present)))
    is_num <- mean(!is.na(num)) > 0.95
    all_num <- suppressWarnings(as.numeric(as.character(x)))
    label <- pretty_name(cn)
    values <- NULL

    if (is_num && cn %in% ds$records) {
      values <- band_numeric(all_num)
      if (length(unique(all_num[!is.na(all_num)])) > 6) label <- paste(label, "(banded)")
    } else if (is_num && grepl("tenure|service|experience|years", cn) && !grepl("manager", cn)) {
      values <- band_tenure(all_num)
      label <- paste(label, "(banded)")
    } else if (is_num && grepl("(^|_)age($|_)", cn)) {
      values <- band_age(all_num)
      label <- "Age group"
    } else {
      k <- length(unique(present))
      if (k >= 2 && k <= 15 && mean(nchar(as.character(present))) <= 40) values <- as.character(x)
    }
    if (is.null(values) || length(unique(values[!is.na(values)])) < 2) next

    rank <- which(vapply(DIMENSION_ORDER, function(w) grepl(w, cn, fixed = TRUE), logical(1)))[1]
    dims[[length(dims) + 1]] <- list(key = cn, label = label, values = values,
                                     rank = if (is.na(rank)) 99 else rank)
  }
  if (length(dims) == 0) return(list())
  dims <- dims[order(vapply(dims, `[[`, numeric(1), "rank"))]
  utils::head(dims, max_n)
}

# % favourable by group; groups below MIN_GROUP_N are counted but hidden
compare_groups <- function(ds, values, cols) {
  fm <- favourability_matrix(ds, cols)
  g <- as.character(values)
  groups <- sort(unique(g[!is.na(g)]))
  res <- do.call(rbind, lapply(groups, function(lv) {
    rows <- which(g == lv)
    answers <- fm[rows, , drop = FALSE]
    data.frame(group = lv, n = length(rows),
               pct_fav = 100 * mean(answers == "fav", na.rm = TRUE),
               stringsAsFactors = FALSE)
  }))
  res$shown <- res$n >= MIN_GROUP_N
  res[order(-res$pct_fav), ]
}

# Readable label for short coded headings: "Recommend_Employer_0_10" -> "Recommend employer"
display_question <- function(x) {
  vapply(x, function(s) {
    if (grepl(" ", trimws(s))) return(s)
    t <- gsub("([a-z])([A-Z])", "\\1 \\2", s)
    t <- gsub("[_.]+", " ", t)
    t <- gsub("\\s+[01]\\s+(to\\s+)?(5|7|10)$", "", t, ignore.case = TRUE)
    t <- gsub("\\s+(score|index|rating|item)$", "", t, ignore.case = TRUE)
    t <- trimws(t)
    words <- strsplit(t, " ")[[1]]
    words[-1] <- ifelse(grepl("^[A-Z][a-z]", words[-1]), tolower(words[-1]), words[-1])
    out <- paste(words, collapse = " ")
    paste0(toupper(substr(out, 1, 1)), substr(out, 2, nchar(out)))
  }, character(1), USE.NAMES = FALSE)
}


# ---- Driver analysis --------------------------------------------

# How strongly each item moves with an outcome (Spearman correlation).
# This is association, not cause.
driver_correlations <- function(ds, outcome_cols, item_cols) {
  nm <- normalised_matrix(ds, c(outcome_cols, item_cols))
  outcome <- rowMeans(nm[, outcome_cols, drop = FALSE], na.rm = TRUE)
  outcome[is.nan(outcome)] <- NA
  res <- do.call(rbind, lapply(item_cols, function(cn) {
    ok <- !is.na(outcome) & !is.na(nm[, cn])
    r <- if (sum(ok) >= MIN_DRIVER_N && stats::sd(nm[ok, cn]) > 0 && stats::sd(outcome[ok]) > 0)
           stats::cor(nm[ok, cn], outcome[ok], method = "spearman") else NA
    data.frame(clean = cn, r = r, n = sum(ok), stringsAsFactors = FALSE)
  }))
  res
}


# ---- Chart helpers (Plotly) -------------------------------------

# Break long question text onto several lines for axis labels
wrap_label <- function(x, width = 42) {
  vapply(x, function(s) paste(strwrap(s, width = width), collapse = "<br>"), character(1),
         USE.NAMES = FALSE)
}

# Shared look: Inter font, recessive grid, light hover box, trimmed toolbar
style_chart <- function(p, ...) {
  p <- plotly::layout(
    p,
    font = list(family = "Inter, -apple-system, Segoe UI, sans-serif",
                color = CHART_COLORS$ink, size = 12),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)",
    hoverlabel = list(bgcolor = "#FFFFFF", bordercolor = CHART_COLORS$grid,
                      font = list(color = CHART_COLORS$ink, size = 12)),
    legend = list(orientation = "h", x = 0, y = 1.08, xanchor = "left",
                  font = list(color = CHART_COLORS$muted)),
    ...
  )
  plotly::config(p, displaylogo = FALSE, responsive = TRUE,
                 modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d",
                                            "toggleSpikelines", "hoverCompareCartesian",
                                            "hoverClosestCartesian"))
}

axis_style <- function(...) {
  list(gridcolor = CHART_COLORS$grid, zeroline = FALSE, linecolor = CHART_COLORS$grid,
       tickfont = list(color = CHART_COLORS$muted), automargin = TRUE, ...)
}

# 100% stacked bar: unfavourable | neutral | favourable, one row per item
chart_favourability <- function(items, label_col = "question") {
  items <- items[order(items$pct_fav), ]           # weakest at the bottom
  labels <- wrap_label(items[[label_col]])
  y <- factor(labels, levels = labels)
  seg_text <- function(v) ifelse(v >= 8, sprintf("%.0f%%", v), "")
  hover <- function(name, v) sprintf("%s<br>%s: %.0f%%<br>n = %d",
                                     items[[label_col]], name, v, items$n)

  p <- plotly::plot_ly()
  p <- plotly::add_trace(p, type = "bar", orientation = "h", x = items$pct_unfav, y = y, name = "Unfavourable",
                         marker = list(color = CHART_COLORS$unfav,
                                       line = list(color = "#FFFFFF", width = 2)),
                         text = seg_text(items$pct_unfav), textposition = "inside",
                         insidetextanchor = "middle", textfont = list(color = "#FFFFFF"),
                         hovertext = hover("Unfavourable", items$pct_unfav), hoverinfo = "text")
  p <- plotly::add_trace(p, type = "bar", orientation = "h", x = items$pct_neutral, y = y, name = "Neutral",
                         marker = list(color = CHART_COLORS$neutral,
                                       line = list(color = "#FFFFFF", width = 2)),
                         text = seg_text(items$pct_neutral), textposition = "inside",
                         insidetextanchor = "middle", textfont = list(color = CHART_COLORS$ink),
                         hovertext = hover("Neutral", items$pct_neutral), hoverinfo = "text")
  p <- plotly::add_trace(p, type = "bar", orientation = "h", x = items$pct_fav, y = y, name = "Favourable",
                         marker = list(color = CHART_COLORS$fav,
                                       line = list(color = "#FFFFFF", width = 2)),
                         text = seg_text(items$pct_fav), textposition = "inside",
                         insidetextanchor = "middle", textfont = list(color = "#FFFFFF"),
                         hovertext = hover("Favourable", items$pct_fav), hoverinfo = "text")
  style_chart(p, barmode = "stack", bargap = 0.35,
              height = max(260, 70 + 58 * nrow(items)),
              xaxis = axis_style(title = "", range = c(0, 100), ticksuffix = "%"),
              yaxis = axis_style(title = "", showgrid = FALSE),
              margin = list(l = 10, r = 10, t = 40, b = 30))
}

# Horizontal bars of % favourable by group, with the overall level marked
chart_groups <- function(groups, overall, group_label) {
  shown <- groups[groups$shown, ]
  shown <- shown[order(shown$pct_fav), ]
  y <- factor(shown$group, levels = shown$group)
  p <- plotly::plot_ly(
    x = shown$pct_fav, y = y, type = "bar", orientation = "h",
    marker = list(color = CHART_COLORS$single),
    text = sprintf("%.0f%%  (n = %d)", shown$pct_fav, shown$n),
    textposition = "outside", textfont = list(color = CHART_COLORS$muted),
    cliponaxis = FALSE,
    hovertext = sprintf("%s<br>%.0f%% favourable<br>n = %d", shown$group, shown$pct_fav, shown$n),
    hoverinfo = "text", showlegend = FALSE
  )
  style_chart(p, bargap = 0.4,
              height = max(220, 80 + 44 * nrow(shown)),
              xaxis = axis_style(title = "% favourable", range = c(0, 115), ticksuffix = "%"),
              yaxis = axis_style(title = "", showgrid = FALSE),
              margin = list(l = 10, r = 30, t = 30, b = 40),
              shapes = list(list(type = "line", x0 = overall, x1 = overall, y0 = -0.5,
                                 y1 = nrow(shown) - 0.5, yref = "y",
                                 line = list(color = CHART_COLORS$muted, width = 2, dash = "dot"))),
              annotations = list(list(x = overall, y = nrow(shown) - 0.5, yref = "y",
                                      text = sprintf("Overall %.0f%%", overall),
                                      showarrow = FALSE, yanchor = "bottom",
                                      font = list(color = CHART_COLORS$muted, size = 11))))
}

# Scatter: link with the outcome (x) against % favourable (y)
chart_drivers <- function(drv, outcome_label = "intent to stay") {
  mx <- stats::median(drv$r)
  my <- stats::median(drv$pct_fav)
  short <- vapply(drv$question, function(s) {
    if (nchar(s) > 38) paste0(substr(s, 1, 36), "…") else s
  }, character(1), USE.NAMES = FALSE)
  p <- plotly::plot_ly(
    x = drv$r, y = drv$pct_fav, type = "scatter", mode = "markers+text",
    marker = list(size = 12, color = ifelse(drv$priority, CHART_COLORS$unfav, CHART_COLORS$single),
                  line = list(color = "#FFFFFF", width = 2)),
    text = short, textposition = "top center",
    textfont = list(color = CHART_COLORS$muted, size = 11),
    hovertext = sprintf("%s<br>Link with outcome: %.2f<br>%.0f%% favourable",
                        drv$question, drv$r, drv$pct_fav),
    hoverinfo = "text", showlegend = FALSE
  )
  rng_x <- range(c(drv$r, mx)) + c(-0.1, 0.1)
  style_chart(p, height = 380,
              xaxis = axis_style(title = paste0("Link with ", outcome_label, " (correlation)"),
                                 range = rng_x),
              yaxis = axis_style(title = "% favourable", range = c(0, 105), ticksuffix = "%"),
              margin = list(l = 10, r = 20, t = 30, b = 50),
              shapes = list(
                list(type = "line", x0 = mx, x1 = mx, y0 = 0, y1 = 100,
                     line = list(color = CHART_COLORS$grid, width = 1, dash = "dot")),
                list(type = "line", x0 = rng_x[1], x1 = rng_x[2], y0 = my, y1 = my,
                     line = list(color = CHART_COLORS$grid, width = 1, dash = "dot"))),
              annotations = list(list(x = rng_x[2], y = 2, xanchor = "right", yanchor = "bottom",
                                      text = "Priority area: strong link, low score",
                                      showarrow = FALSE,
                                      font = list(color = CHART_COLORS$unfav, size = 11))))
}

# Single 100% bar splitting respondents into favourable / neutral / unfavourable
chart_profile <- function(pct_fav, pct_neutral, pct_unfav, labels) {
  p <- plotly::plot_ly()
  vals <- c(pct_unfav, pct_neutral, pct_fav)
  cols <- c(CHART_COLORS$unfav, CHART_COLORS$neutral, CHART_COLORS$fav)
  txt_cols <- c("#FFFFFF", CHART_COLORS$ink, "#FFFFFF")
  for (k in 1:3) {
    p <- plotly::add_trace(p, type = "bar", orientation = "h", x = vals[k], y = "Respondents", name = labels[k],
                           marker = list(color = cols[k], line = list(color = "#FFFFFF", width = 2)),
                           text = if (vals[k] >= 8) sprintf("%.0f%%", vals[k]) else "",
                           textposition = "inside", insidetextanchor = "middle",
                           textfont = list(color = txt_cols[k]),
                           hovertext = sprintf("%s: %.0f%% of respondents", labels[k], vals[k]),
                           hoverinfo = "text")
  }
  style_chart(p, barmode = "stack", height = 150, bargap = 0.2,
              xaxis = axis_style(title = "", range = c(0, 100), ticksuffix = "%"),
              yaxis = list(visible = FALSE),
              margin = list(l = 10, r = 10, t = 40, b = 30))
}

# Describe the favourable / unfavourable cut-off in the scale's own terms
scale_point <- function(scale_min, scale_max, which = c("fav", "unfav")) {
  which <- match.arg(which)
  mid <- (scale_min + scale_max) / 2
  v <- if (which == "fav") floor(mid) + 1 else ceiling(mid) - 1
  sprintf("%g on the %g–%g scale", v, scale_min, scale_max)
}


# =============================================================
# Analysis-screen building blocks (HTML charts)
# Charts are drawn as styled HTML so they match the app's look exactly,
# stay readable at any width, and show details on hover (title tooltips).
# Colours: favourable blue, neutral grey, unfavourable orange (checked for
# colour-blind separation); hotspot colours show the gap from the company.
# =============================================================

UI_COL <- list(fav = "#2B5C9E", unfav = "#C2571A", neutral = "#B7BFCC",
               mid = "#7C8AA5", navy = "#1B2A4A")

pct0 <- function(x) sprintf("%.0f%%", x)

# Colour for a % favourable value relative to a reference (e.g. the company)
tone_colour <- function(value, reference, band = 5) {
  if (is.na(value)) return(UI_COL$neutral)
  if (value <= reference - band) UI_COL$unfav else if (value >= reference + band) UI_COL$fav else UI_COL$mid
}

# Colour for one answer point on a scale: below the middle orange, middle grey, above blue
point_colour <- function(point, scale_min, scale_max) {
  mid <- (scale_min + scale_max) / 2
  if (point < mid) UI_COL$unfav else if (point > mid) UI_COL$fav else UI_COL$neutral
}

# Order group labels naturally: "Under 1 year", "1-3 years", "3-5 years", "10+ years"
natural_order <- function(labels) {
  key <- vapply(labels, function(l) {
    if (grepl("^under", tolower(l))) return(-Inf)
    n <- suppressWarnings(as.numeric(regmatches(l, regexpr("-?[0-9]+(\\.[0-9]+)?", l))))
    if (length(n) == 0 || is.na(n)) Inf else n
  }, numeric(1))
  if (all(is.infinite(key) & key > 0)) return(seq_along(labels))
  order(key, labels)
}

legend_ui <- function(items) {
  div(class = "hl-legend",
      lapply(items, function(it) span(span(class = "hl-swatch", style = paste0("background:", it[2], ";")), it[1])))
}

# ---- Vertical columns (answer distributions, eNPS, HR records) ----
# bars: data.frame(label, value, colour, text, tip)
chart_columns <- function(bars, max_value = NULL, compact = FALSE) {
  if (is.null(max_value)) max_value <- max(bars$value, na.rm = TRUE)
  if (!is.finite(max_value) || max_value <= 0) max_value <- 1
  div(
    class = paste("hl-cols", if (compact) "hl-cols-compact"),
    div(class = "hl-cols-plot",
        lapply(seq_len(nrow(bars)), function(i) {
          h <- if (is.na(bars$value[i])) 0 else 82 * bars$value[i] / max_value
          div(class = "hl-col", title = bars$tip[i],
              span(class = "hl-col-value", bars$text[i]),
              div(class = "hl-col-bar",
                  style = sprintf("height:%.1f%%;background:%s;", h, bars$colour[i])))
        })),
    div(class = "hl-cols-labels", lapply(bars$label, function(l) span(l)))
  )
}

# ---- Centred (diverging) bars: unfavourable left, favourable right ----
# items: data.frame(question, pct_fav, pct_neutral, pct_unfav, n)
chart_diverging <- function(items) {
  seg <- function(w, colour, cls = "") {
    div(class = paste("hl-seg", cls), style = sprintf("width:%.1f%%;background:%s;", w, colour))
  }
  div(class = "hl-scroll",
      div(class = "hl-div",
          lapply(seq_len(nrow(items)), function(i) {
            it <- items[i, ]
            tip <- sprintf("%s\nFavourable %.0f%% · Neutral %.0f%% · Unfavourable %.0f%% (n = %d)",
                           it$question, it$pct_fav, it$pct_neutral, it$pct_unfav, it$n)
            div(class = "hl-div-row", title = tip,
                span(class = "hl-div-label", it$question),
                div(class = "hl-div-track",
                    div(class = "hl-div-left",
                        seg(it$pct_unfav, UI_COL$unfav, "hl-seg-l"),
                        seg(it$pct_neutral / 2, "#D5D9DF")),
                    div(class = "hl-div-right",
                        seg(it$pct_neutral / 2, "#D5D9DF"),
                        seg(it$pct_fav, UI_COL$fav, "hl-seg-r"))),
                span(class = "hl-div-value", pct0(it$pct_fav)))
          })))
}

# ---- Hotspot map: groups x areas, coloured by gap from the company ----
# values: matrix [group, area] of % favourable; n: group sizes; overall: per area
heat_fill <- function(d) {
  if (is.na(d)) return(c("#FFFFFF", "#9AA3B4"))
  if (d <= -10) c("#E9A67F", "#4A1E06") else if (d <= -4) c("#F6D9C6", "#5C2A0D")
  else if (d < 4) c("#F1F3F6", "#283044") else if (d < 10) c("#CFDDF0", "#173563")
  else c("#9DBBE3", "#0F2A52")
}

chart_heatmap <- function(values, n, overall, col_labels, group_title) {
  k <- ncol(values)
  tmpl <- sprintf("grid-template-columns:minmax(130px,170px) 52px repeat(%d,minmax(56px,1fr));", k)
  row_ui <- function(cells, cls = "") div(class = paste("hl-heat-row", cls), style = tmpl, cells)
  div(
    legend_ui(list(c("10+ points below the company", "#E9A67F"), c("4–9 below", "#F6D9C6"),
                   c("about the same", "#F1F3F6"), c("4–9 above", "#CFDDF0"),
                   c("10+ above", "#9DBBE3"))),
    div(class = "hl-scroll",
        div(class = "hl-heat", style = sprintf("min-width:%dpx;", 240 + 64 * k),
            row_ui(tagList(span(class = "hl-heat-h", group_title), span(class = "hl-heat-h hl-right", "n"),
                           lapply(col_labels, function(l) span(class = "hl-heat-h hl-center", l))), "hl-heat-head"),
            lapply(seq_len(nrow(values)), function(i) {
              row_ui(tagList(
                span(class = "hl-heat-name", rownames(values)[i]),
                span(class = "hl-heat-n num", n[i]),
                lapply(seq_len(k), function(j) {
                  v <- values[i, j]
                  f <- heat_fill(v - overall[j])
                  span(class = paste("hl-heat-cell num", if (j == 1) "hl-strong"),
                       style = sprintf("background:%s;color:%s;", f[1], f[2]),
                       title = sprintf("%s · %s: %s favourable (company %s, n = %d)",
                                       rownames(values)[i], col_labels[j],
                                       if (is.na(v)) "n/a" else pct0(v), pct0(overall[j]), n[i]),
                       if (is.na(v)) "–" else sprintf("%.0f", v))
                })))
            }),
            row_ui(tagList(span(class = "hl-heat-name hl-strong", "Company"),
                           span(class = "hl-heat-n num", sum(n)),
                           lapply(overall, function(v) span(class = "hl-heat-cell num hl-strong hl-plain", sprintf("%.0f", v)))),
                   "hl-heat-total")))
  )
}

# ---- Horizontal group bars with the company level marked ----
# rows: data.frame(group, n, pct_fav) in display order
chart_group_bars <- function(rows, overall) {
  div(class = "hl-gbars",
      lapply(seq_len(nrow(rows)), function(i) {
        r <- rows[i, ]
        div(class = "hl-gbar-row",
            title = sprintf("%s: %.0f%% favourable (n = %d); company %.0f%%", r$group, r$pct_fav, r$n, overall),
            span(class = "hl-gbar-label", r$group),
            div(class = "hl-gbar-track",
                div(class = "hl-gbar-fill",
                    style = sprintf("width:%.1f%%;background:%s;", r$pct_fav, tone_colour(r$pct_fav, overall))),
                div(class = "hl-gbar-mark", style = sprintf("left:%.1f%%;", overall))),
            span(class = "hl-gbar-value num", tags$b(pct0(r$pct_fav)), span(class = "hl-muted", sprintf(" n=%d", r$n))))
      }))
}

# ---- Driver table: link strength next to today's score, with a tag ----
# drv: data.frame(question, r, pct_fav, tag)
DRIVER_TAGS <- list(
  "Priority" = c("#FBE3D4", "#8A3A0F"), "Protect" = c("#E1EAF7", "#1F4078"),
  "Monitor" = c("#EEF0F4", "#3E485C"), "No link" = c("#F4F4F4", "#6B7280"))

tag_chip <- function(tag, colours = DRIVER_TAGS[[tag]]) {
  span(class = "hl-chip", style = sprintf("background:%s;color:%s;", colours[1], colours[2]), tag)
}

chart_driver_table <- function(drv, fav_reference, outcome_label) {
  max_r <- max(0.3, max(drv$r, na.rm = TRUE))
  div(
    div(class = "hl-scroll",
        div(class = "hl-drv",
            div(class = "hl-drv-row hl-drv-head",
                span("Area"), span(paste("Link with", outcome_label)), span("% favourable today"), span("What to do")),
            lapply(seq_len(nrow(drv)), function(i) {
              d <- drv[i, ]
              div(class = "hl-drv-row",
                  title = sprintf("%s: correlation %.2f with %s; %.0f%% favourable", d$question, d$r, outcome_label, d$pct_fav),
                  span(class = "hl-drv-name", d$question),
                  div(class = "hl-drv-bar",
                      div(class = "hl-drv-track", div(style = sprintf("width:%.1f%%;background:%s;", max(0, 100 * d$r / max_r), UI_COL$navy))),
                      span(class = "num", sprintf("%.2f", d$r))),
                  div(class = "hl-drv-bar",
                      div(class = "hl-drv-track", div(style = sprintf("width:%.1f%%;background:%s;", d$pct_fav,
                                                                     if (d$pct_fav <= fav_reference) UI_COL$unfav else UI_COL$fav))),
                      span(class = "num", pct0(d$pct_fav))),
                  tag_chip(d$tag))
            }))),
    div(class = "hl-drv-key",
        span(tag_chip("Priority"), " strong link, below-median score"),
        span(tag_chip("Protect"), " strong link, good score"),
        span(tag_chip("Monitor"), " weaker link"),
        span(tag_chip("No link"), " does not move with ", outcome_label))
  )
}

# ---- Ranked list (strengths / areas to improve) ----
ranked_list <- function(names, values, colour) {
  div(class = "hl-rank",
      lapply(seq_along(names), function(i) {
        div(class = "hl-rank-row", span(names[i]),
            span(class = "num hl-strong", style = sprintf("color:%s;", colour), pct0(values[i])))
      }))
}
