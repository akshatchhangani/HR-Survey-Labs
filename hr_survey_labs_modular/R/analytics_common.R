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


# Each question against the average of all the OTHER questions (so a
# question is never correlated with itself). Same shape as driver_correlations().
item_rest_correlations <- function(ds, cols) {
  nm <- normalised_matrix(ds, cols)
  do.call(rbind, lapply(cols, function(cn) {
    rest <- rowMeans(nm[, setdiff(cols, cn), drop = FALSE], na.rm = TRUE)
    rest[is.nan(rest)] <- NA
    ok <- !is.na(rest) & !is.na(nm[, cn])
    r <- if (sum(ok) >= MIN_DRIVER_N && stats::sd(nm[ok, cn]) > 0 && stats::sd(rest[ok]) > 0)
           stats::cor(nm[ok, cn], rest[ok], method = "spearman") else NA
    data.frame(clean = cn, r = r, n = sum(ok), stringsAsFactors = FALSE)
  }))
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


# =============================================================
# Shared survey analysis engine
# Builds the tabbed analysis screen (Overview, Scores by area, Groups &
# hotspots, Drivers, Action plan) for any survey module. Each module passes a
# config with its own words, outcome questions and HR guidance:
#   noun, Noun, index_label   e.g. "engagement", "Engagement", "Engagement index"
#   people                    c(fav = "engaged", neutral = "neutral", unfav = "disengaged")
#   core_labels               concept labels that count as the survey's outcome
#   outcomes                  list(list(concept, label), ...) for the driver analysis, in priority order
#   no_outcome_text           why the driver analysis is skipped when no outcome exists
#   guide                     function(concept) -> list(explain, action)
# Output: list(status = "ok", headline, findings, tabs) (see CLAUDE.md, section 5)
# =============================================================

# Explanations and first actions for areas that appear in many surveys.
# Explanations are hypotheses to check, not conclusions.
COMMON_GUIDE <- list(
  "Manager support" = list(
    explain = "managers may lack time, skills or incentives to support their teams, or spans of control may be too wide.",
    action  = "Equip managers with regular one-to-ones and coaching skills, and include team support in how managers are assessed."),
  "Leadership (overall)" = list(
    explain = "trust in leadership often falls after unexplained decisions, change that is poorly communicated, or a gap between what leaders say and do.",
    action  = "Have senior leaders explain recent decisions and next steps in open forums, and follow up on what they commit to."),
  "Trust" = list(
    explain = "trust tends to erode when decisions are not explained, promises are not kept, or information reaches people late.",
    action  = "Explain the reasoning behind key decisions, keep commitments visible, and report back on progress."),
  "Communication" = list(
    explain = "people may hear about changes late, through informal channels, or without the reasons behind them.",
    action  = "Set a regular rhythm of team and company updates, and give people a simple way to ask questions."),
  "Coaching and development" = list(
    explain = "development conversations may be rare, or managers may not have the skills or time to coach.",
    action  = "Make development part of regular one-to-ones and give managers simple coaching guides."),
  "Career opportunities" = list(
    explain = "people may not see a path forward: promotions may be rare, criteria unclear, or internal moves hard to make.",
    action  = "Publish career paths and promotion criteria, and advertise internal roles before hiring externally."),
  "Pay and recognition" = list(
    explain = "recognition may be infrequent, generic, or seen as unfair, or pay may be felt to lag the market.",
    action  = "Make recognition timely and specific, and check pay against the market for the lowest-scoring roles."),
  "Total rewards" = list(
    explain = "the reward package may not be well understood, or may be felt to lag what others offer.",
    action  = "Explain the full reward package clearly and benchmark it against the market."),
  "Working conditions" = list(
    explain = "workload, hours or flexibility may be stretching people, especially in teams with vacancies or peak periods.",
    action  = "Review workload and staffing in the lowest-scoring teams, and agree clear norms on hours and flexibility."),
  "Work-life and workload" = list(
    explain = "workload, hours or flexibility may be stretching people, especially in teams with vacancies or peak periods.",
    action  = "Review workload and staffing in the lowest-scoring teams, and agree clear norms on hours and flexibility."),
  "Workload" = list(
    explain = "teams may be under-staffed, priorities unclear, or work unevenly shared.",
    action  = "Review workload and priorities with the affected teams, and stop or postpone low-value work."),
  "Belonging" = list(
    explain = "some people may feel outside the group: new joiners, remote workers or under-represented groups are common examples.",
    action  = "Check belonging by group, strengthen team rituals, and involve under-represented groups in designing solutions."),
  "Inclusion" = list(
    explain = "some groups may feel their views are not heard, or that opportunities are not open to everyone.",
    action  = "Look at inclusion scores by group, review how opportunities are allocated, and train managers in inclusive practices."),
  "Overall satisfaction" = list(
    explain = "satisfaction is usually shaped by the job itself, the manager, pay and working conditions.",
    action  = "Use the driver analysis to find which part of the job matters most, and start there."),
  "Respect and fairness" = list(
    explain = "people may see unequal treatment in decisions about work, recognition or promotion.",
    action  = "Make decision criteria transparent and give people a safe way to raise concerns about fairness."),
  "Collaboration" = list(
    explain = "teams may work in silos, with unclear ownership or competing goals.",
    action  = "Set shared goals across teams and make it easy to ask other teams for help."),
  "Values" = list(
    explain = "people may see a gap between stated values and everyday decisions.",
    action  = "Link values to concrete behaviours and recognise people who live them."),
  "Wellbeing" = list(
    explain = "sustained pressure, long hours or uncertainty may be wearing people down.",
    action  = "Review workload and support services, and make it normal to talk about wellbeing."),
  "Pay" = list(
    explain = "pay may be felt to lag the market or similar roles inside the organisation, or the way pay is set may not be understood.",
    action  = "Benchmark pay for the lowest-scoring roles and explain clearly how pay is set and reviewed."),
  "Benefits" = list(
    explain = "benefits may not match what people need at their life stage, or people may not know what is on offer.",
    action  = "Check which benefits people value and use, and communicate the full package clearly."),
  "Recognition" = list(
    explain = "recognition may be infrequent, generic, or seen as uneven across teams.",
    action  = "Make recognition timely and specific, and give managers simple ways to recognise good work every week."),
  "Growth and development" = list(
    explain = "people may not see a path forward: promotions may be rare, criteria unclear, or development limited to formal training.",
    action  = "Publish career paths and promotion criteria, and agree a development goal with every employee."),
  "Manager and supervision" = list(
    explain = "managers may lack time, skills or support to lead their teams well, or spans of control may be too wide.",
    action  = "Give managers coaching in one-to-ones and feedback, and include team satisfaction in how managers are assessed."),
  "Colleagues and teamwork" = list(
    explain = "teams may lack shared goals, or friction between colleagues may go unaddressed.",
    action  = "Agree team norms and shared goals, and address conflicts early."),
  "Work-life balance" = list(
    explain = "workload, hours or rigid schedules may be stretching people, especially in teams with vacancies.",
    action  = "Review workload and staffing in the lowest-scoring teams and agree clear norms on hours and flexibility."),
  "Job security" = list(
    explain = "recent changes, restructuring or business uncertainty may be making people worry about their jobs.",
    action  = "Communicate openly about the business outlook and any planned changes, and answer questions directly."),
  "Policies" = list(
    explain = "policies may be unclear, inconsistently applied, or seen as outdated.",
    action  = "Review the policies people mention most, simplify them, and make sure managers apply them consistently."),
  "The work itself" = list(
    explain = "work may feel repetitive, poorly matched to people's skills, or disconnected from results.",
    action  = "Discuss role design with teams: variety, ownership and how each role contributes to results."),
  "Other" = list(
    explain = "this question is not one of the standard areas for this survey, so look at the wording and comments for context.",
    action  = "Discuss this area with the teams concerned to understand the issue before acting.")
)

# Combine a survey's own guidance with the common guidance
make_guide <- function(specific) {
  function(concept) {
    g <- specific[[concept]]
    if (is.null(g)) g <- COMMON_GUIDE[[concept]]
    if (is.null(g)) COMMON_GUIDE[["Other"]] else g
  }
}

# Suggested owner for an action, by area
ACTION_OWNERS <- c(
  "Manager support" = "Line managers + HR business partner",
  "Leadership (overall)" = "Senior leadership team", "Trust" = "Senior leadership team",
  "Communication" = "Internal communications + leaders",
  "Career opportunities" = "Talent / L&D", "Coaching and development" = "Talent / L&D",
  "Pay and recognition" = "Compensation & Benefits + managers",
  "Total rewards" = "Compensation & Benefits",
  "Working conditions" = "HR business partners + team managers",
  "Work-life and workload" = "HR business partners + team managers",
  "Workload" = "HR business partners + team managers",
  "Belonging" = "DEI lead + team managers", "Inclusion" = "DEI lead + team managers",
  "Wellbeing" = "HR + wellbeing lead",
  "Pay" = "Compensation & Benefits", "Benefits" = "Compensation & Benefits",
  "Recognition" = "Line managers + HR", "Growth and development" = "Talent / L&D",
  "Manager and supervision" = "Line managers + HR business partner",
  "Colleagues and teamwork" = "Team managers", "Work-life balance" = "HR business partners + team managers",
  "Job security" = "Senior leadership team", "Policies" = "HR policy owner",
  "The work itself" = "Team managers + HR business partner")

action_owner <- function(concept) {
  o <- ACTION_OWNERS[concept]
  if (is.na(o)) "HR business partner + managers" else unname(o)
}

block <- function(title, subtitle, chart, insight = NULL, note = NULL, width = "full") {
  list(title = title, subtitle = subtitle, chart = chart, insight = insight, note = note, width = width)
}

finding <- function(tag, tone, title, text) list(tag = tag, tone = tone, title = title, text = text)

# Short column headings for the hotspot map
short_label <- function(x, max_chars = 14) {
  vapply(x, function(s) {
    s <- sub(" \\(reverse-scored\\)$", "", s)
    if (nchar(s) <= max_chars) s else paste0(substr(s, 1, max_chars - 1), "…")
  }, character(1), USE.NAMES = FALSE)
}

lower_first <- function(s) paste0(tolower(substr(s, 1, 1)), substr(s, 2, nchar(s)))


build_survey_analysis <- function(ds, cfg) {
  # Suggested owner: the survey's own guidance first, then the common list
  owner_of <- function(concept) {
    o <- cfg$guide(concept)$owner
    if (is.null(o)) action_owner(concept) else o
  }

  items <- score_questions(ds)
  if (is.null(items) || nrow(items) == 0) {
    return(list(status = "no_scores",
                message = "None of the questions use a rating scale that can be scored."))
  }

  n_resp <- ds$n_responses

  # ---------- Sort questions into outcomes, drivers and eNPS ----------
  nps_item <- items[items$is_nps, , drop = FALSE]
  rated    <- items[!items$is_nps, , drop = FALSE]
  is_core  <- rated$concept %in% cfg$core_labels
  core     <- if (any(is_core)) rated[is_core, , drop = FALSE] else rated
  drivers  <- if (any(is_core)) rated[!is_core, , drop = FALSE] else rated[0, ]
  driver_design <- nrow(drivers) >= 3

  if (nrow(core) == 0) {
    return(list(status = "no_scores", message = paste0("No ", cfg$noun, " questions could be scored.")))
  }

  core_cols <- core$clean
  overall <- mean(core$pct_fav)
  fav_word <- if (nrow(core) == 1) paste0("% favourable on the ", cfg$noun, " question") else paste0("% favourable across the ", cfg$noun, " questions")

  # ---------- Respondent profile ----------
  prof  <- respondent_profile(ds, core_cols, core$scale_min[1], core$scale_max[1])
  valid <- !is.na(prof$group)
  pf <- 100 * mean(prof$group[valid] == "fav")
  pn <- 100 * mean(prof$group[valid] == "neutral")
  pu <- 100 * mean(prof$group[valid] == "unfav")

  findings <- list()
  overview <- list(); areas_tab <- list(); groups_tab <- list(); drivers_tab <- list()

  # ---------- Overview: answer distribution ----------
  smin <- core$scale_min[1]; smax <- core$scale_max[1]
  nm <- normalised_matrix(ds, core_cols)
  avg_points <- smin + (smax - smin) * rowMeans(nm, na.rm = TRUE) / 100
  avg_points <- round(avg_points[is.finite(avg_points)])
  pts <- seq(smin, smax)
  counts <- vapply(pts, function(p) sum(avg_points == p), numeric(1))
  shares <- 100 * counts / max(1, sum(counts))
  dist_bars <- data.frame(
    label = as.character(pts), value = shares,
    colour = vapply(pts, point_colour, character(1), scale_min = smin, scale_max = smax),
    text = ifelse(shares >= 0.5, pct0(shares), ""),
    tip = sprintf("%g: %.0f%% of respondents (%d)", pts, shares, as.integer(counts)),
    stringsAsFactors = FALSE)
  mid_share <- if (any(pts == (smin + smax) / 2)) shares[pts == (smin + smax) / 2] else NA
  top_pt <- pts[which.max(shares)]

  overview[[length(overview) + 1]] <- block(
    title = if (nrow(core) == 1) paste0("How people answered the ", cfg$noun, " question") else paste0("How ", cfg$people[["fav"]], " people are"),
    subtitle = if (nrow(core) == 1) sprintf("%s, %g = lowest, %g = highest", quote_q(core$question[1]), smin, smax)
               else sprintf("Each respondent's average across %d %s questions, rounded to the scale (%g–%g)", nrow(core), cfg$noun, smin, smax),
    chart = chart_columns(dist_bars),
    insight = make_insight(
      shows = paste0(sprintf("%s of respondents are %s, %s %s and %s %s.", pct0(pf), cfg$people[["fav"]], pct0(pn), cfg$people[["neutral"]], pct0(pu), cfg$people[["unfav"]]),
                     sprintf(" The most common answer is %g.", top_pt)),
      explains = if (pu >= 20) {
        paste0("A ", cfg$people[["unfav"]], " group of this size usually points to specific pain points such as workload, career prospects or management, rather than a general mood.")
      } else if (pn >= 30) {
        "A large neutral group usually means people are not unhappy but lack a strong reason to commit: growth, recognition or workload."
      } else {
        "Most people answer favourably, so the remaining gaps are more likely tied to a few specific areas than to overall morale."
      },
      next_step = if (pn >= 30) {
        "Target the neutral middle first: they are the easiest to move and the most open to outside offers."
      } else if (pu >= 20) {
        "Start with the hotspots and priority drivers in the other tabs, and commit to two or three visible actions within 90 days."
      } else {
        "Protect what works: share the results, thank teams, and keep monitoring with a short pulse survey."
      }),
    width = "half")

  # ---------- Overview: eNPS ----------
  enps <- NA
  if (nrow(nps_item) > 0) {
    v <- suppressWarnings(as.numeric(ds$data[[nps_item$clean[1]]]))
    v <- v[!is.na(v)]
    prom <- 100 * mean(v >= 9); pas <- 100 * mean(v >= 7 & v <= 8); det <- 100 * mean(v <= 6)
    enps <- prom - det
    enps_word <- if (enps >= 30) "strong" else if (enps > 0) "positive" else if (enps == 0) "neutral" else "negative"
    nps_counts <- vapply(0:10, function(p) sum(v == p), numeric(1))
    nps_share <- 100 * nps_counts / length(v)
    nps_bars <- data.frame(
      label = as.character(0:10), value = nps_share,
      colour = ifelse(0:10 <= 6, UI_COL$unfav, ifelse(0:10 <= 8, UI_COL$neutral, UI_COL$fav)),
      text = ifelse(nps_share >= 1, pct0(nps_share), ""),
      tip = sprintf("%d: %.0f%% (%d people)", 0:10, nps_share, as.integer(nps_counts)),
      stringsAsFactors = FALSE)
    mismatch <- (enps >= 30 && overall < 60) || (enps < 0 && overall >= 60)

    overview[[length(overview) + 1]] <- block(
      title = "Employee Net Promoter Score",
      subtitle = sprintf("%s, 0–10 · eNPS %+.0f (%s)", quote_q(nps_item$question[1]), enps, enps_word),
      chart = tagList(chart_columns(nps_bars, compact = TRUE),
                      legend_ui(list(c(sprintf("Detractors 0–6: %s", pct0(det)), UI_COL$unfav),
                                     c(sprintf("Passives 7–8: %s", pct0(pas)), UI_COL$neutral),
                                     c(sprintf("Promoters 9–10: %s", pct0(prom)), UI_COL$fav)))),
      insight = make_insight(
        shows = sprintf("eNPS is %+.0f: %s promoters against %s detractors (n = %d).", enps, pct0(prom), pct0(det), length(v)),
        explains = if (mismatch) {
          sprintf("This sits oddly next to a %s %s index. People may rate the employer brand differently from their own day-to-day experience.", pct0(overall), cfg$noun)
        } else if (det >= 25) {
          "A detractor group of this size usually reflects specific frustrations; comparing detractors' scores with promoters' shows which areas separate them."
        } else {
          "Promoters outnumber detractors, which usually reflects a positive overall experience."
        },
        next_step = if (det >= 25) {
          "Hold follow-up conversations with detractor-heavy teams and track eNPS again after acting."
        } else {
          "Use promoters for referrals and employer branding, and keep tracking eNPS each survey."
        }),
      note = "eNPS = % promoters − % detractors, from −100 to +100. Above 0 is positive, above 30 is strong.",
      width = "half")

    if (mismatch) {
      findings[[length(findings) + 1]] <- finding("Check", "warn",
        sprintf("eNPS %+.0f looks %s next to %s %s", enps, if (enps >= 30) "high" else "low", pct0(overall), cfg$people[["fav"]]),
        "People may rate the employer brand differently from their own day-to-day experience. Worth checking in follow-up conversations.")
    }
  }

  # ---------- Scores by area ----------
  rated_sorted <- rated[order(-rated$pct_fav), , drop = FALSE]
  best <- rated_sorted[1, ]; worst <- rated_sorted[nrow(rated_sorted), ]
  g_worst <- cfg$guide(worst$concept)
  n_weak <- sum(rated$pct_fav < 60)

  areas_tab[[length(areas_tab) + 1]] <- block(
    title = if (driver_design) "Scores by area" else "Scores by question",
    subtitle = "Unfavourable to the left of the centre line, favourable to the right; neutral answers are split across it. Strongest first.",
    chart = tagList(
      legend_ui(list(c("Unfavourable", UI_COL$unfav), c("Neutral", "#D5D9DF"), c("Favourable", UI_COL$fav))),
      chart_diverging(rated_sorted)),
    insight = make_insight(
      shows = sprintf("%s scores highest (%s favourable) and %s lowest (%s favourable, %s unfavourable). %d of %d are below 60%% favourable.",
                      quote_q(best$question), pct0(best$pct_fav), quote_q(worst$question),
                      pct0(worst$pct_fav), pct0(worst$pct_unfav), n_weak, nrow(rated)),
      explains = paste0("Possible explanation to check for ", lower_first(worst$question), ": ", g_worst$explain),
      next_step = g_worst$action))

  k <- min(3, floor(nrow(rated_sorted) / 2))
  if (k >= 1) {
    top <- rated_sorted[seq_len(k), ]
    bottom <- rated_sorted[rev(seq(nrow(rated_sorted) - k + 1, nrow(rated_sorted))), ]
    areas_tab[[length(areas_tab) + 1]] <- block("Strengths to protect", NULL,
      ranked_list(top$question, top$pct_fav, UI_COL$fav), width = "half")
    areas_tab[[length(areas_tab) + 1]] <- block("Areas to improve", NULL,
      ranked_list(bottom$question, bottom$pct_fav, UI_COL$unfav), width = "half")
  }
  findings[[length(findings) + 1]] <- finding("Strength", "good",
    sprintf("%s leads", best$question),
    sprintf("%s favourable, the strongest %s in the survey.", pct0(best$pct_fav), if (driver_design) "area" else "question"))

  # ---------- Driver analysis ----------
  # Outcome: the first of the survey's outcome concepts present in the data;
  # otherwise, when separate driver questions exist, the core questions together
  outcome_cols <- character(0)
  outcome_label <- cfg$noun
  for (oc in cfg$outcomes) {
    hit <- rated$clean[rated$concept == oc$concept]
    if (length(hit) > 0) { outcome_cols <- hit; outcome_label <- oc$label; break }
  }
  if (length(outcome_cols) == 0 && driver_design) { outcome_cols <- core_cols; outcome_label <- cfg$noun }
  item_cols <- if (driver_design) drivers$clean else setdiff(rated$clean, outcome_cols)
  drv <- NULL
  priority <- rated[0, ]

  # No outcome question at all: link each question with the average of the
  # other questions (item-rest correlation), when the survey allows it
  item_rest <- length(outcome_cols) == 0 && !driver_design && isTRUE(cfg$item_rest) && nrow(rated) >= 4
  if (item_rest) {
    item_cols <- rated$clean
    outcome_label <- paste("overall", cfg$noun)
  }

  if ((length(outcome_cols) >= 1 && length(item_cols) >= 3) || item_rest) {
    drv <- if (item_rest) item_rest_correlations(ds, item_cols) else driver_correlations(ds, outcome_cols, item_cols)
    drv <- merge(drv, rated[, c("clean", "question", "concept", "pct_fav")], by = "clean")
    drv <- drv[!is.na(drv$r), ]
    if (nrow(drv) >= 3) {
      med_r <- stats::median(drv$r); med_f <- stats::median(drv$pct_fav)
      drv$tag <- ifelse(drv$r < 0.05, "No link",
                 ifelse(drv$r >= med_r & drv$pct_fav <= med_f, "Priority",
                 ifelse(drv$r >= med_r, "Protect", "Monitor")))
      drv <- drv[order(-drv$r), ]
      pri <- drv[drv$tag == "Priority", ]
      pri <- pri[order(-pri$r), ]
      priority <- rated[match(pri$clean, rated$clean), ]
      top_link <- drv[1, ]
      strength_word <- if (max(drv$r) < 0.3) paste0("All links here are modest (below 0.3): ", outcome_label, " depends on several things together rather than one dominant factor.") else
        paste0("These areas rise and fall together with ", outcome_label, "; this is an association, not proof of cause.")

      drivers_tab[[length(drivers_tab) + 1]] <- block(
        title = paste("What moves with", outcome_label),
        subtitle = sprintf("How closely each %s rises and falls with %s, next to how well it scores today. Strongest link first.",
                           if (driver_design) "area" else "question", outcome_label),
        chart = chart_driver_table(drv, med_f, outcome_label),
        insight = make_insight(
          shows = paste0(sprintf("%s has the strongest link with %s (%.2f).", quote_q(top_link$question), outcome_label, top_link$r),
                         if (nrow(pri) > 0) sprintf(" Priority: %s, linked but scoring below the middle.",
                                                    and_list(vapply(pri$question, quote_q, character(1)))) else ""),
          explains = strength_word,
          next_step = if (nrow(pri) > 0) {
            paste(sprintf("Make %s the first priority.", quote_q(pri$question[1])), cfg$guide(pri$concept[1])$action)
          } else {
            sprintf("Keep strengthening %s, the area most closely linked with %s.", quote_q(top_link$question), outcome_label)
          }),
        note = sprintf("Spearman correlation between each %s and %s, based on up to %d respondents. Correlation shows association, not cause.",
                       if (driver_design) "area" else "question",
                       if (item_rest) "the average of all the other questions"
                       else if (length(outcome_cols) == 1) quote_q(rated$question[rated$clean == outcome_cols])
                       else paste("the average of the", outcome_label, "questions"),
                       max(drv$n)))

      if (nrow(pri) > 0) {
        findings[[length(findings) + 1]] <- finding("Driver", "info",
          sprintf("%s matters most", pri$question[1]),
          sprintf("Closely linked with %s (%.2f), yet only %s favourable.", outcome_label, pri$r[1], pct0(pri$pct_fav[1])))
      }
    }
  }
  if (length(drivers_tab) == 0) {
    reason <- if (length(outcome_cols) == 0) cfg$no_outcome_text
              else if (length(item_cols) < 3) "at least three other questions are needed"
              else sprintf("at least %d complete responses are needed", MIN_DRIVER_N)
    drivers_tab[[1]] <- block("Driver analysis", NULL, NULL, note = paste0("This analysis was skipped because ", reason, "."))
  }

  # Concern finding (after drivers, so it can say whether the weakest area is linked)
  worst_r <- if (!is.null(drv) && worst$clean %in% drv$clean) drv$r[drv$clean == worst$clean] else NA
  findings[[length(findings) + 1]] <- finding("Concern", "bad",
    sprintf("%s is lowest", worst$question),
    paste0(sprintf("Only %s favourable and %s unfavourable", pct0(worst$pct_fav), pct0(worst$pct_unfav)),
           if (!is.na(worst_r) && worst_r < 0.05) paste0(", but it barely moves with ", outcome_label, ".") else "."))

  # ---------- Groups and hotspots ----------
  dims <- group_dimensions(ds, max_n = 10)
  record_dims <- Filter(function(d) d$key %in% ds$records, dims)
  plain_dims <- Filter(function(d) !d$key %in% ds$records, dims)
  hot <- NULL
  gaps <- list()

  if (length(plain_dims) > 0) {
    d1 <- plain_dims[[1]]
    base <- compare_groups(ds, d1$values, core_cols)
    base <- base[base$shown, ]
    if (nrow(base) >= 2) {
      # Driver surveys: one column per driver area; classic surveys: one per question
      area_rows <- if (driver_design) rated_sorted[!rated_sorted$clean %in% core_cols, , drop = FALSE] else rated_sorted
      area_rows <- utils::head(area_rows, 13)
      col_labels <- c(if (nrow(core) == 1) cfg$Noun else cfg$index_label, short_label(area_rows$question))
      overall_cols <- c(overall, area_rows$pct_fav)
      vals <- matrix(NA_real_, nrow = nrow(base), ncol = length(col_labels),
                     dimnames = list(base$group, col_labels))
      vals[, 1] <- base$pct_fav
      for (j in seq_len(nrow(area_rows))) {
        cg <- compare_groups(ds, d1$values, area_rows$clean[j])
        vals[, j + 1] <- cg$pct_fav[match(base$group, cg$group)]
      }
      hidden <- sum(!compare_groups(ds, d1$values, core_cols)$shown)
      low <- base[nrow(base), ]; top <- base[1, ]
      below <- if (ncol(vals) > 1) sum(vals[nrow(vals), ] < overall_cols - 0.5, na.rm = TRUE) else NA
      hot <- list(dim = d1$label, low = low, top = top, gap = top$pct_fav - low$pct_fav, below = below, k = ncol(vals))
      gaps[[length(gaps) + 1]] <- list(label = d1$label, low = low, gap = hot$gap)

      groups_tab[[length(groups_tab) + 1]] <- block(
        title = sprintf("Hotspot map: %s by area", tolower(sub(" \\(banded\\)$", "", d1$label))),
        subtitle = "% favourable. Colour shows the gap from the company result for that area, so hotspots stand out at a glance.",
        chart = chart_heatmap(vals, base$n, overall_cols, col_labels, sub(" \\(banded\\)$", "", d1$label)),
        insight = make_insight(
          shows = paste0(sprintf("%s is lowest on %s (%s vs %s for the company) and below the company on %d of %d columns. %s is highest (%s).",
                                 low$group, cfg$noun, pct0(low$pct_fav), pct0(overall), below, ncol(vals), top$group, pct0(top$pct_fav))),
          explains = if (hot$gap >= 10) {
            if (below >= 0.7 * ncol(vals)) sprintf("A gap across almost every area points to something local to %s, such as workload, shift patterns or team leadership, rather than one policy.", low$group)
            else sprintf("The gap is concentrated in a few areas, so look at what is specific to %s in those areas.", low$group)
          } else "Groups score close to each other, so the issues are more likely organisation-wide.",
          next_step = if (hot$gap >= 10) sprintf("Start with a listening session in %s, and pair its managers with the leads of %s to share what works.", low$group, top$group)
                      else "Address the weakest areas across the organisation rather than targeting one group."),
        note = if (hidden > 0) sprintf("%d group%s with fewer than %d responses %s hidden to protect anonymity.",
                                       hidden, if (hidden == 1) "" else "s", MIN_GROUP_N, if (hidden == 1) "is" else "are") else NULL)

      if (hot$gap >= 10) {
        findings[[length(findings) + 1]] <- finding("Hotspot", "bad",
          sprintf("%s is %.0f points behind", low$group, hot$gap),
          sprintf("%s favourable on %s against %s in %s.", pct0(low$pct_fav), cfg$noun, pct0(top$pct_fav), top$group))
      }
    }
  }

  # Other comparisons: compact bars, two per row
  for (d in utils::head(plain_dims[-1], 6)) {
    cmp <- compare_groups(ds, d$values, core_cols)
    shown <- cmp[cmp$shown, ]
    if (nrow(shown) < 2) next
    if (grepl("banded|Age group", d$label)) shown <- shown[natural_order(shown$group), ]
    gap <- max(shown$pct_fav) - min(shown$pct_fav)
    lowg <- shown[which.min(shown$pct_fav), ]
    gaps[[length(gaps) + 1]] <- list(label = d$label, low = lowg, gap = gap)
    groups_tab[[length(groups_tab) + 1]] <- block(
      title = paste("By", tolower(sub(" \\(banded\\)$", "", d$label))),
      subtitle = sprintf("%s · dotted line = company %s · %.0f-point gap", fav_word, pct0(overall), gap),
      chart = chart_group_bars(shown, overall),
      note = if (any(!cmp$shown)) sprintf("Groups under %d responses hidden.", MIN_GROUP_N) else NULL,
      width = "half")
  }

  # HR records: engagement by band
  rec_blocks <- list()
  rec_find <- NULL
  for (d in utils::head(record_dims, 4)) {
    cmp <- compare_groups(ds, d$values, core_cols)
    shown <- cmp[cmp$shown, ]
    if (nrow(shown) < 2) next
    shown <- shown[natural_order(shown$group), ]
    name <- lower_first(sub(" \\(banded\\)$", "", d$label))
    rec_blocks[[length(rec_blocks) + 1]] <- div(class = "hl-mini",
      span(class = "hl-mini-title", sprintf("%% favourable by %s", name)),
      chart_columns(data.frame(
        label = shown$group, value = shown$pct_fav,
        colour = vapply(shown$pct_fav, tone_colour, character(1), reference = overall),
        text = pct0(shown$pct_fav),
        tip = sprintf("%s %s: %.0f%% favourable (n = %d)", d$label, shown$group, shown$pct_fav, shown$n),
        stringsAsFactors = FALSE), max_value = 100, compact = TRUE))
    spread <- shown$pct_fav[nrow(shown)] - shown$pct_fav[1]
    if (is.null(rec_find) || abs(spread) > abs(rec_find$spread)) {
      rec_find <- list(name = name, first = shown[1, ], last = shown[nrow(shown), ], spread = spread)
    }
  }
  if (length(rec_blocks) > 0) {
    direction <- if (rec_find$spread < 0) "falls" else "rises"
    groups_tab[[length(groups_tab) + 1]] <- block(
      title = paste(cfg$Noun, "and HR records"),
      subtitle = "From the HR data in the same file, grouped into bands. These show association, not cause.",
      chart = div(class = "hl-mini-grid", rec_blocks),
      insight = make_insight(
        shows = sprintf("%s %s from %s at %s %s to %s at %s.", cfg$Noun, direction,
                        pct0(rec_find$first$pct_fav), rec_find$first$group, rec_find$name,
                        pct0(rec_find$last$pct_fav), rec_find$last$group),
        explains = paste(cfg$Noun, "and HR outcomes such as absence or performance often move together through workload, stress or the manager relationship. Either can come first."),
        next_step = if (abs(rec_find$spread) >= 10) sprintf("Use %s as an early-warning signal: prompt managers to check in when it moves the wrong way.", rec_find$name)
                    else "No strong pattern yet; keep tracking these records alongside the next survey."))
    if (abs(rec_find$spread) >= 10) {
      findings[[length(findings) + 1]] <- finding("HR records", "info",
        sprintf("%s %s with %s", cfg$Noun, direction, rec_find$name),
        sprintf("%s favourable at %s, %s at %s.", pct0(rec_find$first$pct_fav), rec_find$first$group,
                pct0(rec_find$last$pct_fav), rec_find$last$group))
    }
  }
  if (length(groups_tab) == 0) {
    groups_tab[[1]] <- block("Group comparisons", NULL, NULL,
      note = "No respondent details (such as department, location or tenure) were found to compare groups.")
  }

  # ---------- Action plan ----------
  acts <- list()
  add_act <- function(area, kind, evidence, action, owner, when) {
    acts[[length(acts) + 1]] <<- list(area = area, kind = kind, evidence = evidence,
                                      action = action, owner = owner, when = when)
  }
  for (k in seq_len(min(2, nrow(priority)))) {
    it <- priority[k, ]
    rr <- drv$r[drv$clean == it$clean]
    add_act(it$question, "Priority driver",
            sprintf("Linked with %s (%.2f), only %s favourable.", outcome_label, rr, pct0(it$pct_fav)),
            cfg$guide(it$concept)$action, owner_of(it$concept),
            if (k == 1) "Start within 30 days" else "Within 60 days")
  }
  big <- Filter(function(g) g$gap >= 10, gaps)
  if (length(big) > 0) {
    g <- big[[which.max(vapply(big, `[[`, numeric(1), "gap"))]]
    add_act(g$low$group, paste("Group hotspot ·", sub(" \\(banded\\)$", "", g$label)),
            sprintf("%s favourable on %s, %.0f points below the highest group.", pct0(g$low$pct_fav), cfg$noun, g$gap),
            sprintf("Hold a listening session with %s, then agree two or three local actions with its leaders.", g$low$group),
            "Group leader + HR business partner", "Start within 30 days")
  }
  if (!is.null(rec_find) && abs(rec_find$spread) >= 10) {
    add_act(paste(tools::toTitleCase(rec_find$name), "as an early warning"), "HR records",
            sprintf("%s favourable at %s, %s at %s.", pct0(rec_find$first$pct_fav), rec_find$first$group,
                    pct0(rec_find$last$pct_fav), rec_find$last$group),
            sprintf("Prompt managers to hold a check-in conversation when %s moves the wrong way.", rec_find$name),
            "Line managers", "Within 90 days")
  }
  weak <- rated_sorted[rev(seq_len(nrow(rated_sorted))), ]
  weak <- weak[weak$pct_fav < 60 & !weak$clean %in% priority$clean & !weak$clean %in% core_cols, ]
  if (nrow(weak) > 0) {
    it <- weak[1, ]
    rr <- if (!is.null(drv) && it$clean %in% drv$clean) drv$r[drv$clean == it$clean] else NA
    no_link <- !is.na(rr) && rr < 0.05
    add_act(it$question, "Lowest score",
            paste0(sprintf("%s favourable", pct0(it$pct_fav)), if (no_link) paste0(", but no link with ", outcome_label, " in this data.") else "."),
            if (no_link) paste(cfg$guide(it$concept)$action, paste0("Treat it as a fairness and retention measure; do not expect it to lift ", outcome_label, " on its own."))
            else cfg$guide(it$concept)$action,
            owner_of(it$concept), "Within 90 days")
  }
  actions_block <- if (length(acts) > 0) block(
    title = "Action plan",
    subtitle = paste0("Drawn from the strongest links with ", outcome_label, ", the weakest scores, the largest group gaps and the HR records. Owners and timings are suggestions to adapt."),
    chart = div(class = "hl-actions",
      lapply(seq_along(acts), function(i) {
        a <- acts[[i]]
        tags$article(class = "hl-action",
          div(class = "hl-action-head", span(class = "hl-action-n num", i),
              div(span(class = "hl-action-area", a$area), span(class = "hl-action-kind", a$kind))),
          p(class = "hl-action-evidence", a$evidence),
          p(class = "hl-action-text", a$action),
          div(class = "hl-action-chips",
              span(class = "hl-chip hl-chip-blue", a$owner), span(class = "hl-chip hl-chip-grey", a$when)))
      })),
    note = "Every action rests on the evidence shown in the other tabs; correlations show association, not cause.")
  else block("Action plan", NULL, NULL, note = "No clear priorities were found: scores are even across areas and groups.")

  # Order findings: strength, concern, hotspot, driver, records, check
  tone_order <- c(Strength = 1, Concern = 2, Hotspot = 3, Driver = 4, "HR records" = 5, Check = 6)
  findings <- findings[order(tone_order[vapply(findings, `[[`, character(1), "tag")])]

  list(
    status = "ok",
    headline = list(label = cfg$index_label, people = cfg$people, index = overall, level = level_word(overall),
                    n = n_resp, engaged = pf, neutral = pn, disengaged = pu, enps = enps,
                    n_core = nrow(core), n_drivers = nrow(drivers), has_nps = nrow(nps_item) > 0,
                    scale = sprintf("%g–%g", smin, smax)),
    findings = findings,
    tabs = list(
      list(id = "overview", label = "Overview", blocks = overview),
      list(id = "areas", label = if (driver_design) "Scores by area" else "Scores by question", blocks = areas_tab),
      list(id = "groups", label = "Groups & hotspots", blocks = groups_tab),
      list(id = "drivers", label = paste("What drives", outcome_label), blocks = drivers_tab),
      list(id = "actions", label = "Action plan", blocks = list(actions_block))
    )
  )
}
