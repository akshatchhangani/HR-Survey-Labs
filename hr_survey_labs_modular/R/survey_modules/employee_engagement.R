# =============================================================
# HR Labs — Employee Engagement module
#
# Works for two common survey designs:
#   * classic: several engagement statements (pride, advocacy, intent to stay ...)
#   * driver-based: one or two engagement outcomes (e.g. "Engagement score",
#     "Recommend employer 0-10") plus driver questions (manager support,
#     growth, recognition, workload ...)
#
# Output (rendered by app.R as a tabbed analysis screen):
#   list(status = "ok", headline, findings, tabs)
#     headline: index, level, engaged/neutral/disengaged, eNPS, n
#     findings: list(tag, tone, title, text)         -> "Key findings" cards
#     tabs:     list(id, label, blocks)               -> one tab each
#     block:    list(title, subtitle, chart, insight, note, width)
#               chart is an HTML chart from analytics_common.R
#   Every chart carries three insights (shows / might explain / do next).
# =============================================================

# What each area usually reflects, and a first action.
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
    action  = "Share results openly with every team and agree two or three team-level actions."),
  # --- common drivers
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
  "Other" = list(
    explain = "this question is not one of the standard engagement areas, so look at the wording and comments for context.",
    action  = "Discuss this area with the teams concerned to understand the issue before acting.")
)


engagement_guide <- function(concept) {
  g <- ENGAGEMENT_GUIDE[[concept]]
  if (is.null(g)) ENGAGEMENT_GUIDE[["Other"]] else g
}

# Concept labels that count as engagement OUTCOMES (everything else is a driver)
engagement_core_labels <- function() {
  vapply(SURVEY_CONCEPTS$employee_engagement, `[[`, character(1), "label")
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
  "Wellbeing" = "HR + wellbeing lead")

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


survey_module_employee_engagement <- function(ds, ...) {
  items <- score_questions(ds)
  if (is.null(items) || nrow(items) == 0) {
    return(list(status = "no_scores",
                message = "None of the questions use a rating scale that can be scored."))
  }

  n_resp <- ds$n_responses

  # ---------- Sort questions into outcomes, drivers and eNPS ----------
  nps_item <- items[items$is_nps, , drop = FALSE]
  rated    <- items[!items$is_nps, , drop = FALSE]
  is_core  <- rated$concept %in% engagement_core_labels()
  core     <- if (any(is_core)) rated[is_core, , drop = FALSE] else rated
  drivers  <- if (any(is_core)) rated[!is_core, , drop = FALSE] else rated[0, ]
  driver_design <- nrow(drivers) >= 3

  if (nrow(core) == 0) {
    return(list(status = "no_scores", message = "No engagement questions could be scored."))
  }

  core_cols <- core$clean
  overall <- mean(core$pct_fav)
  fav_word <- if (nrow(core) == 1) "% favourable on the engagement question" else "% favourable across the engagement questions"

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
    title = if (nrow(core) == 1) "How people answered the engagement question" else "How engaged people are",
    subtitle = if (nrow(core) == 1) sprintf("%s, %g = lowest, %g = highest", quote_q(core$question[1]), smin, smax)
               else sprintf("Each respondent's average across %d engagement questions, rounded to the scale (%g–%g)", nrow(core), smin, smax),
    chart = chart_columns(dist_bars),
    insight = make_insight(
      shows = paste0(sprintf("%s of respondents are engaged, %s neutral and %s disengaged.", pct0(pf), pct0(pn), pct0(pu)),
                     sprintf(" The most common answer is %g.", top_pt)),
      explains = if (pu >= 20) {
        "A disengaged group of this size usually points to specific pain points such as workload, career prospects or management, rather than a general mood."
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
          sprintf("This sits oddly next to a %s engagement index. People may rate the employer brand differently from their own day-to-day experience.", pct0(overall))
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
        sprintf("eNPS %+.0f looks %s next to %s engaged", enps, if (enps >= 30) "high" else "low", pct0(overall)),
        "People may rate the employer brand differently from their own day-to-day experience. Worth checking in follow-up conversations.")
    }
  }

  # ---------- Scores by area ----------
  rated_sorted <- rated[order(-rated$pct_fav), , drop = FALSE]
  best <- rated_sorted[1, ]; worst <- rated_sorted[nrow(rated_sorted), ]
  g_worst <- engagement_guide(worst$concept)
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
  outcome_cols <- rated$clean[rated$concept == "Engagement (overall)"]
  outcome_label <- "engagement"
  if (length(outcome_cols) == 0) {
    outcome_cols <- rated$clean[rated$concept == "Intent to stay"]
    outcome_label <- "intent to stay"
  }
  if (length(outcome_cols) == 0 && driver_design) outcome_cols <- core_cols
  item_cols <- if (driver_design) drivers$clean else setdiff(rated$clean, outcome_cols)
  drv <- NULL
  priority <- rated[0, ]

  if (length(outcome_cols) >= 1 && length(item_cols) >= 3) {
    drv <- driver_correlations(ds, outcome_cols, item_cols)
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
      strength_word <- if (max(drv$r) < 0.3) "All links here are modest (below 0.3): engagement depends on several things together rather than one dominant factor." else
        "These areas rise and fall together with engagement; this is an association, not proof of cause."

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
            paste(sprintf("Make %s the first priority.", quote_q(pri$question[1])), engagement_guide(pri$concept[1])$action)
          } else {
            sprintf("Keep strengthening %s, the area most closely linked with %s.", quote_q(top_link$question), outcome_label)
          }),
        note = sprintf("Spearman correlation between each %s and %s, based on up to %d respondents. Correlation shows association, not cause.",
                       if (driver_design) "area" else "question",
                       if (length(outcome_cols) == 1) quote_q(rated$question[rated$clean == outcome_cols]) else paste("the average of the", outcome_label, "questions"),
                       max(drv$n)))

      if (nrow(pri) > 0) {
        findings[[length(findings) + 1]] <- finding("Driver", "info",
          sprintf("%s matters most", pri$question[1]),
          sprintf("Closely linked with %s (%.2f), yet only %s favourable.", outcome_label, pri$r[1], pct0(pri$pct_fav[1])))
      }
    }
  }
  if (length(drivers_tab) == 0) {
    reason <- if (length(outcome_cols) == 0) "no overall engagement or intent-to-stay question was recognised"
              else if (length(item_cols) < 3) "at least three other questions are needed"
              else sprintf("at least %d complete responses are needed", MIN_DRIVER_N)
    drivers_tab[[1]] <- block("Driver analysis", NULL, NULL, note = paste0("This analysis was skipped because ", reason, "."))
  }

  # Concern finding (after drivers, so it can say whether the weakest area is linked)
  worst_r <- if (!is.null(drv) && worst$clean %in% drv$clean) drv$r[drv$clean == worst$clean] else NA
  findings[[length(findings) + 1]] <- finding("Concern", "bad",
    sprintf("%s is lowest", worst$question),
    paste0(sprintf("Only %s favourable and %s unfavourable", pct0(worst$pct_fav), pct0(worst$pct_unfav)),
           if (!is.na(worst_r) && worst_r < 0.05) ", but it barely moves with engagement." else "."))

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
      col_labels <- c(if (nrow(core) == 1) "Engagement" else "Engagement index", short_label(area_rows$question))
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
          shows = paste0(sprintf("%s is lowest on engagement (%s vs %s for the company) and below the company on %d of %d columns. %s is highest (%s).",
                                 low$group, pct0(low$pct_fav), pct0(overall), below, ncol(vals), top$group, pct0(top$pct_fav))),
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
          sprintf("%s favourable on engagement against %s in %s.", pct0(low$pct_fav), pct0(top$pct_fav), top$group))
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
      title = "Engagement and HR records",
      subtitle = "From the HR data in the same file, grouped into bands. These show association, not cause.",
      chart = div(class = "hl-mini-grid", rec_blocks),
      insight = make_insight(
        shows = sprintf("Engagement %s from %s at %s %s to %s at %s.", direction,
                        pct0(rec_find$first$pct_fav), rec_find$first$group, rec_find$name,
                        pct0(rec_find$last$pct_fav), rec_find$last$group),
        explains = "Engagement and HR outcomes such as absence or performance often move together through workload, stress or the manager relationship. Either can come first.",
        next_step = if (abs(rec_find$spread) >= 10) sprintf("Use %s as an early-warning signal: prompt managers to check in when it moves the wrong way.", rec_find$name)
                    else "No strong pattern yet; keep tracking these records alongside the next survey."))
    if (abs(rec_find$spread) >= 10) {
      findings[[length(findings) + 1]] <- finding("HR records", "info",
        sprintf("Engagement %s with %s", direction, rec_find$name),
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
            engagement_guide(it$concept)$action, action_owner(it$concept),
            if (k == 1) "Start within 30 days" else "Within 60 days")
  }
  big <- Filter(function(g) g$gap >= 10, gaps)
  if (length(big) > 0) {
    g <- big[[which.max(vapply(big, `[[`, numeric(1), "gap"))]]
    add_act(g$low$group, paste("Group hotspot ·", sub(" \\(banded\\)$", "", g$label)),
            sprintf("%s favourable on engagement, %.0f points below the highest group.", pct0(g$low$pct_fav), g$gap),
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
            paste0(sprintf("%s favourable", pct0(it$pct_fav)), if (no_link) ", but no link with engagement in this data." else "."),
            if (no_link) paste(engagement_guide(it$concept)$action, "Treat it as a fairness and retention measure; do not expect it to lift engagement on its own.")
            else engagement_guide(it$concept)$action,
            action_owner(it$concept), "Within 90 days")
  }
  actions_block <- if (length(acts) > 0) block(
    title = "Action plan",
    subtitle = "Drawn from the strongest links with engagement, the weakest scores, the largest group gaps and the HR records. Owners and timings are suggestions to adapt.",
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
    headline = list(label = "Engagement index", index = overall, level = level_word(overall),
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
