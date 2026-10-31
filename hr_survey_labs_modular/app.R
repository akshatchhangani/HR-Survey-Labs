# =============================================================
# HR Survey Labs — main entry point
# Step 8: redesigned analysis screen (tabs, key findings, hotspot map, driver table, action plan)
# =============================================================

# ---- Packages (rsconnect detects dependencies from these lines) ----
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(readxl)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(plotly)
  library(DT)
  library(janitor)
})

# Allow uploads up to 30 MB per request (Shiny's default is only 5 MB)
options(shiny.maxRequestSize = 30 * 1024^2)

# File types the app accepts
ALLOWED_EXTENSIONS <- c("csv", "xlsx", "xls")

# ---- Load project files in a FIXED order ----
# R/_disable_autoload.R stops Shiny from auto-sourcing R/,
# so this block is the only place files are loaded.
source("R/data_processing.R")
source("R/survey_mapping.R")
source("R/analytics_common.R")
source("R/insights_common.R")

module_files <- sort(list.files("R/survey_modules", pattern = "\\.R$", full.names = TRUE))
for (f in module_files) source(f)

source("R/survey_registry.R")   # always last: it refers to module functions

# ---- Small helpers ----

# 2048 -> "2.0 KB", 3145728 -> "3.0 MB"
format_size <- function(bytes) {
  ifelse(bytes < 1024^2,
         sprintf("%.1f KB", bytes / 1024),
         sprintf("%.1f MB", bytes / 1024^2))
}

# Plain HTML table from a data frame (used for file lists and results)
simple_table <- function(df, status_col = NULL) {
  tags$table(
    class = "simple-table",
    tags$thead(tags$tr(lapply(names(df), tags$th))),
    tags$tbody(
      lapply(seq_len(nrow(df)), function(i) {
        tags$tr(
          lapply(names(df), function(col) {
            value <- as.character(df[[col]][i])
            cls <- NULL
            if (!is.null(status_col) && col == status_col) {
              cls <- switch(value,
                            "Ready"        = "status-ok",
                            "Check notes"  = "status-warn",
                            "Needs review" = "status-warn",
                            "status-issue")
            }
            tags$td(class = cls, value)
          })
        )
      })
    )
  )
}

# ---- Reusable UI pieces ----
app_header <- function() {
  tags$header(
    class = "app-header",
    div(
      class = "header-inner",
      tags$img(class = "brand-logo", src = "logo.svg", alt = "", width = 46, height = 46),
      div(
        h1(class = "brand-title", "HR Survey Labs"),
        p(class = "brand-tagline",
          "Turn survey responses into clear findings and practical next steps")
      )
    )
  )
}

# One clickable card per survey type, built from the registry
survey_card <- function(s) {
  tags$button(
    type = "button",
    class = "survey-card",
    `data-key` = s$key,
    `aria-pressed` = "false",
    onclick = sprintf("hslSelectSurvey(this, '%s')", s$key),
    div(class = "card-top",
        span(class = "card-icon", survey_icon(s$icon)),
        if (isTRUE(s$ready)) span(class = "card-status card-status-ready", "Full analysis")
        else span(class = "card-status", "Coming soon"),
        span(class = "card-check", survey_icon("check", 14))),
    span(class = "card-title", s$label),
    span(class = "card-focus", s$focus)
  )
}

# Home screen: short introduction and how it works
home_intro <- function() {
  step <- function(n, title, text) {
    div(class = "intro-step",
        span(class = "intro-step-n", n),
        div(span(class = "intro-step-title", title), span(class = "intro-step-text", text)))
  }
  div(class = "intro",
      div(class = "intro-copy",
          span(class = "intro-eyebrow", "Survey analysis for HR teams"),
          h2(class = "intro-title", "Turn survey responses into findings you can act on"),
          p(class = "intro-text",
            "Upload the Excel or CSV export of an employee survey. HR Survey Labs checks the data, recognises the questions however they are worded, and explains what the results show, what may be behind them and what to do next.")),
      div(class = "intro-steps",
          step("1", "Choose the survey type", "16 HR surveys, from engagement to exit"),
          step("2", "Upload your files", "Excel or CSV, from any survey tool"),
          step("3", "Get findings and actions", "Charts, hotspots, drivers and an action plan")))
}

# Survey cards grouped by category, in registry order
survey_groups <- function() {
  cats <- unique(vapply(survey_registry, `[[`, character(1), "category"))
  lapply(cats, function(cat) {
    members <- Filter(function(s) identical(s$category, cat), survey_registry)
    div(class = "survey-group",
        h3(class = "survey-group-title", cat),
        div(class = "survey-grid", unname(lapply(members, survey_card))))
  })
}

# Small script: highlight the clicked card and tell the server which one it was
card_script <- tags$script(HTML("
  function hslSelectSurvey(el, key) {
    document.querySelectorAll('.survey-card').forEach(function (c) {
      c.classList.remove('selected');
      c.setAttribute('aria-pressed', 'false');
    });
    el.classList.add('selected');
    el.setAttribute('aria-pressed', 'true');
    Shiny.setInputValue('survey_card', key, {priority: 'event'});
  }
"))

app_footer <- function() {
  tags$footer(
    class = "app-footer",
    div(
      class = "footer-inner",
      span(class = "footer-label", "Contact"),
      div(
        class = "footer-emails",
        tags$a(href = "mailto:akshat.chhangani070@nmims.in",
               "akshat.chhangani070@nmims.in"),
        tags$a(href = "mailto:akshatchhangani78@gmail.com",
               "akshatchhangani78@gmail.com")
      )
    )
  )
}

# ---- UI ----
ui <- page_fluid(
  title = "HR Survey Labs",
  theme = bs_theme(
    version   = 5,
    primary   = "#1B2A4A",
    secondary = "#6B7280",
    bg        = "#FFFFFF",
    fg        = "#1F2937"
  ),
  tags$head(
    tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"),
    # The version number changes whenever styles.css changes, so browsers
    # never keep showing an old copy of the styles
    tags$link(rel = "stylesheet",
              href = paste0("styles.css?v=", as.integer(file.mtime("www/styles.css")))),
    card_script
  ),
  div(
    class = "app-shell",
    app_header(),
    tags$main(
      class = "app-main",
      tabsetPanel(
        id = "screen", type = "hidden",

        # ---------- Screen 1: prepare data ----------
        tabPanel(
          "Prepare data", value = "prepare",

          # Introduction, then 1. Survey selection
          home_intro(),
          div(
            class = "section-intro",
            h2("1. Choose a survey type"),
            p("Select the kind of survey your response files come from.")
          ),
          survey_groups(),
          uiOutput("selection_panel"),

          # 2. Upload (appears once a survey is chosen)
          conditionalPanel(
            condition = "input.survey_card",
            div(
              class = "work-section",
              div(
                class = "section-intro",
                h2("2. Upload response files"),
                p("CSV or Excel (.xlsx, .xls). Select several files at once, or add more in a second pick.")
              ),
              uiOutput("file_input_ui"),
              uiOutput("file_list"),
              div(
                class = "action-row",
                actionButton("analyse", "Analyse", class = "btn-primary btn-analyse"),
                actionButton("clear", "Clear files and results",
                             class = "btn-outline-secondary")
              )
            )
          ),

          # 3. File checks, 4. Datasets ready for analysis
          uiOutput("results_panel"),
          uiOutput("datasets_panel")
        ),

        # ---------- Screen 2: analysis ----------
        tabPanel(
          "Analysis", value = "analysis",
          div(
            class = "screen-bar",
            actionButton("back_to_data", "\u2190 Back to files",
                         class = "btn-outline-secondary",
                         onclick = "window.scrollTo(0, 0);"),
            uiOutput("analysis_title", inline = TRUE)
          ),
          uiOutput("analysis_panel")
        )
      )
    ),
    app_footer()
  )
)

# ---- Server ----
server <- function(input, output, session) {

  # App state
  selected_survey <- reactiveVal(NULL)   # survey key, e.g. "leadership"
  uploaded_files  <- reactiveVal(NULL)   # data frame: name, size, type, datapath
  results         <- reactiveVal(NULL)   # output of the last Analyse click
  upload_nonce    <- reactiveVal(0)      # bumping this re-creates the upload box
  built           <- reactiveVal(NULL)   # datasets formed after decisions
  run_id          <- reactiveVal(0)      # keeps each decision dialog's inputs separate

  # ---- Survey selection ----
  observeEvent(input$survey_card, {
    key <- input$survey_card
    if (is.null(get_survey(key))) return()
    if (!identical(selected_survey(), key)) {
      selected_survey(key)
      results(NULL)   # old results belong to the previous survey type
      built(NULL)
      updateTabsetPanel(session, "screen", selected = "prepare")
    }
  })

  output$selection_panel <- renderUI({
    key <- selected_survey()
    if (is.null(key)) return(NULL)
    s <- get_survey(key)

    div(
      class = "selection-panel",
      div(
        class = "selection-head",
        span(class = "card-icon card-icon-lg", survey_icon(s$icon, 26)),
        div(
          h3(s$label),
          p(class = "selection-focus", s$focus),
          p(class = "selection-status",
            if (isTRUE(s$ready)) "Full analysis: charts, hotspots, drivers and an action plan."
            else "Data checks are available now. The full analysis for this survey type is coming soon.")
        )
      )
    )
  })

  # ---- Upload box ----
  # A fresh input id after each Clear guarantees an empty upload box.
  current_input_id <- reactive(paste0("files_", upload_nonce()))

  output$file_input_ui <- renderUI({
    fileInput(
      current_input_id(),
      label = NULL,
      multiple = TRUE,
      accept = paste0(".", ALLOWED_EXTENSIONS),
      buttonLabel = "Browse files",
      placeholder = "No files selected"
    )
  })

  # Add each new pick to the list; a file with the same name replaces the old one
  observeEvent(input[[current_input_id()]], {
    new_files <- input[[current_input_id()]]
    existing  <- uploaded_files()
    if (!is.null(existing)) {
      existing  <- existing[!existing$name %in% new_files$name, , drop = FALSE]
      new_files <- rbind(existing, new_files)
    }
    uploaded_files(new_files)
    results(NULL)   # file set changed, so previous results are out of date
    built(NULL)
  })

  output$file_list <- renderUI({
    files <- uploaded_files()
    if (is.null(files) || nrow(files) == 0) return(NULL)
    div(
      class = "file-list",
      p(class = "file-count",
        sprintf("%d file%s ready", nrow(files), if (nrow(files) == 1) "" else "s")),
      simple_table(data.frame(
        File = files$name,
        Size = format_size(files$size),
        check.names = FALSE
      ))
    )
  })

  # ---- Analyse ----
  observeEvent(input$analyse, {
    key   <- selected_survey()
    files <- uploaded_files()

    if (is.null(key)) {
      showNotification("Choose a survey type first.", type = "warning")
      return()
    }
    if (is.null(files) || nrow(files) == 0) {
      showNotification("Upload at least one CSV or Excel file before analysing.",
                       type = "warning")
      return()
    }

    # Read and check every file (originals stay untouched)
    processed <- withProgress(message = "Checking files", value = 0, {
      lapply(seq_len(nrow(files)), function(i) {
        incProgress(1 / nrow(files), detail = files$name[i])
        r <- process_file(files$datapath[i], files$name[i])
        assess_file(r, key)   # survey match, column roles, scale checks
      })
    })
    names(processed) <- files$name

    # Decide which files can be combined, and what the user must confirm
    plan <- plan_datasets(processed, key)
    results(list(survey = key, processed = processed, plan = plan))
    run_id(run_id() + 1)
    built(NULL)

    if (length(plan$decisions) > 0) {
      showModal(decision_dialog(plan))
    } else {
      built(build_datasets(processed, plan))
    }
  })

  # ---- Decision dialog ----
  decision_input_id <- function(d) paste0("decision_", run_id(), "_", d$id)

  # One dropdown per coded question (Q1, Q2 ...)
  label_inputs <- function(d) {
    div(
      class = "label-grid",
      p(class = "label-hint",
        "If you include this file, tell the app what each question measures. Leave \u201CGeneral / not sure\u201D where you don't know."),
      lapply(seq_len(nrow(d$questions)), function(k) {
        selectInput(paste0(decision_input_id(d), "_q_", d$questions$clean[k]),
                    label = d$questions$original[k], choices = d$label_choices,
                    selected = "General", width = "100%")
      })
    )
  }

  decision_dialog <- function(plan) {
    modalDialog(
      title = "A few things need your decision",
      size = "l",
      easyClose = FALSE,
      p(class = "decision-intro",
        "The app will not guess. Choose what should happen to each item, then confirm."),
      lapply(plan$decisions, function(d) {
        div(
          class = paste0("decision-item decision-", d$kind),
          h5(d$title),
          p(d$message),
          if (d$kind == "reverse") {
            checkboxGroupInput(decision_input_id(d), label = NULL,
                               choices = d$choices, selected = unname(d$choices))
          } else {
            radioButtons(decision_input_id(d), label = NULL,
                         choices = d$choices, selected = character(0))
          },
          if (d$kind == "label") label_inputs(d)
        )
      }),
      footer = tagList(
        modalButton("Decide later"),
        actionButton("confirm_decisions", "Confirm", class = "btn-primary")
      )
    )
  }

  observeEvent(input$confirm_decisions, {
    res <- results()
    req(res)
    answers <- lapply(res$plan$decisions, function(d) input[[decision_input_id(d)]])
    names(answers) <- vapply(res$plan$decisions, `[[`, character(1), "id")

    needs_choice <- vapply(res$plan$decisions, function(d) !d$kind %in% c("reverse"), logical(1))
    if (any(vapply(answers, is.null, logical(1)) & needs_choice)) {
      showNotification("Choose an option for every item before confirming.", type = "warning")
      return()
    }
    # Labels picked for coded questions
    for (d in res$plan$decisions) {
      if (d$kind == "label") {
        labs <- vapply(d$questions$clean, function(cn) {
          v <- input[[paste0(decision_input_id(d), "_q_", cn)]]
          if (is.null(v)) "General" else v
        }, character(1))
        answers[[paste0(d$id, "_labels")]] <- labs
      }
    }

    removeModal()
    built(build_datasets(res$processed, res$plan, answers))
  })

  observeEvent(input$reopen_decisions, {
    res <- results()
    req(res)
    showModal(decision_dialog(res$plan))
  })

  # ---- Moving between the two screens ----
  observeEvent(input$go_analysis, {
    updateTabsetPanel(session, "screen", selected = "analysis")
  })
  observeEvent(input$back_to_data, {
    updateTabsetPanel(session, "screen", selected = "prepare")
  })

  # ---- Clear: files and results go, the survey type stays ----
  observeEvent(input$clear, {
    uploaded_files(NULL)
    results(NULL)
    built(NULL)
    upload_nonce(upload_nonce() + 1)
    updateTabsetPanel(session, "screen", selected = "prepare")
  })

  # ---- Results ----
  output$results_panel <- renderUI({
    res <- results()
    if (is.null(res)) return(NULL)
    s <- get_survey(res$survey)
    p_list <- res$processed

    status_label <- c(ok = "Ready", warning = "Check notes",
                      review = "Needs review", error = "Rejected")
    show_num <- function(x, fmt = "%d") if (is.null(x) || is.na(x)) "—" else sprintf(fmt, x)

    match_label <- function(r) {
      rel <- r$relevance
      if (is.null(rel)) return("—")
      switch(rel$decision,
             match         = "Matches",
             mismatch      = paste("Looks like", get_survey(rel$suggested)$label),
             unclear       = "Unclear",
             unrecognised  = "Not recognised",
             coded_headers = "Coded headings",
             no_questions  = "No questions",
             "—")
    }

    summary_df <- data.frame(
      File         = names(p_list),
      Responses    = vapply(p_list, function(r) show_num(r$n_rows), character(1)),
      Questions    = vapply(p_list, function(r) show_num(r$n_questions), character(1)),
      Scale        = vapply(p_list, function(r) {
                       if (is.null(r$scale_summary) || is.na(r$scale_summary)) "—"
                       else r$scale_summary
                     }, character(1)),
      Missing      = vapply(p_list, function(r) show_num(r$pct_missing, "%.1f%%"), character(1)),
      `Survey match` = vapply(p_list, match_label, character(1)),
      Status       = unname(status_label[vapply(p_list, `[[`, character(1), "status")]),
      check.names = FALSE
    )
    n_usable <- sum(summary_df$Status %in% c("Ready", "Check notes"))
    n_review <- sum(summary_df$Status == "Needs review")

    role_label <- c(id = "Respondent ID", detail = "Respondent detail",
                    time = "Date / period", comment = "Comment (free text)",
                    empty = "No answers", system = "Survey tool information (ignored)",
                    record = "Employee record (used for comparisons)")

    # How each column was read, shown inside the file's notes
    column_table <- function(r) {
      cols <- r$columns
      if (is.null(cols) || nrow(cols) == 0) return(NULL)
      read_as <- ifelse(cols$role == "question",
                        ifelse(is.na(cols$concept), "Question — not recognised", cols$concept),
                        unname(role_label[cols$role]))
      answers <- ifelse(cols$role == "question", cols$scale_text,
                        ifelse(cols$role %in% c("id", "empty"), "", cols$scale_text))
      flagged <- ifelse(cols$n_out_of_range > 0,
                        sprintf("%s (%d outside scale)", answers, cols$n_out_of_range), answers)
      div(
        class = "column-map",
        p(class = "column-map-title", "How the columns were read"),
        simple_table(data.frame(Column = cols$original, `Read as` = read_as,
                                Answers = flagged, check.names = FALSE))
      )
    }

    # One expandable block of notes per file
    level_icon <- c(error = "✖", warning = "⚠", info = "ℹ")
    file_notes <- lapply(p_list, function(r) {
      if (nrow(r$issues) == 0) {
        items <- list(tags$li(class = "note-info", "No problems found."))
      } else {
        items <- lapply(seq_len(nrow(r$issues)), function(i) {
          lvl <- r$issues$level[i]
          tags$li(class = paste0("note-", lvl),
                  span(class = "note-icon", level_icon[[lvl]]), r$issues$message[i])
        })
      }
      tags$details(
        class = paste0("file-notes notes-", r$status),
        open = if (r$status != "ok") NA else NULL,
        tags$summary(r$name, span(class = "notes-status", status_label[[r$status]])),
        tags$ul(items),
        column_table(r)
      )
    })

    div(
      class = "work-section results-section",
      div(
        class = "section-intro",
        h2("3. Results"),
        p(sprintf("%s · %d of %d file%s ready%s",
                  s$label, n_usable, nrow(summary_df),
                  if (nrow(summary_df) == 1) "" else "s",
                  if (n_review > 0) sprintf(", %d need%s review", n_review,
                                            if (n_review == 1) "s" else "") else ""))
      ),
      div(class = "table-scroll", simple_table(summary_df, status_col = "Status")),
      h4(class = "notes-heading", "File checks"),
      unname(file_notes)
    )
  })

  # ---- 4. Datasets ready for analysis ----
  output$datasets_panel <- renderUI({
    res <- results()
    if (is.null(res)) return(NULL)
    s <- get_survey(res$survey)
    b <- built()

    intro <- div(
      class = "section-intro",
      h2("4. Data ready for analysis")
    )

    # Decisions still open
    if (is.null(b)) {
      return(div(
        class = "work-section",
        intro,
        p("Some files need your decision before they can be combined."),
        actionButton("reopen_decisions", "Review decisions", class = "btn-primary")
      ))
    }

    if (length(b$datasets) == 0) {
      return(div(
        class = "work-section",
        intro,
        p(class = "empty-note", sprintf(
          "No files can be analysed as %s. Check the file notes above, or choose a different survey type.",
          s$label))
      ))
    }

    cards <- lapply(b$datasets, function(ds) {
      q <- ds$questions
      notes <- list(
        if (ds$n_excluded > 0) tags$li(sprintf(
          "%d answer%s outside the scale %s excluded from scores (your files are unchanged).",
          ds$n_excluded, if (ds$n_excluded == 1) "" else "s",
          if (ds$n_excluded == 1) "is" else "are")),
        if (ds$n_repeats > 0) tags$li(sprintf(
          "%d exact repeat%s %s included \u2014 see the file notes above.",
          ds$n_repeats, if (ds$n_repeats == 1) "" else "s",
          if (ds$n_repeats == 1) "is" else "are")),
        tags$li(sprintf("%.1f%% of answers are missing and are left as missing.", ds$pct_missing))
      )
      div(
        class = "dataset-card",
        div(
          class = "dataset-head",
          h3(sprintf("Dataset %d", ds$id)),
          span(class = "dataset-tag", sprintf("%s scale", ds$scale_text))
        ),
        div(
          class = "dataset-stats",
          div(class = "stat", span(class = "stat-value", ds$n_responses),
              span(class = "stat-label", "responses")),
          div(class = "stat", span(class = "stat-value", length(ds$files)),
              span(class = "stat-label", if (length(ds$files) == 1) "file" else "files")),
          div(class = "stat", span(class = "stat-value", nrow(q)),
              span(class = "stat-label", "questions"))
        ),
        p(class = "dataset-files", paste("Files:", paste(ds$files, collapse = ", "))),
        tags$ul(class = "dataset-notes", Filter(Negate(is.null), notes)),
        div(class = "table-scroll",
            simple_table(data.frame(
              Question = q$question,
              Measures = ifelse(is.na(q$concept), "\u2014", q$concept),
              Answers  = q$scale,
              Answered = q$answered,
              check.names = FALSE
            )))
      )
    })

    div(
      class = "work-section",
      intro,
      p(class = "section-sub", sprintf(
        "%s \u00B7 %d dataset%s ready", s$label, length(b$datasets),
        if (length(b$datasets) == 1) "" else "s")),
      unname(cards),
      if (length(b$left_out) > 0) p(class = "left-out",
        paste("Left out by your choice:", paste(b$left_out, collapse = ", "))),
      div(
        class = "action-row",
        actionButton("go_analysis", "View analysis \u2192", class = "btn-primary btn-analyse",
                     onclick = "window.scrollTo(0, 0);"),
        if (length(res$plan$decisions) > 0)
          actionButton("reopen_decisions", "Change decisions", class = "btn-outline-secondary")
      )
    )
  })

  # ---- 5. Analysis ----
  analyses <- reactive({
    b <- built()
    res <- results()
    if (is.null(b) || is.null(res) || length(b$datasets) == 0) return(NULL)
    lapply(b$datasets, function(ds) {
      out <- tryCatch(
        run_survey_module(res$survey, ds),
        error = function(e) list(status = "error", message = conditionMessage(e))
      )
      out$dataset <- ds
      out
    })
  })

  # ---- Analysis screen pieces ----
  has_value <- function(x) !is.null(x) && length(x) == 1 && !is.na(x)

  # Navy summary band: index, profile strip, tiles
  hero_ui <- function(h) {
    people <- if (!is.null(h$people)) h$people else c(fav = "favourable", neutral = "neutral", unfav = "unfavourable")
    tile <- function(value, label, cls) {
      div(class = paste("hl-tile", cls), span(class = "hl-tile-value num", value), span(class = "hl-tile-label", label))
    }
    div(class = "hl-hero",
        div(class = "hl-hero-main",
            span(class = "hl-hero-eyebrow", h$label),
            span(class = "hl-hero-value num", fmt_pct(h$index)),
            span(class = "hl-hero-sub", "favourable \u00B7 ", tags$b(tools::toTitleCase(h$level)), " result"),
            if (has_value(h$engaged)) div(class = "hl-hero-strip",
              div(style = sprintf("width:%.1f%%;background:#E98F5A;", h$disengaged)),
              div(style = sprintf("width:%.1f%%;background:#8A97B1;", h$neutral)),
              div(style = sprintf("width:%.1f%%;background:#7FA6DB;", h$engaged)))),
        div(class = "hl-tiles",
            if (has_value(h$engaged)) tile(fmt_pct(h$engaged), people[["fav"]], "hl-tile-fav"),
            if (has_value(h$neutral)) tile(fmt_pct(h$neutral), people[["neutral"]], "hl-tile-neutral"),
            if (has_value(h$disengaged)) tile(fmt_pct(h$disengaged), people[["unfav"]], "hl-tile-unfav"),
            if (has_value(h$enps)) tile(sprintf("%+.0f", h$enps), "eNPS", "hl-tile-plain"),
            tile(format(h$n, big.mark = ","), "responses", "hl-tile-plain")))
  }

  finding_colours <- list(good = c("#E1EAF7", "#1F4078"), bad = c("#FBE3D4", "#8A3A0F"),
                          info = c("#E6EDF8", "#24467F"), warn = c("#FFF4D6", "#7A5A00"))

  findings_ui <- function(fs) {
    if (length(fs) == 0) return(NULL)
    div(class = "hl-card",
        span(class = "hl-eyebrow", "Key findings"),
        div(class = "hl-findings",
            lapply(fs, function(f) {
              col <- finding_colours[[f$tone]]
              div(class = "hl-finding",
                  span(class = "hl-chip", style = sprintf("background:%s;color:%s;", col[1], col[2]), f$tag),
                  div(class = "hl-finding-body",
                      span(class = "hl-finding-title", f$title),
                      span(class = "hl-finding-text", f$text)))
            })))
  }

  block_ui <- function(b) {
    tags$section(
      class = paste("hl-card", if (identical(b$width, "half")) "hl-half" else "hl-full"),
      if (!is.null(b$title)) div(h3(class = "hl-title", b$title),
                                 if (!is.null(b$subtitle)) p(class = "hl-sub", b$subtitle)),
      if (!is.null(b$chart)) div(class = "hl-chart", b$chart),
      if (!is.null(b$insight)) insight_ui(b$insight),
      if (!is.null(b$note) && nzchar(b$note)) p(class = "hl-note", b$note)
    )
  }

  module_ui <- function(out, ds) {
    tab_panels <- lapply(out$tabs, function(t) {
      bslib::nav_panel(t$label, value = t$id, div(class = "hl-grid", lapply(t$blocks, block_ui)))
    })
    tagList(
      hero_ui(out$headline),
      findings_ui(out$findings),
      div(class = "hl-tabs",
          do.call(bslib::navset_pill, c(tab_panels, list(id = paste0("analysis_tabs_", ds$id)))))
    )
  }

  output$analysis_panel <- renderUI({
    outs <- analyses()
    if (is.null(outs)) return(NULL)
    s <- get_survey(results()$survey)

    blocks <- lapply(outs, function(out) {
      ds <- out$dataset
      head <- div(class = "dataset-head",
                  h3(sprintf("Dataset %d", ds$id)),
                  span(class = "dataset-tag", sprintf("%s scale", ds$scale_text)),
                  span(class = "dataset-meta", sprintf("%s responses \u00B7 %s",
                                                       format(ds$n_responses, big.mark = ","),
                                                       paste(ds$files, collapse = ", "))))

      body <- switch(
        out$status,
        ok = if (!is.null(out$tabs)) module_ui(out, ds) else p(class = "empty-note", "No analysis available."),
        not_implemented = p(class = "empty-note", sprintf(
          "The %s analysis is still being built. The data checks already apply.", s$label)),
        error = p(class = "empty-note status-issue", paste("The analysis could not run:", out$message)),
        p(class = "empty-note", if (!is.null(out$message)) out$message else "No analysis available.")
      )
      div(class = "analysis-block", head, body)
    })

    if (length(blocks) == 1) return(blocks[[1]])

    # Several datasets: one tab each
    tabs <- lapply(seq_along(blocks), function(k) {
      ds <- outs[[k]]$dataset
      bslib::nav_panel(sprintf("Dataset %d \u00B7 %s", ds$id, ds$scale_text), blocks[[k]])
    })
    do.call(bslib::navset_pill, c(tabs, list(id = "dataset_tabs")))
  })

  output$analysis_title <- renderUI({
    res <- results()
    if (is.null(res)) return(NULL)
    s <- get_survey(res$survey)
    div(class = "screen-title",
        span(class = "card-icon card-icon-lg", survey_icon(s$icon, 26)),
        div(h2(paste(s$label, "\u2014 analysis")),
            p("Findings, possible explanations and next steps for each dataset.")))
  })
}

shinyApp(ui, server)
