# =============================================================
# HR Survey Labs — data processing
# Reads each uploaded file, checks it, and builds a SAFE working copy.
#
# Rules (from the product vision):
#   * originals are never modified — all cleaning happens on a copy
#   * blanks and recognised missing labels become NA (missing), never answers
#   * missing answers are preserved, never imputed
#   * duplicate-looking rows are REPORTED, never deleted
#
# Out-of-range scores need to know the expected scale, so they are
# checked later in survey_mapping.R.
# =============================================================

# Text that means "no answer" (compared in lower case, after trimming)
MISSING_LABELS <- c("", "na", "n/a", "n.a", "n.a.", "#n/a", "null",
                    "nan", "-", "--", "missing", "no response", "not applicable",
                    "no answer", "skipped")

# Column names that usually identify a respondent rather than an answer
ID_PATTERN <- "(^|_)(id|respondent|respondent_id|employee_id|emp_id|response_id)$"


# ---- Issue log -------------------------------------------------
# Every check adds rows to a small data frame:
#   level   "error"   -> file cannot be used
#           "warning" -> usable, but the user should know
#           "info"    -> routine note
new_issues <- function() {
  data.frame(level = character(0), message = character(0), stringsAsFactors = FALSE)
}

add_issue <- function(issues, level, message) {
  rbind(issues, data.frame(level = level, message = message, stringsAsFactors = FALSE))
}


# ---- 1. Read the file -------------------------------------------
# Everything is read as TEXT and WITHOUT assuming row 1 holds the headings,
# so title rows above the headings and extra rows below them (as in survey
# tool exports) can be found and skipped.
read_raw_file <- function(path, name) {
  issues <- new_issues()
  ext <- tolower(tools::file_ext(name))

  if (!ext %in% c("csv", "xlsx", "xls")) {
    return(list(data = NULL, issues = add_issue(issues, "error",
      sprintf("Unsupported file type (.%s). Upload a CSV or Excel file.", ext))))
  }

  if (!file.exists(path) || isTRUE(file.size(path) == 0)) {
    return(list(data = NULL, issues = add_issue(issues, "error",
      "The file is empty (0 bytes).")))
  }

  raw <- tryCatch({
    if (ext == "csv") {
      read_csv_safely(path)
    } else {
      sheets <- readxl::excel_sheets(path)
      all <- lapply(sheets, function(sh) {
        as.data.frame(readxl::read_excel(path, sheet = sh, col_names = FALSE,
                                         col_types = "text", .name_repair = "minimal"),
                      stringsAsFactors = FALSE, check.names = FALSE)
      })
      # Use the sheet holding the most data (responses, not instructions or charts)
      filled <- vapply(all, function(d) sum(!is.na(as.matrix(d)) & trimws(as.matrix(d)) != ""),
                       numeric(1))
      best <- which.max(filled)
      if (length(sheets) > 1) {
        issues <- add_issue(issues, "info", sprintf(
          "The workbook has %d sheets; the one with the most data (\"%s\") was read.",
          length(sheets), sheets[best]))
      }
      all[[best]]
    }
  }, error = function(e) {
    issues <<- add_issue(issues, "error", paste(
      "The file could not be opened. It may be corrupted or not a real",
      toupper(ext), "file."))
    NULL
  })

  if (is.null(raw)) return(list(data = NULL, issues = issues))
  raw[] <- lapply(raw, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })
  if (nrow(raw) == 0 || ncol(raw) == 0) {
    return(list(data = NULL, issues = add_issue(issues, "error", "The file has no content.")))
  }

  layout <- find_header(raw)
  headings <- layout$headings
  body_rows <- setdiff(seq_len(nrow(raw)), seq_len(layout$last_header_row))

  # Rows with nothing in any column are gaps, not responses
  row_has_data <- rowSums(trimws(as.matrix(raw)) != "") > 0
  empty <- body_rows[!row_has_data[body_rows]]
  body_rows <- setdiff(body_rows, empty)

  data <- raw[body_rows, , drop = FALSE]
  names(data) <- headings
  rownames(data) <- NULL

  if (layout$header_row > 1) {
    above <- layout$header_row - 1
    issues <- add_issue(issues, "info", sprintf(
      "The column headings were found on row %d. %d row%s above %s skipped (titles, notes or question codes).",
      layout$header_row, above, if (above == 1) "" else "s", if (above == 1) "was" else "were"))
  }
  if (layout$last_header_row > layout$header_row) {
    below <- layout$last_header_row - layout$header_row
    issues <- add_issue(issues, "info", sprintf(
      "%d row%s under the headings %s skipped because %s not responses (for example answer labels or import codes from a survey tool).",
      below, if (below == 1) "" else "s", if (below == 1) "was" else "were",
      if (below == 1) "it is" else "they are"))
  }
  if (length(empty) > 0) {
    issues <- add_issue(issues, "info", sprintf(
      "%d completely empty row%s ignored.", length(empty),
      if (length(empty) == 1) " was" else "s were"))
  }

  list(data = data, issues = issues, src_rows = body_rows)
}

# Locate the heading row.
#   * title rows: only one or two cells filled while the data has many columns
#   * heading-like rows: text that never appears again further down the same
#     column (answers repeat; headings do not)
# Several heading-like rows in a row (e.g. Qualtrics: codes, question text,
# import IDs) -> the one with the most descriptive text is used as headings.
find_header <- function(raw) {
  n <- nrow(raw)
  cells <- function(i) trimws(unlist(raw[i, ], use.names = FALSE))
  used_cols <- sum(vapply(raw, function(x) any(trimws(x) != ""), logical(1)))
  if (n < 5) return(list(header_row = 1, last_header_row = 1, headings = cells(1)))

  is_title <- function(i) sum(cells(i) != "") < max(2, 0.5 * used_cols)
  # Rows under the headings can be sparse (SurveyMonkey puts "Response" only
  # under some columns), so a lower fill level is enough for them
  is_heading_like <- function(i, min_share = 0.3) {
    v <- cells(i)
    filled <- which(v != "")
    if (length(filled) < max(2, min_share * used_cols)) return(FALSE)
    # Headings are words; a row that is mostly numbers is a response
    if (mean(grepl("^-?[0-9]+([.,][0-9]+)?$", v[filled])) >= 0.5) return(FALSE)
    body <- raw[(i + 1):min(n, i + 200), , drop = FALSE]
    if (nrow(body) == 0) return(FALSE)
    unseen <- vapply(filled, function(j) !(v[j] %in% trimws(body[[j]])), logical(1))
    mean(unseen) >= 0.7
  }

  i <- 1
  while (i <= min(n, 10) && is_title(i)) i <- i + 1
  if (i > min(n, 10)) return(list(header_row = 1, last_header_row = 1, headings = cells(1)))

  block <- i
  while (block[length(block)] + 1 <= min(n - 1, i + 4) && is_heading_like(block[length(block)] + 1)) {
    block <- c(block, block[length(block)] + 1)
  }

  # Most descriptive row in the block = most words per filled cell
  words <- vapply(block, function(r) {
    v <- cells(r)
    v <- v[v != "" & !startsWith(v, "{")]
    if (length(v) == 0) 0 else mean(lengths(strsplit(v, "\\s+")))
  }, numeric(1))
  header_row <- block[which.max(words)]
  headings <- cells(header_row)

  # Fill blank headings from the other heading rows (e.g. a code row)
  for (j in which(headings == "")) {
    alt <- vapply(block, function(r) cells(r)[j], character(1))
    alt <- alt[alt != "" & !startsWith(alt, "{")]
    if (length(alt) > 0) headings[j] <- alt[1]
  }

  list(header_row = header_row, last_header_row = max(block), headings = headings)
}

# CSV reader: comma, semicolon or tab separated; UTF-8 first, then Latin-1
# (common for files saved from Excel). The number of columns is counted over
# the WHOLE file, because a title row at the top may have only one cell.
read_csv_safely <- function(path) {
  bytes <- readLines(path, warn = FALSE, encoding = "UTF-8")
  encoding <- if (all(validUTF8(bytes))) "UTF-8-BOM" else "latin1"

  first_lines <- utils::head(bytes, 10)
  count <- function(ch) sum(lengths(regmatches(first_lines, gregexpr(ch, first_lines, fixed = TRUE))))
  seps <- c("," = count(","), ";" = count(";"), "\t" = count("\t"))
  delim <- if (max(seps) == 0) "," else names(seps)[which.max(seps)]

  n_cols <- suppressWarnings(max(utils::count.fields(
    path, sep = delim, quote = "\"", blank.lines.skip = FALSE,
    comment.char = ""), na.rm = TRUE))
  if (!is.finite(n_cols) || n_cols < 1) n_cols <- 1

  df <- utils::read.table(
    path, sep = delim, quote = "\"", header = FALSE,
    col.names = paste0("X", seq_len(n_cols)), colClasses = "character",
    fill = TRUE, na.strings = character(0), comment.char = "",
    blank.lines.skip = FALSE, strip.white = FALSE, check.names = FALSE,
    stringsAsFactors = FALSE, fileEncoding = encoding
  )
  df
}


# ---- 2. Check structure -----------------------------------------
check_structure <- function(df, issues) {
  if (is.null(df)) return(issues)

  if (ncol(df) == 0) {
    return(add_issue(issues, "error", "The file has no columns."))
  }
  if (nrow(df) == 0) {
    return(add_issue(issues, "error",
      "The file has column headings but no responses."))
  }

  # Blank headings
  blank_names <- which(is.na(names(df)) | trimws(names(df)) == "")
  if (length(blank_names) > 0) {
    issues <- add_issue(issues, "warning", sprintf(
      "%d column%s had no heading and were named Unnamed_1, Unnamed_2, ...",
      length(blank_names), if (length(blank_names) == 1) "" else "s"))
  }

  # Repeated headings
  trimmed <- trimws(names(df))
  dup_names <- unique(trimmed[duplicated(trimmed) & trimmed != ""])
  if (length(dup_names) > 0) {
    issues <- add_issue(issues, "warning", sprintf(
      "These column headings appear more than once: %s.",
      paste(dup_names, collapse = "; ")))
  }

  # Columns with no data at all
  is_blank <- function(x) all(is.na(x) | tolower(trimws(x)) %in% MISSING_LABELS)
  empty_cols <- names(df)[vapply(df, is_blank, logical(1))]
  if (length(empty_cols) == ncol(df)) {
    return(add_issue(issues, "error", "Every column in the file is blank."))
  }
  if (length(empty_cols) > 0) {
    issues <- add_issue(issues, "info", sprintf(
      "%d column%s contain no answers: %s.",
      length(empty_cols), if (length(empty_cols) == 1) "" else "s",
      paste(empty_cols, collapse = "; ")))
  }

  issues
}


# ---- 3. Build the working copy ----------------------------------
# Returns the cleaned data plus a map from clean names back to the
# original question wording (needed later for mapping and charts).
make_working_copy <- function(df) {
  original_names <- names(df)
  original_names[is.na(original_names) | trimws(original_names) == ""] <-
    paste0("Unnamed_", seq_len(sum(is.na(original_names) | trimws(original_names) == "")))
  original_names <- stringr::str_squish(original_names)

  # Survey tools write matrix questions as "Shared intro - Item". When two or
  # more headings share the same intro, keep only the item text.
  has_intro <- grepl(" - ", original_names, fixed = TRUE)
  if (sum(has_intro) >= 2) {
    intro <- sub(" - .*$", "", original_names)
    repeated <- names(which(table(intro[has_intro]) >= 2))
    shared <- has_intro & intro %in% repeated
    original_names[shared] <- sub("^.*? - ", "", original_names[shared], perl = TRUE)
  }

  clean_names <- janitor::make_clean_names(original_names)
  names(df) <- clean_names

  case_fixes <- 0L

  for (col in clean_names) {
    x <- df[[col]]
    x <- stringr::str_squish(x)                         # trim + collapse spaces
    x[is.na(x) | tolower(x) %in% MISSING_LABELS] <- NA  # missing labels -> NA

    non_missing <- x[!is.na(x)]
    as_num <- suppressWarnings(as.numeric(non_missing))

    if (length(non_missing) > 0 && all(!is.na(as_num))) {
      # Every answer is a number -> store as numeric
      x <- suppressWarnings(as.numeric(x))
    } else {
      # Text column: unify spellings that differ only by letter case,
      # using the most common spelling (e.g. "OPERATIONS" -> "Operations")
      if (length(non_missing) > 0) {
        key <- tolower(non_missing)
        most_common <- tapply(non_missing, key, function(v) names(sort(table(v), decreasing = TRUE))[1])
        fixed <- unname(most_common[tolower(x)])
        changed <- !is.na(x) & fixed != x
        case_fixes <- case_fixes + sum(changed, na.rm = TRUE)
        x <- ifelse(is.na(x), NA, fixed)
      }
    }
    df[[col]] <- x
  }

  list(
    data = df,
    column_map = data.frame(clean = clean_names, original = original_names,
                            stringsAsFactors = FALSE),
    case_fixes = case_fixes
  )
}


# ---- 4. Missing data profile ------------------------------------
find_id_columns <- function(df) {
  names(df)[grepl(ID_PATTERN, names(df))]
}

profile_missing <- function(df) {
  id_cols <- find_id_columns(df)
  # Columns with no data at all (e.g. survey-tool fields that were never
  # filled) are reported separately and do not count as missing answers
  empty_cols <- names(df)[vapply(df, function(x) all(is.na(x)), logical(1))]
  answer_cols <- setdiff(names(df), c(id_cols, empty_cols))

  by_column <- data.frame(
    column = names(df),
    missing = vapply(df, function(x) sum(is.na(x)), integer(1)),
    pct_missing = round(100 * vapply(df, function(x) mean(is.na(x)), numeric(1)), 1),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (length(answer_cols) == 0) {
    return(list(by_column = by_column, overall_pct = 0, blank_rows = integer(0)))
  }

  answers <- df[, answer_cols, drop = FALSE]
  list(
    by_column = by_column,
    overall_pct = round(100 * mean(is.na(as.matrix(answers))), 1),
    blank_rows = which(rowSums(!is.na(answers)) == 0)
  )
}


# ---- 5. Duplicate check (report only, never delete) -------------
flag_duplicates <- function(df) {
  id_cols <- find_id_columns(df)

  # Identical in every column, including the ID: pair each repeat with
  # the first row it copies
  row_keys <- do.call(paste, c(lapply(df, as.character), sep = "\r"))
  repeats <- which(duplicated(row_keys))
  originals <- match(row_keys[repeats], row_keys)

  # Same respondent ID used with DIFFERENT answers
  # (exact copies are already reported above, so they are excluded here)
  dup_ids <- character(0)
  if (length(id_cols) > 0) {
    ids <- as.character(df[!duplicated(df), id_cols[1]])
    dup_ids <- unique(ids[!is.na(ids) & duplicated(ids)])
  }

  list(repeats = repeats,
       originals = originals,
       n_extra_copies = length(repeats),
       duplicate_ids = dup_ids)
}


# ---- Main entry point: one file in, one result out --------------
process_file <- function(path, name) {
  read <- read_raw_file(path, name)
  issues <- check_structure(read$data, read$issues)

  # Original file row number of each response, for messages
  src <- if (!is.null(read$src_rows)) read$src_rows else seq_len(max(0, nrow(read$data))) + 1
  file_row <- function(i) src[i]

  result <- list(name = name, status = "error", issues = issues,
                 data = NULL, column_map = NULL, src_rows = src,
                 n_rows = NA_integer_, n_cols = NA_integer_,
                 pct_missing = NA_real_, n_duplicates = NA_integer_)

  if (any(issues$level == "error")) return(result)

  work <- make_working_copy(read$data)
  df <- work$data

  if (work$case_fixes > 0) {
    issues <- add_issue(issues, "info", sprintf(
      "%d text value%s differed only in capitalisation and were standardised (e.g. \"OPERATIONS\" -> \"Operations\").",
      work$case_fixes, if (work$case_fixes == 1) "" else "s"))
  }

  miss <- profile_missing(df)
  if (miss$overall_pct > 0) {
    worst <- miss$by_column[miss$by_column$pct_missing < 100, ]
    worst <- worst[order(-worst$pct_missing), ]
    worst <- worst[worst$pct_missing > 0, ][1:min(3, sum(worst$pct_missing > 0)), ]
    issues <- add_issue(issues, if (miss$overall_pct >= 20) "warning" else "info", sprintf(
      "%.1f%% of answers are missing. Most affected: %s. Missing answers are kept as missing, not filled in.",
      miss$overall_pct,
      paste(sprintf("%s (%.0f%%)", work$column_map$original[match(worst$column, work$column_map$clean)],
                    worst$pct_missing), collapse = "; ")))
  }
  if (length(miss$blank_rows) > 0) {
    issues <- add_issue(issues, "warning", sprintf(
      "%d response%s %s completely blank (row %s).",
      length(miss$blank_rows), if (length(miss$blank_rows) == 1) "" else "s",
      if (length(miss$blank_rows) == 1) "is" else "are",
      paste(file_row(miss$blank_rows), collapse = ", ")))
  }

  dups <- flag_duplicates(df)
  if (dups$n_extra_copies > 0) {
    shown <- utils::head(seq_along(dups$repeats), 10)
    pairs <- sprintf("row %d repeats row %d",
                     file_row(dups$repeats[shown]), file_row(dups$originals[shown]))
    issues <- add_issue(issues, "warning", sprintf(
      "%d exact repeat%s found: %s%s. They were kept; check whether they are genuine.",
      dups$n_extra_copies, if (dups$n_extra_copies == 1) "" else "s",
      paste(pairs, collapse = "; "),
      if (dups$n_extra_copies > 10) "; ..." else ""))
  }
  if (length(dups$duplicate_ids) > 0) {
    issues <- add_issue(issues, "warning", sprintf(
      "These respondent IDs appear more than once: %s.",
      paste(utils::head(dups$duplicate_ids, 10), collapse = ", ")))
  }

  result$status <- if (any(issues$level == "warning")) "warning" else "ok"
  result$issues <- issues
  result$data <- df
  result$column_map <- work$column_map
  result$n_rows <- nrow(df)
  result$n_cols <- ncol(df)
  result$pct_missing <- miss$overall_pct
  result$n_duplicates <- dups$n_extra_copies
  result
}
