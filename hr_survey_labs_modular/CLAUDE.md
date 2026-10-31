# HR Labs (HR Survey Labs) — project handover

Read this first. It holds the product vision, the decisions behind the code, what
is built and tested, and what comes next. Keep it up to date as work progresses.

## 1. What the product is

An R Shiny app that analyses **existing HR survey response files**. An HR user picks
one of 16 survey types, uploads one or more Excel/CSV files, clicks Analyse, and gets
a validated, consolidated analysis: charts, findings, possible explanations and
recommended next steps. It will be deployed to shinyapps.io once stable.

Owner: Akshat Chhangani (MBA HR, NMIMS Mumbai). Product name: **HR Labs**
(the app header shows "HR Labs").

**The users are HR professionals, not data scientists, and the owner will not be
there when they use it.** The app must either produce a correct analysis or stop and
ask a clear question in plain HR language. It must never produce a quietly wrong
result. Every design decision follows from this.

## 2. Rules that must not be broken (from the owner's product vision)

- Exactly **16 predefined survey types** (see `R/survey_registry.R`). No custom types,
  no "More survey types" option.
- Analyses existing responses only. Not a survey creation or distribution tool.
- Input is **Excel (.xlsx, .xls) and CSV only**. No connections to Google Forms,
  Qualtrics etc. (but their Excel/CSV *exports* must be read correctly).
- Never silently guess survey type or question equivalence. Ask when unsure.
- Never invent answers, impute missing data, or delete duplicate-looking rows.
  Missing stays missing; duplicates are reported, not removed.
- Originals are never modified; all cleaning happens on a working copy.
- Never combine incompatible questions or answer scales as if equivalent
  (a 1–5 file and a 1–7 file are kept separate).
- **Every chart carries three insights**: what it shows, what might explain it
  (labelled as hypotheses), what HR should do next. Correlation is never presented
  as causation.
- Build and verify one complete module (Employee Engagement) before scaling to 16.
- Design: navy / white / grey palette, modern and professional, a home screen with
  exactly 16 survey cards each with an icon. The owner rejected emojis as
  unprofessional: cards use the line icons in `SURVEY_ICONS` (survey_registry.R),
  grouped by category, with a 'Full analysis' / 'Coming soon' status from the
  registry's `ready` flag (set it to TRUE when a module is built).
- Footer on every page with **both** emails, equally prominent:
  akshat.chhangani070@nmims.in and akshatchhangani78@gmail.com
- The privacy page was deliberately removed. Do not add it back, and do not add a
  data-handling note unless asked.

## 3. How the code is organised

```
app.R                         UI, two screens (Prepare data -> Analysis), workflow, routing
R/_disable_autoload.R         empty on purpose: stops Shiny auto-sourcing R/
R/data_processing.R           read files, find heading row, working copy, missing, duplicates
R/survey_mapping.R            concept library, column roles, survey match, scales,
                              reverse-worded questions, combining files into datasets
R/analytics_common.R          scoring, favourability, group comparisons, drivers, Plotly charts
R/insights_common.R           three-part insight object and UI, wording helpers
R/survey_registry.R           the 16 survey types (key, label, emoji, focus, module fn name)
R/survey_modules/*.R          one file per survey; only employee_engagement.R is real,
                              the other 15 return list(status = "not_implemented")
www/styles.css                all styling
test_data/                    generated test files (see section 6)
```

**Loading order matters.** `app.R` sources files explicitly: data_processing,
survey_mapping, analytics_common, insights_common, every module, then
survey_registry LAST. The registry stores module function names as strings and looks
them up with `get0()` at run time, so the app starts even if a module is unfinished.
This fixed an early "survey_module_employee_satisfaction not found" startup error.
Do not put any other `.R` file in `R/survey_modules/`: every file there is sourced
at start-up.

## 4. The pipeline (what happens on Analyse)

1. **Read** (`read_raw_file`): CSV (comma/semicolon/tab, UTF-8 or Latin-1; column
   count taken from the whole file) or Excel (the sheet with the most data). Read
   as text with no header assumed.
2. **Find the heading row** (`find_header`): skips title rows above; detects extra
   heading rows below (Qualtrics: code / question text / ImportId; SurveyMonkey:
   "Response" row) and keeps the most descriptive one. Empty rows are dropped.
   Original file row numbers are kept (`src_rows`) so messages cite real rows.
3. **Working copy** (`make_working_copy`): squish whitespace, missing labels -> NA,
   numeric columns -> numbers, case variants unified ("OPERATIONS" -> "Operations"),
   matrix-question prefixes stripped ("Intro - Item" -> "Item").
4. **Checks**: structure, missing answers (fully empty columns excluded from the %),
   exact duplicate rows and reused IDs (reported only).
5. **Column roles** (`column_role`): id, detail (respondent detail), time, system
   (survey-tool fields, ignored), record (HR-system data such as absence days or
   performance rating, used for group comparisons), comment, empty, question.
6. **Answer profile + scale** (`profile_answers`, `check_scales`): numeric rating
   scales (1–5, 0–5, 1–7, 1–10, 0–10) or text scales (agreement, satisfaction,
   frequency, likelihood, quality, extent, importance; variants like "Tend to agree"
   and "5 - Strongly agree" are normalised by `norm_label`). Each question gets its
   own scale: a scale written in the heading wins ("_0_10"), NPS/recommend 0–10 is
   recognised, otherwise the scale most questions use. Out-of-range answers are
   flagged, left unchanged, excluded from scores.
7. **Survey match** (`assess_relevance`): every question heading is matched against
   the concept library for all 16 types. Matching: a phrase matches when every word
   is a prefix of some word in the heading (order-free; abbreviations expanded,
   e.g. MGR -> manager, WLB -> work life balance). Decisions: match / mismatch
   (names the better type) / unclear / unrecognised / coded_headers / no_questions.
   **Broad surveys**: Engagement and Satisfaction accept a few core questions plus
   driver questions from related areas (manager support, growth, recognition...).
8. **Negatively worded questions** (`is_negatively_worded`): a negative cue XOR a
   negation ("I often feel burned out" is reversed; "Mistakes are not held against
   me" is not). Reverse-scored in analysis; the user can untick them.
9. **Plan and decisions** (`plan_datasets`): files with the same questions on the
   same scale are combined; otherwise the user decides in a dialog (different scale,
   different questions, needs review, coded headings Q1/Q2 with a dropdown per
   question, reverse-scored list). Nothing proceeds silently.
10. **Datasets** (`build_datasets`) -> **module** (`run_survey_module`) -> Analysis
    screen.

## 5. Employee Engagement module (the reference module)

`R/survey_modules/employee_engagement.R`. Handles classic designs (several
engagement statements) and driver designs (1–2 outcomes + driver questions).

- Favourable = above the scale midpoint (1–5: 4–5 fav, 3 neutral, 1–2 unfav).
- Engagement index = % favourable on the core engagement questions only.
- **Output contract** (used by app.R, to be reused by all 16 modules):
  `list(status = "ok", headline, findings, tabs)`. `headline` feeds the navy
  summary band; `findings` (tag, tone good/bad/info/warn, title, text) feed the
  Key findings cards; `tabs` is a list of `list(id, label, blocks)`, each block
  `list(title, subtitle, chart, insight, note, width = "full"|"half")` where
  `chart` is an HTML chart built with the helpers at the end of
  `analytics_common.R` (chart_columns, chart_diverging, chart_heatmap,
  chart_group_bars, chart_driver_table, ranked_list, legend_ui).
- Tabs: Overview (answer distribution, eNPS 0–10) · Scores by area (centred
  diverging bars, strengths / areas to improve) · Groups & hotspots (hotspot map
  of the first group dimension × areas coloured by gap from the company, compact
  bars for other dimensions, HR records by band) · What drives engagement
  (driver table with Priority / Protect / Monitor / No link tags) · Action plan
  (cards with evidence, action, suggested owner, timing).
- Charts are HTML (not Plotly) by design decision: they match the approved
  design exactly and show details on hover via title tooltips.
- Groups under 5 responses are hidden. Correlation is never presented as cause.
- The approved design preview lives in the owner's Claude artifacts
  ("HR Labs Analysis Screen").

## 6. Testing

R could not be run in the original development environment, so the logic was
mirrored and tested in Python. **If you can run R, prefer real end-to-end tests.**

- `test_data/*.csv`: engagement, 3 compatible leadership files, an exit survey,
  edge cases (empty, headers only, missing + duplicates + out-of-range, 1–7 scale).
- `test_data/formats/fmt01–fmt11`: the same survey as different tools export it
  (Google Forms, Microsoft Forms, Qualtrics, SurveyMonkey, title rows, multi-sheet
  Excel, numbered labels, coded headings, negative wording, HRIS codes with eNPS,
  semicolon CSV). All 11 were confirmed working in the app by the owner.
- Expected verdicts: every engagement file matches Employee Engagement and is
  rejected (with the right suggestion) under Leadership; leadership files match
  only Leadership; the exit file only Exit Survey; coded headings ask for labels.
- Always re-run the earlier files after a change to confirm verdicts are unchanged.

## 7. Status and next steps

Done: the redesigned tabbed analysis screen (approved design). Steps 1–6 of the roadmap (setup, skeleton, 16 cards, upload/analyse/clear,
file checks, survey matching, scale detection, combining + decisions) and step 7
(Employee Engagement module, separate analysis screen, 11 export formats).

**Recently fixed:** HR-system record columns (absence days, training hours, promotions,
performance rating, overtime, salary...) are now read as role `record`, not as survey
questions: by name (`HR_RECORD_PATTERN`, excluding opinion words like "satisfaction")
and by value (a short numeric column with no meaning in any survey type whose numbers
do not fit the file's answer scale). Records are banded (`band_numeric`) and used in
group comparisons. Test file: `test_data/test_data_hr_records.csv`. The owner's own
6,000-row file (which surfaced the bug) still needs re-testing.

Then, in order:
1. Finish hardening Engagement with the owner's real files.
2. Build the other 15 modules on the engagement pattern, most similar first:
   Satisfaction, Workplace Culture, Leadership, Psychological Safety, HR Services;
   then Onboarding, Performance Management, Compensation & Benefits, DEI, Candidate
   Experience, Employer Branding; then TNA (gap scoring), Training Effectiveness
   (before/after), Pulse (trends over time), Exit (reason ranking + comment themes,
   tidytext).
3. Reporting and polish; a full test pass (valid, wrong type, ambiguous, multiple
   uploads, missing data, incompatible scales, duplicates).
4. Deploy to shinyapps.io (`rsconnect::deployApp()`; keep relative paths; upload
   limit set to 30 MB in app.R; free tier ~1 GB RAM).

Ideas noted but not decided: an optional AI layer that sends only column headings
(never responses) to suggest question mappings, added on top of the concept library.

## 8. Working with the owner

- The owner works in RStudio on a Mac; project at `~/Documents/R/hr_survey_labs_modular`.
- Explain changes in plain language; give clear test steps with expected results.
- Downloads from chat have been unreliable for the owner; prefer changes they can
  apply in one step.
- Use `dplyr::filter()` style namespacing to avoid masking clashes (stats, plotly).
