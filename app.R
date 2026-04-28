library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(ggplot2)

# Load ground truth data
ground_truth <- read_csv("data/ground_truth_summary.csv", show_col_types = FALSE)
objects <- read_csv("data/ground_truth_objects.csv", show_col_types = FALSE)

# Sample metadata
samples <- ground_truth |>
  arrange(fabric_type, sample_name, sample_id) |>
  mutate(
    orig_path = paste0("images/original/", file_name),
    anno_path = paste0("images/annotated/", gsub(".jpg", "_result.jpg", file_name, fixed = TRUE))
  )

n_samples <- nrow(samples)

# Size class colors — MUST match the annotated image colors
size_colors <- c(
  "Fine"        = "#3498db",  # blue
  "Medium"      = "#2ecc71",  # green
  "Coarse"      = "#f39c12",  # orange
  "Very Coarse" = "#e74c3c"   # red
)

area_pct_choices <- c(1, 3, 5, 7, 10, 15, 20, 25, 30, 40, 50, 55, 60)
default_area_pct <- 10
default_module <- "area"
lesson_target <- 10
strong_threshold <- 85
review_threshold <- 60
miss_review_delay <- 2
practice_review_delay <- 6

lesson_defs <- data.frame(
  value = c("area", "count", "size", "sphericity", "all"),
  title = c("Inclusion Area", "Grain Count", "Grain Size", "Grain Shape", "Full Practice"),
  description = c(
    "Estimate total inclusion coverage.",
    "Estimate the number of visible inclusions.",
    "Identify the dominant size pattern.",
    "Practice roundedness and sphericity.",
    "Combine every skill in one attempt."
  ),
  tag = c("Start here", "Focused", "Focused", "Focused", "Advanced"),
  icon = c("chart-pie", "list-ol", "grip", "circle-half-stroke", "layer-group"),
  stringsAsFactors = FALSE
)

sample_pool <- samples |>
  mutate(sample_index = row_number(), enabled = TRUE)

# Scoring function: 0-100 based on relative error
calc_score <- function(guess, actual) {
  if (is.null(guess) || is.na(guess)) return(0)
  if (actual == 0) return(if (guess == 0) 100 else 0)
  error_pct <- abs(guess - actual) / actual * 100
  max(0, round(100 - error_pct))
}

size_dist_from_guess <- function(dom, sorting, sec) {
  cats <- c("Fine", "Medium", "Coarse", "Very Coarse")
  pcts <- setNames(rep(0, length(cats)), cats)

  if (is.null(dom) || is.na(dom) || !dom %in% cats) dom <- "Medium"
  if (is.null(sorting) || is.na(sorting)) sorting <- "moderate"
  if (is.null(sec) || is.na(sec) || !sec %in% cats) sec <- "Coarse"

  if (sorting == "well") {
    pcts[dom] <- 70
    idx <- which(cats == dom)
    neighbors <- cats[max(1, idx - 1):min(4, idx + 1)]
    neighbors <- setdiff(neighbors, dom)
    if (length(neighbors) > 0) {
      pcts[neighbors] <- 30 / length(neighbors)
    } else {
      pcts[dom] <- 100
    }
  } else if (sorting == "moderate") {
    pcts[dom] <- 45
    idx <- which(cats == dom)
    neighbors <- cats[max(1, idx - 1):min(4, idx + 1)]
    neighbors <- setdiff(neighbors, dom)
    remaining <- setdiff(cats, c(dom, neighbors))
    if (length(neighbors) > 0) pcts[neighbors] <- 35 / length(neighbors)
    if (length(remaining) > 0) pcts[remaining] <- 20 / length(remaining)
  } else if (sorting == "poor") {
    pcts[] <- 25
  } else if (sorting == "bimodal") {
    if (sec == dom) {
      idx <- which(cats == dom)
      candidates <- c(idx + 1, idx - 1, idx + 2, idx - 2)
      candidates <- candidates[candidates >= 1 & candidates <= length(cats)]
      sec <- cats[candidates[1]]
    }
    pcts[dom] <- 40
    pcts[sec] <- 40
    remaining <- setdiff(cats, c(dom, sec))
    if (length(remaining) > 0) pcts[remaining] <- 20 / length(remaining)
  } else {
    pcts <- size_dist_from_guess(dom, "moderate", sec)
  }

  as.numeric(pcts)
}

score_grade <- function(score) {
  if (score >= 90) list(letter = "A", color = "#2ecc71", label = "Excellent")
  else if (score >= 75) list(letter = "B", color = "#27ae60", label = "Good")
  else if (score >= 60) list(letter = "C", color = "#f39c12", label = "Fair")
  else if (score >= 40) list(letter = "D", color = "#e67e22", label = "Needs practice")
  else list(letter = "F", color = "#e74c3c", label = "Keep trying")
}

# ggplot theme
theme_petro <- function() {
  theme_minimal(base_size = 14) %+replace%
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "top",
      plot.margin = margin(10, 10, 10, 10)
    )
}

# --- CSS ---
app_css <- "
/* Viewport-aware layout */
.main-content {
  max-height: calc(100vh - 80px);
  overflow-y: auto;
}

/* Constrain images to viewport */
.img-container img {
  max-height: calc(100vh - 200px);
  width: 100%;
  object-fit: contain;
}

.img-container-split img {
  max-height: calc(100vh - 300px);
  width: 100%;
  object-fit: contain;
}

.image-frame {
  position: relative;
  display: inline-block;
  width: 100%;
}

.scale-overlay {
  position: absolute;
  left: 14px;
  bottom: 14px;
  padding: 6px 8px;
  border-radius: 4px;
  background: rgba(0, 0, 0, 0.62);
  color: #fff;
  font-size: 12px;
  line-height: 1;
  text-shadow: 0 1px 2px rgba(0, 0, 0, 0.7);
}

.scale-line {
  width: 100px;
  height: 8px;
  border-left: 2px solid #fff;
  border-right: 2px solid #fff;
  border-bottom: 3px solid #fff;
  margin-bottom: 5px;
}

.area-reference {
  display: grid;
  grid-template-columns: 72px 1fr;
  gap: 8px;
  align-items: center;
  margin-top: 6px;
  padding: 6px;
  border: 1px solid #dee2e6;
  border-radius: 6px;
  background: #f8f9fa;
}

.area-reference img {
  width: 72px;
  height: 72px;
  object-fit: contain;
  border: 1px solid #adb5bd;
  background: #fff;
}

.area-reference-title {
  font-size: 12px;
  font-weight: 700;
}

.area-reference-note {
  font-size: 11px;
  color: #6c757d;
}

.size-legend-large {
  display: flex;
  flex-wrap: wrap;
  gap: 10px 16px;
  align-items: center;
  margin-top: 8px;
  padding: 8px 10px;
  border: 1px solid #dee2e6;
  border-radius: 6px;
  background: #fff;
  font-size: 14px;
}

.size-legend-title {
  font-weight: 700;
  margin-right: 2px;
}

.size-legend-item {
  display: inline-flex;
  align-items: center;
  gap: 5px;
}

.size-legend-swatch {
  width: 16px;
  height: 16px;
  border-radius: 3px;
  display: inline-block;
  border: 1px solid rgba(0, 0, 0, 0.2);
}

.shape-guide-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(110px, 1fr));
  gap: 10px;
  margin: 8px 0 16px;
}

.shape-guide-item {
  text-align: center;
  font-size: 12px;
}

.shape-guide-item svg {
  display: block;
  width: 88px;
  height: 58px;
  margin: 0 auto 4px;
}

/* Score ring */
.score-ring {
  position: relative;
  width: 100px;
  height: 100px;
  margin: 0 auto;
}
.score-ring svg { transform: rotate(-90deg); }
.score-ring .score-text {
  position: absolute;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%);
  text-align: center;
}
.score-ring .score-letter {
  font-size: 28px;
  font-weight: 700;
  line-height: 1;
}
.score-ring .score-label {
  font-size: 10px;
  opacity: 0.7;
}

/* Stat card */
.stat-card {
  background: #f8f9fa;
  border: 1px solid #dee2e6;
  border-radius: 8px;
  padding: 12px;
  text-align: center;
}
.stat-card .stat-value { font-size: 24px; font-weight: 700; }
.stat-card .stat-label { font-size: 11px; opacity: 0.7; text-transform: uppercase; letter-spacing: 0.5px; }
.stat-card .stat-diff { font-size: 13px; margin-top: 2px; }

/* Smooth transitions */
.fade-in { animation: fadeIn 0.3s ease-in; }
@keyframes fadeIn { from { opacity: 0; transform: translateY(10px); } to { opacity: 1; transform: translateY(0); } }

/* Performance trend mini-chart */
.perf-dot {
  width: 10px;
  height: 10px;
  border-radius: 50%;
  display: inline-block;
  margin: 0 2px;
  opacity: 0.3;
}
.perf-dot.completed { opacity: 1; }

.session-nav .btn {
  width: 34px;
  height: 32px;
  display: inline-flex;
  align-items: center;
  justify-content: center;
}

.lesson-header {
  display: grid;
  grid-template-columns: minmax(0, 1fr) minmax(180px, 260px);
  gap: 16px;
  align-items: center;
  margin-bottom: 10px;
  padding: 10px 12px;
  border: 1px solid #dee2e6;
  border-radius: 8px;
  background: #fff;
}

.lesson-header h5 {
  margin: 0;
  font-size: 16px;
  font-weight: 700;
}

.lesson-progress-meta {
  margin-top: 2px;
  color: #6c757d;
  font-size: 12px;
}

.progress-track {
  width: 100%;
  height: 8px;
  overflow: hidden;
  border-radius: 999px;
  background: #e9ecef;
}

.progress-fill {
  height: 100%;
  border-radius: 999px;
  background: #2c3e50;
  transition: width 0.25s ease;
}

.progress-label {
  display: flex;
  justify-content: space-between;
  margin-bottom: 4px;
  color: #495057;
  font-size: 11px;
  font-weight: 700;
}

.lesson-grid {
  display: grid;
  gap: 6px;
}

.lesson-card.btn {
  width: 100%;
  white-space: normal;
  text-align: left;
  border: 1px solid #dee2e6;
  border-radius: 8px;
  background: #fff;
  color: #212529;
  padding: 8px 10px;
}

.lesson-card.btn:hover {
  border-color: #95a5a6;
  background: #f8f9fa;
}

.lesson-card.btn.active {
  border-color: #2c3e50;
  background: #eef3f7;
  box-shadow: 0 0 0 2px rgba(44, 62, 80, 0.12);
}

.lesson-card.btn.practicing .lesson-tag {
  background: #fff3cd;
  color: #664d03;
}

.lesson-card.btn.strong .lesson-tag {
  background: #d1e7dd;
  color: #0f5132;
}

.lesson-card.btn.complete .lesson-tag {
  background: #cfe2ff;
  color: #084298;
}

.lesson-card-active {
  border: 1px solid #2c3e50;
  background: #eef3f7;
  box-shadow: 0 0 0 2px rgba(44, 62, 80, 0.12);
  border-radius: 8px;
  padding: 8px 10px;
}

.lesson-card-active.practicing .lesson-tag { background: #fff3cd; color: #664d03; }
.lesson-card-active.strong .lesson-tag { background: #d1e7dd; color: #0f5132; }
.lesson-card-active.complete .lesson-tag { background: #cfe2ff; color: #084298; }

.round-recap-skill {
  font-size: 16px;
  font-weight: 700;
  margin-bottom: 12px;
}
.round-recap-skill i { margin-right: 6px; }

.round-recap-dims {
  display: flex;
  flex-wrap: wrap;
  justify-content: center;
  gap: 8px;
}

.round-recap-chip {
  display: inline-block;
  padding: 4px 10px;
  border-radius: 999px;
  background: #f8f9fa;
  border: 1px solid #dee2e6;
  font-size: 13px;
}

.lesson-title {
  display: flex;
  align-items: center;
  gap: 7px;
  font-size: 13px;
  font-weight: 700;
}

.lesson-title i {
  width: 16px;
  text-align: center;
}

.lesson-tag {
  margin-left: auto;
  padding: 1px 6px;
  border-radius: 999px;
  background: #e9ecef;
  color: #495057;
  font-size: 10px;
  font-weight: 600;
}

.lesson-desc {
  margin-top: 3px;
  color: #6c757d;
  font-size: 11px;
}

.module-native {
  display: none;
}

.feedback-card {
  background: #fff;
  border: 1px solid #dee2e6;
  border-radius: 8px;
  padding: 10px 12px;
  margin-bottom: 8px;
}

.feedback-card h6 {
  margin-bottom: 6px;
  font-size: 13px;
  font-weight: 700;
}

.feedback-card ul {
  margin: 0;
  padding-left: 18px;
  font-size: 13px;
}

.result-actions {
  display: flex;
  gap: 8px;
  justify-content: flex-end;
  margin-top: 8px;
}

.result-actions .btn {
  min-width: 130px;
}

@media (max-width: 768px) {
  .lesson-header {
    grid-template-columns: 1fr;
  }
}

/* Module badge */
.module-badge {
  display: inline-block;
  padding: 2px 8px;
  border-radius: 4px;
  font-size: 11px;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}

/* Compact sidebar */
.sidebar-section { margin-bottom: 8px; }
.sidebar-section h6 { margin-bottom: 4px; font-size: 13px; }
"

# --- UI ---
ui <- page_navbar(
  id = "main_nav",
  title = "Ceramic Petrography Trainer",
  selected = "Practice",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Inter")
  ),

  header = tags$head(
    tags$style(HTML(app_css))
  ),

  # --- Practice tab with module selector ---
  nav_panel(
    "Practice",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,

        # Session controls
        div(
          class = "session-nav d-flex justify-content-between align-items-center mb-2",
          actionButton("restart_lesson", icon("rotate-left"),
                       class = "btn-sm btn-outline-secondary",
                       title = "Restart lesson"),
          h6(textOutput("sample_label"), style = "margin:0;"),
          actionButton("skip_sample", icon("forward"),
                       class = "btn-sm btn-outline-secondary",
                       title = "Skip this question")
        ),

        # Sample info badges
        uiOutput("sample_info"),

        # Performance dots
        uiOutput("perf_dots"),

        hr(style = "margin: 8px 0;"),

        # Module selector
        div(
          class = "sidebar-section",
          h6("Current skill"),
          uiOutput("lesson_cards"),
          div(
            class = "module-native",
            radioButtons("module", NULL,
                         choices = c(
                           "Inclusion Area" = "area",
                           "Grain Count" = "count",
                           "Grain Size" = "size",
                           "Grain Shape" = "sphericity",
                           "Full Practice" = "all"
                         ),
                         selected = default_module)
          )
        ),

        # --- Inclusion area inputs ---
        conditionalPanel(
          condition = "input.module == 'all' || input.module == 'area'",
          div(
            class = "sidebar-section",
            sliderInput("guess_pct", "Estimated inclusion area",
                        min = min(area_pct_choices), max = max(area_pct_choices),
                        value = default_area_pct, step = 1, post = "%"),
            uiOutput("area_reference")
          )
        ),

        # --- Count inputs ---
        conditionalPanel(
          condition = "input.module == 'all' || input.module == 'count'",
          div(
            class = "sidebar-section",
            sliderInput("guess_count", "Estimated number of inclusions",
                        min = 0, max = 300, value = 50, step = 5)
          )
        ),

        # --- Grain size inputs ---
        conditionalPanel(
          condition = "input.module == 'all' || input.module == 'size'",
          div(
            class = "sidebar-section",
            h6("Grain size distribution"),
            selectInput("guess_dominant", "Dominant size",
                        choices = c("Fine", "Medium", "Coarse", "Very Coarse"),
                        selected = "Medium"),
            selectInput("guess_sorting", "Sorting",
                        choices = c("Well-sorted (one size dominates)" = "well",
                                    "Moderately sorted" = "moderate",
                                    "Poorly sorted (mixed sizes)" = "poor",
                                    "Bimodal (two distinct sizes)" = "bimodal"),
                        selected = "moderate"),
            conditionalPanel(
              condition = "input.guess_sorting == 'bimodal'",
              selectInput("guess_secondary", "Secondary size",
                          choices = c("Fine", "Medium", "Coarse", "Very Coarse"),
                          selected = "Coarse")
            )
          )
        ),

        # --- Sphericity inputs ---
        conditionalPanel(
          condition = "input.module == 'all' || input.module == 'sphericity'",
          div(
            class = "sidebar-section",
            h6("Grain shape"),
            selectInput("guess_roundedness", "Roundedness",
                        choices = c("Angular" = "angular",
                                    "Sub-angular" = "subangular",
                                    "Sub-rounded" = "subrounded",
                                    "Rounded" = "rounded",
                                    "Well-rounded" = "wellrounded"),
                        selected = "subrounded"),
            selectInput("guess_sphericity", "Sphericity",
                        choices = c("Low (elongated)" = "low",
                                    "Medium" = "medium",
                                    "High (equant)" = "high"),
                        selected = "medium"),
            actionButton("shape_guide", "Shape Guide",
                         class = "btn-outline-secondary btn-sm w-100",
                         icon = icon("circle-info"))
          )
        ),

        hr(style = "margin: 8px 0;"),

        actionButton("reveal", "Reveal Ground Truth",
                     class = "btn-primary w-100 mt-1",
                     icon = icon("eye")),
        actionButton("reset", "Reset & Try Again",
                     class = "btn-outline-secondary w-100 mt-1",
                     icon = icon("rotate-left"))
      ),

      # Main content
      div(
        class = "main-content",
        uiOutput("lesson_header"),
        uiOutput("image_display"),
        uiOutput("results_panel")
      )
    )
  ),

  # --- Performance tab ---
  nav_panel(
    "Performance",
    div(
      class = "container mt-4",
      style = "max-width: 900px;",
      h3("Your Performance"),
      uiOutput("performance_summary"),
      div(class = "mt-4",
          plotOutput("performance_trend", height = "280px")),
      div(class = "mt-4",
          plotOutput("bias_plot", height = "230px"))
    )
  ),

  # --- About tab ---
  nav_panel(
    "About",
    div(
      class = "container mt-4",
      style = "max-width: 800px;",
      h2("Ceramic Petrography Trainer"),
      p("An AI-assisted training tool for learning ceramic thin section analysis."),
      p("Use the Practice tab to work through focused lessons, estimate one feature at a time, and then reveal the ground truth reference."),
      h4("Training Modules"),
      p("Use the module selector in the Practice tab to focus on specific skills:"),
      tags$ul(
        tags$li(tags$strong("Inclusion Area"), " \u2014 Estimate what percentage of the image is occupied by inclusions (temper + aplastics)"),
        tags$li(tags$strong("Grain Count"), " \u2014 Estimate the total number of visible inclusions"),
        tags$li(tags$strong("Grain Size"), " \u2014 Characterize the grain size distribution across Fine, Medium, Coarse, and Very Coarse categories"),
        tags$li(tags$strong("Grain Shape"), " \u2014 Assess grain roundedness and sphericity (elongation)"),
        tags$li(tags$strong("Full Practice"), " \u2014 Combine all skills in one attempt")
      ),
      h4("How to Use"),
      tags$ol(
        tags$li("Select a focused lesson or use Full Practice for the combined exercise"),
        tags$li("Examine the thin section image carefully"),
        tags$li("Enter your estimates using the controls in the sidebar"),
        tags$li(tags$strong("Click 'Reveal Ground Truth'"), " to compare against the reference segmentation"),
        tags$li("Review your score and the color-coded overlay to calibrate your visual estimation"),
        tags$li("Navigate between samples to practice with different compositions"),
        tags$li("Check the Performance tab to see your trends and biases")
      ),
      h4("Image Scale"),
      p("Each image includes a 100 px scale bar. This is a pixel reference for comparing grains within this demo; converting it to microns requires microscope calibration metadata."),
      h4("Color-Coded Ground Truth Overlay"),
      p("When you reveal the ground truth, inclusions are color-coded by size class:"),
      tags$div(
        class = "d-flex gap-3 mb-3",
        tags$span(style = "color: #3498db; font-weight: 600;", "\u25A0 Fine"),
        tags$span(style = "color: #2ecc71; font-weight: 600;", "\u25A0 Medium"),
        tags$span(style = "color: #f39c12; font-weight: 600;", "\u25A0 Coarse"),
        tags$span(style = "color: #e74c3c; font-weight: 600;", "\u25A0 Very Coarse")
      ),
      p("These colors match the bar chart showing grain size distribution, so you can see exactly",
        "which detected grains fall into which size class."),
      h4("Current Shape Metric"),
      p("Roundedness is currently compared with mean circularity, and sphericity is compared with mean aspect ratio across segmented grains.",
        "This is an aggregate reference metric for the app, not a replacement for petrographic description by temper type."),
      h4("About the Images"),
      p("These are cross-polarized light (XPL) photomicrographs of ceramic thin sections from 5 fabric types (A, C, D, E, W)",
        "across multiple samples. Inclusions (temper and natural aplastics) appear as bright, colorful grains in the clay matrix.",
        "The reference segmentation uses a model (RF-DETR Seg) that detects individual inclusion boundaries and measures",
        "their morphological properties (area, circularity, aspect ratio, solidity).",
        "All images are interior views \u2014 tiles near the sample edge have been excluded to avoid artifacts."),
      h4("Size Categories"),
      tags$table(
        class = "table table-sm",
        tags$thead(tags$tr(
          tags$th("Category"), tags$th("Color"), tags$th("Description")
        )),
        tags$tbody(
          tags$tr(tags$td("Fine"), tags$td(style = "color: #3498db;", "\u25A0"), tags$td("Small grains, hard to distinguish individually")),
          tags$tr(tags$td("Medium"), tags$td(style = "color: #2ecc71;", "\u25A0"), tags$td("Clearly visible individual grains")),
          tags$tr(tags$td("Coarse"), tags$td(style = "color: #f39c12;", "\u25A0"), tags$td("Large, prominent grains")),
          tags$tr(tags$td("Very Coarse"), tags$td(style = "color: #e74c3c;", "\u25A0"), tags$td("Dominant grains, easily identified"))
        )
      ),
      hr(),
      p(class = "text-muted",
        "Built by the Archaeological Intelligence Lab, University of Florida.",
        "Powered by the ", tags$code("petrographer"), " R package.")
    )
  )
)

# --- Server ---
server <- function(input, output, session) {

  lesson_card_button <- function(value, prefix, selected = NULL, status = NULL) {
    lesson <- lesson_defs[lesson_defs$value == value, ][1, ]
    tag_text <- if (!is.null(status) && !is.null(status$tag)) status$tag else lesson$tag
    status_class <- if (!is.null(status) && !is.null(status$class)) status$class else ""
    actionButton(
      inputId = paste0(prefix, "_", value),
      label = tagList(
        div(
          class = "lesson-title",
          icon(lesson$icon),
          span(lesson$title),
          span(class = "lesson-tag", tag_text)
        ),
        div(class = "lesson-desc", lesson$description)
      ),
      class = paste(
        "lesson-card",
        status_class,
        if (!is.null(selected) && selected == value) "active" else ""
      )
    )
  }

  lesson_picker <- function(prefix, selected = NULL, statuses = NULL) {
    div(
      class = "lesson-grid",
      lapply(lesson_defs$value, function(value) {
        status <- if (!is.null(statuses) && value %in% names(statuses)) statuses[[value]] else NULL
        lesson_card_button(value, prefix, selected, status)
      })
    )
  }

  show_skill_picker_modal <- function(selected = default_module,
                                       statuses = NULL,
                                       title = "Pick a skill",
                                       body_text = NULL,
                                       footnote = NULL) {
    showModal(modalDialog(
      title = title,
      easyClose = TRUE,
      footer = modalButton("Cancel"),
      if (!is.null(body_text)) p(body_text),
      lesson_picker("intro", selected, statuses),
      if (!is.null(footnote)) p(class = "text-muted", footnote)
    ))
  }

  session$onFlushed(function() {
    show_skill_picker_modal(
      selected = default_module,
      title = "Pick a skill to start",
      body_text = "Start with one focused skill. You can switch any time.",
      footnote = "Inspect the image, enter your estimate, then click Reveal Ground Truth. Scores measure agreement with the current reference segmentation."
    )
  }, once = TRUE)

  build_round_summary <- function() {
    if (rv$target <= 0 || nrow(rv$scores) < rv$target) return(NULL)
    round_rows <- tail(rv$scores, rv$target)

    dim_avg <- function(col) {
      vals <- round_rows[[col]]
      vals <- vals[vals >= 0]
      if (length(vals) == 0) NA_real_ else round(mean(vals))
    }

    list(
      module = input_value(input$module, default_module),
      overall = round(mean(round_rows$overall)),
      area = dim_avg("area_score"),
      count = dim_avg("count_score"),
      size = dim_avg("size_score"),
      spher = dim_avg("spher_score")
    )
  }

  weakest_dim <- function(summary) {
    dims <- list(
      list(score = summary$area, module = "area", title = "Inclusion Area"),
      list(score = summary$count, module = "count", title = "Grain Count"),
      list(score = summary$size, module = "size", title = "Grain Size"),
      list(score = summary$spher, module = "sphericity", title = "Grain Shape")
    )
    dims <- Filter(function(d) !is.na(d$score), dims)
    if (length(dims) == 0) return(NULL)
    scores <- vapply(dims, function(d) d$score, numeric(1))
    dims[[which.min(scores)]]
  }

  show_round_complete_modal <- function() {
    summary <- build_round_summary()
    if (is.null(summary)) return(NULL)

    lesson <- lesson_defs[lesson_defs$value == summary$module, ][1, ]

    chip <- function(label, score) {
      span(class = "round-recap-chip", paste0(label, ": "), tags$strong(paste0(score, "/100")))
    }

    chips <- list()
    if (!is.na(summary$area)) chips[[length(chips) + 1]] <- chip("Area", summary$area)
    if (!is.na(summary$count)) chips[[length(chips) + 1]] <- chip("Count", summary$count)
    if (!is.na(summary$size)) chips[[length(chips) + 1]] <- chip("Size", summary$size)
    if (!is.na(summary$spher)) chips[[length(chips) + 1]] <- chip("Shape", summary$spher)

    weakest <- weakest_dim(summary)
    suggestion <- if (summary$overall >= strong_threshold) {
      "Strong round — try a different skill or keep grinding."
    } else if (!is.null(weakest) && weakest$score < strong_threshold &&
               weakest$module != summary$module && summary$module == "all") {
      paste0(weakest$title, " was weakest at ", weakest$score,
             "/100. Consider focusing on that skill next.")
    } else if (summary$overall < review_threshold) {
      "Below the practice threshold — another round on the same skill should help."
    } else {
      "Solid practice — try another round or pick a new skill."
    }

    showModal(modalDialog(
      title = tagList(icon("circle-check"), " Round complete!"),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Keep practicing"),
        actionButton("round_view_perf", "View Performance",
                     class = "btn-outline-primary",
                     icon = icon("chart-line")),
        actionButton("round_switch_skill", "Switch skill",
                     class = "btn-outline-primary",
                     icon = icon("arrow-right-arrow-left")),
        actionButton("round_again", "Practice again",
                     class = "btn-primary",
                     icon = icon("rotate-right"))
      ),
      div(
        class = "text-center",
        div(class = "round-recap-skill", icon(lesson$icon), span(lesson$title)),
        score_ring_html(summary$overall, 110),
        div(class = "round-recap-dims mt-3", chips)
      ),
      p(class = "text-muted text-center mt-3", suggestion)
    ))
  }

  # Convert dominant + sorting into size distribution percentages
  guess_size_dist <- reactive({
    size_dist_from_guess(input$guess_dominant, input$guess_sorting, input$guess_secondary)
  })

  # Reactive values
  rv <- reactiveValues(
    current = 1,
    queue = integer(),
    answered = 0L,
    target = min(lesson_target, n_samples),
    revealed = FALSE,
    last_attempt = NULL,
    last_scored_sample = NA_integer_,
    lesson_attempts = setNames(rep(0L, nrow(lesson_defs)), lesson_defs$value),
    lesson_strong = setNames(rep(0L, nrow(lesson_defs)), lesson_defs$value),
    scores = data.frame(
      attempt = integer(),
      sample = integer(),
      module = character(),
      area_score = numeric(),
      count_score = numeric(),
      size_score = numeric(),
      spher_score = numeric(),
      overall = numeric(),
      area_bias = numeric(),
      count_bias = numeric()
    )
  )

  input_value <- function(x, default) {
    if (is.null(x) || length(x) == 0 || is.na(x[[1]])) default else x[[1]]
  }

  reset_inputs <- function() {
    updateSliderInput(session, "guess_pct", value = default_area_pct)
    updateSliderInput(session, "guess_count", value = 50)
    updateSelectInput(session, "guess_dominant", selected = "Medium")
    updateSelectInput(session, "guess_sorting", selected = "moderate")
    updateSelectInput(session, "guess_secondary", selected = "Coarse")
    updateSelectInput(session, "guess_roundedness", selected = "subrounded")
    updateSelectInput(session, "guess_sphericity", selected = "medium")
  }

  hide_reference <- function() {
    rv$revealed <- FALSE
    rv$last_attempt <- NULL
  }

  eligible_sample_indices <- function(module = default_module) {
    indices <- sample_pool$sample_index[sample_pool$enabled]
    if (length(indices) == 0) seq_len(n_samples) else indices
  }

  start_lesson <- function(module = default_module) {
    pool <- eligible_sample_indices(module)
    rv$queue <- sample(pool)
    rv$answered <- 0L
    rv$target <- min(lesson_target, length(pool))

    if (length(rv$queue) > 0) {
      rv$current <- rv$queue[1]
      rv$queue <- rv$queue[-1]
    }

    rv$last_scored_sample <- NA_integer_
    hide_reference()
    reset_inputs()
  }

  next_question <- function() {
    module <- input_value(input$module, default_module)
    if (length(rv$queue) == 0) {
      pool <- setdiff(eligible_sample_indices(module), rv$current)
      if (length(pool) == 0) pool <- eligible_sample_indices(module)
      rv$queue <- sample(pool)
    }

    if (length(rv$queue) > 0) {
      rv$current <- rv$queue[1]
      rv$queue <- rv$queue[-1]
    }

    rv$last_scored_sample <- NA_integer_
    hide_reference()
    reset_inputs()
  }

  skip_question <- function() {
    module <- input_value(input$module, default_module)
    pool <- eligible_sample_indices(module)
    if (length(setdiff(pool, rv$current)) > 0) {
      rv$queue <- c(rv$queue[rv$queue != rv$current], rv$current)
    }
    next_question()
  }

  schedule_review <- function(attempt) {
    module <- if (attempt$module %in% names(rv$lesson_attempts)) attempt$module else default_module

    rv$answered <- rv$answered + 1L

    attempts <- rv$lesson_attempts
    attempts[[module]] <- attempts[[module]] + 1L
    rv$lesson_attempts <- attempts

    if (attempt$overall >= strong_threshold) {
      strong <- rv$lesson_strong
      strong[[module]] <- strong[[module]] + 1L
      rv$lesson_strong <- strong
    }

    rv$queue <- rv$queue[rv$queue != attempt$sample_index]
    if (attempt$overall < strong_threshold) {
      delay <- if (attempt$overall < review_threshold) miss_review_delay else practice_review_delay
      rv$queue <- append(rv$queue, attempt$sample_index, after = min(delay, length(rv$queue)))
    }
  }

  current_question_number <- function() {
    if (rv$target <= 0) return(0L)
    question <- rv$answered + if (isTRUE(rv$revealed)) 0L else 1L
    max(1L, min(question, rv$target))
  }

  lesson_statuses <- function() {
    statuses <- setNames(vector("list", length(lesson_defs$value)), lesson_defs$value)
    for (value in lesson_defs$value) {
      lesson <- lesson_defs[lesson_defs$value == value, ][1, ]
      attempts <- rv$lesson_attempts[[value]]
      strong <- rv$lesson_strong[[value]]

      if (attempts == 0) {
        statuses[[value]] <- list(tag = lesson$tag, class = "")
      } else if (strong >= 3) {
        statuses[[value]] <- list(tag = "Strong", class = "strong")
      } else if (attempts >= lesson_target) {
        statuses[[value]] <- list(tag = "Complete", class = "complete")
      } else {
        statuses[[value]] <- list(tag = paste0(attempts, "/", lesson_target), class = "practicing")
      }
    }
    statuses
  }

  select_module <- function(module) {
    updateRadioButtons(session, "module", selected = module)
    start_lesson(module)
  }

  isolate(start_lesson(default_module))

  for (value in lesson_defs$value) {
    local({
      module_value <- value
      observeEvent(input[[paste0("lesson_", module_value)]], {
        select_module(module_value)
      })
      observeEvent(input[[paste0("intro_", module_value)]], {
        select_module(module_value)
        removeModal()
      })
    })
  }

  build_attempt <- function(sample_index) {
    s <- samples[sample_index, ]
    mod <- input_value(input$module, default_module)
    guess_pct <- input_value(input$guess_pct, default_area_pct)
    guess_count <- input_value(input$guess_count, 50)
    guess_dominant <- input_value(input$guess_dominant, "Medium")
    guess_sorting <- input_value(input$guess_sorting, "moderate")
    guess_secondary <- input_value(input$guess_secondary, "Coarse")
    guess_roundedness <- input_value(input$guess_roundedness, "subrounded")
    guess_sphericity <- input_value(input$guess_sphericity, "medium")

    gt_pct <- s$inclusion_pct
    gt_count <- s$n_inclusions
    gt_size <- c(s$Fine_pct, s$Medium_pct, s$Coarse_pct, s$`Very Coarse_pct`)
    guess_size <- size_dist_from_guess(guess_dominant, guess_sorting, guess_secondary)

    area_s <- if (mod %in% c("all", "area")) calc_score(guess_pct, gt_pct) else NA_real_
    count_s <- if (mod %in% c("all", "count")) calc_score(guess_count, gt_count) else NA_real_

    size_s <- NA_real_
    if (mod %in% c("all", "size")) {
      size_mae <- mean(abs(guess_size - gt_size))
      size_s <- max(0, round(100 - size_mae * 2))
    }

    gt_circ_val <- if (!is.null(s$mean_circularity) && !is.na(s$mean_circularity)) s$mean_circularity else NA_real_
    gt_ar_val <- if (!is.null(s$mean_aspect_ratio) && !is.na(s$mean_aspect_ratio)) s$mean_aspect_ratio else NA_real_

    spher_s <- NA_real_
    if (mod %in% c("all", "sphericity")) {
      round_map <- c(angular = 0.4, subangular = 0.55, subrounded = 0.7,
                     rounded = 0.82, wellrounded = 0.92)
      spher_map <- c(low = 2.5, medium = 1.6, high = 1.15)
      guess_circ <- round_map[guess_roundedness]
      guess_ar <- spher_map[guess_sphericity]
      gt_circ <- if (!is.na(gt_circ_val)) gt_circ_val else 0.75
      gt_ar <- if (!is.na(gt_ar_val)) gt_ar_val else 1.6
      circ_err <- abs(guess_circ - gt_circ) / 0.5 * 100
      ar_err <- abs(guess_ar - gt_ar) / 1.5 * 100
      spher_s <- max(0, round(100 - mean(c(circ_err, ar_err))))
    }

    valid_scores <- c(area_s, count_s, size_s, spher_s)
    valid_scores <- valid_scores[!is.na(valid_scores)]
    overall <- if (length(valid_scores) > 0) round(mean(valid_scores)) else 0

    circ_label <- if (is.na(gt_circ_val)) "-"
                  else if (gt_circ_val >= 0.88) "Well-rounded"
                  else if (gt_circ_val >= 0.76) "Rounded"
                  else if (gt_circ_val >= 0.62) "Sub-rounded"
                  else if (gt_circ_val >= 0.48) "Sub-angular"
                  else "Angular"

    ar_label <- if (is.na(gt_ar_val)) "-"
                else if (gt_ar_val <= 1.3) "High"
                else if (gt_ar_val <= 1.9) "Medium"
                else "Low"

    list(
      sample_index = sample_index,
      sample = s,
      module = mod,
      guess_pct = guess_pct,
      guess_count = guess_count,
      guess_size = guess_size,
      gt_size = gt_size,
      area_score = area_s,
      count_score = count_s,
      size_score = size_s,
      spher_score = spher_s,
      overall = overall,
      area_bias = if (!is.na(area_s)) guess_pct - gt_pct else NA_real_,
      count_bias = if (!is.na(count_s)) guess_count - gt_count else NA_real_,
      circ_label = circ_label,
      ar_label = ar_label,
      gt_circ = gt_circ_val,
      gt_ar = gt_ar_val
    )
  }

  attempt_to_row <- function(attempt) {
    data.frame(
      attempt = attempt$attempt,
      sample = attempt$sample_index,
      module = attempt$module,
      area_score = ifelse(is.na(attempt$area_score), -1, attempt$area_score),
      count_score = ifelse(is.na(attempt$count_score), -1, attempt$count_score),
      size_score = ifelse(is.na(attempt$size_score), -1, attempt$size_score),
      spher_score = ifelse(is.na(attempt$spher_score), -1, attempt$spher_score),
      overall = attempt$overall,
      area_bias = attempt$area_bias,
      count_bias = attempt$count_bias,
      stringsAsFactors = FALSE
    )
  }

  # Session navigation
  observeEvent(input$restart_lesson, {
    start_lesson(input_value(input$module, default_module))
  })

  observeEvent(input$skip_sample, {
    skip_question()
  })

  observeEvent(input$reset, {
    hide_reference()
    reset_inputs()
  })

  observeEvent(input$try_again_result, {
    hide_reference()
  })

  observeEvent(input$next_result, {
    next_question()
  })

  observeEvent(input$reveal, {
    if (isTRUE(rv$revealed)) return(NULL)

    attempt <- build_attempt(rv$current)

    already_scored <- !is.na(rv$last_scored_sample) &&
      rv$last_scored_sample == rv$current
    if (already_scored) {
      rv$last_attempt <- attempt
      rv$revealed <- TRUE
      return(NULL)
    }

    attempt$attempt <- nrow(rv$scores) + 1
    rv$last_attempt <- attempt
    rv$revealed <- TRUE
    rv$scores <- rbind(rv$scores, attempt_to_row(attempt))
    rv$last_scored_sample <- rv$current
    schedule_review(attempt)

    if (rv$target > 0 && rv$answered == rv$target) {
      show_round_complete_modal()
    }
  })

  shape_icon <- function(kind) {
    common <- list(viewBox = "0 0 100 60", role = "img")
    fill <- "#e9ecef"
    stroke <- "#2c3e50"
    if (kind == "angular") {
      do.call(tags$svg, c(common, list(
        tags$polygon(points = "12,36 28,9 49,25 71,8 90,38 58,51 37,42",
                     fill = fill, stroke = stroke, `stroke-width` = 3)
      )))
    } else if (kind == "subangular") {
      do.call(tags$svg, c(common, list(
        tags$polygon(points = "13,35 27,14 50,16 75,23 88,39 65,51 34,47",
                     fill = fill, stroke = stroke, `stroke-width` = 3)
      )))
    } else if (kind == "subrounded") {
      do.call(tags$svg, c(common, list(
        tags$ellipse(cx = 50, cy = 31, rx = 38, ry = 20,
                     fill = fill, stroke = stroke, `stroke-width` = 3)
      )))
    } else if (kind == "rounded") {
      do.call(tags$svg, c(common, list(
        tags$ellipse(cx = 50, cy = 30, rx = 32, ry = 23,
                     fill = fill, stroke = stroke, `stroke-width` = 3)
      )))
    } else if (kind == "wellrounded") {
      do.call(tags$svg, c(common, list(
        tags$circle(cx = 50, cy = 30, r = 24,
                    fill = fill, stroke = stroke, `stroke-width` = 3)
      )))
    } else if (kind == "low") {
      do.call(tags$svg, c(common, list(
        tags$ellipse(cx = 50, cy = 30, rx = 43, ry = 10,
                     fill = fill, stroke = stroke, `stroke-width` = 3)
      )))
    } else if (kind == "medium") {
      do.call(tags$svg, c(common, list(
        tags$ellipse(cx = 50, cy = 30, rx = 34, ry = 18,
                     fill = fill, stroke = stroke, `stroke-width` = 3)
      )))
    } else {
      do.call(tags$svg, c(common, list(
        tags$ellipse(cx = 50, cy = 30, rx = 25, ry = 23,
                     fill = fill, stroke = stroke, `stroke-width` = 3)
      )))
    }
  }

  shape_guide_item <- function(label, kind) {
    div(class = "shape-guide-item", shape_icon(kind), tags$strong(label))
  }

  observeEvent(input$shape_guide, {
    showModal(modalDialog(
      title = "Roundedness and Sphericity Guide",
      size = "l",
      easyClose = TRUE,
      h5("Roundedness"),
      div(
        class = "shape-guide-grid",
        shape_guide_item("Angular", "angular"),
        shape_guide_item("Sub-angular", "subangular"),
        shape_guide_item("Sub-rounded", "subrounded"),
        shape_guide_item("Rounded", "rounded"),
        shape_guide_item("Well-rounded", "wellrounded")
      ),
      h5("Sphericity"),
      div(
        class = "shape-guide-grid",
        shape_guide_item("Low", "low"),
        shape_guide_item("Medium", "medium"),
        shape_guide_item("High", "high")
      ),
      p(class = "text-muted",
        "The current app compares your choices with aggregate circularity and aspect-ratio measurements from the segmented grains.")
    ))
  })

  # Current sample data
  current_sample <- reactive({
    samples[rv$current, ]
  })

  current_objects <- reactive({
    objects |> filter(file_name == current_sample()$file_name)
  })

  # Outputs
  output$lesson_cards <- renderUI({
    module <- input$module %||% default_module
    lesson <- lesson_defs[lesson_defs$value == module, ][1, ]
    statuses <- lesson_statuses()
    status <- statuses[[module]]
    status_class <- if (!is.null(status$class) && nchar(status$class) > 0) status$class else ""
    tag_text <- if (!is.null(status$tag)) status$tag else lesson$tag

    tagList(
      div(
        class = paste("lesson-card-active", status_class),
        div(
          class = "lesson-title",
          icon(lesson$icon),
          span(lesson$title),
          span(class = "lesson-tag", tag_text)
        ),
        div(class = "lesson-desc", lesson$description)
      ),
      actionButton(
        "switch_skill", "Switch skill",
        class = "btn-sm btn-outline-secondary w-100 mt-2",
        icon = icon("arrow-right-arrow-left")
      )
    )
  })

  observeEvent(input$switch_skill, {
    show_skill_picker_modal(
      selected = input$module %||% default_module,
      statuses = lesson_statuses(),
      title = "Switch skill",
      body_text = "Pick a different skill to practice."
    )
  })

  observeEvent(input$round_again, {
    removeModal()
    start_lesson(input_value(input$module, default_module))
  })

  observeEvent(input$round_switch_skill, {
    show_skill_picker_modal(
      selected = input$module %||% default_module,
      statuses = lesson_statuses(),
      title = "Switch skill",
      body_text = "Pick a different skill to practice."
    )
  })

  observeEvent(input$round_view_perf, {
    removeModal()
    nav_select("main_nav", selected = "Performance", session = session)
  })

  output$sample_label <- renderText({
    if (rv$answered >= rv$target && !isTRUE(rv$revealed)) {
      "Review"
    } else {
      paste0("Q ", current_question_number(), " / ", rv$target)
    }
  })

  output$lesson_header <- renderUI({
    module <- input_value(input$module, default_module)
    lesson <- lesson_defs[lesson_defs$value == module, ][1, ]
    answered <- min(rv$answered, rv$target)
    progress <- if (rv$target > 0) round(answered / rv$target * 100) else 0
    progress_text <- if (rv$answered >= rv$target && !isTRUE(rv$revealed)) {
      "Review round"
    } else {
      paste0("Question ", current_question_number(), " of ", rv$target)
    }

    div(
      class = "lesson-header",
      div(
        h5(tagList(icon(lesson$icon), " ", lesson$title)),
        div(class = "lesson-progress-meta", progress_text)
      ),
      div(
        div(
          class = "progress-label",
          span("Round progress"),
          span(paste0(progress, "%"))
        ),
        div(
          class = "progress-track",
          div(class = "progress-fill", style = paste0("width:", progress, "%;"))
        )
      )
    )
  })

  # Sample info badge (sample name, fabric type, polarization)
  output$sample_info <- renderUI({
    s <- current_sample()
    pol <- if (!is.null(s$polarization) && !is.na(s$polarization)) s$polarization else "XPL"
    fabric <- if (!is.null(s$fabric_type) && !is.na(s$fabric_type)) s$fabric_type else ""
    sample_name <- if (!is.null(s$sample_name) && !is.na(s$sample_name)) s$sample_name else ""

    pol_color <- if (pol == "PPL") "#8e44ad" else "#2980b9"

    div(
      class = "text-center mb-1",
      if (nchar(sample_name) > 0) tags$span(
        class = "badge bg-secondary me-1",
        sample_name
      ),
      if (nchar(fabric) > 0) tags$span(
        class = "badge bg-info me-1",
        paste0("Fabric ", fabric)
      ),
      tags$span(
        class = "badge",
        style = paste0("background:", pol_color),
        pol
      )
    )
  })

  output$area_reference <- renderUI({
    pct <- input$guess_pct %||% default_area_pct
    ref_pct <- area_pct_choices[which.min(abs(area_pct_choices - pct))]
    div(
      class = "area-reference",
      tags$img(src = sprintf("images/comparison/pct_%02d.png", ref_pct),
               alt = paste0(ref_pct, "% area reference")),
      div(
        div(class = "area-reference-title",
            paste0("Nearest reference: ", ref_pct, "%")),
        div(class = "area-reference-note",
            paste0("Your estimate: ", pct, "%. Use the image as a visual calibration aid."))
      )
    )
  })

  # Performance dots in sidebar
  output$perf_dots <- renderUI({
    dots <- lapply(seq_len(rv$target), function(i) {
      completed <- i <= rv$answered
      tags$span(
        class = paste("perf-dot", if (completed) "completed" else ""),
        style = paste0("background:", if (completed) "#2c3e50" else "#95a5a6", ";"),
        title = paste0("Question ", i)
      )
    })
    div(class = "text-center", dots)
  })

  scale_bar_html <- function() {
    div(
      class = "scale-overlay",
      div(class = "scale-line"),
      div("100 px")
    )
  }

  image_with_scale <- function(src, alt) {
    div(
      class = "image-frame",
      tags$img(src = src, class = "img-fluid", alt = alt),
      scale_bar_html()
    )
  }

  size_legend_html <- function() {
    div(
      class = "size-legend-large",
      span(class = "size-legend-title", "Size class"),
      lapply(names(size_colors), function(label) {
        span(
          class = "size-legend-item",
          span(class = "size-legend-swatch",
               style = paste0("background:", size_colors[[label]], ";")),
          label
        )
      })
    )
  }

  # Image display
  output$image_display <- renderUI({
    s <- current_sample()

    pol <- if (!is.null(s$polarization) && !is.na(s$polarization)) s$polarization else "XPL"

    if (rv$revealed) {
      div(
        class = "row g-2 fade-in",
        div(
          class = "col-md-6",
              div(class = "card",
                  div(class = "card-header py-1", tags$small(paste0("Original (", pol, ")"))),
                  div(class = "card-body p-1 img-container-split",
                      image_with_scale(s$orig_path, "Original thin section")))
        ),
        div(
          class = "col-md-6",
          div(class = "card",
              div(class = "card-header py-1", tags$small("Ground Truth Overlay")),
              div(class = "card-body p-1 img-container-split",
                  image_with_scale(s$anno_path, "Ground truth annotated thin section")))
        ),
        div(class = "col-12", size_legend_html())
      )
    } else {
      div(
        class = "card fade-in",
        div(class = "card-header py-1", tags$small(paste0("Thin Section (", pol, ")"))),
        div(class = "card-body p-1 img-container",
            image_with_scale(s$orig_path, "Original thin section"))
      )
    }
  })

  # Score ring helper
  score_ring_html <- function(score, size = 100) {
    grade <- score_grade(score)
    r <- (size / 2) - 10
    circumference <- 2 * pi * r
    offset <- circumference * (1 - score / 100)

    div(
      class = "score-ring fade-in",
      style = paste0("width:", size, "px; height:", size, "px;"),
      HTML(sprintf('
        <svg width="%d" height="%d" viewBox="0 0 %d %d">
          <circle cx="%d" cy="%d" r="%d" fill="none" stroke="#e9ecef" stroke-width="7"/>
          <circle cx="%d" cy="%d" r="%d" fill="none" stroke="%s" stroke-width="7"
                  stroke-dasharray="%.1f" stroke-dashoffset="%.1f"
                  stroke-linecap="round" style="transition: stroke-dashoffset 0.8s ease;"/>
        </svg>',
        size, size, size, size,
        size/2, size/2, r,
        size/2, size/2, r, grade$color, circumference, offset)),
      div(
        class = "score-text",
        div(class = "score-letter", style = paste0("color:", grade$color), grade$letter),
        div(class = "score-label", grade$label)
      )
    )
  }

  # Results comparison panel
  output$results_panel <- renderUI({
    req(rv$revealed, rv$last_attempt)
    attempt <- rv$last_attempt
    s <- attempt$sample

    guess_pct <- attempt$guess_pct
    guess_count <- attempt$guess_count
    gt_pct <- s$inclusion_pct
    gt_count <- s$n_inclusions

    area_s <- if (!is.na(attempt$area_score)) attempt$area_score else NULL
    count_s <- if (!is.na(attempt$count_score)) attempt$count_score else NULL
    size_s <- if (!is.na(attempt$size_score)) attempt$size_score else NULL
    spher_s <- if (!is.na(attempt$spher_score)) attempt$spher_score else NULL
    overall <- attempt$overall

    diff_display <- function(guess, actual, unit = "") {
      diff <- guess - actual
      icon_name <- if (diff > 0) "arrow-up" else if (diff < 0) "arrow-down" else "check"
      color <- if (abs(diff) < actual * 0.15) "#2ecc71"
               else if (abs(diff) < actual * 0.3) "#f39c12"
               else "#e74c3c"
      tags$span(
        style = paste0("color:", color),
        icon(icon_name),
        paste0(ifelse(diff > 0, "+", ""), round(diff, 1), unit)
      )
    }

    feedback <- c(paste0("Agreement score: ", overall, "/100."))

    if (!is.null(area_s)) {
      area_diff <- guess_pct - gt_pct
      feedback <- c(
        feedback,
        if (abs(area_diff) <= 2) {
          "Area estimate is close to the reference."
        } else if (area_diff > 0) {
          paste0("You overestimated inclusion area by ", round(abs(area_diff), 1), "%.")
        } else {
          paste0("You underestimated inclusion area by ", round(abs(area_diff), 1), "%.")
        }
      )
      if (area_s < strong_threshold) {
        feedback <- c(
          feedback,
          if (area_diff > 0) {
            "Area cue: separate the clay matrix from inclusion-rich patches before moving the slider."
          } else {
            "Area cue: sweep for fine bright grains before settling on the final percentage."
          }
        )
      }
    }

    if (!is.null(count_s)) {
      count_diff <- guess_count - gt_count
      feedback <- c(
        feedback,
        if (abs(count_diff) <= 10) {
          "Grain count is close to the reference."
        } else if (count_diff > 0) {
          paste0("You overcounted by about ", round(abs(count_diff)), " inclusions.")
        } else {
          paste0("You undercounted by about ", round(abs(count_diff)), " inclusions.")
        }
      )
      if (count_s < strong_threshold) {
        feedback <- c(
          feedback,
          if (count_diff > 0) {
            "Count cue: avoid splitting one elongated grain into several inclusions."
          } else {
            "Count cue: scan the image in rows so isolated grains are less likely to be missed."
          }
        )
      }
    }

    if (!is.null(size_s)) {
      size_cats <- c("Fine", "Medium", "Coarse", "Very Coarse")
      guess_dom <- size_cats[which.max(attempt$guess_size)]
      gt_dom <- size_cats[which.max(attempt$gt_size)]
      feedback <- c(
        feedback,
        if (guess_dom == gt_dom) {
          paste0("Dominant size class matches: ", gt_dom, ".")
        } else {
          paste0("Reference is mostly ", gt_dom, "; your estimate emphasized ", guess_dom, ".")
        }
      )
      if (size_s < strong_threshold) {
        feedback <- c(
          feedback,
          "Size cue: prioritize the size class that repeats across the field, not a single standout grain."
        )
      }
    }

    if (!is.null(spher_s)) {
      feedback <- c(
        feedback,
        paste0("Shape reference: ", attempt$circ_label, " with ", attempt$ar_label, " sphericity.")
      )
      if (spher_s < strong_threshold) {
        feedback <- c(
          feedback,
          "Shape cue: judge the larger grains first; small matrix grains can pull the aggregate reference."
        )
      }
    }

    feedback_panel <- div(
      class = "feedback-card",
      h6("Feedback"),
      tags$ul(lapply(feedback, tags$li))
    )

    # Build stat cards based on active module
    stat_cards <- list()

    stat_cards[[1]] <- div(class = "col text-center", score_ring_html(overall, 90))

    if (!is.null(area_s)) {
      stat_cards[[length(stat_cards) + 1]] <- div(
        class = "col",
        div(class = "stat-card h-100",
            div(class = "stat-label", "Inclusion Area"),
            div(class = "stat-value", paste0(gt_pct, "%")),
            div(class = "stat-diff", diff_display(guess_pct, gt_pct, "%")))
      )
    }

    if (!is.null(count_s)) {
      stat_cards[[length(stat_cards) + 1]] <- div(
        class = "col",
        div(class = "stat-card h-100",
            div(class = "stat-label", "Inclusion Count"),
            div(class = "stat-value", gt_count),
            div(class = "stat-diff", diff_display(guess_count, gt_count)))
      )
    }

    if (!is.null(size_s)) {
      stat_cards[[length(stat_cards) + 1]] <- div(
        class = "col",
        div(class = "stat-card h-100",
            div(class = "stat-label", "Size Distribution"),
            div(class = "stat-value", paste0(size_s, "%")),
            div(class = "stat-diff", tags$span(
              style = paste0("color:", score_grade(size_s)$color),
              score_grade(size_s)$label
            )))
      )
    }

    if (!is.null(spher_s)) {
      gt_circ_val <- attempt$gt_circ
      gt_ar_val <- attempt$gt_ar
      circ_label <- attempt$circ_label
      ar_label <- attempt$ar_label

      stat_cards[[length(stat_cards) + 1]] <- div(
        class = "col",
        div(class = "stat-card h-100",
            div(class = "stat-label", "Grain Shape"),
            div(class = "stat-value", style = "font-size: 18px;", circ_label),
            div(class = "stat-diff text-muted",
                paste0(ar_label, " sph. | circ=",
                       if (!is.na(gt_circ_val)) round(gt_circ_val, 2) else "?",
                       " AR=", if (!is.na(gt_ar_val)) round(gt_ar_val, 2) else "?"))
        )
      )
    }

    # Size distribution chart (shown in size and all modes)
    size_chart <- NULL
    if (!is.null(size_s)) {
      size_chart <- div(
        class = "card mt-2",
        div(class = "card-header py-1", tags$small("Grain Size Distribution")),
        div(class = "card-body p-2",
            plotOutput("size_comparison_plot", height = "200px"))
      )
    }

    div(
      class = "mt-2 fade-in",
      feedback_panel,
      div(class = "row g-2 mb-2", stat_cards),
      size_chart,
      div(
        class = "result-actions",
        actionButton("try_again_result", "Try Again",
                     class = "btn-outline-secondary",
                     icon = icon("rotate-left")),
        actionButton("next_result", "Next Question",
                     class = "btn-primary",
                     icon = icon("arrow-right"))
      )
    )
  })

  # Size distribution comparison plot with size-class colors
  output$size_comparison_plot <- renderPlot({
    req(rv$revealed, rv$last_attempt)
    attempt <- rv$last_attempt
    req(!is.na(attempt$size_score))

    size_cats <- c("Fine", "Medium", "Coarse", "Very Coarse")

    gt_vals <- attempt$gt_size
    gu_vals <- attempt$guess_size

    comparison <- data.frame(
      category = rep(factor(size_cats, levels = size_cats), 2),
      source = rep(c("Your Estimate", "Ground Truth"), each = 4),
      pct = c(gu_vals, gt_vals)
    )

    # Split into estimate and AI data for separate color mapping
    ai_data <- comparison |> filter(source == "Ground Truth")
    est_data <- comparison |> filter(source == "Your Estimate")

    ggplot() +
      # Estimate bars (muted, left side of dodge)
      geom_col(data = est_data,
               aes(x = category, y = pct),
               fill = "#bdc3c7", alpha = 0.7, width = 0.35,
               position = position_nudge(x = -0.2)) +
      # AI bars colored by size class (right side of dodge)
      geom_col(data = ai_data,
               aes(x = category, y = pct, fill = category),
               width = 0.35,
               position = position_nudge(x = 0.15)) +
      scale_fill_manual(values = size_colors, guide = "none") +
      # Manual legend
      annotate("text", x = 0.7, y = max(c(gt_vals, gu_vals)) + 3,
               label = "\u25A0 Your estimate   ", color = "#bdc3c7",
               size = 3.5, hjust = 0, fontface = "bold") +
      annotate("text", x = 2.5, y = max(c(gt_vals, gu_vals)) + 3,
               label = "\u25A0 Ground truth (color = size class)",
               color = "#555", size = 3.5, hjust = 0, fontface = "bold") +
      labs(x = NULL, y = "Percentage (%)") +
      theme_petro()
  })

  # --- Performance tab ---
  output$performance_summary <- renderUI({
    scores <- rv$scores
    if (nrow(scores) == 0) {
      return(div(
        class = "text-center mt-5",
        h4(class = "text-muted", "Complete some samples to see your performance"),
        p(class = "text-muted", "Go to the Practice tab and analyze at least one sample.")
      ))
    }

    avg_overall <- round(mean(scores$overall))
    grade <- score_grade(avg_overall)

    # Filter valid scores
    area_scores <- scores$area_score[scores$area_score >= 0]
    count_scores <- scores$count_score[scores$count_score >= 0]
    size_scores <- scores$size_score[scores$size_score >= 0]
    spher_scores <- scores$spher_score[scores$spher_score >= 0]

    avg_area <- if (length(area_scores) > 0) round(mean(area_scores)) else NA
    avg_count <- if (length(count_scores) > 0) round(mean(count_scores)) else NA
    avg_size <- if (length(size_scores) > 0) round(mean(size_scores)) else NA
    avg_spher <- if (length(spher_scores) > 0) round(mean(spher_scores)) else NA

    area_biases <- na.omit(scores$area_bias)
    count_biases <- na.omit(scores$count_bias)

    mean_area_bias <- if (length(area_biases) > 0) mean(area_biases) else 0
    mean_count_bias <- if (length(count_biases) > 0) mean(count_biases) else 0

    area_tendency <- if (is.na(avg_area)) "not tested"
                     else if (abs(mean_area_bias) < 1) "well-calibrated"
                     else if (mean_area_bias > 0) paste0("overestimating by ~", round(abs(mean_area_bias), 1), "%")
                     else paste0("underestimating by ~", round(abs(mean_area_bias), 1), "%")

    count_tendency <- if (is.na(avg_count)) "not tested"
                      else if (abs(mean_count_bias) < 5) "well-calibrated"
                      else if (mean_count_bias > 0) paste0("overcounting by ~", round(abs(mean_count_bias)))
                      else paste0("undercounting by ~", round(abs(mean_count_bias)))

    score_text <- function(score) if (!is.na(score)) paste0(score, "/100") else "\u2014"

    div(
      class = "row row-cols-2 row-cols-md-5 g-3 mt-3",
      div(class = "col text-center", score_ring_html(avg_overall)),
      div(class = "col",
          div(class = "stat-card h-100",
              div(class = "stat-label", "Area Score"),
              div(class = "stat-value", score_text(avg_area)),
              div(class = "stat-diff text-muted", area_tendency))),
      div(class = "col",
          div(class = "stat-card h-100",
              div(class = "stat-label", "Count Score"),
              div(class = "stat-value", score_text(avg_count)),
              div(class = "stat-diff text-muted", count_tendency))),
      div(class = "col",
          div(class = "stat-card h-100",
              div(class = "stat-label", "Size Score"),
              div(class = "stat-value", score_text(avg_size)),
              div(class = "stat-diff text-muted", paste0(length(size_scores), " attempts")))),
      div(class = "col",
          div(class = "stat-card h-100",
              div(class = "stat-label", "Shape Score"),
              div(class = "stat-value", score_text(avg_spher)),
              div(class = "stat-diff text-muted", paste0(length(spher_scores), " attempts"))))
    )
  })

  # Performance trend plot
  output$performance_trend <- renderPlot({
    scores <- rv$scores
    req(nrow(scores) > 0)

    plot_data <- scores |>
      mutate(attempt = row_number()) |>
      tidyr::pivot_longer(
        cols = c(area_score, count_score, size_score, spher_score),
        names_to = "metric", values_to = "score"
      ) |>
      filter(score >= 0) |>
      mutate(metric = case_when(
        metric == "area_score" ~ "Area %",
        metric == "count_score" ~ "Count",
        metric == "size_score" ~ "Size Dist.",
        metric == "spher_score" ~ "Sphericity"
      ))

    if (nrow(plot_data) == 0) return(NULL)

    line_data <- plot_data |>
      group_by(metric) |>
      filter(n() > 1) |>
      ungroup()

    p <- ggplot(plot_data, aes(x = attempt, y = score, color = metric))
    if (nrow(line_data) > 0) {
      p <- p + geom_line(data = line_data, aes(group = metric), linewidth = 1.2, alpha = 0.8)
    }

    p +
      geom_point(size = 3) +
      scale_color_manual(values = c("Area %" = "#3498db", "Count" = "#e74c3c",
                                    "Size Dist." = "#f39c12", "Sphericity" = "#9b59b6")) +
      scale_y_continuous(limits = c(0, 100)) +
      scale_x_continuous(breaks = scales::breaks_pretty()) +
      labs(x = "Attempt", y = "Score", color = NULL, title = "Score by Attempt") +
      theme_petro()
  })

  # Bias plot
  output$bias_plot <- renderPlot({
    scores <- rv$scores
    req(nrow(scores) > 0)

    has_area <- any(!is.na(scores$area_bias))
    has_count <- any(!is.na(scores$count_bias))
    if (!has_area && !has_count) return(NULL)

    plot_scores <- scores |> mutate(attempt = row_number())

    p <- ggplot(plot_scores, aes(x = factor(attempt)))

    if (has_area) {
      p <- p + geom_col(aes(y = area_bias), fill = "#3498db", alpha = 0.8, width = 0.4,
                         position = position_nudge(x = -0.2), na.rm = TRUE)
    }
    if (has_count) {
      p <- p + geom_col(aes(y = count_bias / 5), fill = "#e74c3c", alpha = 0.8, width = 0.4,
                         position = position_nudge(x = 0.2), na.rm = TRUE)
    }

    p + geom_hline(yintercept = 0, color = "#333", linewidth = 0.5) +
      scale_y_continuous(
        name = "Area bias (%)",
        sec.axis = sec_axis(~ . * 5, name = "Count bias")
      ) +
      labs(x = "Attempt", title = "Estimation Bias (above = overestimate)") +
      theme_petro() +
      theme(axis.text.y.right = element_text(color = "#e74c3c"),
            axis.title.y.left = element_text(color = "#3498db"),
            axis.title.y.right = element_text(color = "#e74c3c"))
  })
}

shinyApp(ui, server)
