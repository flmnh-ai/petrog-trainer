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

        # Sample navigation
        div(
          class = "d-flex justify-content-between align-items-center mb-2",
          actionButton("prev_sample", icon("arrow-left"), class = "btn-sm btn-outline-secondary"),
          h6(textOutput("sample_label"), style = "margin:0;"),
          actionButton("next_sample", icon("arrow-right"), class = "btn-sm btn-outline-secondary")
        ),

        # Sample info badges
        uiOutput("sample_info"),

        # Performance dots
        uiOutput("perf_dots"),

        hr(style = "margin: 8px 0;"),

        # Module selector
        radioButtons("module", "Training Module",
                     choices = c(
                       "All Skills" = "all",
                       "Inclusion Area" = "area",
                       "Grain Size" = "size",
                       "Grain Count" = "count",
                       "Sphericity" = "sphericity"
                     ),
                     selected = "all",
                     inline = TRUE),

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
      p("Start here, then use the Practice tab to estimate inclusion abundance, grain count, size distribution, and grain shape before revealing the ground truth reference."),
      h4("Training Modules"),
      p("Use the module selector in the Practice tab to focus on specific skills:"),
      tags$ul(
        tags$li(tags$strong("All Skills"), " \u2014 Practice area estimation, grain counting, and size distribution together"),
        tags$li(tags$strong("Inclusion Area"), " \u2014 Estimate what percentage of the image is occupied by inclusions (temper + aplastics)"),
        tags$li(tags$strong("Grain Size"), " \u2014 Characterize the grain size distribution across Fine, Medium, Coarse, and Very Coarse categories"),
        tags$li(tags$strong("Grain Count"), " \u2014 Estimate the total number of visible inclusions"),
        tags$li(tags$strong("Sphericity"), " \u2014 Assess grain roundedness and sphericity (elongation)")
      ),
      h4("How to Use"),
      tags$ol(
        tags$li("Select a training module (or use All Skills for the combined exercise)"),
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

  session$onFlushed(function() {
    showModal(modalDialog(
      title = "How to use this trainer",
      easyClose = TRUE,
      footer = modalButton("Start Practicing"),
      p("Estimate the thin section before revealing the reference."),
      tags$ol(
        tags$li("Choose a training module or use All Skills."),
        tags$li("Inspect the image, including the 100 px scale bar."),
        tags$li("Enter your estimates with the sidebar controls."),
        tags$li("Click Reveal Ground Truth to compare your estimate with the reference segmentation.")
      ),
      p(class = "text-muted",
        "Scores measure agreement with the current reference segmentation, not a replacement for petrographic judgment.")
    ))
  }, once = TRUE)

  # Convert dominant + sorting into size distribution percentages
  guess_size_dist <- reactive({
    size_dist_from_guess(input$guess_dominant, input$guess_sorting, input$guess_secondary)
  })

  # Reactive values
  rv <- reactiveValues(
    current = 1,
    revealed = FALSE,
    last_attempt = NULL,
    completed = rep(FALSE, n_samples),
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

  build_attempt <- function(sample_index) {
    s <- samples[sample_index, ]
    mod <- input_value(input$module, "all")
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

  # Sample navigation
  observeEvent(input$next_sample, {
    rv$current <- min(rv$current + 1, n_samples)
    rv$revealed <- FALSE
    rv$last_attempt <- NULL
    reset_inputs()
  })

  observeEvent(input$prev_sample, {
    rv$current <- max(rv$current - 1, 1)
    rv$revealed <- FALSE
    rv$last_attempt <- NULL
    reset_inputs()
  })

  observeEvent(input$reset, {
    rv$revealed <- FALSE
    rv$last_attempt <- NULL
    reset_inputs()
  })

  observeEvent(input$reveal, {
    attempt <- build_attempt(rv$current)
    attempt$attempt <- nrow(rv$scores) + 1

    rv$last_attempt <- attempt
    rv$revealed <- TRUE
    rv$completed[rv$current] <- TRUE
    rv$scores <- rbind(rv$scores, attempt_to_row(attempt))
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
  output$sample_label <- renderText({
    paste0(rv$current, " / ", n_samples)
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
    dots <- lapply(seq_len(n_samples), function(i) {
      score_row <- rv$scores[rv$scores$sample == i, ]
      if (nrow(score_row) > 0) {
        best <- max(score_row$overall)
        grade <- score_grade(best)
        tags$span(
          class = paste("perf-dot completed", if (i == rv$current) "border border-dark" else ""),
          style = paste0("background:", grade$color, ";"),
          title = paste0("Sample ", i, ": ", best)
        )
      } else {
        tags$span(
          class = paste("perf-dot", if (i == rv$current) "border border-dark" else ""),
          style = "background:#95a5a6;"
        )
      }
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
      div(class = "row g-2 mb-2", stat_cards),
      size_chart
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

    div(
      class = "row g-3 mt-3",
      div(class = "col-md-3 text-center", score_ring_html(avg_overall)),
      div(class = "col-md-3",
          div(class = "stat-card",
              div(class = "stat-label", "Area Estimation"),
              div(class = "stat-value", if (!is.na(avg_area)) paste0(avg_area, "%") else "\u2014"),
              div(class = "stat-diff text-muted", area_tendency))),
      div(class = "col-md-3",
          div(class = "stat-card",
              div(class = "stat-label", "Count Estimation"),
              div(class = "stat-value", if (!is.na(avg_count)) paste0(avg_count, "%") else "\u2014"),
              div(class = "stat-diff text-muted", count_tendency))),
      div(class = "col-md-3",
          div(class = "stat-card",
              div(class = "stat-label", "Size Distribution"),
              div(class = "stat-value", if (!is.na(avg_size)) paste0(avg_size, "%") else "\u2014"),
              div(class = "stat-diff text-muted", paste0(nrow(scores), " attempts"))))
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
