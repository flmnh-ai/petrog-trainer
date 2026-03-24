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
  arrange(sample_id) |>
  mutate(
    orig_path = paste0("images/original/", file_name),
    anno_path = paste0("images/annotated/", gsub(".jpg", "_result.png", file_name))
  )

n_samples <- nrow(samples)

# Size class colors — MUST match the annotated image colors
size_colors <- c(
  "Fine"        = "#3498db",  # blue
  "Medium"      = "#2ecc71",  # green
  "Coarse"      = "#f39c12",  # orange
  "Very Coarse" = "#e74c3c"   # red
)

# Scoring function: 0-100 based on relative error
calc_score <- function(guess, actual) {
  if (is.null(guess) || is.na(guess)) return(0)
  if (actual == 0) return(if (guess == 0) 100 else 0)
  error_pct <- abs(guess - actual) / actual * 100
  max(0, round(100 - error_pct))
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

/* Comparison chart selector */
.pct-chart-strip {
  display: flex;
  flex-wrap: wrap;
  gap: 3px;
  justify-content: center;
}
.pct-chart-btn {
  width: 48px;
  padding: 2px;
  border: 2px solid transparent;
  border-radius: 6px;
  background: #fff;
  cursor: pointer;
  text-align: center;
  transition: border-color 0.15s, box-shadow 0.15s;
}
.pct-chart-btn:hover {
  border-color: #95a5a6;
}
.pct-chart-btn.selected {
  border-color: #2c3e50;
  box-shadow: 0 0 0 2px rgba(44,62,80,0.2);
}
.pct-chart-btn img {
  width: 100%;
  height: auto;
  display: block;
  border-radius: 3px;
}
.pct-chart-btn .pct-label {
  font-size: 9px;
  font-weight: 600;
  margin-top: 1px;
  color: #555;
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

# --- JS ---
app_js <- "
// Comparison chart click handler
$(document).on('click', '.pct-chart-btn', function() {
  var val = parseInt($(this).data('pct'));
  Shiny.setInputValue('guess_pct', val);
  $('.pct-chart-btn').removeClass('selected');
  $(this).addClass('selected');
});
"

# --- UI ---
ui <- page_navbar(
  title = "Ceramic Petrography Trainer",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Inter")
  ),

  header = tags$head(
    tags$style(HTML(app_css)),
    tags$script(HTML(app_js))
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

        # Performance dots
        uiOutput("perf_dots"),

        hr(style = "margin: 8px 0;"),

        # Module selector
        radioButtons("module", "Training Module",
                     choices = c(
                       "All Skills" = "all",
                       "Inclusion Area" = "area",
                       "Grain Size" = "size",
                       "Grain Count" = "count"
                     ),
                     selected = "all",
                     inline = TRUE),

        # --- Inclusion area inputs ---
        conditionalPanel(
          condition = "input.module == 'all' || input.module == 'area'",
          div(
            class = "sidebar-section",
            h6("Inclusion area (click closest match)"),
            div(
              class = "pct-chart-strip mb-1",
              lapply(c(1, 3, 5, 7, 10, 15, 20, 25, 30, 40, 50), function(pct) {
                div(
                  class = "pct-chart-btn",
                  `data-pct` = pct,
                  tags$img(src = sprintf("images/comparison/pct_%02d.png", pct),
                           alt = paste0(pct, "%")),
                  div(class = "pct-label", paste0(pct, "%"))
                )
              })
            ),
            div(class = "text-center",
                tags$small(class = "text-muted", "Selected: "),
                tags$small(tags$strong(textOutput("selected_pct_display", inline = TRUE))))
          )
        ),

        # --- Count inputs ---
        conditionalPanel(
          condition = "input.module == 'all' || input.module == 'count'",
          div(
            class = "sidebar-section",
            sliderInput("guess_count", "Estimated number of inclusions",
                        min = 0, max = 200, value = 50, step = 5)
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

        hr(style = "margin: 8px 0;"),

        actionButton("reveal", "Show AI Analysis",
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
      h4("Training Modules"),
      p("Use the module selector in the Practice tab to focus on specific skills:"),
      tags$ul(
        tags$li(tags$strong("All Skills"), " \u2014 Practice area estimation, grain counting, and size distribution together"),
        tags$li(tags$strong("Inclusion Area"), " \u2014 Estimate what percentage of the image is occupied by inclusions (temper + aplastics)"),
        tags$li(tags$strong("Grain Size"), " \u2014 Characterize the grain size distribution across Fine, Medium, Coarse, and Very Coarse categories"),
        tags$li(tags$strong("Grain Count"), " \u2014 Estimate the total number of visible inclusions")
      ),
      h4("How to Use"),
      tags$ol(
        tags$li("Select a training module (or use All Skills for the combined exercise)"),
        tags$li("Examine the thin section image carefully"),
        tags$li("Enter your estimates using the controls in the sidebar"),
        tags$li(tags$strong("Click 'Show AI Analysis'"), " to compare against AI-detected ground truth"),
        tags$li("Review your score and the color-coded overlay to calibrate your visual estimation"),
        tags$li("Navigate between samples to practice with different compositions"),
        tags$li("Check the Performance tab to see your trends and biases")
      ),
      h4("Color-Coded AI Overlay"),
      p("When you reveal the AI analysis, inclusions are color-coded by size class:"),
      tags$div(
        class = "d-flex gap-3 mb-3",
        tags$span(style = "color: #3498db; font-weight: 600;", "\u25A0 Fine"),
        tags$span(style = "color: #2ecc71; font-weight: 600;", "\u25A0 Medium"),
        tags$span(style = "color: #f39c12; font-weight: 600;", "\u25A0 Coarse"),
        tags$span(style = "color: #e74c3c; font-weight: 600;", "\u25A0 Very Coarse")
      ),
      p("These colors match the bar chart showing grain size distribution, so you can see exactly",
        "which detected grains fall into which size class."),
      h4("About the Images"),
      p("These are cross-polarized light (XPL) photomicrographs of ceramic thin sections from sample 8-587.",
        "Inclusions (temper and natural aplastics) appear as bright, colorful grains in the clay matrix.",
        "The AI analysis uses an object detection model (RF-DETR) trained to identify and measure individual inclusions.",
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

  # Convert dominant + sorting into size distribution percentages
  guess_size_dist <- reactive({
    dom <- input$guess_dominant
    sorting <- input$guess_sorting
    sec <- input$guess_secondary

    cats <- c("Fine", "Medium", "Coarse", "Very Coarse")
    pcts <- setNames(rep(0, 4), cats)

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
      if (is.null(sec) || sec == dom) sec <- cats[min(4, which(cats == dom) + 2)]
      pcts[dom] <- 40
      pcts[sec] <- 40
      remaining <- setdiff(cats, c(dom, sec))
      if (length(remaining) > 0) pcts[remaining] <- 20 / length(remaining)
    }

    as.numeric(pcts)
  })

  # Reactive values
  rv <- reactiveValues(
    current = 1,
    revealed = FALSE,
    completed = rep(FALSE, n_samples),
    scores = data.frame(
      sample = integer(),
      module = character(),
      area_score = numeric(),
      count_score = numeric(),
      size_score = numeric(),
      overall = numeric(),
      area_bias = numeric(),
      count_bias = numeric()
    )
  )

  # Sample navigation
  observeEvent(input$next_sample, {
    rv$current <- min(rv$current + 1, n_samples)
    rv$revealed <- FALSE
  })

  observeEvent(input$prev_sample, {
    rv$current <- max(rv$current - 1, 1)
    rv$revealed <- FALSE
  })

  observeEvent(input$reset, {
    rv$revealed <- FALSE
  })

  observeEvent(input$reveal, {
    rv$revealed <- TRUE
    rv$completed[rv$current] <- TRUE

    s <- samples[rv$current, ]
    mod <- input$module

    # Calculate scores based on active module
    area_s <- if (mod %in% c("all", "area")) calc_score(input$guess_pct, s$inclusion_pct) else NA
    count_s <- if (mod %in% c("all", "count")) calc_score(input$guess_count, s$n_inclusions) else NA

    size_s <- NA
    if (mod %in% c("all", "size")) {
      gt_size <- c(s$Fine_pct, s$Medium_pct, s$Coarse_pct, s$`Very Coarse_pct`)
      gu_size <- guess_size_dist()
      size_mae <- mean(abs(gu_size - gt_size))
      size_s <- max(0, round(100 - size_mae * 2))
    }

    valid_scores <- na.omit(c(area_s, count_s, size_s))
    overall <- if (length(valid_scores) > 0) round(mean(valid_scores)) else 0

    new_row <- data.frame(
      sample = rv$current,
      module = mod,
      area_score = ifelse(is.na(area_s), -1, area_s),
      count_score = ifelse(is.na(count_s), -1, count_s),
      size_score = ifelse(is.na(size_s), -1, size_s),
      overall = overall,
      area_bias = if (!is.na(area_s) && !is.null(input$guess_pct)) input$guess_pct - s$inclusion_pct else NA,
      count_bias = if (!is.na(count_s)) input$guess_count - s$n_inclusions else NA
    )

    existing <- rv$scores$sample == rv$current & rv$scores$module == mod
    if (any(existing)) {
      rv$scores[existing, ] <- new_row
    } else {
      rv$scores <- rbind(rv$scores, new_row)
    }
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
    paste0("Sample ", rv$current, " / ", n_samples)
  })

  output$selected_pct_display <- renderText({
    pct <- input$guess_pct
    if (is.null(pct)) "none" else paste0(pct, "%")
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

  # Image display
  output$image_display <- renderUI({
    s <- current_sample()

    if (rv$revealed) {
      div(
        class = "row g-2 fade-in",
        div(
          class = "col-md-6",
          div(class = "card",
              div(class = "card-header py-1", tags$small("Original (XPL)")),
              div(class = "card-body p-1 img-container-split",
                  tags$img(src = s$orig_path, class = "img-fluid",
                           alt = "Original thin section")))
        ),
        div(
          class = "col-md-6",
          div(class = "card",
              div(class = "card-header py-1", tags$small("AI Detections (color = size class)")),
              div(class = "card-body p-1 img-container-split",
                  tags$img(src = s$anno_path, class = "img-fluid",
                           alt = "AI annotated thin section")))
        )
      )
    } else {
      div(
        class = "card fade-in",
        div(class = "card-header py-1", tags$small("Thin Section (XPL)")),
        div(class = "card-body p-1 img-container",
            tags$img(src = s$orig_path, class = "img-fluid",
                     alt = "Original thin section"))
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
    req(rv$revealed)
    s <- current_sample()
    mod <- input$module

    guess_pct <- input$guess_pct
    guess_count <- input$guess_count
    gt_pct <- s$inclusion_pct
    gt_count <- s$n_inclusions

    # Calculate scores for active module
    area_s <- if (mod %in% c("all", "area")) calc_score(guess_pct, gt_pct) else NULL
    count_s <- if (mod %in% c("all", "count")) calc_score(guess_count, gt_count) else NULL

    size_s <- NULL
    if (mod %in% c("all", "size")) {
      gt_size <- c(s$Fine_pct, s$Medium_pct, s$Coarse_pct, s$`Very Coarse_pct`)
      gu_size <- guess_size_dist()
      size_mae <- mean(abs(gu_size - gt_size))
      size_s <- max(0, round(100 - size_mae * 2))
    }

    valid_scores <- Filter(Negate(is.null), list(area_s, count_s, size_s))
    overall <- round(mean(unlist(valid_scores)))

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
    req(rv$revealed)
    s <- current_sample()

    size_cats <- c("Fine", "Medium", "Coarse", "Very Coarse")

    gt_vals <- c(s$Fine_pct, s$Medium_pct, s$Coarse_pct, s$`Very Coarse_pct`)
    gu_vals <- guess_size_dist()

    comparison <- data.frame(
      category = rep(factor(size_cats, levels = size_cats), 2),
      source = rep(c("Your Estimate", "AI Measurement"), each = 4),
      pct = c(gu_vals, gt_vals)
    )

    # Split into estimate and AI data for separate color mapping
    ai_data <- comparison |> filter(source == "AI Measurement")
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
               label = "\u25A0 AI measurement (color = size class)",
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

    avg_area <- if (length(area_scores) > 0) round(mean(area_scores)) else NA
    avg_count <- if (length(count_scores) > 0) round(mean(count_scores)) else NA
    avg_size <- if (length(size_scores) > 0) round(mean(size_scores)) else NA

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
        cols = c(area_score, count_score, size_score),
        names_to = "metric", values_to = "score"
      ) |>
      filter(score >= 0) |>
      mutate(metric = case_when(
        metric == "area_score" ~ "Area %",
        metric == "count_score" ~ "Count",
        metric == "size_score" ~ "Size Dist."
      ))

    if (nrow(plot_data) == 0) return(NULL)

    ggplot(plot_data, aes(x = attempt, y = score, color = metric)) +
      geom_line(linewidth = 1.2, alpha = 0.8) +
      geom_point(size = 3) +
      scale_color_manual(values = c("Area %" = "#3498db", "Count" = "#e74c3c", "Size Dist." = "#f39c12")) +
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
