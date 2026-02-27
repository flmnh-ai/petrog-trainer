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

# Scoring function: 0-100 based on relative error
calc_score <- function(guess, actual) {
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
/* Score ring */
.score-ring {
  position: relative;
  width: 120px;
  height: 120px;
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
  font-size: 32px;
  font-weight: 700;
  line-height: 1;
}
.score-ring .score-label {
  font-size: 11px;
  opacity: 0.7;
}

/* Stat card */
.stat-card {
  background: #f8f9fa;
  border: 1px solid #dee2e6;
  border-radius: 8px;
  padding: 16px;
  text-align: center;
}
.stat-card .stat-value { font-size: 28px; font-weight: 700; }
.stat-card .stat-label { font-size: 12px; opacity: 0.7; text-transform: uppercase; letter-spacing: 0.5px; }
.stat-card .stat-diff { font-size: 14px; margin-top: 4px; }

/* Smooth transitions */
.fade-in { animation: fadeIn 0.3s ease-in; }
@keyframes fadeIn { from { opacity: 0; transform: translateY(10px); } to { opacity: 1; transform: translateY(0); } }

/* Performance trend mini-chart */
.perf-dot {
  width: 12px;
  height: 12px;
  border-radius: 50%;
  display: inline-block;
  margin: 0 3px;
  opacity: 0.3;
}
.perf-dot.completed { opacity: 1; }

/* Comparison chart selector */
.pct-chart-strip {
  display: flex;
  flex-wrap: wrap;
  gap: 4px;
  justify-content: center;
}
.pct-chart-btn {
  width: 52px;
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
  font-size: 10px;
  font-weight: 600;
  margin-top: 1px;
  color: #555;
}
"

# --- JS ---
app_js <- "
Shiny.addCustomMessageHandler('updateProgress', function(data) {
  var bar = document.getElementById('progress_bar');
  if (bar) bar.style.width = data.pct + '%';
});

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

  nav_panel(
    "Practice",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        title = "Your Observations",

        # Sample navigation
        div(
          class = "d-flex justify-content-between align-items-center mb-3",
          actionButton("prev_sample", icon("arrow-left"), class = "btn-sm btn-outline-secondary"),
          h5(textOutput("sample_label"), style = "margin:0;"),
          actionButton("next_sample", icon("arrow-right"), class = "btn-sm btn-outline-secondary")
        ),

        # Performance dots
        uiOutput("perf_dots"),

        hr(),

        # Inclusion area — visual comparison chart
        h6("Inclusion area (click closest match)"),
        div(
          class = "pct-chart-strip mb-2",
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
        div(class = "text-center mb-3",
            tags$small(class = "text-muted", "Selected: "),
            tags$small(tags$strong(textOutput("selected_pct_display", inline = TRUE)))),

        # Count
        sliderInput("guess_count", "Estimated number of inclusions",
                    min = 0, max = 200, value = 50, step = 5),

        # Grain size
        h6("Grain size distribution", class = "mt-3"),
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
        ),

        hr(),

        actionButton("reveal", "Show AI Analysis",
                     class = "btn-primary w-100 btn-lg mt-2",
                     icon = icon("eye")),
        actionButton("reset", "Reset & Try Again",
                     class = "btn-outline-secondary w-100 mt-2",
                     icon = icon("rotate-left"))
      ),

      # Main content
      div(
        # Image display
        uiOutput("image_display"),

        # Results comparison (hidden until reveal)
        uiOutput("results_panel")
      )
    )
  ),

  nav_panel(
    "Performance",
    div(
      class = "container mt-4",
      style = "max-width: 900px;",
      h3("Your Performance"),
      uiOutput("performance_summary"),
      div(class = "mt-4",
          plotOutput("performance_trend", height = "300px")),
      div(class = "mt-4",
          plotOutput("bias_plot", height = "250px"))
    )
  ),

  nav_panel(
    "About",
    div(
      class = "container mt-4",
      style = "max-width: 800px;",
      h2("Ceramic Petrography Trainer"),
      p("An AI-assisted training tool for learning ceramic thin section analysis."),
      h4("How to Use"),
      tags$ol(
        tags$li("Examine the thin section image carefully"),
        tags$li("Enter your estimates for inclusion area percentage, count, and grain size distribution"),
        tags$li(tags$strong("Click 'Show AI Analysis'"), " to compare your observations against AI-detected ground truth"),
        tags$li("Review the score and comparison to calibrate your visual estimation skills"),
        tags$li("Navigate between samples to practice with different compositions"),
        tags$li("Check the Performance tab to see your trends and biases")
      ),
      h4("About the Images"),
      p("These are cross-polarized light (XPL) photomicrographs of ceramic thin sections.",
        "Inclusions (temper and natural aplastics) appear as bright, colorful grains in the clay matrix.",
        "The AI analysis uses an object detection model (RF-DETR) trained to identify and measure individual inclusions."),
      h4("Size Categories"),
      tags$table(
        class = "table table-sm",
        tags$thead(tags$tr(
          tags$th("Category"), tags$th("Description")
        )),
        tags$tbody(
          tags$tr(tags$td("Fine"), tags$td("Small grains, hard to distinguish individually")),
          tags$tr(tags$td("Medium"), tags$td("Clearly visible individual grains")),
          tags$tr(tags$td("Coarse"), tags$td("Large, prominent grains")),
          tags$tr(tags$td("Very Coarse"), tags$td("Dominant grains, easily identified"))
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
      # Spread remainder to neighbors
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

    # Calculate and store scores
    s <- samples[rv$current, ]
    area_s <- calc_score(input$guess_pct, s$inclusion_pct)
    count_s <- calc_score(input$guess_count, s$n_inclusions)

    # Size distribution score: mean absolute error across categories
    gt_size <- c(s$Fine_pct, s$Medium_pct, s$Coarse_pct, s$`Very Coarse_pct`)
    gu_size <- guess_size_dist()
    size_mae <- mean(abs(gu_size - gt_size))
    size_s <- max(0, round(100 - size_mae * 2))

    overall <- round(mean(c(area_s, count_s, size_s)))

    new_row <- data.frame(
      sample = rv$current,
      area_score = area_s,
      count_score = count_s,
      size_score = size_s,
      overall = overall,
      area_bias = input$guess_pct - s$inclusion_pct,
      count_bias = input$guess_count - s$n_inclusions
    )

    # Replace if sample already scored, otherwise append
    existing <- rv$scores$sample == rv$current
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
    paste0("Sample ", rv$current, " of ", n_samples)
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
        grade <- score_grade(score_row$overall[1])
        tags$span(
          class = paste("perf-dot completed", if (i == rv$current) "border border-dark" else ""),
          style = paste0("background:", grade$color, ";"),
          title = paste0("Sample ", i, ": ", score_row$overall[1])
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

  # Update progress bar
  observe({
    pct <- round(sum(rv$completed) / n_samples * 100)
    session$sendCustomMessage("updateProgress", list(pct = pct))
  })

  # (size total warning removed — now using categorical inputs)

  # Image display
  output$image_display <- renderUI({
    s <- current_sample()

    if (rv$revealed) {
      # Side by side
      div(
        class = "row g-3 fade-in",
        div(
          class = "col-md-6",
          div(class = "card",
              div(class = "card-header", "Original"),
              div(class = "card-body p-1",
                  tags$img(src = s$orig_path, class = "img-fluid w-100",
                           alt = "Original thin section")))
        ),
        div(
          class = "col-md-6",
          div(class = "card",
              div(class = "card-header", "AI Detections"),
              div(class = "card-body p-1",
                  tags$img(src = s$anno_path, class = "img-fluid w-100",
                           alt = "AI annotated thin section")))
        )
      )
    } else {
      div(
        class = "card fade-in",
        div(class = "card-header", "Thin Section (XPL)"),
        div(class = "card-body p-1",
            tags$img(src = s$orig_path, class = "img-fluid w-100",
                     alt = "Original thin section"))
      )
    }
  })

  # Score ring helper
  score_ring_html <- function(score) {
    grade <- score_grade(score)
    circumference <- 2 * pi * 50
    offset <- circumference * (1 - score / 100)

    div(
      class = "score-ring fade-in",
      HTML(sprintf('
        <svg width="120" height="120" viewBox="0 0 120 120">
          <circle cx="60" cy="60" r="50" fill="none" stroke="#e9ecef" stroke-width="8"/>
          <circle cx="60" cy="60" r="50" fill="none" stroke="%s" stroke-width="8"
                  stroke-dasharray="%.1f" stroke-dashoffset="%.1f"
                  stroke-linecap="round" style="transition: stroke-dashoffset 0.8s ease;"/>
        </svg>', grade$color, circumference, offset)),
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

    guess_pct <- input$guess_pct
    guess_count <- input$guess_count

    gt_pct <- s$inclusion_pct
    gt_count <- s$n_inclusions

    area_s <- calc_score(guess_pct, gt_pct)
    count_s <- calc_score(guess_count, gt_count)

    gt_size <- c(s$Fine_pct, s$Medium_pct, s$Coarse_pct, s$`Very Coarse_pct`)
    gu_size <- guess_size_dist()
    size_mae <- mean(abs(gu_size - gt_size))
    size_s <- max(0, round(100 - size_mae * 2))
    overall <- round(mean(c(area_s, count_s, size_s)))

    # Difference with direction indicator
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

    div(
      class = "mt-4 fade-in",

      # Score + stats row
      div(
        class = "row g-3 mb-3",
        # Overall score ring
        div(
          class = "col-md-3 text-center",
          score_ring_html(overall)
        ),
        # Area %
        div(
          class = "col-md-3",
          div(class = "stat-card h-100",
              div(class = "stat-label", "Inclusion Area"),
              div(class = "stat-value", paste0(gt_pct, "%")),
              div(class = "stat-diff", diff_display(guess_pct, gt_pct, "%")))
        ),
        # Count
        div(
          class = "col-md-3",
          div(class = "stat-card h-100",
              div(class = "stat-label", "Inclusion Count"),
              div(class = "stat-value", gt_count),
              div(class = "stat-diff", diff_display(guess_count, gt_count)))
        ),
        # Size accuracy
        div(
          class = "col-md-3",
          div(class = "stat-card h-100",
              div(class = "stat-label", "Size Distribution"),
              div(class = "stat-value", paste0(size_s, "%")),
              div(class = "stat-diff", tags$span(
                style = paste0("color:", score_grade(size_s)$color),
                score_grade(size_s)$label
              )))
        )
      ),

      # Size distribution comparison
      div(
        class = "card",
        div(class = "card-header", "Grain Size Distribution"),
        div(class = "card-body",
            plotOutput("size_comparison_plot", height = "220px"))
      )
    )
  })

  # Size distribution comparison plot (dark theme)
  output$size_comparison_plot <- renderPlot({
    req(rv$revealed)
    s <- current_sample()

    size_cats <- c("Fine", "Medium", "Coarse", "Very Coarse")

    comparison <- data.frame(
      category = rep(factor(size_cats, levels = size_cats), 2),
      source = rep(c("Your Estimate", "AI Measurement"), each = 4),
      pct = c(
        guess_size_dist(),
        s$Fine_pct, s$Medium_pct, s$Coarse_pct, s$`Very Coarse_pct`
      )
    )

    ggplot(comparison, aes(x = category, y = pct, fill = source)) +
      geom_col(position = "dodge", width = 0.6, alpha = 0.9) +
      scale_fill_manual(values = c("Your Estimate" = "#3498db", "AI Measurement" = "#2ecc71")) +
      labs(x = NULL, y = "Percentage (%)", fill = NULL) +
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
    avg_area <- round(mean(scores$area_score))
    avg_count <- round(mean(scores$count_score))
    avg_size <- round(mean(scores$size_score))
    grade <- score_grade(avg_overall)

    # Bias analysis
    mean_area_bias <- mean(scores$area_bias)
    mean_count_bias <- mean(scores$count_bias)

    area_tendency <- if (abs(mean_area_bias) < 1) "well-calibrated"
                     else if (mean_area_bias > 0) paste0("overestimating by ~", round(abs(mean_area_bias), 1), "%")
                     else paste0("underestimating by ~", round(abs(mean_area_bias), 1), "%")

    count_tendency <- if (abs(mean_count_bias) < 5) "well-calibrated"
                      else if (mean_count_bias > 0) paste0("overcounting by ~", round(abs(mean_count_bias)))
                      else paste0("undercounting by ~", round(abs(mean_count_bias)))

    div(
      class = "row g-3 mt-3",
      div(class = "col-md-3 text-center", score_ring_html(avg_overall)),
      div(class = "col-md-3",
          div(class = "stat-card",
              div(class = "stat-label", "Area Estimation"),
              div(class = "stat-value", paste0(avg_area, "%")),
              div(class = "stat-diff text-muted", area_tendency))),
      div(class = "col-md-3",
          div(class = "stat-card",
              div(class = "stat-label", "Count Estimation"),
              div(class = "stat-value", paste0(avg_count, "%")),
              div(class = "stat-diff text-muted", count_tendency))),
      div(class = "col-md-3",
          div(class = "stat-card",
              div(class = "stat-label", "Size Distribution"),
              div(class = "stat-value", paste0(avg_size, "%")),
              div(class = "stat-diff text-muted", paste0(nrow(scores), " samples analyzed"))))
    )
  })

  # Performance trend plot
  output$performance_trend <- renderPlot({
    scores <- rv$scores
    req(nrow(scores) > 0)

    plot_data <- scores |>
      arrange(sample) |>
      mutate(attempt = row_number()) |>
      tidyr::pivot_longer(cols = c(area_score, count_score, size_score),
                          names_to = "metric", values_to = "score") |>
      mutate(metric = case_when(
        metric == "area_score" ~ "Area %",
        metric == "count_score" ~ "Count",
        metric == "size_score" ~ "Size Dist."
      ))

    ggplot(plot_data, aes(x = attempt, y = score, color = metric)) +
      geom_line(linewidth = 1.2, alpha = 0.8) +
      geom_point(size = 3) +
      scale_color_manual(values = c("Area %" = "#3498db", "Count" = "#e74c3c", "Size Dist." = "#f39c12")) +
      scale_y_continuous(limits = c(0, 100)) +
      scale_x_continuous(breaks = scales::breaks_pretty()) +
      labs(x = "Attempt", y = "Score", color = NULL, title = "Score by Sample") +
      theme_petro()
  })

  # Bias plot
  output$bias_plot <- renderPlot({
    scores <- rv$scores
    req(nrow(scores) > 0)

    ggplot(scores, aes(x = factor(sample))) +
      geom_col(aes(y = area_bias), fill = "#3498db", alpha = 0.8, width = 0.4,
               position = position_nudge(x = -0.2)) +
      geom_col(aes(y = count_bias / 5), fill = "#e74c3c", alpha = 0.8, width = 0.4,
               position = position_nudge(x = 0.2)) +
      geom_hline(yintercept = 0, color = "#333", linewidth = 0.5) +
      scale_y_continuous(
        name = "Area bias (%)",
        sec.axis = sec_axis(~ . * 5, name = "Count bias")
      ) +
      labs(x = "Sample", title = "Estimation Bias (above = overestimate)") +
      theme_petro() +
      theme(axis.text.y.right = element_text(color = "#e74c3c"),
            axis.title.y.left = element_text(color = "#3498db"),
            axis.title.y.right = element_text(color = "#e74c3c"))
  })
}

shinyApp(ui, server)
