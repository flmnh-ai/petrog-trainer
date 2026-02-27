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

# --- UI ---
ui <- page_navbar(
  title = "Ceramic Petrography Trainer",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Inter")
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

        hr(),

        # Student inputs
        sliderInput("guess_pct", "Estimated inclusion area (%)",
                    min = 0, max = 50, value = 15, step = 1),
        sliderInput("guess_count", "Estimated number of inclusions",
                    min = 0, max = 200, value = 50, step = 5),

        h6("Grain size distribution (% of inclusions)", class = "mt-3"),
        div(
          class = "row g-2",
          div(class = "col-6",
              numericInput("guess_fine", "Fine", value = 25, min = 0, max = 100, step = 5)),
          div(class = "col-6",
              numericInput("guess_medium", "Medium", value = 25, min = 0, max = 100, step = 5)),
          div(class = "col-6",
              numericInput("guess_coarse", "Coarse", value = 25, min = 0, max = 100, step = 5)),
          div(class = "col-6",
              numericInput("guess_vcoarse", "V. Coarse", value = 25, min = 0, max = 100, step = 5))
        ),
        textOutput("size_total_warning"),

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
        # Progress bar
        div(
          class = "mb-3",
          div(class = "d-flex justify-content-between",
              span("Progress"),
              textOutput("progress_text", inline = TRUE)
          ),
          div(class = "progress", style = "height: 8px;",
              div(class = "progress-bar bg-success",
                  role = "progressbar",
                  style = htmltools::css(width = "0%"),
                  id = "progress_bar")
          )
        ),

        # Image display
        uiOutput("image_display"),

        # Results comparison (hidden until reveal)
        uiOutput("results_panel")
      )
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
        tags$li("Enter your estimates in the sidebar: inclusion area percentage, count, and grain size distribution"),
        tags$li("Click 'Show AI Analysis' to compare your observations against AI-detected ground truth"),
        tags$li("Review the comparison to calibrate your visual estimation skills"),
        tags$li("Navigate between samples to practice with different compositions")
      ),
      h4("About the Images"),
      p("These are cross-polarized light (XPL) photomicrographs of ceramic thin sections.",
        "Inclusions (temper and natural aplastics) appear as bright, colorful grains in the clay matrix.",
        "The AI analysis uses an object detection model trained to identify and measure individual inclusions."),
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
        "Built by the Computational Archaeology Lab, University of Florida.",
        "Powered by the ", tags$code("petrographer"), " R package.")
    )
  )
)

# --- Server ---
server <- function(input, output, session) {

  # Reactive values
  rv <- reactiveValues(
    current = 1,
    revealed = FALSE,
    completed = rep(FALSE, n_samples)
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

  output$progress_text <- renderText({
    paste0(sum(rv$completed), "/", n_samples, " completed")
  })

  # Update progress bar
  observe({
    pct <- round(sum(rv$completed) / n_samples * 100)
    session$sendCustomMessage("updateProgress", list(pct = pct))
  })

  output$size_total_warning <- renderText({
    total <- sum(input$guess_fine, input$guess_medium, input$guess_coarse, input$guess_vcoarse,
                 na.rm = TRUE)
    if (abs(total - 100) > 1) {
      paste0("Total: ", total, "% (should sum to 100%)")
    } else {
      ""
    }
  })

  # Image display
  output$image_display <- renderUI({
    s <- current_sample()

    if (rv$revealed) {
      # Show both images side by side
      div(
        class = "row g-3",
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
      # Show original only
      div(
        class = "card",
        div(class = "card-header", "Thin Section (XPL)"),
        div(class = "card-body p-1",
            tags$img(src = s$orig_path, class = "img-fluid w-100",
                     alt = "Original thin section"))
      )
    }
  })

  # Results comparison panel
  output$results_panel <- renderUI({
    req(rv$revealed)
    s <- current_sample()

    # Student guesses
    guess_pct <- input$guess_pct
    guess_count <- input$guess_count
    guess_size <- c(
      Fine = input$guess_fine %||% 25,
      Medium = input$guess_medium %||% 25,
      Coarse = input$guess_coarse %||% 25,
      `Very Coarse` = input$guess_vcoarse %||% 25
    )

    # Ground truth
    gt_pct <- s$inclusion_pct
    gt_count <- s$n_inclusions
    gt_size <- c(
      Fine = s$Fine_pct,
      Medium = s$Medium_pct,
      Coarse = s$Coarse_pct,
      `Very Coarse` = s$`Very Coarse_pct`
    )

    # Difference formatting
    diff_badge <- function(guess, actual, unit = "") {
      diff <- guess - actual
      color <- if (abs(diff) < actual * 0.15) "success"
               else if (abs(diff) < actual * 0.3) "warning"
               else "danger"
      sign <- if (diff > 0) "+" else ""
      tags$span(
        class = paste0("badge bg-", color),
        paste0(sign, round(diff, 1), unit)
      )
    }

    div(
      class = "mt-4",

      # Summary comparison
      div(
        class = "row g-3 mb-3",
        div(
          class = "col-md-6",
          div(class = "card",
              div(class = "card-header", "Inclusion Area (%)"),
              div(class = "card-body",
                  div(class = "d-flex justify-content-between align-items-center",
                      div(
                        tags$small(class = "text-muted", "Your estimate"),
                        h4(paste0(guess_pct, "%"))
                      ),
                      div(class = "text-center",
                          diff_badge(guess_pct, gt_pct, "%")),
                      div(class = "text-end",
                          tags$small(class = "text-muted", "AI measurement"),
                          h4(paste0(gt_pct, "%")))
                  )))
        ),
        div(
          class = "col-md-6",
          div(class = "card",
              div(class = "card-header", "Inclusion Count"),
              div(class = "card-body",
                  div(class = "d-flex justify-content-between align-items-center",
                      div(
                        tags$small(class = "text-muted", "Your estimate"),
                        h4(guess_count)
                      ),
                      div(class = "text-center",
                          diff_badge(guess_count, gt_count)),
                      div(class = "text-end",
                          tags$small(class = "text-muted", "AI measurement"),
                          h4(gt_count))
                  )))
        )
      ),

      # Size distribution comparison
      div(
        class = "card",
        div(class = "card-header", "Grain Size Distribution"),
        div(class = "card-body",
            plotOutput("size_comparison_plot", height = "250px"))
      )
    )
  })

  # Size distribution comparison plot
  output$size_comparison_plot <- renderPlot({
    req(rv$revealed)
    s <- current_sample()

    size_cats <- c("Fine", "Medium", "Coarse", "Very Coarse")

    comparison <- data.frame(
      category = rep(factor(size_cats, levels = size_cats), 2),
      source = rep(c("Your Estimate", "AI Measurement"), each = 4),
      pct = c(
        input$guess_fine %||% 25, input$guess_medium %||% 25,
        input$guess_coarse %||% 25, input$guess_vcoarse %||% 25,
        s$Fine_pct, s$Medium_pct, s$Coarse_pct, s$`Very Coarse_pct`
      )
    )

    ggplot(comparison, aes(x = category, y = pct, fill = source)) +
      geom_col(position = "dodge", width = 0.6) +
      scale_fill_manual(values = c("Your Estimate" = "#3498db", "AI Measurement" = "#2ecc71")) +
      labs(x = NULL, y = "Percentage (%)", fill = NULL) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "top",
        panel.grid.major.x = element_blank()
      )
  })
}

# Custom JS for progress bar
js <- "
Shiny.addCustomMessageHandler('updateProgress', function(data) {
  document.getElementById('progress_bar').style.width = data.pct + '%';
});
"

# Wrap UI with custom JS
ui_final <- tagList(
  tags$head(tags$script(HTML(js))),
  ui
)

shinyApp(ui_final, server)
