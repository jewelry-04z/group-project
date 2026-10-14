#CURRENTLY A PROTOTYPE! 
#app.R
# HEXACO & Teamwork Explorer
# Run with: shiny::runApp("hexaco_teamwork_shiny")
#
# This prototype downloads the study dataset from OSF. If the URL or data
# format changes, place a local CSV named `data.csv` in this folder instead.
# Simulation outcomes are hypothetical and are not observed team performance.

library(shiny)
library(ggplot2)

data_url <- "https://osf.io/download/k4syv/"

hexaco_items <- list(
  honesty_humility = c("F06","F30r","F54","F12r","F36","F60r","F18","F42r","F24r","F48r"),
  emotionality = c("F05","F29","F53r","F11","F35r","F17","F41r","F23","F47","F59r"),
  extraversion = c("F04","F28r","F52r","F10r","F34","F58","F16","F40","F22","F46r"),
  agreeableness = c("F03","F27","F09r","F33","F51","F15r","F39","F57r","F21r","F45"),
  conscientiousness = c("F02","F26r","F08","F32r","F14r","F38","F50","F20r","F44r","F56r"),
  openness = c("F01r","F25","F07","F31r","F13","F37","F49r","F19","F43","F55")
)

twq_items <- list(
  communication = c("CMNC01","CMNC02","CMNC03","CMNC04r","CMNC05","CMNC06r","CMNC07r","CMNC08","CMNC09","CMNC10"),
  coordination = c("CRDN1","CRDN2","CRDN3","CRDN4r"),
  balance = c("BLNC1","BLNC2","BLNC3r"),
  support = c("SPRT1","SPRT2","SPRT3","SPRT4","SPRT5","SPRT6"),
  commitment = c("COMT1","COMT2","COMT3","COMT4r"),
  cohesion = c("COHS01","COHS02r","COHS03","COHS04","COHS05","COHS06r","COHS07","COHS08","COHS09","COHS10")
)

reverse_hexaco <- c("F12r","F24r","F30r","F42r","F48r","F60r",
                    "F35r","F41r","F53r","F59r","F10r","F28r","F46r","F52r",
                    "F09r","F15r","F21r","F57r","F14r","F20r","F26r","F32r","F44r","F56r",
                    "F01r","F31r","F49r")
reverse_twq <- c("CMNC04r","CMNC06r","CMNC07r","CRDN4r","BLNC3r","COMT4r","COHS02r","COHS06r")

nice_names <- c(
  honesty_humility = "Honesty–Humility", emotionality = "Emotionality",
  extraversion = "Extraversion", agreeableness = "Agreeableness",
  conscientiousness = "Conscientiousness", openness = "Openness",
  communication = "Communication", coordination = "Coordination",
  balance = "Balance of contributions", support = "Support",
  commitment = "Commitment", cohesion = "Cohesion"
)

load_data <- function() {
  if (file.exists("data.csv")) {
    d <- read.csv("data.csv", check.names = FALSE)
  } else {
    d <- tryCatch(read.csv2(data_url, check.names = FALSE), error = function(e) NULL)
    if (is.null(d) || !nrow(d)) {
      d <- tryCatch(read.csv(data_url, check.names = FALSE), error = function(e) NULL)
    }
  }
  validate(need(!is.null(d) && nrow(d) > 0, paste(
    "Could not load the dataset. Check your internet connection or place a copy",
    "of the dataset in this folder as data.csv."
  )))
  d
}

score_scales <- function(d) {
  for (nm in intersect(reverse_hexaco, names(d))) {
    d[[nm]] <- 6 - as.numeric(d[[nm]])
  }
  for (nm in intersect(reverse_twq, names(d))) {
    d[[nm]] <- 6 - as.numeric(d[[nm]])
  }
  for (scale in names(hexaco_items)) {
    items <- intersect(hexaco_items[[scale]], names(d))
    if (length(items) > 0) d[[scale]] <- rowMeans(d[, items, drop = FALSE], na.rm = TRUE)
  }
  for (scale in names(twq_items)) {
    items <- intersect(twq_items[[scale]], names(d))
    if (length(items) > 0) d[[scale]] <- rowMeans(d[, items, drop = FALSE], na.rm = TRUE)
  }
  d
}

theme_app <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15, colour = "#183153"),
      plot.subtitle = element_text(colour = "#52647A"),
      axis.title = element_text(face = "bold", colour = "#334155"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#E5EAF1"),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600;700&family=Manrope:wght@500;600;700;800&display=swap');
      body { background: #F4F7FB; color: #243247; font-family: 'DM Sans', sans-serif; }
      .container-fluid { max-width: 1450px; padding: 22px 30px 36px; }
      .hero { background: linear-gradient(120deg, #162D50, #245C78); color: white; padding: 28px 32px; border-radius: 20px; margin-bottom: 22px; box-shadow: 0 10px 26px rgba(22,45,80,.13); }
      .hero h1 { font-family: 'Manrope', sans-serif; font-weight: 800; margin: 0 0 8px; font-size: 30px; }
      .hero p { color: #DCEAF5; margin: 0; font-size: 15px; }
      .nav-tabs { border-bottom: 1px solid #DDE5EF; margin-bottom: 20px; }
      .nav-tabs > li > a { border: 0 !important; border-radius: 10px 10px 0 0; color: #64748B; font-weight: 700; padding: 13px 18px; }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover { color: #174A68; background: white; border-bottom: 3px solid #2B7A91 !important; }
      .well { background: white; border: 1px solid #E1E8F0; border-radius: 16px; box-shadow: 0 4px 14px rgba(30,55,90,.04); }
      .panel-card { background: white; border: 1px solid #E1E8F0; border-radius: 16px; padding: 22px; margin-bottom: 18px; box-shadow: 0 4px 14px rgba(30,55,90,.04); }
      h3, h4 { font-family: 'Manrope', sans-serif; color: #183153; font-weight: 800; }
      .btn-primary { background: #216A80; border-color: #216A80; border-radius: 10px; font-weight: 700; padding: 10px 18px; }
      .btn-primary:hover { background: #174A68; border-color: #174A68; }
      .form-control { border-radius: 9px; border-color: #D5DFEA; box-shadow: none; }
      .help-block, .text-muted { color: #718096; }
      .metric { background: #F1F7FA; border: 1px solid #DCEAF0; border-radius: 13px; padding: 14px 16px; margin-bottom: 10px; }
      .metric .metric-label { text-transform: uppercase; font-size: 11px; letter-spacing: .08em; color: #64748B; font-weight: 800; }
      .metric .metric-value { font-size: 25px; color: #174A68; font-family: 'Manrope', sans-serif; font-weight: 800; }
      .notice { background: #FFF8E8; border-left: 4px solid #E4AD42; border-radius: 8px; padding: 12px 14px; color: #755A22; }
      .section-intro { color: #64748B; margin-top: -4px; margin-bottom: 18px; }
      .irs--shiny .irs-bar, .irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to { background: #28778D; border-top-color: #28778D; border-bottom-color: #28778D; }
      .irs--shiny .irs-handle { border-color: #28778D; }
    "))
  ),
  div(class = "hero",
      h1("Personality & Teamwork Explorer"),
      p("Explore observed questionnaire data, then experiment with hypothetical team compositions.")
  ),
  tabsetPanel(
    id = "main_tabs",
    tabPanel(title = "Explore the data", value = "explore",
      br(),
      sidebarLayout(
        sidebarPanel(
          h4("Choose your variables"),
          p(class = "section-intro", "Compare a personality trait with one teamwork dimension."),
          selectInput("trait", "HEXACO trait", choices = nice_names[names(hexaco_items)], selected = "agreeableness"),
          selectInput("outcome", "Teamwork-quality dimension", choices = nice_names[names(twq_items)], selected = "communication"),
          checkboxInput("show_lm", "Show trend line and confidence band", TRUE),
          helpText("Scores are participant-level questionnaire composites.")
        ),
        mainPanel(
          div(class = "panel-card",
              h3("Relationship between scores"),
              p(class = "section-intro", "Each point represents one participant."),
              plotOutput("scatter", height = 410)
          ),
          div(class = "panel-card",
              h4("Exploratory linear regression"),
              verbatimTextOutput("model_summary"),
              p(class = "text-muted", "Interpret cautiously: the sample is small and participants are nested within teams. Association does not imply causation.")
          )
        )
      )
    ),
    tabPanel(title = "Simulate teams", value = "simulate",
      br(),
      fluidRow(
        column(4,
          div(class = "panel-card",
              h3("Build a scenario"),
              p(class = "section-intro", "Use the sliders to set the size of your hypothetical study."),
              sliderInput("n_teams", "Number of teams", min = 2, max = 30, value = 8, step = 1, sep = ""),
              sliderInput("team_size", "People per team", min = 2, max = 10, value = 4, step = 1, sep = ""),
              div(class = "metric",
                  div(class = "metric-label", "Total simulated participants"),
                  textOutput("total_people", inline = TRUE)
              ),
              tags$hr(),
              selectInput("strategy", "Team-formation strategy",
                choices = c("Random assignment" = "random",
                            "Balance the selected trait" = "balanced",
                            "Group similar trait scores" = "similar")),
              selectInput("sim_trait", "Trait used for grouping", choices = nice_names[names(hexaco_items)], selected = "agreeableness"),
              selectInput("sim_outcome", "Teamwork dimension to simulate", choices = nice_names[names(twq_items)], selected = "communication"),
              sliderInput("trait_weight", "Assumed trait contribution", min = 0, max = 1, value = 0.5, step = 0.05),
              sliderInput("noise_sd", "Random noise", min = 0, max = 1.5, value = 0.35, step = 0.05),
              actionButton("run_sim", "Run simulation", class = "btn-primary", icon = icon("play"), width = "100%")
          )
        ),
        column(8,
          div(class = "panel-card",
              h3("Scenario results"),
              p(class = "section-intro", "The values update when you run a new simulation."),
              fluidRow(
                column(4, div(class = "metric", div(class = "metric-label", "Teams"), div(class = "metric-value", textOutput("metric_teams", inline = TRUE)))),
                column(4, div(class = "metric", div(class = "metric-label", "People per team"), div(class = "metric-value", textOutput("metric_size", inline = TRUE)))),
                column(4, div(class = "metric", div(class = "metric-label", "Average simulated score"), div(class = "metric-value", textOutput("metric_avg", inline = TRUE))))
              ),
              plotOutput("sim_plot", height = 340),
              h4("Team-by-team results"),
              tableOutput("sim_table")
          ),
          div(class = "notice",
              strong("Simulation caveat: "),
              "These outcomes are hypothetical and depend on the assumptions you choose. They are not measured team performance or validated predictions."
          )
        )
      )
    ),
    tabPanel(title = "About", value = "about",
      br(),
      div(class = "panel-card",
          h3("About this prototype"),
          p("This app is designed to connect the observed personality/teamwork data with a transparent simulation exercise."),
          tags$ul(
            tags$li("Explore observed relationships between the six HEXACO traits and six teamwork-quality dimensions."),
            tags$li("Keep the teamwork dimensions separate so that differences between them remain visible."),
            tags$li("Compare hypothetical team-formation strategies using adjustable simulation settings."),
            tags$li("Treat the simulation as an illustration of assumptions, not as evidence that a strategy improves real-world performance.")
          ),
          div(class = "notice",
              strong("Before final submission: "),
              "verify reverse-keying and scale construction against the original study documentation, and review the simulation logic with the group."
          )
      )
    )
  )
)

server <- function(input, output, session) {
  raw_data <- reactive({
    d <- load_data()
    score_scales(d)
  })

  output$total_people <- renderText(input$n_teams * input$team_size)
  output$metric_teams <- renderText(input$n_teams)
  output$metric_size <- renderText(input$team_size)

  output$scatter <- renderPlot({
    d <- raw_data()
    validate(need(input$trait %in% names(d), paste("Trait score unavailable:", input$trait)))
    validate(need(input$outcome %in% names(d), paste("Teamwork score unavailable:", input$outcome)))
    p <- ggplot(d, aes(x = .data[[input$trait]], y = .data[[input$outcome]])) +
      geom_point(alpha = 0.75, size = 2.4, colour = "#28778D") +
      labs(x = unname(nice_names[input$trait]), y = unname(nice_names[input$outcome]),
           title = paste(unname(nice_names[input$trait]), "and", unname(nice_names[input$outcome]))) +
      theme_app()
    if (isTRUE(input$show_lm)) p <- p + geom_smooth(method = "lm", se = TRUE, colour = "#D18A35", fill = "#F4D7A6", linewidth = 1)
    p
  })

  output$model_summary <- renderPrint({
    d <- raw_data()
    validate(need(input$trait %in% names(d) && input$outcome %in% names(d), "Scores unavailable."))
    fit <- lm(reformulate(input$trait, response = input$outcome), data = d)
    summary(fit)
  })

  simulation <- eventReactive(input$run_sim, {
    d <- raw_data()
    trait <- input$sim_trait
    outcome <- input$sim_outcome
    validate(need(trait %in% names(d), "Selected trait score is unavailable."))
    validate(need(outcome %in% names(d), "Selected teamwork score is unavailable."))
    x <- d[[trait]]
    y <- d[[outcome]]
    ok <- is.finite(x) & is.finite(y)
    x <- x[ok]; y <- y[ok]
    validate(need(length(x) >= 4, "Not enough complete observations for simulation."))

    n <- input$n_teams * input$team_size
    sampled <- sample(seq_along(x), size = n, replace = TRUE)
    people <- data.frame(trait_score = x[sampled], observed_dimension = y[sampled])
    people$person_id <- seq_len(n)

    # Order people by the selected trait. Assignment is then chosen by strategy.
    ord <- order(people$trait_score)
    people <- people[ord, , drop = FALSE]
    if (input$strategy == "random") {
      people <- people[sample(seq_len(n)), , drop = FALSE]
      people$team <- rep(seq_len(input$n_teams), each = input$team_size)
    } else if (input$strategy == "balanced") {
      # Snake/round-robin assignment distributes low-to-high trait scores across teams.
      team_ids <- rep(seq_len(input$n_teams), times = input$team_size)
      for (r in seq_len(input$team_size)) {
        row <- if (r %% 2 == 1) seq_len(input$n_teams) else rev(seq_len(input$n_teams))
        team_ids[((r - 1) * input$n_teams + 1):(r * input$n_teams)] <- row
      }
      people$team <- team_ids
    } else {
      # Similar scores are adjacent in the sorted list and assigned to the same team.
      people$team <- rep(seq_len(input$n_teams), each = input$team_size)
    }

    team_dat <- aggregate(cbind(trait_score, observed_dimension) ~ team, data = people, FUN = mean)
    trait_z <- as.numeric(scale(team_dat$trait_score))
    if (length(trait_z) == 0 || all(!is.finite(trait_z))) trait_z <- rep(0, nrow(team_dat))
    baseline <- mean(team_dat$observed_dimension, na.rm = TRUE)
    simulated <- baseline + input$trait_weight * trait_z + rnorm(nrow(team_dat), 0, input$noise_sd)
    team_dat$simulated_outcome <- pmin(5, pmax(1, simulated))
    team_dat$team_size <- input$team_size
    team_dat
  }, ignoreInit = TRUE)

  output$metric_avg <- renderText({
    req(simulation())
    sprintf("%.2f", mean(simulation()$simulated_outcome))
  })

  output$sim_plot <- renderPlot({
    req(simulation())
    s <- simulation()
    ggplot(s, aes(x = factor(team), y = simulated_outcome)) +
      geom_col(fill = "#28778D", width = 0.72, alpha = 0.92) +
      geom_hline(yintercept = mean(s$simulated_outcome), linetype = "dashed", colour = "#D18A35", linewidth = 0.8) +
      coord_cartesian(ylim = c(1, 5)) +
      labs(x = "Simulated team", y = "Hypothetical score (1–5-like scale)",
           subtitle = "Dashed line = average across simulated teams") +
      theme_app()
  })

  output$sim_table <- renderTable({
    req(simulation())
    s <- simulation()
    s$trait_score <- round(s$trait_score, 2)
    s$observed_dimension <- round(s$observed_dimension, 2)
    s$simulated_outcome <- round(s$simulated_outcome, 2)
    names(s) <- c("Team", "Mean trait score", "Mean observed dimension", "Simulated score", "Team size")
    s
  }, striped = TRUE, bordered = FALSE, spacing = "m", digits = 2)
}

shinyApp(ui, server)
