library(shiny)
library(shinydashboard)
library(prophet)
library(dplyr)
library(readr)
library(plotly)
library(DT)
library(lubridate)
library(future)
library(promises)

plan(multisession)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Event Impact Analysis"),
  dashboardSidebar(
    fileInput("file", "Upload CSV File", accept = c(".csv")),
    selectInput("date_col", "Select Date Column", choices = NULL),
    selectInput("metric_col", "Select Metric Column", choices = NULL),
    dateInput("event_start", "Event Start Date", 
              value = as.Date("2024-05-30"),
              max = today()),
    dateInput("event_end", "Event End Date", 
              value = today(),
              max = today()),
    checkboxInput("auto_tune", "Auto-tune Hyperparameters", value = TRUE),
    actionButton("analyze", "Analyse")
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("total_impact_box", width = 4),
      valueBoxOutput("percent_impact_box", width = 4),
      valueBoxOutput("daily_impact_box", width = 4)
    ),
    fluidRow(
      box(plotlyOutput("forecast_plot"), width = 12)
    ),
    fluidRow(
      box(DTOutput("results_table"), width = 12, title = "Daily Results")
    ),
    fluidRow(
      box(verbatimTextOutput("best_params"), width = 12, title = "Best Hyperparameters")
    )
  )
)

server <- function(input, output, session) {

  ukbankholidays <- read_csv("ukbankholidays.csv", 
                             col_types = cols(ds = col_date(format = "%d/%m/%Y")))
  
  data <- reactive({
    req(input$file)
    df <- read_csv(input$file$datapath)
    updateDateInput(session, "event_end", value = max(df[[input$date_col]]))
    df
  })
  
  observe({
    req(data())
    updateSelectInput(session, "date_col", choices = names(data()))
    updateSelectInput(session, "metric_col", choices = names(data()))
  })
  
  observe({
    req(data(), input$date_col)
    last_date <- max(as.Date(data()[[input$date_col]]))
    updateDateInput(session, "event_end", value = last_date)
  })
  
  best_params <- reactiveVal(NULL)
  results <- reactiveVal(NULL)
  
  observeEvent(input$analyze, {
    req(data(), input$date_col, input$metric_col, input$event_start, input$event_end)
    
    showNotification("Analysis is running...", duration = NULL, id = "analysis")
    
    df <- data() %>%
      select(ds = input$date_col, y = input$metric_col) %>%
      mutate(ds = as.Date(ds))
    
    # Capture input values before entering future_promise
    auto_tune <- input$auto_tune
    event_start <- input$event_start
    event_end <- input$event_end
    
    future_promise({
      if (auto_tune) {
        # Simplified parameter grid
        param_grid <- expand.grid(
          changepoint_prior_scale = c(0.01, 0.1, 0.5),
          seasonality_prior_scale = c(0.1, 1.0, 10.0),
          holidays_prior_scale = c(0.1, 1.0, 10.0),
          seasonality_mode = c("additive", "multiplicative")
        )
        
        # Initialize variables for best parameters
        best_params <- list()
        best_mae <- Inf
        
        # Loop through parameter combinations
        for (i in 1:nrow(param_grid)) {
          params <- param_grid[i, ]
          
          # Fit model with current parameters
          model <- prophet(
            df[df$ds < event_start,],
           # mcmc.samples = 0,
           # interval.width = 0.95,
           yearly.seasonality = TRUE,
            holidays = ukbankholidays,
            changepoint.prior.scale = params$changepoint_prior_scale,
            seasonality.prior.scale = params$seasonality_prior_scale,
            holidays.prior.scale = params$holidays_prior_scale,
            seasonality.mode = as.character(params$seasonality_mode)
          )
          
          # Make future dataframe for validation
          future <- make_future_dataframe(model, periods = 30)
          forecast <- predict(model, future)
          
          # Calculate MAE and MAPE for validation period
          validation_df <- df[df$ds >= event_start & df$ds < (event_start + days(30)), ]
          forecast_vals <- forecast$yhat[forecast$ds %in% validation_df$ds]
          actual_vals <- validation_df$y
          
          mae <- mean(abs(actual_vals - forecast_vals), na.rm = TRUE)
          mape <- mean(abs((actual_vals - forecast_vals) / actual_vals) * 100, na.rm = TRUE)
          
          # Update best parameters if current MAE is lower
          if (mae < best_mae) {
            best_mae <- mae
            best_params <- list(
              changepoint_prior_scale = params$changepoint_prior_scale,
              seasonality_prior_scale = params$seasonality_prior_scale,
              holidays_prior_scale = params$holidays_prior_scale,
              seasonality_mode = as.character(params$seasonality_mode),
              mae = mae,
              mape = mape
            )
          }
        }
        
        # Fit model with best parameters
        if (length(best_params) > 0) {
          model <- prophet(
            df[df$ds < event_start,],
            holidays = ukbankholidays,
          #  mcmc.samples = 0,
            #interval.width = 0.95,
            yearly.seasonality = TRUE,
            changepoint.prior.scale = best_params$changepoint_prior_scale,
            seasonality.prior.scale = best_params$seasonality_prior_scale,
            holidays.prior.scale = params$holidays_prior_scale,
            seasonality.mode = best_params$seasonality_mode
          )
          
          best_params_df <- as.data.frame(best_params)
        } else {
          best_params_df <- NULL
        }
      } else {
        # Fit model with default parameters
        model <- prophet(df[df$ds < event_start,])
        best_params_df <- NULL
      }
      
      # Make future dataframe
      future <- make_future_dataframe(model, periods = as.numeric(event_end - event_start) + 1)
      forecast <- predict(model, future)
      
      # Combine actual and forecasted data
      results_df <- left_join(forecast, df, by = "ds") %>%
        mutate(
          impact = y - yhat,
          percent_impact = (y / yhat - 1) * 100
        )
      
      # Calculate impact
      event_period <- results_df %>%
        filter(ds >= event_start, ds <= event_end)
      
      total_impact <- sum(event_period$impact, na.rm = TRUE)
      percent_impact <- (sum(event_period$y, na.rm = TRUE) / sum(event_period$yhat, na.rm = TRUE) - 1) * 100
      daily_impact <- total_impact / nrow(event_period)
      
      list(
        results = results_df, 
        event_period = event_period,
        total_impact = total_impact, 
        percent_impact = percent_impact,
        daily_impact = daily_impact,
        best_params = best_params_df
      )
    }) %...>% {
      results(.)
      best_params(.$best_params)
      removeNotification("analysis")
      showNotification("Analysis Complete!", duration = 10)
    }
  })
  
  output$total_impact_box <- renderValueBox({
    req(results())
    valueBox(
      value = round(results()$total_impact, 0),
      subtitle = "Total Impact",
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  
  output$percent_impact_box <- renderValueBox({
    req(results())
    valueBox(
      value = paste0(round(results()$percent_impact, 0), "%"),
      subtitle = "Percentage Impact",
      icon = icon("percentage"),
      color = "green"
    )
  })
  
  output$daily_impact_box <- renderValueBox({
    req(results())
    valueBox(
      value = round(results()$daily_impact, 0),
      subtitle = "Average Daily Impact",
      icon = icon("calendar-day"),
      color = "purple"
    )
  })
  
  output$forecast_plot <- renderPlotly({
    req(results())
    
    max_y <- max(results()$results$y, na.rm = TRUE)
    min_y <- min(results()$results$y, na.rm = TRUE)
    y_range <- max_y - min_y
    
    plot_ly() %>%
      add_lines(data = results()$results, x = ~ds, y = ~yhat, name = "Forecast", line = list(color = "blue")) %>%
      add_ribbons(data = results()$results, x = ~ds, ymin = ~yhat_lower, ymax = ~yhat_upper, 
                  name = "Uncertainty", fillcolor = "rgba(0, 0, 255, 0.2)", line = list(color = "transparent")) %>%
      add_lines(data = results()$results, x = ~ds, y = ~y, name = "Actual", line = list(color = "red")) %>%
      add_annotations(x = input$event_start, y = max_y, 
                      text = "Reject All Enabled", 
                      showarrow = TRUE, arrowhead = 2) %>%
      add_segments(x = input$event_start, xend = input$event_start, 
                   y = min_y - 0.05 * y_range, yend = max_y + 0.05 * y_range,
                   line = list(color = "darkgreen", width = 2), 
                   showlegend = FALSE) %>%
      add_segments(x = input$event_start, xend = input$event_start, 
                   y = min_y - 0.05 * y_range, yend = max_y + 0.05 * y_range,
                   line = list(color = "gray", width = 1.5, dash = "dot"), 
                   showlegend = FALSE) %>%
      layout(title = "Event Impact Analysis",
             xaxis = list(title = "Date"),
             yaxis = list(title = input$metric_col))
  })
  
  output$results_table <- renderDT({
    req(results())
    results()$event_period %>%
      select(Date = ds, Actual = y, Forecast = yhat, Impact = impact, `Percent Impact` = percent_impact) %>%
      mutate(across(where(is.numeric), ~ round(., 2))) %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$best_params <- renderPrint({
    best_params_df <- best_params()
    if (!is.null(best_params_df) && nrow(best_params_df) > 0) {
      best_params_df$mae <- round(as.numeric(best_params_df$mae), 4)
      best_params_df$mape <- round(as.numeric(best_params_df$mape), 2)
      print(best_params_df)
    } else {
      cat("No hyperparameter tuning was performed or no valid results were obtained.")
    }
  })
}

# Run the app
shinyApp(ui, server)