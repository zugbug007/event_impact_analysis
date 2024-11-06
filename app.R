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
#options(shiny.reactlog = TRUE)

plan(multisession)
#https://www3.an.adobe.com/x/3_1doo8z

ui <- dashboardPage(
  dashboardHeader(title = "Time Series Analysis"),
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
    sliderInput('frequency', 'Frequency', 
                value = 7, min = 2, max = 12),
    checkboxInput("auto_tune", "Auto-tune Hyperparameters", value = FALSE),
    radioButtons("decomp_type", "Decomposition Type",
                 choices = c("Additive" = "additive", 
                             "Multiplicative" = "multiplicative"),
                 selected = "additive"),
    actionButton("analyze", "Analyse")
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel("Impact Analysis",
               fluidRow(
                 valueBoxOutput("total_impact_box", width = 4),
                 valueBoxOutput("percent_impact_box", width = 4),
                 valueBoxOutput("daily_impact_box", width = 4)
               ),
               fluidRow(
                 box(plotlyOutput("forecast_plot"), width = 12)
               ),
               fluidRow(
                 box(verbatimTextOutput("best_params"), width = 12, title = "Best Hyperparameters")
               ),
               fluidRow(
                 box(DTOutput("results_table"), width = 12, title = "Daily Results")
               )
      ),
      tabPanel("Time Series Decomposition",
               fluidRow(
                 valueBoxOutput("trend_strength_box", width = 4),
                 valueBoxOutput("seasonal_strength_box", width = 4),
                 valueBoxOutput("remainder_strength_box", width = 4)
               ),fluidRow(
                 box(plotlyOutput("decomp_plot"), width = 12, title = "Time Series Decomposition"),
                 box(plotlyOutput("seasonal_pattern"), width = 12, title = "Seasonal Pattern"),
                 box(uiOutput("information_box"), width = 12, solidHeader = TRUE, status = "primary", title = "Time Series Decomposition")
               )
               
      )
    )
  )
)

server <- function(input, output, session) {

  ukbankholidays <- read_csv("ukbankholidays.csv", col_types = cols(ds = col_date(format = "%d/%m/%Y")))
  
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
    req(data(), input$date_col, input$metric_col, input$event_start, input$event_end, input$frequency, input$decomp_type)
    
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
           yearly.seasonality = TRUE,
            holidays = ukbankholidays,
            changepoint.prior.scale = params$changepoint_prior_scale,
            seasonality.prior.scale = params$seasonality_prior_scale,
            holidays.prior.scale = params$holidays_prior_scale,
            seasonality.mode = as.character(params$seasonality_mode)
          )
          
          # Make future dataframe for validation
          future <- make_future_dataframe(model, periods = 120)
          forecast <- predict(model, future)
          
          # Calculate MAE and MAPE for validation period
          validation_df <- df[df$ds >= event_start & df$ds < (event_start + days(120)), ]
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
    #  browser() 
    best_params_df <- best_params()
    if (!is.null(best_params_df) && nrow(best_params_df) > 0) {
      best_params_df$mae <- round(as.numeric(best_params_df$mae), 4)
      best_params_df$mape <- round(as.numeric(best_params_df$mape), 2)
      print(best_params_df)
    } else {
      cat("No hyperparameter tuning was performed or no valid results were obtained.")
    }
  })
  
  # Create decomposition reactive value
  decomposition <- reactive({
 #  browser() 
     req(data(), input$date_col, input$metric_col)
    
    # Create time series object
    df <- data() %>%
      select(ds = input$date_col, y = input$metric_col) %>%
      mutate(ds = as.Date(ds))
    
    # Convert to ts object (assuming monthly data - adjust frequency if needed)
    ts_data <- ts(df$y, frequency = input$frequency)
    
    # Perform decomposition
    decomp <- decompose(ts_data, type = input$decomp_type)
    
    # Calculate component strengths
    var_trend <- var(decomp$trend, na.rm = TRUE)
    var_seasonal <- var(decomp$seasonal, na.rm = TRUE)
    var_random <- var(decomp$random, na.rm = TRUE)
    var_total <- var_trend + var_seasonal + var_random
    
    # Return both decomposition and strength metrics
    list(
      decomposition = decomp,
      trend_strength = var_trend / var_total * 100,
      seasonal_strength = var_seasonal / var_total * 100,
      remainder_strength = var_random / var_total * 100
    )
  })
  
  # Decomposition plot
  output$decomp_plot <- renderPlotly({
 #   browser() 
    req(decomposition())
    decomp <- decomposition()$decomposition
    
    # Get the latest 12 months of data
    dates <- tail(seq(from = min(data()[[input$date_col]]), by = "day", length.out = length(decomp$x)), 365)
   # data_debug <- decomp
    
    plot_ly() %>%
      add_trace(x = dates, y = tail(decomp$x, 365), name = "Original", type = "scatter", mode = "lines", line = list(color = "black")) %>%
      add_trace(x = dates, y = tail(decomp$trend, 365), name = "Trend", type = "scatter", mode = "lines", line = list(color = "blue")) %>%
      add_trace(x = dates, y = tail(decomp$seasonal, 365), name = "Seasonal", type = "scatter", mode = "lines", line = list(color = "green")) %>%
      add_trace(x = dates, y = tail(decomp$random, 365), name = "Remainder", type = "scatter", mode = "lines", line = list(color = "red")) %>%
      layout(
        title = "Time Series Decomposition",
        xaxis = list(
          title = "Date",
          tickformat = "%b %Y",
          dtick = "M1",
          tickangle = 45
        ),
        yaxis = list(title = input$metric_col)
      )
  })
  
  # Seasonal pattern plot
  output$seasonal_pattern <- renderPlotly({
  #  browser() 
    req(decomposition())
    
    # Get the month names from original data
    month_names <- data() %>%
      mutate(month = format(as.Date(get(input$date_col)), "%b %Y")) %>%
      pull(month) %>%
      unique()
    
    # If we don't have all 12 months in the data, use built-in month abbreviations
    if(length(month_names) < 12) {
      month_names <- month.abb
    }
    
    decomp <- decomposition()$decomposition
    
    # Get the last 12 months of the seasonal component
    months <- 1:12
    seasonal_pattern <- tail(decomp$seasonal, 12)
    
    plot_ly() %>%
      add_trace(x = months, y = seasonal_pattern, 
                type = "scatter", mode = "lines+markers",
                line = list(color = "green")) %>%
      layout(
        title = "Seasonal Pattern",
        xaxis = list(
          title = "Month",
          tickvals = 1:12,
          tickformat = "%b %Y",
          ticktext = tail(month_names, 12),
          tickangle = 45
        ),
        yaxis = list(title = "Seasonal Effect")
      )
  })
  
  # Component strength boxes
  output$trend_strength_box <- renderValueBox({
    req(decomposition())
    valueBox(
      value = paste0(round(decomposition()$trend_strength, 1), "%"),
      subtitle = "Trend Strength",
      icon = icon("line-chart"),
      color = "blue"
    )
  })
  
  output$seasonal_strength_box <- renderValueBox({
    req(decomposition())
    valueBox(
      value = paste0(round(decomposition()$seasonal_strength, 1), "%"),
      subtitle = "Seasonal Strength",
      icon = icon("calendar"),
      color = "green"
    )
  })
  
  output$remainder_strength_box <- renderValueBox({
    req(decomposition())
    valueBox(
      value = paste0(round(decomposition()$remainder_strength, 1), "%"),
      subtitle = "Remainder Strength",
      icon = icon("random"),
      color = "red"
    )
  })
  # Information box content
  output$information_box <- renderUI({
    tagList(
      h3("Time Series Decomposition Information"),
      p("The time series data is decomposed into three components:"),
      tags$ul(
        tags$li(
          strong("Trend Strength:"),
          "The percentage of the total variance that is explained by the trend component. This tells us how important the long-term trend is in driving the overall variability of the series. A high trend strength (e.g. 70%) indicates that the trend-cycle accounts for a large portion of the total variation, while a low trend strength (e.g. 20%) means the trend plays a relatively minor role."
        ),
        tags$li(
          strong("Seasonal Strength:"),
          "The percentage of the total variance that is explained by the seasonal component. This shows how important the seasonal pattern is in driving the overall movement of the series. A high seasonal strength (e.g. 60%) means the seasonal component accounts for a large share of the variability, while a low seasonal strength (e.g. 10%) indicates seasonality plays a minor role."
        ),
        tags$li(
          strong("Remainder Strength:"),
          "The percentage of the total variance that is unexplained by the trend and seasonal components, representing the irregular or random component. This tells us how much of the variability in the series is not captured by the trend and seasonal patterns. A high remainder strength (e.g. 40%) means there is a substantial amount of irregular, unpredictable fluctuations in the data. A low remainder strength (e.g. 10%) indicates the trend and seasonality explain most of the dynamics."
        ),
        tags$li(
          strong("Summary:"),
          "Together, these three metrics provide a clear picture of the relative importance of each underlying component in the overall time series. They help the analyst understand the key drivers of the data and determine which modeling approaches may be most appropriate."
        ),
        tags$li(
      h3("Decomposition Types:"),
      p("How to Choose Between Additive and Multiplicative Decompositions:"),
      tags$ul(
          tags$li(
            strong("Additive:"),
            "The additive model is useful when the seasonal variation is relatively constant over time."
            ),
          tags$li(
            strong("Multiplicative:"),
            "The multiplicative model is useful when the seasonal variation increases over time."
            )
          )
        )
      )
    )
  })
}

# Run the app
shinyApp(ui, server)
