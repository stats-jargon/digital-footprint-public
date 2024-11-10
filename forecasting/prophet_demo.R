library(shiny)
library(prophet)
library(lubridate)
library(highcharter)
library(dplyr)
library(bslib)

# Define a modern theme using bslib
custom_theme <- bs_theme(
  version = 4,
  bootswatch = "minty",
  primary = "#007bff",
  secondary = "#6c757d",
  base_font = font_google("Roboto"),
  heading_font = font_google("Poppins")
)

# Define UI
ui <- fluidPage(
  theme = custom_theme,
  
  # Custom CSS for loading bar
  tags$head(tags$style(HTML("
    #loading-bar {
      width: 100%;
      height: 4px;
      background-color: #007bff;
      margin-top: 10px;
      display: none;
    }
  "))),
  
  # Custom JavaScript for showing/hiding loading bar
  tags$script(HTML("
    $(document).on('shiny:inputchanged', function(event) {
      if (event.name === 'forecast') {
        $('#loading-bar').show();
      }
    });
    Shiny.addCustomMessageHandler('hide-loading', function(message) {
      $('#loading-bar').hide();
    });
  ")),
  
  titlePanel("CSV Upload & Forecast with Prophet"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", 
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      numericInput("periods", "Forecast Period (days):", value = 30, min = 1),
      fluidRow(
        column(6, checkboxInput("dailySeasonality", "Enable Daily Seasonality", value = FALSE)),
        column(6, actionButton("forecast", "Run Forecast", class = "btn btn-primary"))
      ),
      
      # Loading bar positioned below the "Run Forecast" button
      div(id = "loading-bar"),
      
      tags$br(),
      selectInput("visualization", "Select Visualization:", 
                  choices = c("Forecast Plot" = "forecast",
                              "Trend Component" = "trend",
                              "Weekly Seasonality" = "weekly",
                              "Yearly Seasonality" = "yearly",
                              "Forecast Table" = "table")),
      
      # Show "Number of Rows to Display" only when "Forecast Table" is selected
      conditionalPanel(
        condition = "input.visualization == 'table'",
        numericInput("numRows", "Number of Rows to Display:", value = 10, min = 1)
      ),
      
      # Download button for exporting data to CSV
      downloadButton("downloadData", "Download Data Table", class = "btn btn-success")
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.visualization == 'forecast'",
        highchartOutput("forecastPlot")
      ),
      conditionalPanel(
        condition = "input.visualization == 'trend'",
        highchartOutput("trendPlot")
      ),
      conditionalPanel(
        condition = "input.visualization == 'weekly'",
        highchartOutput("weeklyPlot")
      ),
      conditionalPanel(
        condition = "input.visualization == 'yearly'",
        highchartOutput("yearlyPlot")
      ),
      conditionalPanel(
        condition = "input.visualization == 'table'",
        tableOutput("forecastTable")
      ),
      textOutput("errorMessage")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive to read and preprocess CSV file
  data <- reactive({
    req(input$file1)
    tryCatch({
      df <- read.csv(input$file1$datapath)
      
      # Check if 'ds' column exists and parse dates using lubridate
      if ("ds" %in% colnames(df)) {
        df$ds <- parse_date_time(df$ds, orders = c("ymd", "mdy", "dmy", "ymd HMS", "mdy HMS", "dmy HMS"))
      }
      
      # Check if there are still any NA values in 'ds' column after conversion
      if (any(is.na(df$ds))) {
        output$errorMessage <- renderText({"Error: Unable to parse dates in 'ds' column. Ensure date format is correct."})
        return(NULL)
      }
      
      df
    }, error = function(e) {
      output$errorMessage <- renderText({"Error: Unable to read CSV. Make sure it has 'ds' and 'y' columns."})
      NULL
    })
  })
  
  # Reactive to run forecast
  forecastData <- eventReactive(input$forecast, {
    df <- data()
    req(df)
    
    # Check if the data has the required columns for prophet
    if(!all(c("ds", "y") %in% colnames(df))) {
      output$errorMessage <- renderText({"Error: CSV must contain 'ds' and 'y' columns."})
      return(NULL)
    }
    
    # Create Prophet model with user-selected daily seasonality option
    model <- prophet(df, daily.seasonality = input$dailySeasonality)
    
    # Make future dataframe for forecast period
    future <- make_future_dataframe(model, periods = input$periods)
    
    # Run forecast
    forecast <- predict(model, future)
    
    # Send message to hide loading bar
    session$sendCustomMessage(type = "hide-loading", message = list())
    
    # Convert ds column to Date type for readable format
    forecast$ds <- as.Date(forecast$ds)
    df$ds <- as.Date(df$ds)
    
    # Add Unix timestamp for highcharter
    forecast$ds_ts <- as.numeric(as.POSIXct(forecast$ds)) * 1000
    df$ds_ts <- as.numeric(as.POSIXct(df$ds)) * 1000
    
    # Separate forecasted data and align using row index
    forecasted_data <- forecast[forecast$ds > max(df$ds), ]
    actual_data <- df
    
    # Create table with actuals and forecasted values aligned by row index
    actual_table <- data.frame(Date = actual_data$ds, Actual = actual_data$y)
    forecast_table <- data.frame(Date = forecast$ds, Predicted = forecast$yhat, 
                                 `Lower Bound` = forecast$yhat_lower, `Upper Bound` = forecast$yhat_upper)
    
    # Concatenate the tables row-wise
    table_data <- merge(x = forecast_table, y = actual_table, by = "Date", all.x=TRUE)
    
    # Extract only the last full week of weekly seasonality data
    weekly_component <- forecast %>%
      mutate(day_of_week = weekdays(ds)) %>%
      filter(row_number() %% 7 == 0) %>%
      arrange(factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday", 
                                             "Thursday", "Friday", "Saturday", "Sunday"))) %>%
      group_by(day_of_week) %>%
      summarise(weekly_effect = mean(weekly, na.rm = TRUE)) %>%
      ungroup()
    
    # Add decomposition components
    trend <- data.frame(ds_ts = forecast$ds_ts, trend = forecast$trend)
    yearly <- data.frame(ds_ts = forecast$ds_ts, yearly = forecast$yearly)
    
    # Save the model, forecast, actual, and decomposition components for plotting
    list(forecasted_data = forecasted_data, actual_data = actual_data, table_data = table_data,
         model = model, trend = trend, weekly_component = weekly_component, yearly = yearly)
  })
  
  # Render forecast table with actuals and "Predicted" as yhat values
  output$forecastTable <- renderTable({
    req(forecastData())
    table_data <- forecastData()$table_data
    table_data$Date <- format(table_data$Date, "%Y-%m-%d")  # Format date for display
    head(table_data, n = input$numRows)
  }, rownames = FALSE)
  
  # Render interactive forecast plot with highcharter
  output$forecastPlot <- renderHighchart({
    req(forecastData())
    actual_data <- forecastData()$actual_data
    forecasted_data <- forecastData()$forecasted_data
    
    highchart() %>%
      hc_title(text = "Forecast Plot with Historical Actuals") %>%
      hc_chart(zoomType = "x") %>%
      hc_xAxis(type = "datetime", labels = list(format = "{value:%Y-%m-%d}")) %>%
      hc_add_series(name = "Actuals", data = list_parse2(actual_data[, c("ds_ts", "y")]), type = "line") %>%
      hc_add_series(name = "Forecasted", data = list_parse2(forecasted_data[, c("ds_ts", "yhat")]), type = "line") %>%
      hc_add_series(name = "Lower Bound", data = list_parse2(forecasted_data[, c("ds_ts", "yhat_lower")]), type = "line", dashStyle = "ShortDash") %>%
      hc_add_series(name = "Upper Bound", data = list_parse2(forecasted_data[, c("ds_ts", "yhat_upper")]), type = "line", dashStyle = "ShortDash") %>%
      hc_tooltip(xDateFormat = "%Y-%m-%d", pointFormat = "{series.name}: <b>{point.y}</b>")
  })
  
  # Download handler for exporting the entire data table
  output$downloadData <- downloadHandler(
    filename = function() { paste("forecast_data-", Sys.Date(), ".csv", sep="") },
    content = function(file) {
      write.csv(forecastData()$table_data, file, row.names = FALSE)
    }
  )
  
  # Render interactive trend plot with formatted date
  output$trendPlot <- renderHighchart({
    req(forecastData())
    trend <- forecastData()$trend
    
    highchart() %>%
      hc_title(text = "Trend Component") %>%
      hc_chart(zoomType = "x") %>%
      hc_xAxis(type = "datetime", labels = list(format = "{value:%Y-%m-%d}")) %>%
      hc_add_series(name = "Trend", data = list_parse2(trend), type = "line") %>%
      hc_tooltip(xDateFormat = "%Y-%m-%d", pointFormat = "{series.name}: <b>{point.y}</b>")
  })
  
  # Render interactive weekly seasonality plot for last full week
  output$weeklyPlot <- renderHighchart({
    req(forecastData())
    weekly_component <- forecastData()$weekly_component
    
    highchart() %>%
      hc_title(text = "Weekly Seasonality Component (Last Full Week)") %>%
      hc_chart(zoomType = "x") %>%
      hc_xAxis(categories = weekly_component$day_of_week) %>%
      hc_add_series(name = "Weekly Effect", data = weekly_component$weekly_effect, type = "line") %>%
      hc_tooltip(pointFormat = "{series.name}: <b>{point.y}</b>")
  })
  
  # Render interactive yearly seasonality plot with formatted date
  output$yearlyPlot <- renderHighchart({
    req(forecastData())
    yearly <- forecastData()$yearly
    
    highchart() %>%
      hc_title(text = "Yearly Seasonality Component") %>%
      hc_chart(zoomType = "x") %>%
      hc_xAxis(type = "datetime", labels = list(format = "{value:%Y-%m-%d}")) %>%
      hc_add_series(name = "Yearly Seasonality", data = list_parse2(yearly), type = "line") %>%
      hc_tooltip(xDateFormat = "%Y-%m-%d", pointFormat = "{series.name}: <b>{point.y}</b>")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
