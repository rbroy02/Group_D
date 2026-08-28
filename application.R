library(shiny)

source("model_engine.R")


# UI

ui <- fluidPage(
  
  titlePanel("Stock Return Regression"),
  
  tabsetPanel(
    
    # Pick tab
    
    tabPanel(
      "Pick",
      
      br(),
      
      fluidRow(
        
        # Dataset input
        
        column(
          width = 4,
          
          wellPanel(
            
            h3("1. Upload Stock Return Data"),
            
            fileInput(
              "file",
              "Upload CSV File",
              accept = ".csv"
            ),
            
            helpText(
              "Upload a CSV file containing stock returns.
               Each row should represent one observation,
               such as one trading day."
            ),
            
            hr(),
            
            h3("2. Select Target Stock"),
            
            # Generated from uploaded dataset
            uiOutput(
              "response_selector"
            ),
            
            helpText(
              "Select the stock return that will be used
               as the response variable."
            ),
            
            hr(),
            
            h3("3. Select Predictor Stocks"),
            
            # Generated from uploaded dataset
            uiOutput(
              "explanatory_selector"
            ),
            
            helpText(
              "Select one or more stocks or market indices
               to use as explanatory variables."
            ),
            
            hr(),
            
            actionButton(
              "fit_models",
              "Fit Regression Model",
              width = "100%"
            )
          )
        ),
        
        
        # Dataset preview
        
        column(
          width = 8,
          
          h2("Dataset Preview"),
          
          p(
            "The first 10 observations from the uploaded
             stock-return dataset are displayed below."
          ),
          
          tableOutput(
            "data_preview"
          ),
          
          hr(),
          
          h3("Example Model"),
          
          p(
            "For example, if AAPL is selected as the target
             and SPY, QQQ and XLK are selected as predictors:"
          ),
          
          wellPanel(
            
            h4(
              "AAPL Return = Intercept +"
            ),
            
            p(
              "a(SPY Return) + b(QQQ Return) + c(XLK Return)"
            )
          ),
          
          h3("Variable Roles"),
          
          p(
            strong("Response variable: "),
            "the stock whose returns are being modelled."
          ),
          
          p(
            strong("Explanatory variables: "),
            "stocks or market indices whose returns are
             used to explain movements in the target stock."
          )
        )
      )
    ),
    
    
    # Regression tab
    
    tabPanel(
      "Regression",
      
      br(),
      
      h2("Stock Return Regression"),
      
      p(
        "This section displays an Ordinary Least Squares
         regression model fitted using R's optim function."
      ),
      
      hr(),
      
      
      # OLS info
      
      fluidRow(
        
        column(
          width = 12,
          
          wellPanel(
            
            h3("Ordinary Least Squares"),
            
            p(
              "OLS chooses regression coefficients that
               minimise the sum of squared residuals."
            ),
            
            strong("Objective:"),
            
            p(
              "Minimise the Sum of Squared Residuals (RSS)"
            ),
            
            tableOutput(
              "ols_coefficients"
            )
          )
        )
      ),
      
      hr(),
      
      
      # Regression plot
      
      h2("Regression Plot"),
      
      p(
        "This visualisation shows the relationship between
         the selected response and explanatory variable."
      ),
      
      plotOutput(
        "regression_plot",
        height = "450px"
      ),
      
      hr(),
      
      
      # Actual vs Fitted
      
      h2("Actual vs Fitted Returns"),
      
      p(
        "This plot compares the observed return of the
         target stock with the returns predicted by the
         regression model."
      ),
      
      plotOutput(
        "fitted_plot",
        height = "400px"
      ),
      
      hr(),
      
      
      # Residuals
      
      h2("Residual Comparison"),
      
      p(
        "A residual is the difference between the observed
         stock return and the fitted stock return."
      ),
      
      plotOutput(
        "residual_plot",
        height = "400px"
      ),
      
      hr(),
      
      
      # Model Results
      
      h2("Regression Results"),
      
      tableOutput(
        "model_comparison"
      ),
      
      hr(),
      
      
      # Interpretation
      
      h2("How to Interpret the Results"),
      
      wellPanel(
        
        h3("Regression Coefficients"),
        
        p(
          "Each coefficient describes the relationship between
           a predictor stock's return and the target stock's
           return while the other predictors are held constant."
        ),
        
        hr(),
        
        h3("R-squared"),
        
        p(
          "R-squared represents the proportion of variation
           in the target stock's return that is explained by
           the selected predictor variables."
        ),
        
        hr(),
        
        h3("Residuals"),
        
        p(
          "Residuals represent the difference between the
           observed return and the return predicted by the
           regression model."
        )
      ),
      
      hr(),
      
      p(
        strong("Disclaimer: "),
        "This application is an educational demonstration
         of regression methodology and does not provide
         financial or investment advice."
      )
    )
  )
)


# Server logic

server <- function(input, output, session) {
  
  # Read the uploaded CSV
  
  dataset <- reactive({
    
    # Do nothing until a file has been uploaded
    req(input$file)
    
    # Read the uploaded CSV file
    data <- read.csv(
      input$file$datapath,
      header = TRUE
    )
    
    return(data)
  })
  
  
  # Dataset Preview
  
  output$data_preview <- renderTable({
    
    data <- dataset()
    
    # Display the first 10 observations
    head(
      data,
      10
    )
  })
  
  
  # Response variable
  
  output$response_selector <- renderUI({
    
    data <- dataset()
    
    # Only use numeric variables
    numeric_variables <- names(data)[
      sapply(data, is.numeric)
    ]
    
    selectInput(
      "response",
      "Target Stock / Response Variable:",
      choices = numeric_variables,
      selected = if ("AAPL" %in% numeric_variables) "AAPL"
    )
  })
  
  
  # Explanatory variables
  
  output$explanatory_selector <- renderUI({
    
    data <- dataset()
    
    # Only use numeric variables
    numeric_variables <- names(data)[
      sapply(data, is.numeric)
    ]
    
    # Remove response variable from predictor choices
    predictor_variables <- setdiff(
      numeric_variables,
      input$response
    )
    
    selectInput(
      "explanatory",
      "Predictor Stocks / Explanatory Variables:",
      choices = predictor_variables,
      selected = intersect(
        c("SPY", "QQQ", "XLK"),
        predictor_variables
      ),
      multiple = TRUE
    )
  })
  
  
  # Fit regression model
  
  model_result <- eventReactive(input$fit_models, {
    
    req(input$response)
    req(input$explanatory)
    
    data <- dataset()
    
    model_data <- data[
      complete.cases(
        data[, c(input$response, input$explanatory)]
      ),
    ]
    
    fit_model(
      data = model_data,
      response = input$response,
      predictors = input$explanatory
    )
  })
  
  
  # Data used for regression
  
  model_data <- eventReactive(input$fit_models, {
    
    req(input$response)
    req(input$explanatory)
    
    data <- dataset()
    
    data[
      complete.cases(
        data[, c(input$response, input$explanatory)]
      ),
    ]
  })
  
  
  # Regression coefficients
  
  output$ols_coefficients <- renderTable({
    
    result <- model_result()
    
    data.frame(
      Variable = names(result$coefficients),
      Coefficient = round(
        as.numeric(result$coefficients),
        6
      )
    )
  })
  
  
  # Regression plot
  
  output$regression_plot <- renderPlot({
    
    result <- model_result()
    data <- model_data()
    
    validate(
      need(
        length(input$explanatory) == 1,
        "Select exactly one predictor to display the regression plot."
      )
    )
    
    x <- data[[input$explanatory]]
    y <- data[[input$response]]
    
    plot(
      x,
      y,
      pch = 19,
      xlab = input$explanatory,
      ylab = input$response,
      main = paste(
        input$response,
        "vs",
        input$explanatory
      )
    )
    
    order_x <- order(x)
    
    lines(
      x[order_x],
      result$fitted[order_x],
      lwd = 2
    )
  })
  
  
  # Actual vs fitted values
  
  output$fitted_plot <- renderPlot({
    
    result <- model_result()
    data <- model_data()
    
    actual <- data[[input$response]]
    
    plot(
      actual,
      type = "l",
      lwd = 2,
      xlab = "Observation",
      ylab = "Return",
      main = paste(
        input$response,
        "- Actual vs Fitted Returns"
      )
    )
    
    lines(
      result$fitted,
      lwd = 2,
      lty = 2
    )
    
    legend(
      "topright",
      legend = c(
        "Actual",
        "Fitted"
      ),
      lty = c(
        1,
        2
      ),
      lwd = 2
    )
  })
  
  
  # Residual plot
  
  output$residual_plot <- renderPlot({
    
    result <- model_result()
    
    plot(
      result$residuals,
      pch = 19,
      xlab = "Observation",
      ylab = "Residual",
      main = "Regression Residuals"
    )
    
    abline(
      h = 0,
      lty = 2
    )
  })
  
  
  # Regression results
  
  output$model_comparison <- renderTable({
    
    result <- model_result()
    
    data.frame(
      Statistic = c(
        "Residual Sum of Squares",
        "R-squared",
        "Optim Convergence Code"
      ),
      
      Value = c(
        round(result$rss, 6),
        round(result$r_squared, 6),
        result$convergence
      )
    )
  })
}


# Runs the application

shinyApp(
  ui = ui,
  server = server
)
