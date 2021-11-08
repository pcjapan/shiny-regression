## File upload functionality based on https://stackoverflow.com/a/36955396

# Required packages

library(shiny)
library(shinyWidgets)
library(dplyr)
library(psych)
library(ggplot2)
library(ggpmisc)
library(boot)
library(MASS)
library(psychometric)
library(fBasics)
library(lmtest)

# Functions

residTest  <- function(modelName, dataset) {
  attach(dataset)
  #Testing normal distribution and independence assumptions
  test1 <- jarqueberaTest(modelName$resid) #Test residuals for normality
  #Null Hypothesis: Skewness and Kurtosis are equal to zero
  test2 <- dwtest(modelName) #Test for independence of residuals
  #Null Hypothesis: Errors are serially Uncorrelated
  #Simple Regression Residual Plots
  cat("Jarque - Bera Normality Test to check if residuals are normally distributed\nWe want a non-significant result so as not to reject the null hypothesis that skewness and kurtosis are equal to zero\n")
  print(test1)
  if (test1@test[["p.value"]] < 0.05){testN1 <- "The Jarque - Bera Normality Test is significant.\nThis suggests the data is NOT normally distributed, and violates\nthe assumptions of the regression\n\n"}
  else {testN1 <- "The Jarque - Bera Normality Test is non-significant.\nThis suggests the data is normally distributed\n\n"}
  if (test2[["p.value"]] < 0.05){testN2 <- "The Durbin-Watson Test is significant.\nThis suggests the residuals are NOT independent.\nIt is likely your data violates the assumptions of the regression test\n\n"}
  else {testN2 <- "The Durbin-Watson Test is non-significant.\nThis suggests the independence of residuals\n\n"}
  cat(testN1)
  cat("Durbin-Watson Test to check for independence of residuals:\nWe want a non-significant result so as not to reject the null hypothesis:\nThe errors are serially uncorrelated\n")
  print(test2)
  cat(testN2)
}


ui <- shinyUI(fluidPage(
  tags$head(
    # styling output
    
    tags$style(
      "
      textarea {
        padding:10px;
        min-width: 20px; !important;
        position: relative;
      }

      p { line-height: 2; }

      h3 {
      line-height: 2;
      color: red;
      border-bottom: 2px solid;
      }

      h3.side {
      font-style: italic;
      border-bottom: none;
      }

      pre#StatSum { width: 800px; }

      pre#Regress, pre#RegressCI, pre#BsCI, 
      pre#RegressRSET, pre#RegressRSE,
      pre#CIrg { width: 600px; }
      
      pre#RegressRSET { 
      background: none; 
      border: none;
      font-weight: bold;
      }

      .instructText, #text1 {
      font-style: italic;
      line-height:2;
      margin-bottom: 6pt;
      }
      
      .btn.shiny-download-link { background: #FFFED3; }
      "
      
    )
  ),
  
  titlePanel("Regression Analysis"),
  tabsetPanel(
    tabPanel(
      "Upload Data",
      titlePanel("Uploading Data"),
      sidebarLayout(
        sidebarPanel(
          fileInput(
            'file1',
            'Choose CSV File',
            accept = c('text/csv',
                       'text/comma-separated-values,text/plain',
                       '.csv')
          ),
          
          # added interface for uploading data from
          # http://shiny.rstudio.com/gallery/file-upload.html
          p(
            "The variables below control how the application reads your data. Please set as required.",
            class = "lead"
          ),
          p(
            "If your data table has a header row, make sure the checkbox below is checked.",
            class = "instructText"
          ),
          awesomeCheckbox('header', 'Header', TRUE, status = "success"),
          p("How are the columns separated? Select the correct option.", class = "instructText"),
          awesomeRadio(
            'sep',
            'Separator',
            c(
              Comma = ',',
              Semicolon = ';',
              Tab = '\t'
            ),
            ',',
            status = "success"
          ),
          p(
            "Are the table values quoted (e.g.; \"1\",\"2\",...)? Indicate below.",
            class = "instructText"
          ),
          awesomeRadio(
            'quote',
            'Quote',
            c(
              None = '',
              'Double Quote' = '"',
              'Single Quote' = "'"
            ),
            '"',
            status = "success"
          ),
        ),
        mainPanel(tableOutput('contents'))
      )
    ),
    
    ## Second tab with the output
    
    tabPanel(
      "Output",
      pageWithSidebar(
        headerPanel('Analysis Output'),
        sidebarPanel(
          tags$h3("Plotting Options", class = "side"),
          p("Select the x and y values from your data.", class = "instructText"),
          # "Empty inputs" - they will be updated after the data is uploaded
          selectInput('xcol', 'X Variable', "", width = "400px"),
          selectInput('ycol', 'Y Variable', "", selected = "", width = "400px"),
          
          # Input boxes for graph labels and title
          tags$br(),
          p(
            "Enter meaningful values for the X and Y axes, along with a title for the graph.",
            class = "instructText"
          ),
          textInput("XVar", "X Axis Label", value = "X", width = "400px"),
          textInput("YVar", "Y Axis Label", value = "Y", width = "400px"),
          textInput(
            "pTitle",
            "Plot Title",
            value = "My Scatterplot",
            width = "400px",
            placeholder = "Enter a descriptive title"
          ),
          p(
            "Do you want to do a robust regression ?",
            class = "instructText"
          ),
          awesomeRadio("rReg", "Robust Regression", selected = 0, c(Yes = 1,No = 0),status = "success"),
          
        ),
        
        # This next section displays the results
        
        mainPanel(tabsetPanel(
          tabPanel(
            "Regression Results",
            tags$h3("Descriptive Statistics"),
            verbatimTextOutput("StatSum"),
            tags$br(),
            tags$h3("Residual Standard Error"),
            tags$p("The residual standard error (RSE) is a way to measure the standard deviation of the residuals in a regression model. Use this to help decide which regression model to report."),
            tags$ul(tags$li("The lower the value for RSE, the more closely a model is able to fit the data."),
            				tags$li("Compare the RSE for the robust regression model against the standard model. The lower of the two points to a better fit for that particular model."),
            ),
            tags$br(),
            tags$div(HTML("<h3>Multiple R-squared and F-statistic</h3>

* Multiple R-squared is the square of the simple correlation between the predictor and the outcome (measured) variable (the _intercept_).
* Taking the square root gives us the Pearson Correlation Coefficient (R) between these two variables
* This Multiple R-squared value also tells us that the _predictor_ accounts for _R-squared %_ of the variation in the outcome variable (the _intercept_) 
* The F-statistic is the result of an ANOVA of the data. If it is statistically significant, it means that the regression model predicts outcomes of the _intercept_ based on the _predictor_ significantlly well

### Coefficients

* The _Estimate_ of the _Intercept_ is _b~0~_, or the Y intercept, and it is the value of the _intercept_ when the _predictor_ is 0
* Below this, we have the value of _b~1~_. This represents the change in the _intercept_ with a 1 unit change in the _predictor_
* The result of the t-test here (the _t-value_) tells us whether this _predictor_ has a statistically significant effect in predicting outcomes of the _intercept_")),
            verbatimTextOutput("RegressRSET"),
            verbatimTextOutput("RegressRSE"),
            tags$h3("Scatterplot"),
            plotOutput('MyPlot'),
            downloadButton(outputId = "downloadPlotRegP",
                           label = "Download This Plot"),
            tags$h3("Regression Analysis"),
            verbatimTextOutput("ifbstp"),
            verbatimTextOutput("Regress"),
            tags$h4("95% Confidence Intervals for the Slope"),
            verbatimTextOutput("RegressCI"),
            tags$h4("95% Confidence Intervals for R Squared"),
            tags$p(tags$i("LCL is the lower CI, UCL the upper.")),
            verbatimTextOutput("CIrg"),
            verbatimTextOutput("regAPAtab"),
          ),
          
          tabPanel(
            "Checking the Accurancy of the Model",
            tags$h3("Checking Assumptions"),
            tags$div(
            	HTML("<ul>
            			 <li>Check the plot of <i>Residuals vs Fitted</i>. The points should be randomly spread out and evenly dispersed around zero. If you are seeing clear patterns in the data, it means there is some problem with your sample.</li>
            			 <li>Look at the <i>Normal Q=Q Plot</i>. <b>Standardized residuals</b> with an absolute value greater than 3 should be considered as outliers, and may result in inaccurate results if carrying out a standard regression. Also check the that the data generally follows a smooth path along the dotted diagonal line. If the data curves away from this, the samole has problems with normality.</li>
            			 <li>The plot of <i>Cook' Distance</i> shows influential cases which may have an undue effect on the analysis.</li>
            			 <li>You'll need to look into possible solutions when the data doesn't meeet assumptions. At the very least, you won't be able to make any claims beyond your sample. You might need to consider using the robust solution.</li>
            			 </ul>"),
            				 ),
            plotOutput("regResPlot"),
            tags$p("The following tests can also provide more information about your data."),
            verbatimTextOutput("residTest"),
          )
        )
        )
      )
    )
    
  )
))

server <- shinyServer(function(input, output, session) {
  # Read in the data
  
  data <- reactive({
    req(input$file1)
    
    inFile <- input$file1
    
    df <-
      read.csv(
        inFile$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote
      )
    
    
    updateSelectInput(
      session,
      inputId = 'xcol',
      label = 'X Variable',
      choices = names(df),
      selected = names(df)
    )
    updateSelectInput(
      session,
      inputId = 'ycol',
      label = 'Y Variable',
      choices = names(df),
      selected = names(df)[2]
    )
    
    return(df)
  })
  
  output$contents <- renderTable({
    data()
  })
  
  #--------------
  
  # Globals
  
  x <- reactive(data()[, (input$xcol)])
  y <- reactive(data()[, (input$ycol)])
  regressionData <- reactive(data.frame(x = x(),
                                        y = y()))
  
  xname <- reactive(input$XVar)
  
  yname <- reactive(input$YVar)
  
  rg <- reactive(lm(y ~ x, regressionData(), na.action=na.exclude))
  
  rrg <- reactive(rlm(y ~ x, regressionData()))
  
  ## bootstrapped CIs for regression coefficient
  
  bs <- reactive(function(formula, data, indices) {
    d <- data[indices,]
    fit <- lm(formula, data=d)
    return(coef(fit))
  } 
  )
  rReg <- reactive(input$rReg)
  
  ## regression Plot
  
  p <- reactive(ggplot(regressionData()) +
    aes(x, y) +
    geom_point(size = 3, alpha = .8) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      axis.title = element_text(size = 16, lineheight = 2),
      plot.title = element_text(
        size = 16,
        lineheight = 2,
        face = "bold"
      )
    )  +
    xlab(req(input$XVar)) +
    ylab(req(input$YVar)) +
    ggtitle(req(input$pTitle)) +
    scale_linetype_discrete(guide = "none"))
  
  #-------------
  
  # Plot
  
  output$MyPlot <- renderPlot({
    
    # Change based on whether robust or otherwise
    
      if (input$rReg == 1)  {
        p <- p() + geom_smooth(method = "rlm", alpha = 0.3, fill = "blue")
      }
      
      else { p <- p() + geom_smooth(method = "lm", alpha = 0.3, fill = "blue")}
      
    p
  })
  
  ## Download Plots
  output$downloadPlotRegP <- downloadHandler(
    filename = "regression-plot.pdf",
    content = function(file) { 
      if (input$rReg == 1)  {
        ggsave(file, plot = (p() + geom_smooth(method = "rlm", alpha = 0.3, fill = "blue")), device = "pdf", width = 10, height = 6, scale = 1)
      }
      else {
        ggsave(file, plot = (p() + geom_smooth(method = "lm", alpha = 0.3, fill = "blue")), device = "pdf", width = 10, height = 6, scale = 1)
      }
    }
  )
  
  # Descriptive statistics - using the psych describe function
  
  output$StatSum <- renderPrint({
    x <- data()[, c(input$xcol, input$ycol)]
    round(describe(x), digits = 3)
  })
  
  # Plot residuals 

  output$regResPlot <- renderPlot({
  	par(mfrow = c(2, 2)) # Set panel layout to 1 x 2
  	plot(rg(), which = c(1,2,4))
  	par(mfrow = c(1, 1)) # Change back
  })  
  
  ifbstp <- reactive({
    
    if (rReg() == 0)  {
      #  Regular Regression
      
      output$Regress <- renderPrint({
        summary(rg())
      })
      
      # regression CI
      
      output$RegressCI <- renderPrint({
        confint(rg())
      })
      
      # residual standard error (RSE) 
      
      output$RegressRSE <- renderPrint({
        summary(rg())$sigma
      })
      
      output$RegressRSET <- renderText({
        "This is the RSE for the standard regression model"
      })
      
      # CIs for R2 - only appropriate for non-robust regression
      
      output$CIrg <- renderPrint({
        CI.Rsqlm(rg())
        })
    }
    
    else if (rReg() == 1) {
      # Bootstrapped Regression
      output$Regress <- renderPrint({
        summary(rrg())
      })
      
      
      # regression CI
      
      output$RegressCI <- renderPrint({
        confint.default(rrg())
      })
      
      # residual standard error (RSE) 
      
      output$RegressRSE <- renderPrint({
        summary(rrg())$sigma
      })
      
      output$RegressRSET <- renderText({
        "This is the RSE for the robust regression model"
      })
      
      output$CIrg <- renderText({
        "This is not calculated for the robust regression"
      })
    }
    
    
  })  
  
  output$ifbstp <- renderPrint({ifbstp()})
  output$residTest <- renderPrint({residTest(rg(), regressionData())})
  output$residPlot <- renderPlot({residPlot(rg(), regressionData())})
  
})

shinyApp(ui, server)