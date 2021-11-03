## File upload functionality based on https://stackoverflow.com/a/36955396

# Required packages

library(shiny)
library(shinyWidgets)
library(psych)
library(ggplot2)
library(ggpmisc)
library(boot)
library(MASS)

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

      pre#Regress, pre#RegressCI, pre#BsCI, pre#RegressRSET, pre#RegressRSE { width: 600px; }
      
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
      "

    )
  ),

titlePanel("Regression Analysis"),
tabsetPanel(
  tabPanel(
    "Upload File",
    titlePanel("Uploading Files"),
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
        tags$h3("Regression Analysis"),
        verbatimTextOutput("ifbstp"),
        verbatimTextOutput("Regress"),
        tags$h4("95% Confidence Intervals for the intercept"),
        verbatimTextOutput("RegressCI"),
        tags$br(),
        tags$h3("Scatterplot"),
        plotOutput('MyPlot')
      ),
      tabPanel(
        "Residuals",
        tags$h3("Residual Standard Error"),
        tags$p("The residual standard error (RSE) is a way to measure the standard deviation of the residuals in a regression model. Use this to help decide which regresion model to report."),
        tags$ul(tags$li("The lower the value for RSE, the more closely a model is able to fit the data."),
                tags$li("Compare the RSE for the robust regression model against the standard model. The lower of the two points to a better fit for that particular model."),
        ),
        verbatimTextOutput("RegressRSET"),
        verbatimTextOutput("RegressRSE"),
        tags$h3("Graph of Standardized Residuals"),
        tags$p("Standardized residuals with an absolute value greater than 3 should be considered as outliers.",
        tags$br(),
        "These may result in errors in the analysis if using a stahdard (OLS) regression.",
        tags$br(),
        "If they present, a robust regression will likely give better results."),
        plotOutput('stdrPlot'),
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
  #-------------
  
   # Plot
  
  output$MyPlot <- renderPlot({
    p <- ggplot(regressionData()) +
      aes(x = x, y = y) +
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
      scale_linetype_discrete(guide = "none")
    # Change based on whether robust or otherwise
    {
      if (input$rReg == 1)  {
        p <-
          p + geom_smooth(method = "rlm", alpha = 0.3, fill = "blue")
      }
      
      else { p <- p + geom_smooth(method = "lm", alpha = 0.3, fill = "blue")}
      }
    p
  })
  
  # Descriptive statistics - using the psych describe function
  
  output$StatSum <- renderPrint({
    x <- data()[, c(input$xcol, input$ycol)]
    round(describe(x), digits = 3)
  })
  
  #plot standardized residuals
  
 output$stdrPlot <- renderPlot({
   
   rg.stdres =  rstandard(rg())
   plot(y(), rg.stdres, 
       ylab="", 
       xlab="", 
       main="") 
  abline(0, 0)})
  

  ifbstp <- reactive({
    
    if (rReg() == 0)  {
      #  Regression
      
      output$Regress <- renderPrint({
        summary(rg())
      })
      
      # regression CI
      
      output$RegressCI <- renderPrint({
        confint(rg())
      })
      
      # residual standard error (RSE) 
      
      output$RegressRSE <- renderText({
       summary(rg())$sigma
      })
      
      output$RegressRSET <- renderText({
        "This is the RSE for the standard regression (OLS) model"
      })
    }
    
   else if (rReg() == 1) {
      
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
   }

  })  
  
  output$ifbstp <- renderPrint({ifbstp()})
  
})

shinyApp(ui, server)
