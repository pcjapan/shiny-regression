## File upload functionality based on https://stackoverflow.com/a/36955396

# Required packages

library(shiny)
library(shinyWidgets)
library(pastecs)
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

      pre#StatSum { width: 400px; }

      pre#Regress, pre#RegressCI, pre#BsCI { width: 600px; }

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
      
      mainPanel(
        tags$h3("Descriptive Statistics"),
        verbatimTextOutput("StatSum"),
        tags$br(),
        tags$h3("Regression Analysis"),
        verbatimTextOutput("ifbstp"),
        verbatimTextOutput("Regress"),
        verbatimTextOutput("RegressCI"),
        tags$br(),
        tags$h3("Scatterplot"),
        plotOutput('MyPlot')
        
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
  
  
  rg <- reactive(lm(y ~ x, regressionData()))
  
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
          p + geom_smooth(method = "rlm", alpha = 0.1)
      }
      
      else { p <- p + geom_smooth(method = "lm", alpha = 0.1)}
      }
    p
  })
  
  # Descriptive statistics
  
  output$StatSum <- renderPrint({
    x <- data()[, c(input$xcol, input$ycol)]
    round(stat.desc(x,
                    basic = TRUE,
                    norm = TRUE), digits = 3)
  })
  

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
    }
    
   else if (rReg() == 1) {
      
     output$Regress <- renderPrint({
       summary(rrg())
     })
   
      
# regression CI
     
     output$RegressCI <- renderPrint({
       confint.default(rrg())
     })
   }

  })  
  
  output$ifbstp <- renderPrint({ifbstp()})
  
})

shinyApp(ui, server)
