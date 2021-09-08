## File upload functionality based on https://stackoverflow.com/a/36955396

library(shiny)
library(pastecs)
library(ggplot2)




ui <- shinyUI(fluidPage(
    titlePanel("Column Plot"),
    tabsetPanel(
        tabPanel("Upload File",
                 titlePanel("Uploading Files"),
                 sidebarLayout(
                     sidebarPanel(
                         fileInput('file1', 'Choose CSV File',
                                   accept=c('text/csv', 
                                            'text/comma-separated-values,text/plain', 
                                            '.csv')),
                         
                         # added interface for uploading data from
                         # http://shiny.rstudio.com/gallery/file-upload.html
                         tags$br(),
                         checkboxInput('header', 'Header', TRUE),
                         radioButtons('sep', 'Separator',
                                      c(Comma=',',
                                        Semicolon=';',
                                        Tab='\t'),
                                      ','),
                         radioButtons('quote', 'Quote',
                                      c(None='',
                                        'Double Quote'='"',
                                        'Single Quote'="'"),
                                      '"'),
                         # Input boxes for graph labels and title
                         textInput("XVar", "X-Variable Label", value = "X", width = "400px", placeholder = "Enter a label for the X Axis"),
                         textInput("YVar", "Y-Variable Label", value = "Y", width = "400px", placeholder = "Enter a label for the Y Axis"),
                         textInput("pTitle", "Plot Title", value = "My Scatterplot", width = "400px", placeholder = "Enter a descriptive title")
                     ),
                     mainPanel(
                         tableOutput('contents')
                     )
                 )
        ),
      
        
        tabPanel("Output",
                 pageWithSidebar(
                     headerPanel('Analysis Output'),
                     sidebarPanel(
                         
                         # "Empty inputs" - they will be updated after the data is uploaded
                         selectInput('xcol', 'X Variable', ""),
                         selectInput('ycol', 'Y Variable', "", selected = "")
                         
                     ),
                     mainPanel(
                         plotOutput('MyPlot'),
                         verbatimTextOutput('StatSum')
                     )
                 )
        )
        
    )
)
)

server <- shinyServer(function(input, output, session) {

    
data <- reactive({ 
        req(input$file1)
        
        inFile <- input$file1 
        
        df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                       quote = input$quote)
        
        
        
        updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                          choices = names(df), selected = names(df))
        updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                          choices = names(df), selected = names(df)[2])
        
        return(df)
    })
    
    output$contents <- renderTable({
        data()
    })
    
    output$valueX <- renderText({ input$XVar })
    
    output$MyPlot <- renderPlot({
        x <- data()[, (input$xcol)]
        y <- data()[, (input$ycol)]
        df2 <- data.frame(x = x,
                         y = y)
        ggplot(df2) +
        aes(x = x, y = y) +
            geom_point(size = 3, alpha = .8) +
            geom_smooth(method = "lm", aes(linetype = "dotted"), alpha = 0.1) +
            theme_minimal() +
            theme(axis.text.x = element_text(size=14),
                  axis.text.y = element_text(size = 14),
                  axis.title = element_text(size = 16, lineheight = 2),
                  plot.title = element_text(size = 14, lineheight = 2, face="bold"),
                  strip.text.x = element_text(size = 14)) +
          xlab(req(input$XVar)) +
          ylab(req(input$YVar)) +
          ggtitle(req(input$pTitle))
        
    })
    
    output$StatSum <- renderPrint({
        x <- data()[, c(input$xcol, input$ycol)]
        round(stat.desc(
            x,
            basic = TRUE,
            norm = TRUE
        ), digits = 3)
    })
})

shinyApp(ui, server)
