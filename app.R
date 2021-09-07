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
                                      '"')
                         
                     ),
                     mainPanel(
                         tableOutput('contents')
                     )
                 )
        ),
      
        
        tabPanel("First Type",
                 pageWithSidebar(
                     headerPanel('My First Plot'),
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
    # added "session" because updateSelectInput requires it
    
    
    data <- reactive({ 
        req(input$file1) ## ?req #  require that the input is available
        
        inFile <- input$file1 
        
        # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
        # and                              write.csv(iris, "iris.csv")
        df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                       quote = input$quote)
        
        
        # Update inputs (you could create an observer with both updateSel...)
        # You can also constraint your choices. If you wanted select only numeric
        # variables you could set "choices = sapply(df, is.numeric)"
        # It depends on what do you want to do later on.
        
        updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                          choices = names(df), selected = names(df))
        updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                          choices = names(df), selected = names(df)[2])
        
        return(df)
    })
    
    output$contents <- renderTable({
        data()
    })
    
    output$MyPlot <- renderPlot({
        x <- data()[, (input$xcol)]
        y <- data()[, (input$ycol)]
        df <- data.frame(x = x,
                         y = y)
        ggplot(df) +
        aes(x = x, y = y) +
            geom_point(size = 3, alpha = .8) +
            geom_smooth(method = "lm", aes(linetype = "dotted"), alpha = 0.1) +
            theme_minimal() +
            theme(axis.text.x = element_text(size=14),
                  axis.text.y = element_text(size = 14),
                  axis.title = element_text(size = 16, lineheight = 2),
                  plot.title = element_text(size = 14, lineheight = 2, face="bold"),
                  strip.text.x = element_text(size = 14))
    })
    
    output$StatSum <- renderPrint({
        x <- data()[, c(input$xcol, input$ycol)]
        round(stat.desc(
            x,
            basic = FALSE,
            norm = TRUE
        ), digits = 3)
    })
})

shinyApp(ui, server)
