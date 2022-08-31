ui <- fluidPage(

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://r-pc.net/shiny/rstudio/www/app.css")
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
            "Do you want to do a robust regression?",
            class = "instructText"
          ),
          awesomeRadio("rReg", "Robust Regression", selected = 0, c(Yes = 1,No = 0),status = "success"),
          p(
            "Once you have entered all the data for your test, click the \"Submit\" button. This will also update the output if any variables have been changed. The application", span("will not", class = "warn"), "run or update until you", span(" click \"submit\".", class = "warn"),
            class = "instructText"
          ),
          actionButton(
            inputId = "submit_data",
            label = "Submit",
            lib = "glyphicon",
            icon = icon("circle-play")
          ),
          tags$br(),
          tags$a(href = "/shiny/rstudio", "Return"),

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
            verbatimTextOutput("RegressRSET"),
            verbatimTextOutput("RegressRSE"),
            tags$h3("Scatterplot"),
            plotOutput("MyPlot"),
            tags$p("The scatterplot shows the regression line. The band surrounding it is the confidence band / interval."),
            downloadButton(outputId = "downloadPlotRegP",
                           label = "Download This Plot"),
            tags$h3("Regression Analysis"),
            verbatimTextOutput("ifbstp"),
            verbatimTextOutput("Regress"),
            tags$hr(),
            tags$br(),
            tags$h3("Confidence Intervals"),
            tags$h4("95% Confidence Intervals for the Coefficients"),
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
            			 <li>The plot of <i>Cook's Distance</i> shows influential cases which may have an undue effect on the analysis.</li>
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
)
