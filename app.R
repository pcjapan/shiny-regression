## File upload functionality based on https://stackoverflow.com/a/36955396

# Required packages

library(shiny)
library(shinyWidgets)
library(psych)
library(ggplot2)
library(ggpmisc)
library(boot)
library(MASS)
library(psychometric)
library(fBasics)
library(lmtest)
library(dabestr)

source("ui.R", local = TRUE)

server <- function(input, output, session) {
	
	
	source("functions.R", local = TRUE)
	
	
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
  
  rg <- reactive(lm(y ~ x, regressionData(), na.action = na.exclude))
  
  rrg <- reactive(rlm(y ~ x, regressionData()))
  
  ## bootstrapped CIs for regression coefficient
  
  bs <- reactive(function(formula, data, indices) {
    d <- data[indices,]
    fit <- lm(formula, data=d)
    return(coef(fit))
  } 
  )
  rReg <- reactive(input$rReg)
  
  observeEvent(eventExpr = input[["submit_data"]],
  						 handlerExpr = {
  
  
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
          lineheight = 2,face = "bold"
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
  #output$residPlot <- renderPlot({residPlot(rg(), regressionData())})
  
})
  	
}

shinyApp(ui, server)