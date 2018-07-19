#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


#####################################################################
#                 BACKEND
#####################################################################
curdf <- data.frame(x=1:10)
values <- "dummy"


utlCmdMenu <- list('On Data Frame' = c("names", "nrow","ncol"),
                   'On "x" variable' = c("mean", "median"))


# smooth
plotParamConsts <- list(
  regrPlotSmooth = c("None","Lm","Loess")
  
)
setClass("plotParsClass",
         slots = c(regrSmoot = "character"
                   , pointSize = "numeric")
)
plotPars = new("plotParsClass")


initBE <- function() {
  data("mtcars")
  values <<- reactiveValues()
  values$msg <<- "reactive value message"
  
  curdf <<- mtcars
}

setMsg <- function(m) { 
  values$msg <<- m; 
}

plotRegression <- function(xpar, ypar,dfra,plotPar) {
  p <- ggplot(data = dfra, aes(x=dfra[[xpar]],y=dfra[[ypar]]))
  p <- p + geom_point(size=plotPar@pointSize)
  p <- p + xlab(xpar) + ylab(ypar)
  if (plotPar@regrSmoot == "Lm")
    p <- p + geom_smooth(method='lm',formula=y~x)
  if (plotPar@regrSmoot == "Loess")
    p <- p + geom_smooth(method='loess',formula=y~x)
  
  p
}


performVariableCommand <- function(cmdPar, var) {
  cmd <- match.fun(cmdPar)
  setMsg(paste("function:",cmdPar,"on",var))
  return(cmd(mtcars[[var]]))
}


performDFCommand <- function(cmdPar, var) {
  cmd <- match.fun(cmdPar)
  ret <- cmd(mtcars)
  ret
}



#####################################################################
#                           UI
#####################################################################
ui <- fluidPage(
    
    # Application title
    titlePanel("Stats and Plot on mtcars"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        
        selectInput("yVar", "Choose Y Variable:",
                    names(mtcars), selected = 1
        )
        
        ,selectInput("xVar", "Choose X Variable:",
                     names(mtcars)
                     ,selected = names(mtcars)[length(names(mtcars))]
        )
        
        ,radioButtons("regrLine", "Regression Smoothing",
                      plotParamConsts$regrPlotSmooth,
                      selected = "Loess"
        )
        ,sliderInput("pointSize",
                     "Size of points in plot:"
                     ,min = 1, max = 8,value = 2)
        
        ,hr()
        ,selectInput("utlCmdId", "Choose a Statistic:",utlCmdMenu,
                     selected = "median")
      ) # sidebar panel
      # Show a plot of the generated distribution
      ,mainPanel(
        h3(strong("Documentation")
           ,tags$a(href="https://enrico200165.shinyapps.io/appdocumentation/"
                   ,"here",style="color:blue;"),style="color:red"
        )
        ,hr()
        ,plotOutput("regrPlot")
        ,h3("Stat functions output",style="color:blue")
        ,textOutput("utlCmdOut")
        ,textOutput("utlCmdChosen")
        ,hr()
        ,h3("AppStatus",style="color:blue")
        ,textOutput("globalStatus")
        ,hr()
        ,h3("Debug messages",style="color:blue")
        ,textOutput("traceOut")
      )
    )
  )


#########################################################################
#                                 SERVER
########################################################################
server <- function(input, output) {
  
  output$globalStatus <- renderText({
    status <- paste("y var:",input$yVar
                    ," ","x var:",input$xVar
                    ,sep="");
    return(status)
  })
  
  
  # Utility command 
  output$utlCmdChosen <- renderText({
    
    ret <- paste("function: \"",input$utlCmdId,"\"",sep="")
    
    if (input$utlCmdId %in% unlist(utlCmdMenu['On Data Frame'])) {
      cmd_out <- performDFCommand(input$utlCmdId,input$xVar)
    } else if(input$utlCmdId %in% unlist(utlCmdMenu['On "x" variable'])) {
      ret <- paste(ret,"on Variable:\"",input$xVar,"\"",sep = "")
      cmd_out <- performVariableCommand(input$utlCmdId,input$xVar)
    } else {
      cmd_out <- "unable To execute function"    
    }
    cmd_out <- paste(cmd_out,sep = " ", collapse = " ")
    ret <- paste(ret,"Raw Output: ",cmd_out, sep = " ", collapse = " ")
    return(ret)
  })
  
  output$regrPlot <- renderPlot({
    plotPars@regrSmoot <- input$regrLine
    plotPars@pointSize <- input$pointSize
    plotRegression(input$xVar,input$yVar,mtcars,
                   plotPars);
  })
  
  
  # "trace" msgs
  output$traceOut <- renderText({
    paste(values$msg)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

