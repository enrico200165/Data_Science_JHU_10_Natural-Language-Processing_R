# resources 
# VITAL: https://shiny.rstudio.com/articles/js-send-message.html in particular
# https://stackoverflow.com/questions/47215230/listen-to-button-events-with-shiny-oninputchange-r-shiny
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
require(ggplot2)


# ------------------------ IDs -------------------------

EVT_KEY_PRESS <- "keypress"
EVT_KEY_DOWN <- "keydown"
EVT_KEY_UP <- "keyup"

LOG_TEXT <- "traceOut"

TXT_IN_ID <- "txti"


#####################################################################
#                 BACKEND
#####################################################################


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


# initBE <- function() {
#   data("mtcars")
#   values <<- reactiveValues()
#   values$msg <<- "reactive value message"
#   
#   curdf <<- mtcars
# }

setMsg <- function(m) { values$msg <<- m; }

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

  setMsg(paste("function:",cmdPar,"on",var))
  match.fun(cmdPar)(mtcars[[var]])
}


performDFCommand <- function(cmdPar , var) match.fun(cmdPar)(mtcars)



#####################################################################
#                           UI
#####################################################################
ui <- fluidPage(
    
    # Application title
    titlePanel("Stats and Plot on mtcars"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        
        selectInput("yVar", "Choose Y Variable:", names(mtcars), selected = 1)
        
        ,selectInput("xVar", "Choose X Variable:", names(mtcars) 
                     ,selected = names(mtcars)[length(names(mtcars))])
        
        ,radioButtons("regrLine", "Regression Smoothing", plotParamConsts$regrPlotSmooth ,selected = "Loess")
        ,sliderInput("pointSize", "Size of points in plot:" ,min = 1, max = 8,value = 2)
        
        ,hr()
        ,selectInput("utlCmdId", "Choose a Statistic:",utlCmdMenu, selected = "median")
      ) # sidebar panel

      
      
      # Show a plot of the generated distribution
      ,mainPanel(
        h3(strong("Documentation")
           ,tags$a(href="https://enrico200165.shinyapps.io/appdocumentation/"
                   ,"here",style="color:blue;"),style="color:red"
        )
        ,hr()
        
        ,tags$script('
        $(document).on("keypress", function (e) {
          //if(e.which == " ") {
            Shiny.onInputChange("tasto", e.which);
            alert("key_press" + e.which)
          //}
        });')
        ,div(style="display: inline-block;vertical-align:top;"
            ,textInput(TXT_IN_ID, label = h3("Text input"), value = ""))
        ,div(style="display: inline-block;vertical-align:top;",
            textOutput(LOG_TEXT))
      
        
        ,plotOutput("regrPlot")
        ,h3("Stat functions output",style="color:blue")
        ,textOutput("utlCmdOut")
        ,textOutput("utlCmdChosen")
        ,hr()
        ,h3("AppStatus",style="color:blue")
        ,textOutput("globalStatus")
        ,hr()
        ,h3("Debug messages",style="color:blue")

      )
    )
  )


#########################################################################
#                                 SERVER
########################################################################
server <- function(input, output) {
  
  print("Server started")
  
  # ---- EVENTS
  do_test <- function() { 
    print(rep("#",50))
    output$globalStatus <<- "premuto"; 
    output$globalStatus <- "premuto"; 
    output$traceOut <- "premuto" 
  }
  observeEvent(input$tasto, {
    #output$globalStatus <- "premuto"
    print("YEAH")
  })
  onevent(EVT_KEY_PRESS, "textSample",do_test )
  #onevent("mouseleave", "disappear", show("text"))
  onevent(EVT_KEY_UP, "textSample", do_test)
  onevent(EVT_KEY_DOWN, "textSample", do_test)
  
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

