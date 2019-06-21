# resources 
# VITAL: https://shiny.rstudio.com/articles/js-send-message.html in particular
# https://stackoverflow.com/questions/47215230/listen-to-button-events-with-shiny-oninputchange-r-shiny
#
# https://github.com/daattali/advanced-shiny/tree/master/message-javascript-to-r-force
#

library(shiny)
library(shinyjs)
require(ggplot2)

# ------------------------ IDs -------------------------

EVT_KEY_PRESS <- "keypress"
EVT_KEY_DOWN  <- "keydown"
EVT_KEY_UP    <- "keyup"



LOG_TEXT <- "traceOut"
CMD_CHOSEN <- "utlCmdChosen"
GLOBAL_STATUS  <- "globalStatus"
X_VAR <- "xVar"
Y_VAR <- "yVar"
REGR_LINE <- "regrLine"
UTL_CMD_ID <- "utlCmdId"

TXT_IN_ID  <- "txti"


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


initBE <- function() {
 data("mtcars")
 values <<- reactiveValues()
 values$msg <<- "reactive value message"
 
 curdf <<- mtcars
}

setMsg <- function(m) { values$msg <<- m; }


performVariableCommand <- function(cmdPar, var) {

  setMsg(paste("function:",cmdPar,"on",var))
  match.fun(cmdPar)(mtcars[[var]])
}


performDFCommand <- function(cmdPar, var) match.fun(cmdPar)(mtcars)


#####################################################################
#                           UI
#####################################################################

# ----------------- SIDEBAR ----------------------------------------

esidebar_panel <-       sidebarPanel(
  
  selectInput(Y_VAR, "Choose Y Variable:", names(mtcars), selected = 1)
  
  ,selectInput(X_VAR, "Choose X Variable:", names(mtcars) 
               ,selected = names(mtcars)[length(names(mtcars))])
  
  ,radioButtons(REGR_LINE, "Regression Smoothing", plotParamConsts$regrPlotSmooth ,selected = "Loess")
  ,sliderInput("pointSize", "Size of points in plot:" ,min = 1, max = 8,value = 2)
  
  ,hr()
  ,selectInput(UTL_CMD_ID, "Choose a Statistic:",utlCmdMenu, selected = "median")
) # sidebar panel

# ----------------- Main Panel -------------------------------

emain_panel <- mainPanel(
  p(strong("Documentation")
     ,tags$a(href="https://enrico200165.shinyapps.io/appdocumentation/"
             ,"here",style="color:blue;"),style="color:red"
  )
  ,hr()
  
  # javascript in HTML tag
  ,tags$script('
        $(document).on("keypress", function (e) {
          if(e.which == 32) {
            Shiny.onInputChange("tasto", [e.key, Math.random()]);
            console.log("rilevato keypress significativo")
            // alert("key_press " + e.which)
          }
        });')
  ,div(style="display: inline-block;vertical-align:top;"
       ,textInput(TXT_IN_ID, label = h3("Text input"), value = ""))
  ,div(style="display: inline-block;vertical-align:top;"
       #,textOutput(LOG_TEXT)
       )
  
  ,h3("Stat functions output",style="color:blue")
  ,textOutput("utlCmdOut")
  ,textOutput(CMD_CHOSEN)
  ,hr()
  ,h3("AppStatus",style="color:blue")
  ,textOutput(GLOBAL_STATUS)
  ,hr()
  ,h3("Debug messages",style="color:blue")
  ,textOutput(LOG_TEXT)
  )


# ---------------------------------------------------------
ui <- fluidPage(
    
    # Application title
    titlePanel("Next Word Prediction"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(esidebar_panel, emain_panel, position = "right"
    )
  )


#####################################################################
#                    EVENT HANDLERS
#####################################################################
do_test <- function() { 
  print(rep("#",50))
  setMsg(paste(input[[TXT_IN_ID]]))
  print(paste("do_test()",input[[TXT_IN_ID]]))
}

#########################################################################
#                                 SERVER
########################################################################

server <- function(input, output) {
  
  initBE()
  print("Server started")

  onevent(EVT_KEY_PRESS, "", do_test)
  onevent(EVT_KEY_UP, "textSample", do_test)
  onevent(EVT_KEY_DOWN, "textSample", do_test)
    
  observeEvent(input$tasto, {
    setMsg(paste(input[[TXT_IN_ID]] ,"### should predict now ###"))
    print(paste("Enrico server side, ricevuto evento a",Sys.time(), input[[TXT_IN_ID]][1]))
  })

  
  output[[GLOBAL_STATUS]] <- renderText({
    status <- paste("x var:",input[[X_VAR]], sep="");
    return(status)
  })
  
  
  # Utility command 
  output[[CMD_CHOSEN]] <- renderText({
    
    ret <- paste("dummy", CMD_CHOSEN, collapse = "")
    return(ret)
  })
  
  # "trace" msgs
  output[[LOG_TEXT]] <- renderText({
     ret <- paste0("enrico trace",values$msg);
   return(ret)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

