# resources
#  
# You can see your logs in the shinyapps.io dashboard under the Logs tab 
#in the Application view. Alternatively, you can use the rsconnect::showLogs() 
# function to show the log messages of a deployed application.
# FAILS Attempting to change the working directory. 
# Use of packages that require Windows (shinyapps.io runs on Linux)

# STORAGE
#https://docs.rstudio.com/shinyapps.io/Storage.html#Storage
# The application only has access to the data that was uploaded with the 
# application at the time of deployment. 
# Redeploying your application will reset the storage for your application 
# to only what is included in the upload bundle.
# http://shiny.rstudio.com/articles/persistent-data-storage.html#basic

# https://shiny.rstudio.com/articles/scoping.html
# objects to be visible across all sessions. For example, if you have large data 
# structures, or if you have utility functions that are not reactive 
# (ones that don’t involve the input or output objects), then you can create 
# these objects once and share them across all user sessions 
# (within the same R process), by placing them in app.R, but outside of the server function definition.

# VITAL: https://shiny.rstudio.com/articles/js-send-message.html in particular
# https://stackoverflow.com/questions/47215230/listen-to-button-events-with-shiny-oninputchange-r-shiny
#
# https://github.com/daattali/advanced-shiny/tree/master/message-javascript-to-r-force
#

rm(list = ls())

library(shiny)
library(shinyjs)

source("08_prediction.R")

# ------------------------ IDs -------------------------

EVT_KEY_PRESS <- "keypress"
EVT_KEY_DOWN  <- "keydown"
EVT_KEY_UP    <- "keyup"



LOG_TEXT <- "traceOut"
PREDICTIONS <- "predictions"
APP_STATUS  <- "appStatus"

Y_VAR <- "yVar"
PREDICTION_TYPE <- "prediction_type"
UTL_CMD_ID <- "utlCmdId"

TXT_IN_ID  <- "text_input"



###################################################################
#           NECESSARY HELP
####################################################################
# Change focus
# https://www.reddit.com/r/rstats/comments/7fmkah/moving_focus_to_next_input_in_shiny/


###########################################################
#               GLOBAL INIZIALITAIONS
###########################################################

# SOURCED files will execute code

print(paste("working dir: ",getwd()))
s <- paste(head(list.files(getwd()),5), collapse = " ")
print(paste("files in working dir:",s))

source("shiny_globals.R")

source("08_prediction.R")



#####################################################################
#                 BACKEND
#####################################################################


utlCmdMenu <- list('On Data Frame' = c("names", "nrow","ncol"),
                   'On "x" variable' = c("mean", "median"))


# smooth
regrPlotSmooth = c("onegram","bigram","trigram")

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

# ---------------------------------------------------------
performDFCommand <- function(cmdPar, var) 
# ---------------------------------------------------------
  match.fun(cmdPar)(var)



#####################################################################
#                           UI
#####################################################################

jscode <- "shinyjs.refocus = function(e_id) { console.log('refocusing'); document.getElementById(e_id).focus(); }"

# ----------------- SIDEBAR ----------------------------------------

esidebar_panel <- sidebarPanel(
  
  selectInput(Y_VAR, "Choose Y Variable:", names(mtcars), selected = 1)
  ,selectInput(UTL_CMD_ID, "Choose a Statistic:",utlCmdMenu, selected = "median")
  
  ,radioButtons(PREDICTION_TYPE, "Regression Smoothing", regrPlotSmooth ,selected = regrPlotSmooth[1])
  ,sliderInput("pointSize", "Size of points in plot:" ,min = 1, max = 8,value = 2)
  
) # sidebar panel


# ----------------- Main Panel -------------------------------

emain_panel <- mainPanel(
  
  tags$a(href="https://enrico200165.shinyapps.io/appdocumentation/"
             ,"Documentation here",style="color:blue;")
  ,hr()
  
  # javascript in HTML tag
  ,tags$script('
        $(document).on("keypress", function (e) {
          if(e.which == 32) {
            Shiny.onInputChange("tasto_prediz", [e.key, Math.random()]);
            console.log("premuto spazio, è attivo:",document.activeElement.id);
            // $("#txti").removeClass("active"); $("#yVar").addClass("active");
            // console.log("tentato di cambiare focus:",document.activeElement.id)
          }
        });')
  ,div(style="display: inline-block;vertical-align:top;"
       ,textInput(TXT_IN_ID, label = h3("Text input"), value = ""))
  ,h3("Predictions",style="color:blue")
  ,hr()
  # ,textOutput(PREDICTIONS)
  , uiOutput(PREDICTIONS)
  ,hr()
  ,h3("AppStatus",style="color:blue")
  ,textOutput(APP_STATUS)
  ,hr()
  ,h3("Debug messages",style="color:blue")
  ,textOutput(LOG_TEXT)
  )


# ---------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(text = jscode, functions = "refocus"),
  titlePanel("Next Word Prediction"),
  sidebarLayout(esidebar_panel, emain_panel, position = "right")
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

server <- function(input, output, session) {
  
  initBE()
  print("Server started")

  onevent(EVT_KEY_PRESS, "", do_test)
  onevent(EVT_KEY_UP, "textSample", do_test)
  onevent(EVT_KEY_DOWN, "textSample", do_test)
    
  observeEvent(input$tasto_prediz, {
    print(paste("Enrico server side, ricevuto evento a",Sys.time(), input[[TXT_IN_ID]][1]))
    setMsg(paste(input[[TXT_IN_ID]] ,"### should predict now ###"))
    # TODO REMOVE EXPERIMENT
    updateSelectInput(session, Y_VAR, label = NULL, choices = c("a","b"))
    
    input_text <- input[[TXT_IN_ID]]
    print(paste("should predict for:",input_text))
    predecessor_tokens <- last_n_tokens(input_text,2)
    ret <- pred_successors_aggregate(predecessor_tokens,F,  5)
    pred_html_table <- result_lines_html(ret)
    cat(pred_html_table)
    # output[[PREDICTIONS]] <- renderText({paste("should predict for:"
    #                                   ,input_text)})
    output[[PREDICTIONS]] <- renderUI({HTML(pred_html_table)})
    
  })

  
  output[[APP_STATUS]] <- renderText({
    # status <- paste("x var:",input[[X_VAR]], sep="");
    status <- if (exists("ngrams_freqs")) "ok, ngrams_freqs esiste" else "ngrams_freqs NON ESISTE"
    n <- nrow(ngrams_freqs[[3]])
    status <- paste(status," nr rows:",n)
    return(status)
  })
  
  # Utility command 
  output[[PREDICTIONS]] <- renderText({
    
    ret <- paste("dummy prdictions", PREDICTIONS, collapse = "")
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

