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

# clean up to reduce risk of working thanks to desktop variables
rm(list = ls())

library(shiny)
library(shinyjs)

source("08_prediction.R")

# ------------------------ GUI IDs -------------------------

EVT_KEY_PRESS <- "keypress"
EVT_KEY_DOWN  <- "keydown"
EVT_KEY_UP    <- "keyup"


LOG_TEXT <- "traceOut"
PREDICTIONS <- "predictions"
APP_STATUS  <- "appStatus"

TXT_IN_ID  <- "text_input"

SUB_BUTTON <- "bottone"
SUB_BUTTON_OUT <- "bottone_out"
SLIDER_NR_PRED <- "nr_pred_slider"


###################################################################
#           NECESSARY HELP
####################################################################
# Change focus
# https://www.reddit.com/r/rstats/comments/7fmkah/moving_focus_to_next_input_in_shiny/


###########################################################
#               GLOBAL INIZIALITAIONS
###########################################################

# NB SOURCED files will execute code

print(paste("working dir: ",getwd()))
s <- paste(head(list.files(getwd()),5), collapse = " ")
print(paste("files in working dir:",s))

source("shiny_globals.R")

source("08_prediction.R")

dir_size <- 0L


#####################################################################
#                 BACKEND
#####################################################################


initBE <- function() {
 data("mtcars")
 values <<- reactiveValues()
 values$msg <<- "app status ok"
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
  # Input: Simple integer interval ----
  sliderInput(SLIDER_NR_PRED, "Nr. Predictions:",
              min = 0, max = 15,
              value = 8)
  
  ,hr()
  ,h5("Predictions",style="color:blue")

  ,uiOutput(PREDICTIONS)
) # sidebar panel


# ----------------- Main Panel -------------------------------

emain_panel <- mainPanel(
  
  # javascript in HTML tag
  tags$script('
        $(document).on("keypress", function (e) {
          if(e.which == 32) {
            Shiny.onInputChange("tasto_prediz", [e.key, Math.random()]);
            console.log("premuto spazio, è attivo:",document.activeElement.id);
            // $("#txti").removeClass("active"); $("#yVar").addClass("active");
            // console.log("tentato di cambiare focus:",document.activeElement.id)
          }
        });')
  ,div(style="display: inline-block;vertical-align:top;")
  ,textInput(TXT_IN_ID, label = h4("Type here"), value = "")
  ,actionButton(SUB_BUTTON, "Predict")
  ,hr()
  ,h3("AppStatus",style="color:blue")
  ,uiOutput(APP_STATUS)
  ,textOutput(hr())
  # ,tags$a(href="https://enrico200165.shinyapps.io/appdocumentation/"
  #         ,"Documentation here",style="color:blue;")
  
  ,hr()
  ,h3("Debug messages",style="color:blue")
  ,textOutput(LOG_TEXT)
  ,textOutput(SUB_BUTTON_OUT)
  )


# ---------------------------------------------------------
ui <- fluidPage(
  #  useShinyjs(),
  # extendShinyjs(text = jscode, functions = "refocus"),

  titlePanel("Next Word Prediction"),
  sidebarLayout(esidebar_panel, emain_panel, position = "right")
  
  # fluidRow(
  #   titlePanel("Next Word Prediction"),
  #   
  #   column(6,emain_panel),
  #   column(10,esidebar_panel)
  # )
  
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

initial <- T

server <- function(input, output, session) {
  
  initBE()
  print("Server started")

  onevent(EVT_KEY_PRESS, "", do_test)
  onevent(EVT_KEY_UP, "textSample", do_test)
  onevent(EVT_KEY_DOWN, "textSample", do_test)

  react_pred_button <- eventReactive(input[[SUB_BUTTON]], {
      # print(paste("Predict button at",Sys.time(), input[[TXT_IN_ID]][1]))
      input_text <- input[[TXT_IN_ID]]
      print(paste("should predict for:",input_text))
      predecessor_tokens <- last_n_tokens(input_text,2)
      ret <- pred_successors_aggregate(predecessor_tokens,F
        ,predictions_nr())
      pred_html_table <- result_lines_html(ret)
      HTML(pred_html_table)
    })
  output[[PREDICTIONS]] <- renderUI({react_pred_button()})
  
  predictions_nr <- reactive({ as.integer(input[[SLIDER_NR_PRED]])})
  
  
  # "trace" msgs
  output[[LOG_TEXT]] <- renderText({
    ret <- paste0(values$msg);
    return(ret)
  })
  
  
  dir_size <<- 0L
  sapply(list.files(SHINY_LOCAL_DATA_DIR), function(x) { 
    dir_size <<- dir_size+file.info(file.path(SHINY_LOCAL_DATA_DIR,x))$size})
  class(dir_size) <- "object_size"
  models_disk_size <- paste(format(dir_size,"Mb"))
  size_pct_max <- paste("(this is",format(unclass(dir_size)/32000000,digits = 2)
      ,"% of the 32Mb allowed by shinyapps)")
  models_RAM_size <- paste("Models RAM size:",format(object.size(ngrams_freqs),"Mb"))
  total_ram_usage <- paste("Application RAM size, Mb:",colSums(gc())[4])
  ngram1_rows <- paste("1-gram model nr rows",nrow(ngrams_freqs[[1]]))
  ngram2_rows <- paste("2-gram model nr rows",nrow(ngrams_freqs[[2]]))
  ngram3_rows <- paste("3- gram model nr rows",nrow(ngrams_freqs[[3]]))
  status <- ""
  # status <- paste(status,"Size of models on disk:",models_disk_size,br)
  # status <- paste(status,size_pct_max,br)
  # status <- paste(status,models_RAM_size,br)
  status <- paste(status,total_ram_usage, br)
  # status <- paste(status, ngram1_rows, br)
  # status <- paste(status, ngram2_rows, br)
  # status <- paste(status, ngram3_rows, br)
  output[[APP_STATUS]] <- renderUI({ return(HTML(status))})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

