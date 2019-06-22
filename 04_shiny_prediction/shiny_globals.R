
require(shiny)

shstop <- function(m, rv = 99) {
  print("exiting app")
  print(m)
  if (shiny::isRunning())
    stopApp(rv)
  stop(m)
}

# --------------------------------------------------------
tstmp_fname <- function() 
  # --------------------------------------------------------
{
  # used in file names, avoid funny charss
  format(Sys.time(), "%Y%m%d_%H%M%S")
}

