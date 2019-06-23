
require(shiny)

MAX_PREDECESSORS <- 2 # parametrize to eventually add 4grams

shstop <- function(m, rv = 99) {
  print("exiting app")
  print(m)
  if (shiny::isRunning())
    stopApp(rv)
  stop(m)
}

