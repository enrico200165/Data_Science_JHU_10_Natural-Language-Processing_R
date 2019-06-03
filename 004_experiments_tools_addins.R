require(dplyr)
require(ggplot2)
require(data.table)


Square <- function(x) {
  return(x^2)
}


debugSource("01_globals.R")

# prova ---------------------------------------------------------


cube <- function(x) {
  return(x^2)
}


prt(Square(4))
#' Title
#'
#' @return
#' @export
#'
#' @examples
estrai2 <- function() {
  prt(Square(x = 4)) # same thing
  # altro commento asas
  prt(Square(4))
  estrai_var <-   prt(Square(4))
  estrai_var
  prt(Square(4))
}
prt(Square(4))
prt(Square(4))

lapply(list(
mean,
base::mean,
get("mean"),
evalq(mean),
match.fun("mean")), lobstr::obj_addr)


tracemem(x)
x[[3]] <- 4


  f <- function(a) g(a)
  g <- function(b) h(b)
  h <- function(c) i(c)
  i <- function(d) "a" + d
  f(10)