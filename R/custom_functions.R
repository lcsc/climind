#' sum with na.rm TRUE
#' @param ... ...
#' @param na.rm na.rm
#' @return sum
sumf <- function(..., na.rm=TRUE) {
    sum(..., na.rm=na.rm)   
}

#' mean with na.rm TRUE
#' @param ... ...
#' @param na.rm na.rm
#' @return mean
meanf <- function(..., na.rm=TRUE) {
    mean(..., na.rm=na.rm)   
}

#' max with na.rm TRUE
#' @param ... ...
#' @param na.rm na.rm
#' @return max
maxf <- function (..., na.rm = TRUE){
    max(..., na.rm = na.rm)
}

#' min with na.rm TRUE
#' @param ... ...
#' @param na.rm na.rm
#' @return min
minf <- function (..., na.rm = TRUE){
    min(..., na.rm = na.rm)
}

#' which with arr.ind TRUE
#' @param x x
#' @param arr.ind na.rm
#' @param useNames na.rm
#' @return which
whichf <- function (x, arr.ind = TRUE, useNames = TRUE){
    which(x = x, arr.ind = arr.ind, useNames = useNames)
}
