
#' @title Mac Arthur Forest Fire Danger Index
#' @description Implementation of the Mac Arthur's Forest Fire Danger Index (FFDI)
#' @param madf Mac Arthur's Drought Index. This is the output of the \code{\link{kbdindex}} function, using the option \code{what = "madf"}.
#' @param t t
#' @param h Vector of relative humidity data (in percentage)
#' @param w Vector of wind velocity records (in km/h)
#' @return A vector of (daily) FFDI data
#' @references McArthur, A.G. (1973) Forest Fire Danger Meter Mk.5. Commonwealth of Australian Forestry and Timber Bureau.
#' @seealso kbdindex
#' @author Joaquin Bedia-Jiménez
## @export
#' @keywords internal
ffdiIndex <- function(madf, t, h, w) {
    m <- matrix(data = c(madf, t, h, w), ncol = 2)
    if (any(is.na(m))) {
        # warning("Missing values deleted from the input series")
        na <- unique(which(is.na(m), arr.ind = TRUE)[ ,1])
        madf <- madf[-na]
        t <- t[-na] 
        h <- h[-na] 
        w <- w[-na]
    }
    FDI <- 2 * exp(-.45 + .987 * log(madf) - .0345 * h + .0338 * ((t - 32)*(5 / 9)) + .0234 * w)  
    rownames(FDI) <- rownames(madf)
    return(FDI)
}

