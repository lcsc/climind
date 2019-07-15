#' @title Keetch-Byram drought index and Mac Arthur Drought Factor
#' @description Implementation of the Keetch Byram Drought Index and Mac Arthur's Drought Factor for vector data
#' @param dates Vector of dates. This is a character string in the form \code{d/m/y} 
#' @param t Vector of daily temperature (in deg Celsius)
#' @param p Vector of daily accumulated preciptation (mm)
#' @param wrs Minimum total weekly precipitation value used to define a "rainy" week (in mm). Default to 5 mm. Ignored if  \code{start.date} is supplied.
#' @param start.date Starting date for computation. Default to \code{NULL}, meaning that the computation starts since the first input record. Otherwise,
#' the calculation starts the given date, that assumes a state of saturation of the soil (and hence KBDI=0, see Details), for instance just after snow melt.
#' This argument overrides \code{wrs}.
#' @param what What index should be returned by the function?. Current options include:
#' \itemize{
#' \item \code{kbdi}: Keetch-Byram Drought Index. This is the default.
#' \item \code{madf}: Mac Arthur's Drought Factor. Derived from the latter. See Details.
#' }
#' @return A numeric vector containing the (daily) KBDI (or MADF) time series 
#' @details The physical theory for the Keetch-Byram Drought Index (Keetch and Byram, 1968) is based on a number of assumptions: 
#' The first assumption is that soil moisture is at field capacity with a water depth equivalent to about 200 mm. 
#' The second assumption is that the rate of moisture loss in an area depends on the vegetation cover in the area, and vegetation density
#'  is a function of the mean annual rainfall. Hence, daily transpiration is approximated by an inverse exponential function of the mean annual rainfall.
#'   Finally, the evaporation rate of soil moisture with time is assumed to be an estimation of relative evapotranspiration from exponential 
#'   function of the daily maximum air temperature. Sensitivity analyses from earlier researchers have revealed that KBDI decays 
#'   exponentially with an assumed maximum soil water deficit and is sensitive to the daily maximum air 
#'   temperature (Dennison \emph{et al.}, 2013). Its values range from 0 to 800 (inches), with 800 in (203.2 mm after conversion) indicating extreme
#'   drought and zero indicating saturated soil.
#'  
#' The McArthur's Drought Factor was developed to predict the amount of fine fuel which would be available to be consumed in the flaming front of a fire.
#'  The predictive model used by McArthur was based on a combination of the Keetch Byram Drought Index, and the amount, and time since fall, of recent rain. That is the
#'  reason both indices are calculated by the same function.   
#' @note The original equations of the code presented by Keetch and Byram (1968) were later corrected for two significant typographical errors
#'  affecting the index output Alexander (1990).
## @export
#' @importFrom chron chron years
#' @references 
#' \itemize{
#' \item Keetch, J.J. and Byram, G.M. (1968) A drought index for forest fire control. USDA Forest Service.
#' \item Alexander, M.E., 1990. Computer calculation of the Keetch-Byram Drought Index - programmers beware. Fire Management Notes 51, 23–25.
#' \item Dennison, P.E., Roberts, D.A., Thorgusen, S.R., Regelbrugge, J.C., Weise, D., Christopher, L., 2003. Modeling seasonal changes in live fuel moisture and equivalent water thickness using a cumulative water balance index. Remote Sensing of the Environment 88, 442–452.
#' }
#' @author Joaquin Bedia-Jiménez
#' @keywords internal
kbdindex <- function(dates, t, p, wrs = 5, start.date = NULL, what = "kbdi") { # requires date to compute mean annual precipitation
    if (length(t) != length(p) | length(t) != length(dates)) stop("Length of input data vectors differ")
    what <- match.arg(what, choices = c("kbdi", "madf"), several.ok = FALSE)
    d <- chron(dates)
    # Data conversion to inches and fahrenheit
    t <- (t * (9 / 5)) + 32
    p <- p * .0393700787402
    wrs <- wrs * .0393700787402
    start <- start.date
    # ----------------------------------------------------------------------
    m <- matrix(data = c(t, p), ncol = 2)
    if (any(is.na(m))) {
        # warning("Missing values deleted from the input series")
        na <- unique(which(is.na(m), arr.ind = TRUE)[ ,1])
        t <- t[-na] 
        p <- p[-na] 
        d <- d[-na]
    }
    # mean annual precip (M) -----------------------------------------------
    yr <- chron::years(d)
    ind <- which(table(yr) < 360)
    ind1 <- which(is.na(match(yr, as.numeric(names(ind)))))
    M <- mean(tapply(p[ind1], yr[ind1], FUN = sum, na.rm = TRUE))
    # Week index ------------------------------------------------------------
    if (length(d) %% 7 == 0) {
        ap <- c()
    } else {
        ap <- rep(ceiling(length(d) / 7), length(d) %% 7)
    }
    wks <- c(rep(1:(length(d) / 7), each = 7), ap)
    # Detection of the first rainy week-----------------------------------------------
    if (is.null(start)) {
        wp <- tapply(p, wks, sum)
        ind <- which(wp >= wrs)[1]
        if (length(ind) == 0 | is.na(ind)) {
            stop(paste("'wrs' parameter not reached. Maximum weekly precipitation was", max(tapply(p, wks, sum)) / .0393700787402, "mm"))
        }
        ind2 <- which(wks == unique(wks[ind]))[1]
        start <- which(p[ind2:length(p)] == 0)[1] + ind2 - 1 # first day after the wet spell leading to the wrs parameter
    } else {# Fixed start date (typically just after snow melt...)
        start <- which(d == as.POSIXlt(start))
        if (length(start) == 0) {
            start = 1
        }
        # print(paste("Start date of the index set to", as.Date(d[start])))
    }
    # Data filtering, starts after heavy rain----------------------------------------------
    t <- t[start:length(t)]
    p <- p[start:length(p)] 
    d <- d[start:length(d)] 
    ## Wet spell counter --------------------------------------------------------------------------------
    w.spell <- c() # counter of consecutive rainy days
    n <- c()  # counter of days since last rain (N in Noble, 1980)
    net.rain <- rep(NA, length(p)) 
    N <- rep(NA, length(p)) 
    sat <- TRUE # Logical flag to indicate wether the 0.2 precip has been reached (K&B 1968, p.12, Col.3) # Assumption that soil is saturated to init the index
    for (i in 1:length(p)) {
        if (isTRUE(all.equal(0, p[i], tolerance = .001))) {
            n <- c(i, n)
            N[i] <- length(n)
            w.spell <- c()
            net.rain[i] <- 0
            sat <- FALSE
        }
        else {
            n <- c() # reset N counter
            N[i] <- 0 
            w.spell <- c(w.spell,i)
            if (length(w.spell) == 1) {
                if (p[i] > .2) {
                    net.rain[i] <- p[i] - .2
                    sat <- TRUE
                }
                else {
                    net.rain[i] <- 0
                    sat <- FALSE
                }
            }
            if (length(w.spell) > 1) {
                if (sat == TRUE & sum(p[w.spell]) > .2) {
                    net.rain[i] <- p[i]
                }
                if (sat == FALSE & sum(p[w.spell]) > .2) {
                    net.rain[i] <- sum(p[w.spell]) - .2
                    sat <- TRUE
                }
                if (sat == FALSE & sum(p[w.spell]) < .2) {
                    net.rain[i] <- 0
                }
            } 
        }
    }
    Q <- c(0, rep(NA, (length(t) - 1)))
    dQ <- rep(NA, (length(t))) 
    Ep <- rep(NA, (length(t))) 
    ## KBDI computing-----------------------------------------------------------------------------------------------------------------
    for (i in 2:length(t)) {
        if (net.rain[i] > 0) {
            Q[i] <- Q[i - 1] - (100 * net.rain[i])
        } else {
            Q[i] <- Q[i - 1]
        }
        # Potential evapotranspiration (K&B, 1968)
        # NOTE Alexander (1990) corregidendum on the original equation!!
        Ep[i] <- ((.968 * exp(.0486 * t[i]) - 8.30) * .001) / (1 + (10.88 * exp(-.0441 * M))) 
        dQ[i] <- (800 - Q[i]) * Ep[i]
        Q[i] <- Q[i] + dQ[i]      
    }
    Q[which(Q < 0)] <- 0
    out <- (Q / 100) / .0393700787402 # <-- This is the KBDI    
    if (what == "madf") {
        D <- (.191*(out + 104) * ((N + 1) ^ 1.5)) / ((3.52 * ((N + 1) ^ 1.5)) + (p / .0393700787402) - 1) # <-- McArthur's Drought Factor     
        out <- D
    }
    ## Add dates attribute
    # names(out) <- as.character(out)
    names(out) <- names(t)

    return(out)
}
# End
