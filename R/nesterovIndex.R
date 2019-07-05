#' @title Nesterov Index
#' @description Implementation of the Nesterov Index for fire danger estimation
#' @param t Temperature
#' @param rh Relative humidity
#' @param p Precipitation
#' @param modified Logical flag indicating wheter to use the classical index definition (default) or the modified version (see Details)
#' @return A vector of (daily) NI data
#' @details 
#'  The Nesterov Index (NI) was developed in former Soviet Union as an empirical function reflecting the relationship between observed weather
#'  conditions and fire occurrence, defined as follows:
#' 
#' \deqn{NI=\sum_{\forall p_i\leq 3mm}^i T_i(T_i-Td_i)}
#' 
#'   where \eqn{T} is midday temperature and \eqn{Td} is the dewpoint temperature at that moment, calculated from relative humidity and \eqn{T}.
#'    NI is a cumulative index, but summation is performed for those days when the daily precipitation (\eqn{p}) does not exceed 3 mm.
#'    At  \eqn{p >3 mm}, the NI value is reset to zero. Usually, the values from NI are divided into five ranges to provide an
#'     estimate of fire danger potential. Conditions with \eqn{NI<300} (regime I) are not considered hazardous. 
#'     Conditions in the ranges 300-1000, 1000-4000, 4000-10000, and  above 10000 are considered regimes with low (II), moderate (III), high (IV), 
#'    and extreme (V) level of fire hazard. 
#'      
#'  Previous studies reveal that NI may be unstable in some cases, and a modification on this index has been proposed by introducing
#'   to its values a \emph{K} scale coefficient, in the range 0-1, accounting for the amount of precipitation and previous dryness in a more detailed 
#'   way than the original NI equation. Details on the values of  \emph{K} are provided by Groisman \emph{et al.} 2007.
#'   @references 
#'   \itemize{
#' \item Groisman, P.Y., Sherstyukov, B.G., Razuvaev, V.N., Knight, R.W., Enloe, J.G., Stroumentova, N.S., Whitfield, P.H., Forland, E., Hannsen-Bauer, I., Tuomenvirta, H., Aleksandersson, H., Mescherskaya, A.V., Karl, T.R., 2007. Potential forest fire danger over Northern Eurasia: Changes during the 20th century. Global and Planetary Change 56, 371–386. 
#' \item Holsten, A., Dominic, A.R., Costa, L., Kropp, J.P., 2013. Evaluation of the performance of meteorological forest fire indices for German federal states. Forest Ecology and Management 287, 123–131. 
#' } 
#'   @author Joaquin Bedia-Jiménez
#' @keywords internal
nesterovIndex <- function(t, rh, p, modified = FALSE) {
    t <- t
    r <- rh
    p <- p
    modified <- modified
    p[which(p < 0.1)] <- 0
    if (length(t) != length(r) | length(t) != length(p)) {
        stop("Input time series of differing lengths")
    }
    vec <- rep(NA, length(t))
    if (any(is.na(t) | is.na(r) | is.na(p))) {
        unique(c(which(is.na(t)), which(is.na(r)), which(is.na(r)))) -> na
        t <- t[-na] 
        r <- r[-na] 
        p <- p[-na] 
    }
    rep(NA,length(t)) -> td
    for (i in 1:length(t)) {
        td[i] <- 237.7 * ((17.271 * t[i] / (237.7 + t[i])) + log(r[i]/100)) / (17.271 - ((17.271 * t[i] / (237.7 + t[i])) + log(r[i] / 100)))
    }
    NI <- c(0, rep(NA,(length(t) - 1)))
    for (i in 2:length(t)) {
        if (p[i] < 3) {
            NI[i] <- t[i]*(t[i] - td[i]) + NI[i - 1]
        }
        if (p[i] >= 3 | t[i] < 0) {
            NI[i] <- 0 
        }
    }
    if (isTRUE(modified)) {
        k <- rep(NA,length(p))
        z <- c(1,.8,.6,.4,.2,.1,0)
        p.int <- cut(p, breaks = c(-Inf, 0, .9, 2.9, 5.9, 14.9, 19, Inf), ordered_result = TRUE)
        for (i in 1:length(p)) {
            k[i] <- z[which(levels(p.int) == as.character(p.int[i]))]
        }
        NI <- NI * k
    }
    if (exists("na")) {
        vec[-na] <- NI
        return(vec)
    } else {
        return(NI)
    }
}
