#' @title Finnish Forest Fire Danger Index 
#' @description Implementation of the FFFDI for vector data
#' @param pr A vector of daily precipitation (in mm)
#' @param pet A vector of daily (potential) evapotranspiration data (in mm).
#' @param Wvol.init Initialization value for volumetric moisture, in the range 0.1-0.5. Default to 0.5 (very wet soil), but see Details.
#' @param z reference surface layer thickness (mm). Default to 60.
#' @references Vajda, A., Venalainen, A., Suomi, I., Junila, P. and Makela, H., 2014. Assessment of forest 
#' fire danger in a boreal forest environment: description and evaluation of the operational 
#' system applied in Finland. Meteorol. Appl., 21: 879-887, DOI: 10.1002/met.1425
#' @return A numeric vector containing FFFDI time series
#' @details 
#' \strong{Volumetric moisture}
#' The default is 0.5, indicating that the soil is very wet and near field capacity. This is so, assuming that the
#'  index is started in early spring. This value is applied to all locations as a spatially constant initialization value.
#'  However, Vajda \emph{et al.} (2014, Table 1) provide reference values for different soil moisture conditions. This value ranges
#'  from 0.1 (very dry) to 0.5 (very wet).
#' @author Joaquin Bedia-Jiménez
## @export
#' @keywords internal
fffdi <- function(pr, pet, Wvol.init = 0.5, z = 60) {
    stopifnot(length(pr) == length(pet))
    if (any(is.na(pr) | is.na(pet))) {
        unique(c(which(is.na(pr)), which(is.na(pet)))) -> na
        pr <- pr[-na] 
        pet <- pet[-na] 
    }
    ntimes <- length(pr)
    Wvol <- Wvol.init
    Wt <- Wvol * z 
    fffdi <- pet
    for (i in 1:ntimes) {
        dw <-  (-pet[i] * 0.757) / (1 + exp(2.74 - 16.67 * (Wvol - 0.1))) + 5.612 * (1 - exp(-pr[i] / 5.612))
        Wt <- Wt + dw
        Wvol <- Wt/z
        Wvol[which(Wvol > 0.5)] <- 0.5
        Wvol[which(Wvol < 0.1)] <- 0.1
        fffdi[i] <- 8.76 - 30.879 * Wvol + 30.712 * Wvol^2
    }
    return(fffdi)
}
