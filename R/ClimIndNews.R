#' @title ClimIndNews
#' 
#' @description Show the NEWS file of the \pkg{ClimInd} package.
#'
#' @details (See description)
#' 
#' @export
#' 
ClimIndNews <- function() {
    file <- file.path(system.file(package="ClimInd"), "NEWS.md")
    file.show(file)
}