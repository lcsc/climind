#' @title indecisNews
#' 
#' @description Show the NEWS file of the \pkg{indecis} package.
#'
#' @details (See description)
#' 
#' @export
#' 
indecisNews <- function() {
    file <- file.path(system.file(package="indecis"), "NEWS.md")
    file.show(file)
}