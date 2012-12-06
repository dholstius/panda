#' panda
#' 
#' @name panda
#' @docType package
NULL

is.installed <- function(x) {
    return(x %in% .packages(all.available=TRUE))
}

if (!is.installed('holstius')) {
    message("Installing package 'holstius' from GitHub ...")
    install_github('holstius', 'holstius')
}

if (!is.installed('shiny')) {
    message("Installing package 'shiny' from RStudio ...")
    install.packages('shiny', repos='http://rstudio.org/_packages')
}

