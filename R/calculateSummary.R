#' Calculate the summary statistics for each
#'
#' @param x A numeric vector
#' @importFrom stats sd median
#' @return A list containing the values of mean, standard deviation, median,
#' minimum, and maximum.
#' @export
calculateSummary <- function(x) {
    if (all(is.na(x))) {
        result <- list(NA, NA, NA, NA, NA)
    } else {
        funs <- c(mean, sd, median, min, max)
        result <- lapply(funs, function(f) f(x, na.rm = TRUE))
    }
    return(result)
}

