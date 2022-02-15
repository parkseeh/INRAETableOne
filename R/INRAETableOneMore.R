#' Generate the descriptive statistics table
#'
#' It produces a nicely formatted table of descriptive statistics for numeric or categorical variables
#'
#' @details
#' The dependent and independent variable should be indicated as \code{formula} format.
#' The formula may contain a dot (".") to refer to "all variables in \code{data}
#' other than those that appear elsewhere in the formula".
#' The maximum combination of dependent variables are two, and can be formulated with
#' '+' sign such as 'dependent1' + 'dependent2' ~ 'independent1' + 'independent2' + ....
#'
#' @param x A formula indicating the dependent variable(s) on the left hand side, and
#' the independent variable(s) on the right hand side of '~'.
#' @param formula
#' @param max.x.level A maximum level of x level. If the level of x level is less than
#' max.x.level, then it consider the column as categorical.  The default value is 5
#' @param show.missing A Boolean expression (T/F) whether to show the missing values on the
#' table.
#' @param paired A Boolean expression (T/F) whether to perform a paired test in order to
#' calculate p-value.  However, the numbers of sample size at before and after
#' needs to be equal, otherwise, it would perform just two sample t-test.
#' @param show.total A Boolean expression (T/F) whether to show a total group of value.
#' @param show.detail A Boolean expression (T/F) whether to display the other extra
#' summary statistics value such as min, max, and median.
#' @param verbose A Boolean expression (T/F) whether to print the log messages for
#' every step.
#' @param ... further arguments to be passed to or from methods.
#'
#' @return An object of class "cbind.INRAETableOne".
INRAETableOneMore <- function(formula,
                              data,
                              max.x.level = 5,
                              show.missing = TRUE,
                              paired = FALSE,
                              show.total = FALSE,
                              show.detail = FALSE,
                              verbose = FALSE) {

    model.terms <- terms(formula, data = data)
    y <- as.character(formula[[2]])
    y <- unlist(strsplit(y,"+",fixed=TRUE))
    if (length(y) > 1) {
        y <- y[-1]
    }
    if (length(y) > 2) {
        cat("Only two variables are permitted as grouping vairables\n")
        return(invisible())
    }
    y1 <- y[1]
    y2 <- y[2]
    uniquey <- unique(data[[y1]])
    ycount <- length(uniquey)
    out <- list()

    for (i in 1:ycount) {
        sub.data <- data[data[[y1]] == uniquey[i],]

        y.table <- table(sub.data[[y2]])
        if (show.total == TRUE) {
            y.table <- addmargins(y.table)
            names(y.table)[length(y.table)] <- "Total"
        }

        result.list <- list(y = y2,
                            names = names(y.table),
                            count = unname(y.table),
                            length = length(y.table),
                            show.detail = show.detail)

        x.variables <- labels(model.terms)

        for (x.variable in x.variables) {
            if ((length(unique(data[[x.variable]]))) <= max.x.level & !is.factor(sub.data[[x.variable]])) {
                data[[x.variable]] <- factor(data[[x.variable]])
                sub.data <- data[data[[y1]] == uniquey[i],]
            }

            summary.result <- createSummary(x = x.variable,
                                            y = y2,
                                            data = sub.data,
                                            max.x.level = max.x.level,
                                            show.total = show.total,
                                            paired = paired,
                                            show.missing = show.missing,
                                            verbose = verbose)

            result.list[[x.variable]] <- summary.result
        }
        result <- makeTableOne(result.list, digits = 1)
        class(result) <- "INRAETableOne"
        out[[i]] <- result
    }

    if (ycount == 2) {
        final.out <- cbind(out[[1]], out[[2]], caption = uniquey, y = y)
    } else if (ycount == 3) {
        final.out <- cbind(out[[1]], out[[2]], out[[3]], caption = uniquey, y = y)
    } else if (ycount == 4) {
        final.out <- cbind(out[[1]], out[[2]], out[[3]], out[[4]], caption = uniquey, y = y)
    } else if (ycount == 5) {
        final.out <- cbind(out[[1]], out[[2]], out[[3]], out[[4]], out[[5]], caption = uniquey, y = y)
    } else if (ycount == 6) {
        final.out <- cbind(out[[1]], out[[2]], out[[3]], out[[4]], out[[5]], out[[6]], caption = uniquey, y = y)
    } else {
        cat("Maximum y level is six")
        return(invisible())
    }
    return(final.out)
}

