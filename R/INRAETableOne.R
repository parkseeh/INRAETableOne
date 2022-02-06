#' Produces the descriptive summary table
#'
#' It produces the baseline characteristics table on the dependent variables.
#' The maximum combination of dependent variables are two, and can be formulated with
#' '+' sign such as variable1 + variable2 ~ ..
#' Also it shows the p-value according to the type of independent variable whether
#' perform \code{t.test}, \code{wilcox.test}, \code{kruskal.test}, \code{anova} if continuous,
#'  or \code{chisq.test}, \code{fisher.test} if categorical.
#'
#' @param x A formula indicating the dependent variable(s) on the left hand side, and
#' the independent variable(s) on the right hand side.
#' @param ... More arguments can be added.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(INRAETableOne)
#' INRAETableOne(Species ~ ., iris)
#' }
#'
INRAETableOne <- function(x, ...) {
    UseMethod("INRAETableOne")
}



#' @describeIn INRAETableOne S3 Method for formula
#'
#' @param formula A formula format
#' @param data A \code{data.frame}
#'
#' @export
INRAETableOne.formula <- function(formula,
                                  data,
                                  paired = FALSE,
                                  ... ) {

    # data <- acs ; formula <- Dx ~ .
    model.terms <- terms(formula, data = data)
    if (length(formula) > 2) {
        y <- as.character(formula[[2]])
    } else {
        y <- ''
    }

    x <- labels(model.terms)


}

#' Title
#'
#' @param x
#' @param y
#' @param data
#' @param paired
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
createTable <- function(x,
                        y,
                        data,
                        show.total = FALSE,
                        paired = FALSE,
                        missing = TRUE,
                        verbose = FALSE) {
    # data = acs ; y = "Dx" ; x = "sex"
    df <- data.frame(y = data[[y]], x = data[[x]])

    if (missing == TRUE) {
        df <- na.omit(df)
    }

    contingency.table <- table(df$x, df$y)
    contingency.table.with.total <- addmargins(contingency.table, 2)
    total.number <- sum(contingency.table)

    x.level <- length(unique(data[[x]]))
    variableClass <- ifelse(is.numeric(data[[x]]), 'continuous', 'categorical')

    if (variableClass == 'continuous') {
        calculated.summary.list <- tapply(data[[x]], data[[y]], calculateSummary)
        if (show.total == TRUE) {
            calculated.summary.list[[length(calculated.summary.list) + 1]] <- calculateSummary(data[[x]])
            names(calculated.summary.list)[length(calculated.summary.list)] <- 'Total'
        }
        p.value <- perform.t.test(x = df$x, y = df$y, paired = paired)
        result <- list(class = variableClass,
                       count = total.number,
                       summary.list = calculated.summary.list,
                       p = p.value)
    } else if (variableClass == 'categorical') {
        subgroup <- list()
        for (i in 1:x.level) {
            if (show.total == TRUE) {
                subgroup.element.count <- contingency.table.with.total[i, ]
                attr(subgroup.element.count, "names") <- NULL
                ratio <- apply(contingency.table.with.total, 2, function(x) x * 100 / sum(x))
                ratio.table <- list(count = subgroup.element.count,
                                    ratio = ratio[i,])
                subgroup[[i]] <- ratio.table
            } else {
                subgroup.element.count <- contingency.table[i, ]
                attr(subgroup.element.count, "names") <- NULL
                ratio <- apply(contingency.table, 2, function(x) x * 100 / sum(x))
                ratio.table <- list(count = subgroup.element.count,
                                   ratio = ratio[i,])
                subgroup[[i]] <- ratio.table
            }
        }

        names(subgroup) <- rownames(contingency.table)
        p.value <- perform.chisq.test(x = df$x, y = df$y)
        result <- list(class = variableClass,
                       count = total.number,
                       subgroup = subgroup,
                       p = p.value)
    }
    return(result)
}


createTable(x = 'sex', y =  'Dx', data= acs)











