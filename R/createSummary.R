#' Create the statistics summary table
#'
#' It produces the list containing the detail of summary statistics table for
#' the type of \code{y} (dependent variable) either continuous or categorical
#'
#' @param x a independent variable
#' @param y a dependent variable
#' @param data A \code{data.frame}
#' @param paired A Boolean expression (T/F) whether to perform the paired t-test.
#'  The default value is \code{FALSE}
#' @param verbose Print the log message. The default value is \code{FALSE}
#'
#' @importFrom stats na.omit model.frame formula addmargins
#'
#' @return A list containing the class of variable, total count of data, and
#' p-value. In addition, the min, max, median, sd, mean will be produced only
#' for continuous variable
#'
#' @export
createSummary <- function(x,
                          y,
                          data,
                          max.x.level = 5,
                          show.missing = TRUE,
                          paired = FALSE,
                          show.total = FALSE,
                          verbose = FALSE) {
    # show.total=F; paired=F; show.missing=T; show.detail = F; verbose=F
    if (y == "") {
        #reg.exp <- c(" ", ":")
        if (grepl(" ", x) || grepl(":",x)) {
            f <- formula(paste(y, "~", x))
            df <- model.frame(formula(f), data = data)
            colnames(df) <- c("x")
        } else {
            df <- data.frame(x = data[[x]])
        }
        contingency.table <- table(df$x)
        total.number <- sum(contingency.table)
        variable.class <- ifelse(is.numeric(data[[x]]), 'continuous', 'categorical')
        x.level <- nrow(table(df))

        if (x.level <= max.x.level) {
            variable.class <- 'categorical'
        }

        if (variable.class == 'continuous') {
            calculated.summary.list <- lapply(df, calculateSummary)
            result <- result <- list(class = variable.class,
                                     count = total.number,
                                     summary.list = calculated.summary.list,
                                     p = NA)

        } else if (variable.class == 'categorical') {
            contingency.table <- t(t(contingency.table))
            subgroup <- list()
            for (i in 1:x.level) {
                subgroup.element.count <- contingency.table[i, ]
                attr(subgroup.element.count, "names") <- NULL
                ratio <- apply(contingency.table, 2, function(x) x * 100 / sum(x))
                ratio.table <- list(count = subgroup.element.count, ratio = ratio[i,])
                subgroup[[i]] <- ratio.table
            }

            names(subgroup) <- rownames(contingency.table)
            result <- list(class = variable.class,
                           count = total.number,
                           subgroup = subgroup,
                           p = NA)
        }

    } else if (y != "") {
        if (grepl(" ", x) || grepl(":",x)) {
            f <- formula(paste(y, "~", x))
            df <- model.frame(formula(f), data = data)
            colnames(df) <- c("y", "x")
        } else {
            df <- data.frame(y = data[[y]], x = data[[x]])
        }

        if (show.missing == TRUE) { # missing shown
            if (any(is.na(df))) {
                contingency.table <- table(df$x, df$y, useNA = 'ifany')
                dimnames(contingency.table)[[1]][nrow(contingency.table)] <- 'Missing'
            } else {
                contingency.table <- table(df$x, df$y)
            }

            contingency.table.with.total <- addmargins(contingency.table, 2)
            total.number <- sum(contingency.table)

        } else {
            contingency.table <- table(df$x, df$y)
            contingency.table.with.total <- addmargins(contingency.table, 2)
            total.number <- sum(contingency.table)
        }

        x.level <- nrow(contingency.table)
        variable.class <- ifelse(is.numeric(df$x), 'continuous', 'categorical')

        if (x.level <= max.x.level) {
            variable.class <- 'categorical'
        }

        if (variable.class == 'continuous') {
            calculated.summary.list <- tapply(df$x, df$y, calculateSummary)
            if (show.total == TRUE) {
                calculated.summary.list[[length(calculated.summary.list) + 1]] <- calculateSummary(data[[x]])
                names(calculated.summary.list)[length(calculated.summary.list)] <- 'Total'
            }

            df <- na.omit(df)
            p.value <- perform.t.test(x = df$x, y = df$y, paired = paired, verbose = verbose)
            result <- list(class = variable.class,
                           count = total.number,
                           summary.list = calculated.summary.list,
                           p = p.value)

        } else if (variable.class == 'categorical') {
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
                    if (!is.null(dim(ratio))) {
                        ratio.table <- list(count = subgroup.element.count,
                                            ratio = ratio[i,])
                    } else{
                        ratio.table <- list(count = subgroup.element.count,
                                            ratio = ratio)
                    }
                    subgroup[[i]] <- ratio.table
                }
            }

            names(subgroup) <- rownames(contingency.table)
            p.value <- perform.chisq.test(x = df$x, y = df$y, paired = paired, verbose = verbose)
            result <- list(class = variable.class,
                           count = total.number,
                           subgroup = subgroup,
                           p = p.value)
        }
    }
    return(result)
}


