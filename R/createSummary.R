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
                          show.missing = FALSE,
                          paired = FALSE,
                          show.total = FALSE,
                          verbose = FALSE,
                          origData) {

    if (y == "") {
        if (grepl("`", x)) {
            x <- gsub("`", "", x)
        }

        df <- data.frame(x = data[[x]])

        exist.missing <- FALSE

        if (show.missing == TRUE) { # missing shown
            if (any(is.na(df))) {
                contingency.table <- table(df$x, useNA = 'ifany')
                dimnames(contingency.table)[[1]][nrow(contingency.table)] <- 'Missing'
                exist.missing <- TRUE
            } else {
                contingency.table <- table(df$x)
                exist.missing <- FALSE
            }

            contingency.table.with.total <- addmargins(contingency.table, 1)
            total.number <- sum(contingency.table)

        } else {
            contingency.table <- table(df$x)
            contingency.table.with.total <- addmargins(contingency.table, 1)
            total.number <- sum(contingency.table)
        }

        variable.class <- ifelse(is.numeric(df$x), 'continuous', 'categorical')

        if (show.missing == TRUE && exist.missing == TRUE) {
            x.level <- length(unique(origData[[x]]))
            if (x.level - 1 <= max.x.level) {
                variable.class <- 'categorical'
            }
        } else{
            x.level <- length(setdiff(unique(origData[[x]]), NA))
            if (x.level <= max.x.level) {
                variable.class <- 'categorical'
            }
        }

        if (variable.class == 'continuous') {
            calculated.summary.list <- lapply(df, calculateSummary)
            result <- result <- list(class = variable.class,
                                     count = total.number,
                                     summary.list = calculated.summary.list,
                                     p.value = NA)

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
                           p.value = NA)
        }

    } else if (y != "") {
        if (grepl("`", x)) {
            x <- gsub("`", "", x)
        }

        df <- data.frame(y = data[[y]], x = data[[x]])

        exist.missing <- FALSE

        if (show.missing == TRUE) { # missing shown
            if (any(is.na(df))) {
                contingency.table <- table(df$x, df$y, useNA = 'ifany')
                dimnames(contingency.table)[[1]][nrow(contingency.table)] <- 'Missing'
                exist.missing <- TRUE
            } else {
                contingency.table <- table(df$x, df$y)
                exist.missing <- FALSE
            }

            contingency.table.with.total <- addmargins(contingency.table, 2)
            total.number <- sum(contingency.table)

        } else {
            contingency.table <- table(df$x, df$y)
            contingency.table.with.total <- addmargins(contingency.table, 2)
            total.number <- sum(contingency.table)
        }

        variable.class <- ifelse(is.numeric(df$x), 'continuous', 'categorical')

        if (show.missing == TRUE && exist.missing == TRUE) {
            x.level <- length(unique(origData[[x]]))
            if (x.level - 1 <= max.x.level) {
                variable.class <- 'categorical'
            }
        } else{
            x.level <- length(setdiff(unique(origData[[x]]), NA))
            if (x.level <= max.x.level) {
                variable.class <- 'categorical'
            }
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
                           p.value = p.value)

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
            if (show.missing == TRUE && exist.missing == FALSE) {
                mat <- matrix(0, ncol = ncol(ratio))
                colnames(mat) <- names(ratio[1,])
                subgroup[[x.level + 1]] <- list(count = rep(0, length(subgroup.element.count)),
                                                ratio = mat)
                names(subgroup) <- c(rownames(contingency.table), "Missing")
            } else {
                names(subgroup) <- rownames(contingency.table)
            }

            p.value <- perform.chisq.test(x = df$x, y = df$y, paired = paired, verbose = verbose)
            result <- list(class = variable.class,
                           count = total.number,
                           subgroup = subgroup,
                           p.value = p.value)
        }
    }
    return(result)
}




