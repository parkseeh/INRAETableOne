#' INRAETableOne Main function
#' @importFrom stats terms addmargins
#' @export
INRAETableOneMain <- function(formula,
                              data,
                              max.x.level = 5,
                              show.missing = FALSE,
                              paired = FALSE,
                              show.total = FALSE,
                              show.detail = FALSE,
                              verbose = FALSE,
                              origData) {

    if (verbose == TRUE) {
        printLog("Making INRAETable!!")
    }


    model.terms <- terms(formula, data = data)
    x.variables <- labels(model.terms)
    check.variable <- x.variables %in% colnames(data)

    if (!all(check.variable)) {
        idx <- which(check.variable == FALSE)
        if (length(idx) > 1) {
            cat(paste0("The variables '", paste(x.variables[idx], collapse=' & '), "' are invalid, Please check the variable name \n"))
        } else {
            cat(paste0("The variable '", x.variables[idx], "' is invalid, please check the variable name \n"))
        }
        return(invisible())
    }


    if (length(formula) > 2) {
        y <- as.character(formula[[2]])
        if (length(y) > 1) {
            result <- INRAETableOneMore(formula = formula,
                                        data = data,
                                        max.x.level = max.x.level,
                                        show.missing = show.missing,
                                        paired = paired,
                                        show.total = show.total,
                                        show.detail = show.detail,
                                        verbose = verbose,
                                        origData = data)
            return(result)
        } else {
            y.table <- table(data[[y]])

            if (show.total == TRUE) {
                y.table <- addmargins(y.table)
                names(y.table)[length(y.table)] <- 'Total'
            }

            result.list <- list(y = y,
                                names = names(y.table),
                                count = unname(y.table),
                                length = length(y.table),
                                show.detail = show.detail)
        }

    } else {
        y <- ""
        result.list <- list(y = y,
                            names = "Overall",
                            count = nrow(data),
                            length = 1,
                            show.detail = show.detail)
    }


    for (x.variable in x.variables) {
        if (grepl("`", x.variable)) {
            x.variable <- gsub("`", "", x.variable)
        }
        summary.result <- createSummary(x = x.variable,
                                        y = y,
                                        data = data,
                                        max.x.level = max.x.level,
                                        show.missing = show.missing,
                                        paired = paired,
                                        show.total = show.total,
                                        verbose = verbose,
                                        origData = data)

        if (length(summary.result) != 4) {
            print('The summary result does not contain 4 element.')
            next
        }
        result.list[[x.variable]] <- summary.result
    }

    result <- makeTableOne(result.list, digits = 1)
    class(result) <- 'INRAETableOne'

    return(result)
}




