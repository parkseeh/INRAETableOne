#' Perform Chisq Test or Fisher's Exat Test
#'
#' @description This function calculates the p-value for the
#' categorical variable according to the different tpye of data.
#' @param x A categorical vector
#' @param y A Categorical vector
#' @param paired If TRUE, then perform Mcnemar Test.
#'
#' @importFrom stats mcnemar.test chisq.test fisher.test
#' @return Return the p value for categorical contingency table
#' @export
perform.chisq.test <- function(x, y, paired = FALSE) {
    contingency.table <- table(x, y)

    if (dim(contingency.table)[2] == 1){
        p.value <- NA
        return(p.value)

    } else {
        if (paired == TRUE) {
            if (all(dim(contingency.table) == c(2,2))){
                p.value <- mcnemar.test(contingency.table)$p.value
                return(p.value)
            } else {
                message("The contingency table is not 2 X 2 matrix. You need to put paired == TRUE instead")
                return(invisible())
            }

        } else {
            p.value <- tryCatch (
                expr = {
                    p.value <- chisq.test(contingency.table)$p.value

                },
                warning = function(w) {
                    message("Fisher Test used, because the expected value < 5 are over 25% of overall contengency table")
                    p.value <- fisher.test(contingency.table, simulate.p.value=TRUE)$p.value
                },
                error = function(e) {
                    p.value <- 1
                }
            )
        }
        return(p.value)
    }
}

