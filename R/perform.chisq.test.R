#' Title
#'
#' @param x
#' @param y
#' @param paired
#'
#' @return
#' @export
#'
#' @examples
perform.chisq.test <- function(x, y, paired = FALSE) {
    contingency.table <- table(x, y)

    if (dim(contingency.table)[2] == 1){
        p.value <- NA
    } else {
        tryCatch ({
            p.value <- chisq.test(contingency.table)$p.value
            return(p.value)

        }, warning = function(w) {
            p.value <- fisher.test(contingency.table)$p.value
            message("Use Fisher Test, because the expected value < 5 are over 25% of overall contengency table")
            return(p.value)

        }, error = function(e) {
            p.value <- 1
            return(p.value)
        })
    }
    return(p.value)
}


#' Title
#'
#' @param x
#' @param y
#' @param paired
#'
#' @return
#' @export
#'
#' @examples
perform.chisq.test <- function(x, y, paired = FALSE) {
    contingency.table <- table(x, y)

    if (dim(contingency.table)[2] == 1){
        p.value <- NA
        return(p.value)
    } else {
        if (paired == TRUE) {
            p.value <- mcnemar.test(contingency.table)$p.value
        } else {
            tryCatch ({
                p.value <- chisq.test(contingency.table)$p.value

            }, warning = function(w) {
                p.value <- fisher.test(contingency.table)$p.value
                message("Use Fisher Test, because the expected value < 5 are over 25% of overall contengency table")

            }, error = function(e) {
                p.value <- 1
            })
        }
        return(p.value)
    }
}









