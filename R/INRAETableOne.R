#' Generate the descriptive statistics table
#'
#' It produces a nicely formatted table of descriptive statistics for numeric or categorical variables
#'
#' @description
#' The dependent and independent variable should be indicated as \code{formula} format.
#' The formula may contain a dot (".") to refer to "all variables in \code{data}
#' other than those that appear elsewhere in the formula".
#' The maximum combination of dependent variables are two, and can be formulated with
#' '+' sign such as 'dependent1' + 'dependent2' ~ 'independent1' + 'independent2' + ....
#'
#' Also it shows the p-value according to the type of independent variable. The
#' p-value is carefully calculated regarding the number of sample, normality, homogeneity,
#' and independency.  The calculation is based on the \code{t.test}, \code{wilcox.test},
#' \code{kruskal.test}, and \code{anova} for continuous variable, whereas \code{chisq.test}
#' and \code{fisher.test} are used for categorical variable.
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
#' @return An object of class "INRAETableOne".
#'
#' @examples
#' \dontrun{
#' library(INRAETableOne)
#' data(iris)
#' INRAETableOne(Species ~ ., iris)
#' }
#' @export
INRAETableOne <- function(x, ...) {
    UseMethod("INRAETableOne")
}


#' INRAETableOne S3 Method for formula
#' @describeIn INRAETableOne The \code{formula} interface.
#' @export
INRAETableOne.formula <- function(formula,
                                  data,
                                  max.x.level = 5,
                                  show.missing = FALSE,
                                  paired = FALSE,
                                  show.total = FALSE,
                                  show.detail = FALSE,
                                  verbose = FALSE,
                                  origData) {

    if (!inherits(formula, "formula")) {
        stop(paste("Please specift formula with y variable on the left hand side of '~'",
             "and x variable(s) on the right hand side of '~'"))
    }
    if (missing(data)) {
        stop("Please indicate data argument")
    }
    if (is.numeric(max.x.level) == FALSE) {
        stop("The parameter 'max.x.level' must be numeric")
    }
    if (show.missing != TRUE && show.missing != FALSE) {
        stop("The parameter 'show.missing' must be a boolean.")
    }
    if (paired != TRUE && paired != FALSE) {
        stop("The parameter 'paired' must be a boolean.")
    }
    if (show.total != TRUE && show.total != FALSE) {
        stop("The parameter 'show.total' must be a boolean.")
    }
    if (show.detail != TRUE && show.detail != FALSE) {
        stop("The parameter 'show.detail' must be a boolean.")
    }
    if (verbose != TRUE && verbose != FALSE) {
        stop("The parameter 'verbose' must be a boolean.")
    }

    result <- INRAETableOneMain(formula,
                                data,
                                max.x.level = max.x.level,
                                show.missing = show.missing,
                                paired = paired,
                                show.total = show.total,
                                show.detail = show.detail,
                                verbose = verbose,
                                origData)


    return(result)
}

#'@describeIn INRAETableOne default S3 method
#'@export
INRAETableOne.data.frame <- function(x,...){
    INRAETableOne(~.,x,...)
}


