#' @param ... An object of class "INRAETableOne". It can be multiples of INRAETableOne
#' @param caption A unique value of grouping variable
#' @param y A name of grouping variable
#'
#' @export
cbind.INRAETableOne <- function(..., caption, y = NULL) {

    args <- list(...)
    out <- args
    if (is.null(caption) || all(caption == "")) {
        caption <- unlist(lapply(args, function(vv) ifelse(is.null(attr(vv, "yname")),
                                                           "[No groups]", paste("By", attr(vv, "yname")))))
    }

    attr(out, "caption") <- caption
    attr(out, "group") <- y
    class(out) <- "cbind.INRAETableOne"
    return(out)
}


