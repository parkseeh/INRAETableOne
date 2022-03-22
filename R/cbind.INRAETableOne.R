#' @param ... An object of class "INRAETableOne". It can be multiples of INRAETableOne
#' @param caption A unique value of grouping variable
#' @param y A name of grouping variable
#'
#' @importFrom purrr map2_dfc
#'
#' @export


cbind.INRAETableOne <- function(..., caption = NULL, y = NULL) {


    args <- list(...)
    out <- args
    if (is.null(caption) || all(caption == "")) {
        caption <- unlist(lapply(args, function(vv) ifelse(is.null(attr(vv, "yname")),
                                                           "[No groups]", paste("By", attr(vv, "yname")))))
    }

    options(warn = -1)
    out.data.frame <- purrr::map2_dfc(out, 1:length(out), function(df, i) {
        result <- as.data.frame(out[[i]])
        names(result)[2:length(result)] <- paste0(names(result),"_", caption[i])
        if (i > 1) {
            result <- result[,2:length(result)]
        }
        return(result)
    })

    attr(out, "table") <- out.data.frame
    attr(out, "caption") <- caption
    attr(out, "group") <- y
    class(out) <- c("cbind.INRAETableOne")
    #class(out) <- c("cbind.INRAETableOne", "INRAETableOne", "data.frame")
    return(out)
}


