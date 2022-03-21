#' @param ... An object of class "INRAETableOne". It can be multiples of INRAETableOne
#' @param caption A unique value of grouping variable
#' @param y A name of grouping variable
#'
#' @export
<<<<<<< HEAD
cbind.INRAETableOne <- function(..., caption=NULL, y = NULL) {
=======
cbind.INRAETableOne <- function(..., caption = NULL, y = NULL) {
>>>>>>> 48bc2a1fc325309901b9b71c48a0a75007e65375

    args <- list(...)
    out <- args
    if (is.null(caption) || all(caption == "")) {
        caption <- unlist(lapply(args, function(vv) ifelse(is.null(attr(vv, "yname")),
                                                           "[No groups]", paste("By", attr(vv, "yname")))))
    }


    out.data.frame <- map2_dfc(out, 1:length(out), function(df, i) {
        result <- as.data.frame(out[[i]])
        if (i > 1) {
            result <- result[,2:length(result)]
        }
        return(result)
    })

    #print(out.data.frame)
    attr(out, "table") <- out.data.frame



    attr(out, "caption") <- caption
    attr(out, "group") <- y
    #class(out) <- c("cbind.INRAETableOne")
    class(out) <- c("cbind.INRAETableOne", "INRAETableOne", "data.frame")
    return(out)
}


