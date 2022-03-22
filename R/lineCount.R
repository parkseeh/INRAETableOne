#' @param x An object returned by \code{\link{makeTableOne}}.
#' @param ...
#' @export
lineCount <- function(x, ...) {
    obj <- x
    if (attr(obj, "length") == 1) {
        result.table <- obj[1:length(obj)]
    } else {
        result.table <- obj[1:length(obj)-1]
    }


    count.total <- attr(obj,"count")
    column.names <- colnames(result.table)
    y <- column.names[1]
    column.names[1] <- ""

    n.count <- c("", paste0("(n=", count.total,")"))
    column.names.nchar <- nchar(column.names)
    column.nchar <- unname(sapply(result.table,function(x) max(nchar(x))))
    column.length <- apply(rbind(column.names.nchar, column.nchar), 2, max)
    line.length <- sum(column.length) + length(column.names) - 1

    result <- list(y = y,
                   res = result.table,
                   column.names = column.names,
                   n.counut = n.count,
                   column.length = column.length,
                   line.length = line.length)

    return(result)
}


#' Prints the string in the center within the width value
#' @param x A string
#' @param width A length of string
#' @export
centerprint <- function(x,...,width=10){
    mwidth <- max(nchar(x),width)
    sp <- (mwidth-nchar(x))/2
    front <- end <- ""
    front <- space(ceiling(sp))
    end <- space(floor(sp))
    x <- paste(front, x, end, sep="")
    return(x)
}


#' @param num an integer
#' @export
space <- function(num){
    ret <- c()
    if (num < 1) {
        return(ret)
    }
    for (i in 1:num) {
        ret <- paste(" ", ret, sep="")
    }
    return(ret)
}

