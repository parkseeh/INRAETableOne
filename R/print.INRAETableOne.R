#' @param x An object of class "INRAETableOne"
#' @param ... further arguments to be passed to or from methods.
#'
#' @export
print.INRAETableOne <- function(x, ...) {
    obj <- x
    result <- lineCount(obj)
    y <- result$y
    line.length <- result$line.length
    head.line <- paste(rep("_", line.length+1), collapse = "")
    tail.line <- paste(rep("-", line.length+1), collapse = "")
    if (obj$length == 1) {
        res <- result$res[1:(length(result$res)-1)]
        column.names <- result$column.names[1:(length(result$column.names)-1)]
        n.count <- result$n.counut[1:(length(result$n.counut)-1)]
        column.length <- result$column.length[1:(length(result$column.length)-1)]
        cat("\n")
        cat(centerprint(paste0("Over all Summary descriptives table"), width = line.length))
        cat("\n\n")
        cat(head.line, "\n")
    } else {
        res <- result$res
        column.names <- result$column.names
        n.count <- result$n.counut
        column.length <- result$column.length
        line.length <- result$line.length

        cat("\n")
        cat(centerprint(paste0("Summary descriptives table by '", y, "'"), width = line.length))
        cat("\n\n")
        cat(head.line, "\n")
    }

    for (i in 1:length(column.names)) {
        cat((centerprint(column.names[i], width = column.length[i]+1)))
    }
    cat('\n')
    for (i in 1:length(n.count)) {
        cat((centerprint(n.count[i], width = column.length[i]+1)))
    }
    cat("\n")
    cat(tail.line, "\n")

    for (i in 1:dim(res)[1]){
        for(j in 1:length(column.names)){
            cat(sapply(res[i,j], centerprint, width = column.length[j] + 1))
        }
        cat("\n")
    }
    cat(tail.line, '\n\n')
}
