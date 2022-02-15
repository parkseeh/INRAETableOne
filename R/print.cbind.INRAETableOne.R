
#' Print function for class of cbind.INRAETableOne
#'
#' @param x An object of class "cbind.INRAETableOne".  It is returned from
#' \code{\link{cbind.INRAETableOne}}
#' @param ... further arguments to be passed to or from methods.
#'
#' @export
print.cbind.INRAETableOne <- function(x,...) {
    obj <- x
    tcount <- length(obj) # number of tables
    tnames <- unlist(attr(obj,"caption"))
    group <- attr(obj,"group")
    result <- list()

    for (i in 1:tcount) {
        result[[i]] <- lineCount(obj[[i]])
    }

    line.length <- 0
    for (i in 1:tcount) {
        line.length <- line.length + result[[i]]$line.length - result[[i]]$column.length[1] - 1
    }

    line.length <- result[[1]]$column.length[1] + line.length + tcount
    cat("\n")

    temp <- paste("Descriptive Statistics stratified by '",group[1],"' and '",group[2],"'",sep="")
    cat(centerprint(temp, width=line.length))
    cat("\n")

    head.line <- paste(rep("_", line.length+1), collapse = "")
    tail.line <- paste(rep("-", line.length+1), collapse = "")
    cat(head.line, "\n")

    cat(centerprint("", width = result[[1]]$column.length[1] + 2))
    for (i in 1:tcount) {
        if (class(tnames[i]) == "factor") {
            temp <- levels(tnames)[tnames[i]]
        } else {
            temp <- tnames[i]
        }
        #browser()
        cat(centerprint(temp, width = result[[i]]$line.length - result[[i]]$column.length[1] + 1))
    }
    cat("\n")
    cat(centerprint("",width=result[[1]]$column.length[1]+2))
    for (i in 1:tcount) {
        cat(paste(rep("-", result[[i]]$line.length - result[[i]]$column.length[1] - 1),collapse = ""), "")
    }
    cat("\n")
    cat(centerprint(result[[1]]$column.names[1], width = result[[1]]$column.length[1]+1))
    for (i in 1:tcount) {
        for (j in 2:(length(result[[i]]$column.names))) {
            cat(centerprint(result[[i]]$column.names[j], width = result[[i]]$column.length[j]+1))
        }

    }
    cat("\n")
    cat(centerprint("", width = result[[1]]$column.length[1]+1))
    for (i in 1:tcount) {
        for (j in 2:(length(result[[i]]$n.counut))) {
            cat(centerprint(result[[i]]$n.counut[j], width = result[[i]]$column.length[j]+1))
        }
        cat("      ")
    }
    cat("\n")
    cat(tail.line,"\n")

    for (i in 1:dim(result[[1]]$res)[1]){
        for (k in 1:tcount){
            if (k==1) {
                for (j in 1:(length(result[[1]]$column.names))){
                    cat(sapply(result[[k]]$res[i,j], centerprint, width = result[[k]]$column.length[j] +1))
                }
            } else {
                for (j in 2:(length(result[[1]]$column.names))){
                    cat(sapply(result[[k]]$res[i,j], centerprint, width = result[[k]]$column.length[j] +1))
                }
            }
        }
        cat("\n")
    }
    cat(tail.line,"\n")
}

