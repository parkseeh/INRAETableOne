#' Produces the descriptive summary table
#'
#' It produces the baseline characteristics table on the dependent variables.
#' The maximum combination of dependent variables are two, and can be formulated with
#' '+' sign such as variable1 + variable2 ~ ..
#' Also it shows the p-value according to the type of independent variable whether
#' perform \code{t.test}, \code{wilcox.test}, \code{kruskal.test}, \code{anova} if continuous,
#'  or \code{chisq.test}, \code{fisher.test} if categorical.
#'
#' @param x A formula indicating the dependent variable(s) on the left hand side, and
#' the independent variable(s) on the right hand side.
#' @param ... More arguments can be added.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(INRAETableOne)
#' data(iris)
#' INRAETableOne(Species ~ ., iris)
#' }
#'
INRAETableOne <- function(x, ...) {
    UseMethod("INRAETableOne")
}



#' INRAETableOne S3 Method for formula
#'
#' @param formula A formula format
#' @param data A \code{data.frame}
#' @describeIn INRAETableOne The \code{formula} interface.
#' @export
INRAETableOne.formula <- function(formula,
                                  data,
                                  show.total = FALSE,
                                  paired = FALSE,
                                  show.missing = TRUE,
                                  verbose = FALSE) {

    # data <- read.csv('acs1.csv') ; formula <- Dx ~ .
    model.terms <- terms(formula, data = data)
    if (length(formula) > 2) {
        y <- as.character(formula[[2]])
    } else {
        y <- ''
    }

    y.table <- table(data[[y]])
    if (show.total == TRUE) {
        y.table <- addmargins(y.table)
        names(y.table)[length(y.table)] <- 'Total'
    }

    result <- list(y = y,
                   names = names(y.table),
                   count = unname(y.table),
                   length = length(y.table),
                   show.total = show.total)
    x.variables <- labels(model.terms)

    for (x.variable in x.variables) {
        summary.result <- createSummary(x = x.variable,
                                        y = y,
                                        data = data,
                                        show.total = show.total,
                                        paired = paired,
                                        show.missing = show.missing,
                                        verbose = verbose)
        #print(summary.result)
        if (length(summary.result) != 4) {
            print('The summary result does not contain 4 element.')
            next
        }
        result[[x.variable]] <- summary.result
    }

    result1 <- makeTableOne(result, digits = 1)
    class(result1) <- 'INRAETableOne'
    return(result1)

}




#' Create the summary table
#'
#' It produces the list containing the detailed summary table for
#' the type of \code{y} (dependent variable) whether it's continuous or categorical
#' @param x
#' @param y
#' @param data A \code{data.frame}
#' @param paired If \code{paired} is TRUE, then perfrom paired t-test. The default value is \code{FALSE},
#' @param verbose Print the log message. The default value is \code{FALSE}
#' @importFrom stats na.omit
#' @return A list containing the class of variable, total count of data, and
#' p-value. In addition, the min, max, median, sd, mean will be produced only
#' for continuous variable
#' @export
createSummary <- function(x,
                          y,
                          data,
                          show.total = FALSE,
                          paired = FALSE,
                          show.missing = TRUE,
                          verbose = FALSE) {
    # data=acs; y="Dx"; x="height"; show.total=F; paired=F; show.missing=T; verbose=F
    df <- data.frame(y = data[[y]], x = data[[x]])

    if (show.missing == TRUE) { # missing shown
        if (any(is.na(df))) {
            contingency.table <- table(df$x, df$y, useNA = 'ifany')
            dimnames(contingency.table)[[1]][nrow(contingency.table)] <- 'Missing'
        } else {
            contingency.table <- table(df$x, df$y)
        }


        contingency.table.with.total <- addmargins(contingency.table, 2)
        total.number <- sum(contingency.table)

    } else {
        contingency.table <- table(df$x, df$y)
        contingency.table.with.total <- addmargins(contingency.table, 2)
        total.number <- sum(contingency.table)
    }

    x.level <- nrow(contingency.table)
    variable.class <- ifelse(is.numeric(data[[x]]), 'continuous', 'categorical')

    if (variable.class == 'continuous') {
        calculated.summary.list <- tapply(data[[x]], data[[y]], calculateSummary)
        if (show.total == TRUE) {
            calculated.summary.list[[length(calculated.summary.list) + 1]] <- calculateSummary(data[[x]])
            names(calculated.summary.list)[length(calculated.summary.list)] <- 'Total'
        }
        df <- na.omit(df)
        p.value <- perform.t.test(x = df$x, y = df$y, paired = paired)
        result <- list(class = variable.class,
                       count = total.number,
                       summary.list = calculated.summary.list,
                       p = p.value)
    } else if (variable.class == 'categorical') {
        subgroup <- list()
        for (i in 1:x.level) {
            if (show.total == TRUE) {
                subgroup.element.count <- contingency.table.with.total[i, ]
                attr(subgroup.element.count, "names") <- NULL
                ratio <- apply(contingency.table.with.total, 2, function(x) x * 100 / sum(x))
                ratio.table <- list(count = subgroup.element.count,
                                    ratio = ratio[i,])
                subgroup[[i]] <- ratio.table
            } else {
                subgroup.element.count <- contingency.table[i, ]
                attr(subgroup.element.count, "names") <- NULL
                ratio <- apply(contingency.table, 2, function(x) x * 100 / sum(x))
                ratio.table <- list(count = subgroup.element.count,
                                   ratio = ratio[i,])
                subgroup[[i]] <- ratio.table
            }
        }

        names(subgroup) <- rownames(contingency.table)
        p.value <- perform.chisq.test(x = df$x, y = df$y)
        result <- list(class = variable.class,
                       count = total.number,
                       subgroup = subgroup,
                       p = p.value)
    }
    return(result)
}


makeTableOne <- function(obj, digits = 1) {
    # obj <- result ; digits = 1
    plusminus <- "\u00b1"
    variable.names <- c()
    subgroup.names <- c()
    variable.class <- c()
    total.count <- c()
    p.value <- c()

    fmt <- sprintf("%s%df","%3.",digits)
    mean.sd <- paste0("Mean ", plusminus, " SD")
    median.min.max <- paste0("Med [Min;Max]")

    initial.matrix <- matrix(NA, ncol = obj$length)
    colnames(initial.matrix) <- obj$names

    for (i in 6:length(obj)) {
        variable.names <- c(variable.names, names(obj)[i])



        subgroup.names <- c(subgroup.names, "")
        variable.class <- c(variable.class, obj[[i]]$class)
        total.count <- c(total.count, obj[[i]]$count)
        add.matrix <- matrix(NA, ncol = obj$length)
        colnames(add.matrix) <- obj$names

        if (obj[[i]]$class == 'continuous') {

            add.matrix <- matrix("", ncol = obj$length)
            if (all(is.na(initial.matrix))) {
                initial.matrix <- add.matrix
            } else {
                initial.matrix <- rbind(initial.matrix, add.matrix)
            }
            p.value <- c(p.value, obj[[i]]$p)

            add.matrix <- matrix(NA, nrow = 2, ncol = obj$length)

            for (k in 1:2) {
                p.value <- c(p.value, NA)
                variable.class <- c(variable.class, "")
                total.count <- c(total.count, "")
                if (k == 1) {
                    subgroup.names <- c(subgroup.names, mean.sd)
                } else {
                    subgroup.names <- c(subgroup.names, median.min.max)
                }
                for (j in 1:obj$length) {
                    if (k == 1) {
                        temp <- paste(sprintf(fmt, obj[[i]]$summary.list[[j]][[1]]),
                                      plusminus,
                                      sprintf(fmt, obj[[i]]$summary.list[[j]][[2]]),
                                      sep = " ")
                    } else {
                        temp <- paste0(sprintf(fmt, obj[[i]]$summary.list[[j]][[3]]),
                                       "[",
                                       sprintf(fmt, obj[[i]]$summary.list[[j]][[4]]),
                                       ";",
                                       sprintf(fmt, obj[[i]]$summary.list[[j]][[5]]),
                                       "]")
                    }
                    add.matrix[k, j] <- temp
                }
                variable.names <- c(variable.names, "")
            }



            if (all(is.na(initial.matrix))) {
                initial.matrix <- add.matrix
            } else {
                initial.matrix <- rbind(initial.matrix, add.matrix)
            }


        } else if (obj[[i]]$class == 'categorical') {
            add.matrix <- matrix("", ncol = obj$length)
            if (all(is.na(initial.matrix))) {
                initial.matrix <- add.matrix
            } else {
                initial.matrix <- rbind(initial.matrix, add.matrix)
            }

            p.value <- c(p.value, obj[[i]]$p)

            for (subgroup.idx in 1:length(obj[[i]]$subgroup)) {
                subgroup.name <- names(obj[[i]]$subgroup[subgroup.idx])
                variable.names <- c(variable.names, "")
                subgroup.names <- c(subgroup.names, subgroup.name)
                p.value <- c(p.value, NA)
                variable.class <- c(variable.class, "")
                total.count <- c(total.count, "")

                for (j in 1:obj$length) {
                    temp <- paste(obj[[i]]$subgroup[[subgroup.idx]]$count[j],
                                  " (",
                                  sprintf(fmt, obj[[i]]$subgroup[[subgroup.idx]]$ratio[j]),
                                  "%)", sep = "")
                    add.matrix[1, j] <- temp
                }
                if (all(is.na(initial.matrix))) {
                    initial.matrix <- add.matrix
                } else {
                    initial.matrix <- rbind(initial.matrix, add.matrix)
                }
            }

        }
    }
    combined.name <- ifelse(subgroup.names == "",
                            variable.names,
                            paste(variable.names, " - ", subgroup.names, sep = ""))

    combined.name <- formatC(combined.name,"%s",flag="-")
    res <- data.frame(name=combined.name)
    for (j in 1:obj$length) {
        res <- data.frame(res, initial.matrix[, j])
    }
    p.value <- sapply(p.value,function(x) ifelse(is.na(x),"",sprintf("%.3f",x)))
    sig <- sapply(p.value, p2sig)
    res <- data.frame(res,
                      p = p.value,
                      sig = sig,
                      class = variable.class,
                      total.count = total.count)
    colnames(res)[2:(1+length(obj$names))] <- obj$names
    colnames(res)[1] <- obj$y

    result <- list(res = res,
                   count = obj$count,
                   show.total = obj$show.total)
    return(result)

}


#' Print significant symbol
#' @param value a numeric vector
p2sig <- function(value){
    if (is.na(value)) {
        sig <= "   "
    } else if (value == ""){
        sig <- "   "
    } else if (value < 0.01) {
        sig = "***"
    } else if (value < 0.05) {
        sig = "** "
    } else if (value < 0.1) {
        sig = "*  "
    } else {
        sig = "   "
    }
    return(sig)
}


#' @export
centerprint=function(x,...,width=10){

    mwidth=max(nchar(x),width)
    sp=(mwidth-nchar(x))/2
    front=end=""
    front=space(ceiling(sp))
    end=space(floor(sp))
    x=paste(front,x,end,sep="")
    x
}

#' Internal mytable functions
#'
#' Internal mytable functions
#' These are not to be called by the user
#' @param num an integer
space=function(num){
    ret=c()
    if(num <1) return(ret)
    for(i in 1:num) ret=paste(" ",ret,sep="")
    ret
}

#' Internal mytable functions
#'
#' Internal mytable functions
#' These are not to be called by the user
#' @param x a character vector
#' @param times an integer
#' @export
reprint=function(x,times){
    ret=x
    if(times<=1) return(x)
    for(i in 1:times) ret=paste(ret,x,sep="")
    ret
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
lineCount <- function(x, ...) {
    obj <- x
    if (obj$show.total == TRUE) {
        result.table <- obj$res
    } else {
        result.table <- obj$res[1:(length(obj$res)-3)]
    }

    count.total <- obj$count
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





#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
print.INRAETableOne <- function(x, ...) {
    obj <- x
    result <- lineCount(obj)
    y <- result$y
    res <- result$res
    column.names <- result$column.names
    n.count <- result$n.counut
    column.length <- result$column.length
    line.length <- result$line.length
    head.line <- paste(rep("_", line.length+1), collapse = "")
    tail.line <- paste(rep("-", line.length+1), collapse = "")

    cat("\n")
    cat(paste0("Summary descriptives table by '", y, "'"))
    cat("\n\n")
    cat(head.line, "\n")
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





