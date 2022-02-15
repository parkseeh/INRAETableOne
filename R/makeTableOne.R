#' Generate a \code{data.frame}
#'
#' @param obj An object returned by \code{\link{createSummary}}.
#' @param digits A number indicating the decimals digits
#'
#' @export
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

            if(obj$show.detail == TRUE) {
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
            } else {
                p.value <- c(p.value, obj[[i]]$p)
                for (j in 1:obj$length) {
                    temp <- paste(sprintf(fmt, obj[[i]]$summary.list[[j]][[1]]),
                                  plusminus,
                                  sprintf(fmt, obj[[i]]$summary.list[[j]][[2]]),
                                  sep = " ")
                    add.matrix[1,j] <- temp
                }
                if (all(is.na(initial.matrix))) {
                    initial.matrix <- add.matrix
                } else {
                    initial.matrix <- rbind(initial.matrix, add.matrix)
                }

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
                    # if (obj[[i]]$subgroup[[subgroup.idx]]$count[j] == 0) {
                    #     temp <- "0 (0.0%)"
                    # }

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
                   length = obj$length,
                   count = obj$count)
    return(result)

}


#' Print the significant symbol
#' @param value a numeric vector
#' @export
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
