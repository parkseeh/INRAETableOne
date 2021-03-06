#' Perform t-test
#'
#' @description This function calculates the t-test according to the
#' different tpye of data.
#'
#' @param x A numeric value
#' @param y A vector
#' @param paired Whether perform paired t-test or not
#' @param verbose FALSE as default
#'
#' @importFrom stats lm shapiro.test resid var.test t.test kruskal.test anova wilcox.test friedman.test
#' @return A p-value vector according to the different type of data.
#' @export
#' @example
#' \dontrun{
#' data(iris)
#' perform.t.test(x = iris$Sepal.Length, y = iris$Species)}
#'
perform.t.test <- function(x, y, paired = FALSE, verbose = FALSE) {

    minimum.sample.size <- 30
    sample.size <- length(x)
    x.level <- length(unique(x))
    y.level <- length(unique(y))
    tab <- table(y)

    if (y.level == 1) {
        p.value.vector <- NA
        return(p.value.vector)
    }

    if (sample.size < minimum.sample.size) {
        if (verbose == TRUE) {
            printLog(paste0("* We have sample size of ", sample.size))
            printLog(paste0("** Since sample size is smaller than 30",
                            "we have to check the normality assumtion to perform t-test"))
        }

        normality.test <- lm(x ~ y)
        normality.shapiro <- shapiro.test(resid(normality.test))

        if (y.level == 2) {
            if (paired == TRUE && sum(diff(tab)) == 0 && all(!is.na(x))) {
                if (normality.shapiro$p.value < 0.05) { # Not normal, so perform Wilcox's Test
                    if (verbose == TRUE) {
                        printLog("[2 factors] paired Non-normal Wilcox Test")
                    }
                    p.value.vector <- wilcox.test(x ~ y)$p.value
                } else {
                    if (verbose == TRUE) {
                        printLog("[2 factors] paired t-test")
                    }
                    p.value.vector <- t.test(x ~ y, paired = TRUE)$p.value
                }
            } else {
                variance.test <- var.test(x ~ y) # test homogeneity
                if (normality.shapiro$p.value < 0.05) { # Not normal, so perform Mann Whitney U test
                    if (verbose == TRUE) {
                        printLog("[2 factors] non-paired Wilcox Test")
                    }
                    p.value.vector <- wilcox.test(x ~ y)$p.value
                } else {
                    if (variance.test$p.value < 0.05) { #variance are not homogeneity
                        if (verbose == TRUE) {
                            printLog("[2 factors] non-paired two sample t-test")
                        }
                        p.value.vector <- t.test(x ~ y, na.rm = TRUE)$p.value
                    } else {
                        if (verbose == TRUE) {
                            printLog("[2 factor] non-paired Welch test")
                        }
                        p.value.vector <- t.test(x ~ y, var.equal = TRUE)$p.value
                    }
                }
            }
        } else { # If dependent variable contains more than 3 factors
            if (paired == TRUE && sum(diff(tab)) == 0 && all(!is.na(x))) {
                id <- rep(1:tab[[1]])
                df <- data.frame(id, y, x)
                if (normality.shapiro$p.value < 0.05) {
                    if (verbose == TRUE) {
                        printLog("[>3 factors] paired Friedman test")
                    }
                    friedman <- friedman.test(y=df$x, groups=df$y, blocks=df$id)
                    p.value.vector <- friedman$p.value
                } else {
                    if (verbose == TRUE){
                        printLog("[>3 factors] paired RM-Anova test")
                    }
                    rmAnova <- summary(aov(df$x~df$y+Error(df$id/df$y)))
                    p.value.vector <- rmAnova$`Error: Within`[[1]][5]$`Pr(>F)`[1]
                }
            } else {
                if (normality.shapiro$p.value < 0.05) { # Not normal, so perform Kruskal test
                    if (verbose == TRUE) {
                        printLog("[>3 facotrs] non-paired Kruskal test")
                    }
                    p.value.vector <- kruskal.test(as.numeric(x), factor(y))$p.value
                } else {
                    if (verbose == TRUE) {
                        printLog("[>3 factor] non-paired Anova test")
                    }
                    p.value.vector <- anova(lm(x ~ factor(y)))$Pr[1]
                }
            }
        }

    } else { # More than 30 observations
        if (verbose == TRUE) {
            printLog(paste0("* We have sample size of ", sample.size))
            printLog("** We don't need to think of the normality assumption by Central Limit Theorem (CTL)")
            printLog("*** Parametric Method will be used for all t-test")
        }

        if (y.level == 1) {
            p.value.vector <- NA
            return(p.value.vector)
        }

        if (y.level == 2) {

            if (paired == TRUE && sum(diff(tab)) == 0 && all(!is.na(x))) {
                if (verbose == TRUE) {
                    printLog("[2 factors] paired t-test")
                }
                p.value.vector <- t.test(x ~ y, paired = TRUE)$p.value # paired t-test

            } else {
                variance.test <- var.test(x ~ y) # test homogeneity

                if (variance.test$p.value < 0.05) { # variance are not homogeneity
                    if (verbose == TRUE) {
                        printLog("[2 factors] non-paired two sample t-test")
                    }
                    p.value.vector <- t.test(x ~ y, na.rm = TRUE)$p.value
                } else { # variance are homogeneity
                    if (verbose == TRUE) {
                        printLog("[2 factor] non-paired Welch test")
                    }
                    p.value.vector <- t.test(x ~ y, var.equal = TRUE)$p.value
                }
            }

        } else { # If dependent variable contains more than 3 factors
            if (paired == TRUE && sum(diff(tab)) == 0 && all(!is.na(x))) {
                if (verbose == TRUE) {
                    printLog("[>3 factors] paired RM-Anova test")
                }
                rmAnova <- summary(aov(df$x~df$y+Error(df$id/df$y)))
                p.value.vector <- rmAnova$`Error: Within`[[1]][5]$`Pr(>F)`[1]
            } else {
                if (verbose == TRUE) {
                    printLog("[>3 factor] non-paired Anova test")
                }
                p.value.vector <- anova(lm(x ~ factor(y)))$Pr[1]
            }
        }
    }
    return(p.value.vector)

}










