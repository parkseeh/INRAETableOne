#' @title printLog
#' @description
#' Prints the log of the current process
#'
#' @param msg String value message to print along with log type and date
#' @param type String value that represents type of this message. 'INFO' by default.
#'
#' @export
printLog <- function(msg, type = "INFO") {
    print(paste0(type, " [", Sys.time(), "] ", msg))
}
