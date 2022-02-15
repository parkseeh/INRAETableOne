INRAETableOneStartupMessage <- function()  {
    msg <- c(paste0(
        "
_____ ____    ___ ____      __    ________
|_  __|   \\   | |  __ \\     /\\    |  ____|
  | | | |\\ \\  | | |__) |   /  \\   | |__
  | | | | \\ \\ | |  _  /   / /\\ \\  |  __|
 _| |_| |  \\ \\| | | \\ \\  / ____ \\ | |____
|_____|_|   \\_ _|_|  \\_\\/_/    \\_\\|______|

    "))
    return(msg)
}


.onAttach <- function(lib, pkg) {
    packageStartupMessage("Initializing INRAETableOne")
    packageStartupMessage(INRAETableOneStartupMessage())
    packageStartupMessage(paste0("version ", packageVersion("INRAETableOne")))
}

