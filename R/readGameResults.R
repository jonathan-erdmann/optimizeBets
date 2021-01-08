#' @export
readGameResults <- function(iFile) {

    #Expecting CSV without header
    #   BET NAME
    #   WIN = 1, LOSS = 0

    inFile <- read.csv(iFile, header = FALSE)
    colnames(inFile) <- c("name","win_loss")
    return(inFile)

}
