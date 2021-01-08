#' @export
readBetProbabilityOdds <- function(iFile) {

    #Expecting CSV without header
    #   BET NAME
    #   MONEY LINE
    #   PROBABILITY

    inFile <- read.csv(iFile, header = FALSE)
    colnames(inFile) <- c("name","moneyLine","probability")
    return(inFile)

}
