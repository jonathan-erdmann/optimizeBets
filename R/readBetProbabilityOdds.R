#' @export
readBetProbabilityOdds <- function(iFile) {

    #Expecting CSV without header
    #   BET NAME
    #   MONEY LINE
    #   PROBABILITY

    #Attach
    #   ODDS
    #   INDIVIDUAL KELLY BET

    inFile <- read.csv(iFile, header = FALSE)
    colnames(inFile) <- c("name","moneyLine","probability")

    inFile$odds <- moneyLineToOdds(inFile$moneyLine)
    #inFile$kellyBet <- kellyBet(inFile$probability, inFile$odds)

    return(inFile)

}
