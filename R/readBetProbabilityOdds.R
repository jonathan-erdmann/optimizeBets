#' @export
readBetProbabilityOdds <- function(iFile) {

    #Expecting CSV without header
    #   TEAM
    #   MONEY LINE
    #   PROBABILITY

    inFile <- read.csv(iFile, header = FALSE)
    colnames(inFile) <- c("team","moneyLine","probability")
    return(inFile)

}
