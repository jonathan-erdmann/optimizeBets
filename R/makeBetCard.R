#' @export
makeBetCard <- function(iFile, iExact = FALSE, iMinBetSize = 0.0005) {

    betIn <- readBetProbabilityOdds(iFile)
    betIn <- betIn[betIn$kellyBet > 0, ]
    nViable <- nrow(betIn)

    betIn <- betIn[order(-betIn$kellyBet), ]
    betOut <- optimizeBets(betIn, iExact)

    betInViable <- betIn[betOut$bet > 0, ]
    betOutViable <- betOut[betOut$bet > 0, ]

    betCard <- betInViable %>%
        left_join(betOutViable, by = "name") %>%
        arrange(-bet) %>%
        filter(bet > iMinBetSize)

    return(betCard)

}
