#' @export
makeBetCard <- function(iFile, iLimitGamesToBet) {

    betIn <- readBetProbabilityOdds(iFile)
    betIn <- betIn[betIn$kellyBet > 0, ]
    nViable <- nrow(betIn)

    betIn <- betIn[order(-betIn$kellyBet), ]
    if (nViable > iLimitGamesToBet) {

        betIn <- betIn[1:iLimitGamesToBet, ]

    }

    betOut <- optimizeBets(betIn)

    betInViable <- betIn[betOut$bet > 0, ]
    betOutViable <- betOut[betOut$bet > 0, ]

    betCard <- betInViable %>% left_join(betOutViable, by = "name")

    return(betCard)

}
