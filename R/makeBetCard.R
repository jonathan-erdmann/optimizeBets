#' @export
makeBetCard <- function(iFile) {

    betIn <- readBetProbabilityOdds(iFile)
    betIn <- betIn[betIn$kellyBet > 0, ]
    nViable <- nrow(betIn)

    betIn <- betIn[order(-betIn$kellyBet), ]
    betOut <- optimizeBets(betIn)

    betInViable <- betIn[betOut$bet > 0, ]
    betOutViable <- betOut[betOut$bet > 0, ]

    betCard <- betInViable %>% left_join(betOutViable, by = "name")
    betCard <- betCard[order(-betCard$bet), ]

    return(betCard)

}
