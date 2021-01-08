#' @export
makeBetCard <- function(iFile) {

    betIn <- readBetProbabilityOdds(iFile)
    betOut <- optimizeBets(betIn)

    betInViable <- betIn[betOut$bet > 0, ]
    betOutViable <- betOut[betOut$bet > 0, ]

    betCard <- betInViable %>% left_join(betOutViable, by = "name")

    return(betCard)

}
