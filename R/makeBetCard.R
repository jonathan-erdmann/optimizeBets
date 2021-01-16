#' @export
makeBetCard <- function(iFile, iExact = FALSE, iFactorBet = 4, iMinBetSize = 0.0005) {

    betIn <- readBetProbabilityOdds(iFile)
    betIn <- betIn %>% filter(kellyBet > 0)

    betOut <- optimizeBets(betIn, iExact)

    betCard <- betOut %>%
        mutate(bet = bet / iFactorBet)
        left_join(betOutViable, by = "name") %>%
        arrange(-bet) %>%
        filter(bet > iMinBetSize)

    return(betCard)

}
