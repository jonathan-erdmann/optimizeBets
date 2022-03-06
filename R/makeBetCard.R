#' @export
makeBetCard <- function(iFile, iExact = FALSE, iFactorBet = 4, iMinBetSize = 0.0005, iRandomize = TRUE, iSD = 0.01) {

    betIn <- readBetProbabilityOdds(iFile)

    betIn <- betIn %>% mutate(kellyBet = kellyBet(probability, odds))

    betIn <- betIn %>% filter(kellyBet > 0)

    betOut <- optimizeBets(betIn, iExact = iExact, iRandomize = iRandomize, iSD = iSD)
    #betOut <- optimizeBetsParlay(betIn)

    betCard <- betIn %>%
        left_join(betOut, by = "name") %>%
        mutate(bet = bet / iFactorBet) %>%
        arrange(-bet) %>%
        filter(bet > iMinBetSize)

    return(betCard)

}
