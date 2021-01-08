#' @export
profitLossDistribution <- function(iBetInput, iBet) {

    # Input: Two data frames containing probability, odds, and bets
    # Output: profit/loss distribution

    probability <- iBetInput$probability
    odds <- moneyLineToOdds(iBetInput$moneyLine)
    bet <- iBet$bet

    nGames <- length(probability)
    outcomes <- binaryOutcomes(nGames)
    nOutcomes <- nrow(outcomes)

    likelihood <- columnwiseExponentiation(probability, outcomes)
    likelihood <- likelihood * columnwiseExponentiation(1 - probability, 1 - outcomes)
    likelihood <- rowProds(likelihood)

    profit <- rep(0, nOutcomes)

    for (ii in 1:nOutcomes) {

        profit[ii] <- profitLoss(1, outcomes[ii, ], odds, bet)

    }

    profitLossDist <- data.frame(likelihood, profit)
    colnames(profitLossDist) <- c("probability","profit_loss")
    profitLossDist <- profitLossDist[order(profit),]
    profitLossDist <- within(profitLossDist, probability <- cumsum(probability))

    return(profitLossDist)

}
