#' @export
profitLossDistribution <- function(iProbability, iOdds, iBets) {

    # Input: probability, payout odds, and bets
    # Output: profit/loss distribution

    nGames <- length(iProbability)
    outcomes <- binaryOutcomes(nGames)
    nOutcomes <- nrow(outcomes)

    likelihood <- columnwiseExponentiation(iProbability, outcomes)
    likelihood <- likelihood * columnwiseExponentiation(1 - iProbability, 1 - outcomes)
    likelihood <- rowProds(likelihood)

    profit <- rep(0, nOutcomes)

    for (ii in 1:nOutcomes) {

        profit[ii] <- profitLoss(1, outcomes[ii, ], iOdds, iBets)

    }

    profitLossDist <- data.frame(likelihood, profit)
    colnames(profitLossDist) <- c("probability","profit")
    profitLossDist <- profitLossDist[order(profit),]
    profitLossDist <- within(profitLossDist, probability <- cumsum(probability))

    return(profitLossDist)

}
