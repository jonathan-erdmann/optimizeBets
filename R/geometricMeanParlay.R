#' @export
geometricMeanParlay <- function(iBetCard) {

    bets <- iBetCard$bets
    probability <- iBetCard$probability
    odds <- iBetCard$odds

    nGames <- length(bets)
    outcomes <- binaryOutcomes(nGames)

    likelihood <- columnwiseExponentiation(probability, outcomes)
    likelihood <- likelihood * columnwiseExponentiation(1 - probability, 1 - outcomes)
    likelihood <- rowProds(likelihood)

    outcomes <- twoWayOutcomes(outcomes)
    odds <- twoWayOdds(odds)

    payouts <- (outcomes %*% (odds * bets)) - (1 - outcomes) %*% bets

    if (min(payouts) > -1) {

        logPayouts <- log(1 + payouts)
        gm <- sum(likelihood %*% logPayouts)

    } else {

        #gm <- -Inf
        gm <- -1E12

    }

    return(gm)

}
