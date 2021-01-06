#' @export
geometricMeanParlay <- function(iBets, iProb, iPays) {

    nGames <- length(iProb)
    outcomes <- binaryOutcomes(nGames)

    probability <- iProb
    odds <- iPays

    likelihood <- columnwiseExponentiation(probability, outcomes)
    likelihood <- likelihood * columnwiseExponentiation(1 - probability, 1 - outcomes)
    likelihood <- rowProds(likelihood)

    outcomes <- twoWayOutcomes(outcomes)
    odds <- twoWayOdds(odds)

    payouts <- (outcomes %*% (odds * iBets)) - (1 - outcomes) %*% iBets

    if (min(payouts) > -1) {

        logPayouts <- log(1 + payouts)
        gm <- sum(likelihood %*% logPayouts)

    } else {

        #gm <- -Inf
        gm <- -1E12

    }

    return(gm)

}
