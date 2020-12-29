#' @export
geometricMean <- function(iBets, iProb, iPays) {

    nGames <- length(iBets)
    outcomes <- binaryOutcomes(nGames)

    likelihood <- columnwiseExponentiation(iProb, outcomes)
    likelihood <- likelihood * columnwiseExponentiation(1 - iProb, 1 - outcomes)
    likelihood <- rowProds(likelihood)

    payouts <- (outcomes %*% (iPays * iBets)) - (1 - outcomes) %*% iBets

    if (min(payouts) > -1) {

        logPayouts <- log(1 + payouts)
        gm <- sum(likelihood %*% logPayouts)

    } else {

        #gm <- -Inf
        gm <- -1E12

    }

    return(gm)

}
