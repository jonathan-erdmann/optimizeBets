#' @export
geometricMean <- function(iBetCard, iExact = TRUE, iSimulations = 1E4) {

    bets <- iBetCard$bets
    probability <- iBetCard$probability
    odds <- iBetCard$odds

    if (iExact) {

        nGames <- length(bets)
        outcomes <- binaryOutcomes(nGames)

        likelihood <- columnwiseExponentiation(probability, outcomes)
        likelihood <- likelihood * columnwiseExponentiation(1 - probability, 1 - outcomes)
        likelihood <- rowProds(likelihood)

        payouts <- (outcomes %*% (odds * bets)) - (1 - outcomes) %*% bets

        if (min(payouts) > -1) {

            logPayouts <- log(1 + payouts)
            gm <- sum(likelihood %*% logPayouts)

        } else {

            #gm <- -Inf
            gm <- -1E12

        }

    } else {

        set.seed(2)
        prof <- rep(0, iSimulations)

        if (sum(bets) >= 1) {

            gm <- -1E12

        } else {

            for (ii in 1:iSimulations) {

                outcomes <- runif(length(probability))
                wins <- outcomes <= probability

                pl <- profitLoss(1, wins, odds, bets)

                if (pl > 0) {

                    prof[ii] <- log(pl)

                } else {

                    prof[ii] <- -1E12

                }

            }

            gm <- mean(prof)

        }

    }

    return(gm)

}
