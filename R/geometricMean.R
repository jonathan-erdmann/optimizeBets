#' @export
geometricMean <- function(iBet, iBetCard, iExact = FALSE, iSimulations = 1E4, iRandomizeProb = TRUE, iProbSD = 0.01) {

    bets <- iBet
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

                #-- JE EDIT 2022-03-04
                #-- MAKE WIN PROBABILITY VARIABLE
                ifelse(iRandomizeProb, probDelta <- rnorm(length(probability), mean=0, sd=iProbSD), probDelta <- rep(0,length(probability)))

                probDelta <- probability + probDelta
                wins <- outcomes <= probDelta

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
