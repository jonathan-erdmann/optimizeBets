#' @export

#library(matrixStats)

binaryOutcomes <- function(n) {

    if (n > 1) {

        nZero    <- 2^n - 1
        binary32 <- matrix(as.integer(intToBits(0:nZero)), nrow = 32)

        return(t(binary32[1:n,]))

    } else {

        return(matrix(c(0,1), ncol = 1))

    }

}

columnwiseExponentiation <- function(iBase, iExponent) {

    nBase <- length(iBase)
    ce <- iExponent

    for (column in 1:nBase) {

        base <- iBase[column]
        exponent <- iExponent[,column]

        ce[,column] <- base ^ exponent

    }

    return(ce)

}

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
