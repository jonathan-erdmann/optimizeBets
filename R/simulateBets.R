#' @export
simulateBets <- function(iProbability, iPayouts, iBets, iSimulations) {

    prof <- rep(0, iSimulations)

    for (ii in 1:iSimulations) {

        outcomes <- runif(length(iProbability))
        wins <- outcomes <= iProbability

        prof[ii] <- profitLoss(1, wins, iPayouts, iBets) - 1

    }

    sb <- NULL
    probability <- 1:iSimulations / iSimulations
    profit <- sort(prof)

    sb <- data.frame(probability, profit)

    return(sb)

}
