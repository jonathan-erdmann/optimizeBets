#' @export
simulateBets <- function(iProbability, iPayouts, iBets, iSimulations) {

    prof <- rep(0, iSimulations)

    for (ii in 1:iSimulations) {

        outcomes <- runif(length(iProbability))
        wins <- outcomes <= iProbability
        loss <- outcomes >  iProbability

        prof[ii] <- sum((1 + iPayouts[wins]) * iBets[wins]) - sum(iBets[loss])

    }

    sb <- NULL
    sb$probability <- 1:iSimulations / iSimulations
    sb$profit <- sort(prof)
    sb$winProbability <- mean(prof > 0)

    return(sb)

}
