#' @export
optimizeBets <- function (iProbabilities, iPayouts) {

    #Eliminate non-viable bets
    viableBets <- iProbabilities * (iPayouts + 1) - 1 > 0
    viableProb <- iProbabilities[viableBets]
    viablePays <- iPayouts[viableBets]

    nBets     <- length(viableProb)
    initBets  <- runif(nBets, min = 1, max = 10) / 1000
    optimFunc <- function(x) -geometricMean(x, viableProb, viablePays)

    res <- optim(
             initBets
            ,optimFunc
            ,lower = rep(1E-8, nBets)
            ,upper = rep(1 - 1E-8, nBets)
            ,method = "L-BFGS-B"
        )

    res$viableBets        <- viableBets
    res$individualKelly   <- kellyBet(iProbabilities, iPayouts)
    res$individualExp     <- iProbabilities * (iPayouts + 1)
    res$probability       <- iProbabilities
    res$payouts           <- iPayouts
    res$viableProbability <- viableProb
    res$viablePays        <- viablePays
    res$value             <- -res$value

    return(res)

}

