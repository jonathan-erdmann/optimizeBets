#' @export
optimizeBetsParlay <- function (iProbabilities, iPayouts) {

    nBets <- length(iProbabilities)
    nBets <- nBets + nBets * (nBets - 1) / 2
    initBets <- runif(nBets, min = 1, max = 10) / 1000
    optimFunc <- function(x) -geometricMeanParlay(x, iProbabilities, iPayouts)

    res <- optim(
             initBets
            ,optimFunc
            ,lower = rep(1E-8, nBets)
            ,upper = rep(1 - 1E-8, nBets)
            ,method = "L-BFGS-B"
        )

    res$bets <- rep(0, length(iProbabilities))
    res$bets <- res$par
    res$par <- NULL
    res$totalBet <- sum(res$bets)
    res$individualKelly <- kellyBet(iProbabilities, iPayouts)
    res$individualExp <- iProbabilities * (iPayouts + 1)
    res$probability <- iProbabilities
    res$payouts <- iPayouts
    res$value <- -res$value

    return(res)

}
