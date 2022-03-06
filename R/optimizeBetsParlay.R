#' @export
optimizeBetsParlay <- function (iBetCard) {

    probability <- iBetCard$probability
    odds <- iBetCard$odds

    nBets <- length(iProbability)
    nBets <- nBets + nBets * (nBets - 1) / 2
    initBets <- runif(nBets, min = 1, max = 10) / 1000
    optimFunc <- function(x) -geometricMeanParlay(x, probability, odds)

    res <- optim(
             initBets
            ,optimFunc
            ,lower = rep(1E-8, nBets)
            ,upper = rep(1 - 1E-8, nBets)
            ,method = "L-BFGS-B"
        )

    res$bets <- rep(0, length(probability))
    res$bets <- res$par
    res$par <- NULL
    res$totalBet <- sum(res$bets)
    res$individualKelly <- kellyBet(probability, odds)
    res$individualExp <- probability * (odds + 1) - (1 - probability)
    res$probability <- probability
    res$odds <- odds
    res$value <- -res$value

    return(res)

}
