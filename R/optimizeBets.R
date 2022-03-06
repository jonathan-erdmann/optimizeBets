#' @export
optimizeBets <- function(iBetCard, iExact = FALSE, iRandomize = TRUE, iSD = 0.01) {

    #Input Data Frame w/ Columns
    #   Bet Name : name
    #   Money Line : moneyLine
    #   Probability : probability

    odds <- moneyLineToOdds(iBetCard$moneyLine)
    probability <- iBetCard$probability

    #Eliminate non-viable bets
    viableBets <- kellyBet(probability,odds) > 0
    viableProb <- probability[viableBets]
    viablePays <- odds[viableBets]

    nBets <- length(viableProb)
    initBets <- rep(1E-8, nBets)

    optimFunc <- function(x) -geometricMean(x, iBetCard, iExact, iRandomizeProb = iRandomize, iProbSD = iSD)

    res <- optim(
             initBets
            ,optimFunc
            ,lower = rep(1E-8, nBets)
            ,upper = rep(1 - 1E-8, nBets)
            ,method = "L-BFGS-B"
        )

    res$bets <- rep(0, length(probability))
    res$bets[viableBets] <- res$par

    optimalBets <- data.frame(iBetCard$name, res$bets)
    colnames(optimalBets) <- c("name","bet")

    return(optimalBets)

}
