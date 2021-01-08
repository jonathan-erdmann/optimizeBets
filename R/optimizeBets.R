#' @export
optimizeBets <- function(iBetInput) {

    #Input Data Frame w/ Columns
    #   Bet Name : name
    #   Money Line : moneyLine
    #   Probability : probability

    odds <- moneyLineToOdds(iBetInput$moneyLine)
    probability <- iBetInput$probability

    #Eliminate non-viable bets
    viableBets <- probability * (odds + 1) - 1 > 0
    viableProb <- probability[viableBets]
    viablePays <- odds[viableBets]

    nBets <- length(viableProb)
    initBets <- rep(1E-8, nBets)
    optimFunc <- function(x) -geometricMean(x, viableProb, viablePays)

    res <- optim(
             initBets
            ,optimFunc
            ,lower = rep(1E-8, nBets)
            ,upper = rep(1 - 1E-8, nBets)
            ,method = "L-BFGS-B"
        )

    res$bets <- rep(0, length(probability))
    res$bets[viableBets] <- res$par

    optimalBets <- data.frame(iBetInput$name, res$bets)
    colnames(optimalBets) <- c("name","bet")

    return(optimalBets)

}
