#' @export
getBettingResult <- function(iBetCard, iStartingCapital) {

    wagered <- iBetCard$bet * iStartingCapital
    odds <- moneyLineToOdds(iBetCard$moneyLine)
    returned <- iBetCard$win_loss * (1 + odds) * wagered

    betCardResult <- data.frame(iBetCard,wagered,iBetCard$win_loss,returned)
    colnames(betCardResult)[colnames(betCardResult) == "iBetCard.win_loss"] <- "win"

    totalWagered <- sum(wagered)
    totalReturned <- sum(returned)
    remainingCapital <- iStartingCapital - totalWagered
    endingCapital <- remainingCapital + totalReturned
    returnOnWager <- 100 * (totalReturned / totalWagered - 1)
    returnOnCapital <- 100 * (endingCapital / iStartingCapital - 1)

    print(noquote(sprintf(" Starting Capital : $%.2f", iStartingCapital)))
    print(noquote(sprintf("    Total Wagered : $%.2f", totalWagered)))
    print(noquote(sprintf("Remaining Capital : $%.2f", remainingCapital)))
    print(noquote(sprintf("   Money Returned : $%.2f", totalReturned)))
    print(noquote(sprintf("   Ending Capital : $%.2f", endingCapital)))
    print(noquote(sprintf("  Return on Wager : %.2f", returnOnWager)))
    print(noquote(sprintf("Return on Capital : %.2f", returnOnCapital)))

    return(betCardResult)

}
