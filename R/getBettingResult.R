#' @export
getBettingResult <- function(iBetCard, iStartingCapital, iVerbose = FALSE) {

    wagered <- iBetCard$bet * iStartingCapital
    odds <- moneyLineToOdds(iBetCard$moneyLine)
    returned <- iBetCard$win_loss * (1 + odds) * wagered

    betCardResult <- data.frame(iBetCard,wagered,returned)

    totalWagered <- sum(wagered)
    totalReturned <- sum(returned)
    remainingCapital <- iStartingCapital - totalWagered
    endingCapital <- remainingCapital + totalReturned
    netGain <- endingCapital - iStartingCapital
    returnOnWager <- 100 * (totalReturned / totalWagered - 1)
    returnOnCapital <- 100 * (endingCapital / iStartingCapital - 1)

    if (iVerbose) {
	    print(noquote(sprintf(" Starting Capital : $%.2f", iStartingCapital)))
	    print(noquote(sprintf("    Total Wagered : $%.2f", totalWagered)))
	    print(noquote(sprintf("Remaining Capital : $%.2f", remainingCapital)))
	    print(noquote(sprintf("   Money Returned : $%.2f", totalReturned)))
	    print(noquote(sprintf("       Proft/Loss : $%.2f", netGain)))
	    print(noquote(sprintf("   Ending Capital : $%.2f", endingCapital)))
	    print(noquote(sprintf("  Return on Wager : %.2f", returnOnWager)))
	    print(noquote(sprintf("Return on Capital : %.2f", returnOnCapital)))
    }

    return(betCardResult)

}
