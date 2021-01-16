#' @export
plotProfitLossDist <- function(iBetCard, iPoints = 1000) {

    probability <- iBetCard$probability
    odds <- iBetCard$odds
    bet <- iBetCard$bet

    pnl <- simulateBets(probability, odds, bet, iPoints)

    if (iPoints > 1000) {

        g <- ggplot(pnl, aes(x = probability, y = profit)) + geom_line()

    } else {

        g <- ggplot(pnl, aes(x = probability, y = profit)) + geom_point()

    }

    g <- g + ylim(c(-1, 2))
    g <- g + ggtitle("") + xlab("Cumulative Probability") + ylab("Profit/Loss")
    g <- g + theme_minimal()

    plot(g)

}
