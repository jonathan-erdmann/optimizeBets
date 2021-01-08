#' @export
profitLoss <- function(iCapital, iResult, iOdds, iBets) {

	profitLoss <- iCapital - sum(iBets) + sum( iResult * iBets * (1 + iOdds) )

    return(profitLoss)

}
