#' @export
oddsToMoneyLine <- function(iOdds) {

    moneyLine <- ifelse(iOdds < 1, -100 / iOdds, 100 * iOdds)

    return(moneyLine)

}


