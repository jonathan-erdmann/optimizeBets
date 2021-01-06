#' @export
moneyLineToOdds <- function(iMoneyLine) {

    ml <- iMoneyLine[abs(iMoneyLine) >= 100]
    odds <- ifelse(ml > 0, ml / 100, -100 / ml)

    return(odds)

}
