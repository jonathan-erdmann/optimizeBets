#' @export
moneyLineToOdds <- function(iMoneyLine) {

    ml <- iMoneyLine[abs(iMoneyLine) >= 100]
    po <- ifelse(ml > 0, ml / 100, -100 / ml)

    return(po)

}
