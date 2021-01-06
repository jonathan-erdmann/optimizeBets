#' @export
moneyLineToProbability <- function(iMoneyLine) {

    ml <- iMoneyLine[abs(iMoneyLine) >= 100]
    ip <- ifelse(ml > 0, ml / 100, -100 / ml)
    ip <- 1 / (1 + ip)

    return(ip)

}
