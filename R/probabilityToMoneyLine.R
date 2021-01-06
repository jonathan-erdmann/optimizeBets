#' @export
probabilityToMoneyLine <- function(iProbability) {

    moneyLine <- ifelse(iProbability > 0.5, -100 / (1 / iProbability - 1),   100 * (1 / iProbability - 1))

    return(moneyLine)

}
