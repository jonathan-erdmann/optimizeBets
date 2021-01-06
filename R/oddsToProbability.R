#' @export
oddsToProbability <- function(iOdds) {

    probability <- 1 / (iOdds + 1)

    return(probability)

}

