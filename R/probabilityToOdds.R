#' @export
probabilityToOdds <- function(iProbability) {

    odds <- 1/iProbability - 1

    return(odds)

}

