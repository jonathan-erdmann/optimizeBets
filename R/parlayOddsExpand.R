#' @export
parlayOddsExpand <- function(iOdds) {

    odds <- iOdds

    jointProbability <- prod(oddsToProbability(odds))

    parlayOdds <- probabilityToOdds(jointProbability)

    return(parlayOdds)

}

