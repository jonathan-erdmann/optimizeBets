#' @export
twoWayOdds <- function(iOdds) {

    odds <- iOdds
    iiMax <- length(iOdds) - 1
    jjMax <- length(iOdds)

    for (ii in 1:iiMax) {

        jjMin <- ii + 1

        for (jj in jjMin :jjMax) {

            odds <- c(odds, parlayOdds(c(odds[ii],odds[jj])))

        }

    }

    return(odds)

}

