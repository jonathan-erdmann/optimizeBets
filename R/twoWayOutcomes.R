#' @export
twoWayOutcomes <- function(iOutcomes) {

    outcomes <- iOutcomes
    iiMax <- ncol(iOutcomes) - 1
    jjMax <- ncol(iOutcomes)

    for (ii in 1:iiMax) {

        colStart <- ii + 1

        for (jj in colStart:jjMax) {

            outcomes <- cbind(outcomes, outcomes[,ii] * outcomes[,jj])

        }

    }

    return(outcomes)

}
