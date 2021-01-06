#' @export
twoWayProbability <- function(iProbability) {

    probability <- iProbability
    iiMax <- length(probability) - 1
    jjMax <- length(probability)

    for (ii in 1:iiMax) {

        jjMin <- ii + 1

        for (jj in jjMin:jjMax) {

            prob <- probability[ii] * probability[jj]

            probability <- c(probability, prob)

        }

    }

    return(probability)

}

