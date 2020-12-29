#' @export
columnwiseExponentiation <- function(iBase, iExponent) {

    nBase <- length(iBase)
    ce <- iExponent

    for (column in 1:nBase) {

        base <- iBase[column]
        exponent <- iExponent[,column]

        ce[,column] <- base ^ exponent

    }

    return(ce)

}


