#' @export
getDoublingTime <- function(iGeometricReturn) {

    dt <- log(2) / log(iGeometricReturn)

    return(dt)

}
