#' @export
factorBets <- function(iBetCard, iFactor = 1) {

    bc <- iBetCard
    bc$bet <- bc$bet / iFactor

    return(bc)

}
