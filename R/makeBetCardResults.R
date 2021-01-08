#' @export
makeBetCardResults <- function(iBetCard, iBetResults) {

   betCardResults <- iBetCard %>% left_join(iBetResults, by = "name")

    return(betCardResults)

}
