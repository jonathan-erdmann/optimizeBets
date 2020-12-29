#' @export
kellyBet <-  function (iProbabilities, iPayouts) {

    q <- 1 - iProbabilities
    f <- (iPayouts * iProbabilities - q) / iPayouts

    return(f)

}

