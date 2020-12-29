#' @export
kellyBet <-  function (iProbabilities, iPayouts) {

    q <- 1 - iProbabilities
    f <- (iPayouts * iProbabilities - q) / iPayouts
    f <- ifelse(f > 0, f, 0)

    return(f)

}

