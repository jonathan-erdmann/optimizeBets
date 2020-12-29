#' @export
kellyBet <-  function (iProbabilities, iPayouts) {

    q <- 1 - iProbabilities
    f <- (iPayouts * iProbabilities - q) / iPayouts

    if (f > 0) {

        return(f)

    } else {

        return(0)

    }

}

