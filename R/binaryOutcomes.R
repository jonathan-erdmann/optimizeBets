#' @export
binaryOutcomes <- function(n) {

    if (n > 1) {

        nZero    <- 2^n - 1
        binary32 <- matrix(as.integer(intToBits(0:nZero)), nrow = 32)

        return(t(binary32[1:n,]))

    } else {

        return(matrix(c(0,1), ncol = 1))

    }

}

