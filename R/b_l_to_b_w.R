##' Converting between length- and weight-based size spectrum exponents
##'
##' Will write out the math in a write up, but the equations are:
##' \deqn{b_l = \beta (b_w + 1) - 1}
##' and
##' \deqn{b_w = (b_l + 1) / \beta - 1}
##' @param b_l exponent for a length-based individual size spectrum
##' @param beta exponent of the length-weight relationship for the species in
# question
##' @return the `b_w` exponent for the weight-based individual size spectrum
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' b_weight <- b_l_to_b_w(-5.1, beta = 3)
##' }
b_l_to_b_w <- function(b_l,
                       beta = 3.1802){
 (b_l + 1) / beta - 1
}

##' @rdname b_l_to_b_w
##' @param b_w exponent for a weight-based individual size spectrum
##' @return the `b_l` exponent for the length-based individual size spectrum
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' b_length <- b_w_to_b_l(-2, beta = 3)
##' }
b_w_to_b_l <- function(b_w,
                       beta = 3.1802){
  beta * (b_w + 1) - 1
}
