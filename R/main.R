#' Print "foo"
#'
#' Prints the string \code{"foo"} to the console. A minimal placeholder
#' function, useful as a stub or for testing.
#'
#' @return Invisibly returns \code{NULL}. Called for its side effect of
#'   printing to the console.
#' @export
#' @importFrom ggplot2 ggplot theme_bw geom_point aes
#' @examples
#' foo()

foo <- function() {
  
  # deal with no visible binding issue
  x <- NULL
  
  data.frame(x <- 1:10) |>
    ggplot() + theme_bw() + geom_point(aes(x=x, y=1))
  
}



#' Detect Probability of at Least One Success
#'
#' Calculates the probability of observing at least one "success" (or event)
#' across \code{n} independent trials, given a per-trial probability \code{p}.
#' This is often used in power/detection analysis to estimate the likelihood
#' of detecting a rare event given repeated sampling.
#'
#' @param n Integer. The number of independent trials or samples.
#' @param p Numeric. The probability of success (or detection) on a single
#'   trial, between 0 and 1.
#'
#' @return A numeric value (or vector) giving the probability of detecting
#'   at least one success across \code{n} trials.
#'
#' @details
#' The probability of \emph{not} detecting a success in a single trial is
#' \code{1 - p}. Assuming independence across trials, the probability of
#' no successes in \code{n} trials is \code{(1 - p)^n}. Subtracting this
#' from 1 gives the probability of at least one success.
#'
#' @examples
#' # Probability of detecting at least one event in 10 trials with p = 0.1
#' power_detect(n = 10, p = 0.1)
#'
#' # Vectorized over multiple values of n
#' power_detect(n = c(5, 10, 20), p = 0.05)
#'
#' @export
power_detect <- function(n, p) {
  1 - (1-p)^n
}

# *afsljsaf*

#' `afjadlfkja` *faslkf*
#####


####
