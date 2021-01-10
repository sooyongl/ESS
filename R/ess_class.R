#' @include data_ready.r
NULL

#' Class 'ess': results of ESS
#'
#' \code{\linkS4class{examinee}} is an S4 class to represent a single examinee.
#'
#' @slot information a list containing interim thetas and SEs in each module position.
#' @slot tab0 a list containing administered items in each module position.
#' @slot tab1 a list containing administered stimuli in each module position.
#' @slot tab2 a list containing the examinee response in each module position.
#' @slot tab3 a list containing \code{\linkS4class{item_pool}} of administered items.
#' @slot tab4 a vector containing the routing was based on \code{estimated_theta_by_phase} or {estimated_theta_by_test} at each module position.
#' @export
setClass("ess",
  slots = c(
    information = "list",
    tab0        = "list",
    tab1        = "list",
    tab2        = "list",
    tab3        = "list",
    tab4        = "list",
    tab5        = "list"
  ),
  prototype = list(
    information = list(),
    tab0        = list(),
    tab1        = list(),
    tab2        = list(),
    tab3        = list(),
    tab4        = list(),
    tab5        = list()
  ),
  validity = function(object) {
    if (!is.list(object@information)) {
      stop("@informatino must be a type of list")
    }
    return(TRUE)
  }
)
