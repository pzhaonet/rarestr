#' Plot the "rarestr" class
#'
#' @param x a "rarestr" object
#' @param ... other arguments passed to plot()
#'
#' @return Plot the "rarestr" class

#' @export
#' @examples
#' data(share, package = 'rarestR')
#' Output_tes <- tes(share[1,])
#' Output_tes
#' plot(Output_tes)
plot.rarestr <- function(x, ...) {
  if (nrow(x$tbl) == 1) {
    plot_tess(x, ...)
  }
  if (nrow(x$tbl) > 1) {
    plot_tes(x, ...)
  }
}
