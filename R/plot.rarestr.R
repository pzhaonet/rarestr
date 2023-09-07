#' Plot fitted curve for TES and TESS
#'
#' @param x the output from tes() or tess()
#'
#' @return a plot
#' @export
#'
#' @examples
plot.rarestr <- function(x) {
  if (nrow(x$tbl) == 1) {
    plot_tess(x)
  }
  if (nrow(x$tbl) > 1) {
    plot_tes(x)
  }
}
