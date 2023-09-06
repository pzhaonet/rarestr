#' Plot fitted curve for TESS
#'
#' @param TESS_output the output from tess()
#'
#' @return a plot
#' @export
#'
#' @examples
plot_tess <- function(TESS_output) {
  with(
    TESS_output$result,
    plot(
      x = Logm,
      y = Dst,
      xlim = c(0, 2 * TESS_output$xmax),
      ylim = c(0, 1.2 * TESS_output$tbl$est),
      ylab = "ESS",
      xlab = "ln(m)",
      bty = 'n'
    )
  )
  lines(TESS_output$Predx,
        TESS_output$Predy,
        col = "red")

}
