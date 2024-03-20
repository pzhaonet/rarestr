#' Plot fitted curve for TESS
#'
#' @importFrom graphics abline lines par
#' @param TESS_output the output from tess()
#' @param ... other arguments passed to plot()
#' @return a plot
plot_tess <- function(TESS_output, ...) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mgp = c(2.5,1,0), las = 1, mar = c(4,4,2,1))
  with(
    TESS_output$result,
    plot(
      x = Logm,
      y = Dst,
      xlim = c(0, 2 * TESS_output$xmax),
      ylim = c(0, 1.2 * TESS_output$tbl$est),
      ylab = "ESS",
      xlab = "ln(m)",
      ...
    )
  )
  lines(TESS_output$Predx,
        TESS_output$Predy,
        col = "red")
  abline(h = TESS_output$tbl[1], lty = 2)
}
