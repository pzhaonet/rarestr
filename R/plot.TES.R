#' Plot fitted curve for TES
#'
#' @param TES_output the output from TES()
#'
#' @return a plot
#'
#' @examples
plot_tes <- function(TES_output){
  TESa <- TES_output$TESa
  TESb <- TES_output$TESb
  oldpar <- par(mfrow = c(1, 2),mgp=c(2.5,1,0),las=1,mar=c(4,4,2,1))
  if (is.na(TESa$par[1])) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main="(a)")
    text(1, 1, 'NA')
  } else {
    plot(x=TESa$result$Logm,y=TESa$result$value,xlim=c(0,2*TESa$xmax),ylim=c(0,1.2*as.numeric(TESa$par[1])), xlab = "ln(m)", ylab = "Value", main="(a)")
    lines(TESa$Predx, TESa$Predy, col="red")
    abline(h=TESa$par[1],lty=2)
  }

  if (is.na(TESb$par[1])) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, 'NA')
  } else {
    plot(x=TESb$result$Logm,y=TESb$result$value,xlim=c(0,2*TESb$xmax),ylim=c(0,1.2*as.numeric(TESb$par[1])), xlab = "ln(m)", ylab = "Value", main="(b)")
    lines(TESb$Predx, TESb$Predy, col="red")
    abline(h=TESb$par[1],lty=2)
  }
  par(oldpar)
}
