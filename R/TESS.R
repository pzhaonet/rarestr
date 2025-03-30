#' Calculate the Total number of Expected Shared Species between two samples.
#'
#' @importFrom Rdpack reprompt
#' @importFrom stats deviance nls predict
#' @param x a data matrix for two samples representing two communities (plot x species)
#' @return a list in a self-defined class 'rarestr'. See "Details".
#' @details
#' The value returned by the \code{tess()} function in the 'rarestr' class is a list containing five parts:
#' \describe{
#'   \item{par}{A data frame of the summary of the estimated values and their standard deviations based on TESa, TESb, and TESab, and the model used in the estimation of TES, either 'logistic' or 'Weibull'.}
#'   \item{result}{A data frame of the simulated results.}
#'   \item{xmax}{A maximum x value.}
#'   \item{Predx}{A vector of the predicted x value.}
#'   \item{Predy}{A vector of the predicted y value.}
#' }
#' @references \insertRef{zou2025}{rarestR}
#' @export
#' @examples
#' data(share, package = 'rarestR')
#' Output_tess <- tess(share[1:2,])
#' Output_tess
tess <- function (x) {
    x <- as.matrix(x)
    knots <- 40
    if (nrow(x) != 2){
      stop("TESS only works for two samples")
      }
    if (any(is.na(x))) {
      x[is.na(x)] <- 0
      warning("empty data were replaced by '0' values")
      }
    if(!identical(all.equal(as.integer(x),  as.vector(x)), TRUE)) {
      warning("results may be meaningless with non-integer data in the method")
    }
    if (any(x < 0, na.rm = TRUE)) {
      warning("results may be meaningless because data have negative entries in the method")
    }
    nm <- seq(from = 1, to = log(min(rowSums(x))), length = knots)
    fm <- unique(floor(exp(nm)))

    result <- data.frame(Dst = sapply(fm, function(fm) ess(x, m = fm,index = "ESS")),
                         Logm = log(fm))

    a <- NA #Set a=NA if there is insufficient data to do the modelling
    Error_four <- FALSE #Set Error_four as FALSE
    r.sq <- NA

    parameter="Weibull"
    tryCatch({
      md <- nls(Dst ~ SSweibull(Logm, Asym, Drop, lrc, pwr), data = result)
      Coe <- summary(md)$coefficients
      a <- Coe[1,1]
      s.d <- sqrt(Coe[1, 2]^2*(nrow(result) - 4))
      b <- Coe[2, 1]
      c <- exp(Coe[3, 1])
      d <- Coe[4, 1]
      xmax <-  (-(log(0.1 * a/b)) / c) ^ (1 / d) #The 1/2 max value of x axis in plotting, at the value of y=0.9*a
      r.sq <- 1 - (deviance(md) / sum((result$Dst - mean(result$Dst)) ^ 2))
    },
    error = function(e) Error_four <<- TRUE) #Assign TRUE to Error_four
    if (Error_four) { #If users ask for three parameter model, or if an error occur for four parameter model when users ask for "auto", then run three parameter model
        parameter <- "logistic"
        tryCatch({
          md <- nls(Dst ~ SSlogis(Logm, Asym, xmid, scal), data = result)
          Coe <- summary(md)$coefficients
          a <- Coe[1, 1]
          s.d <- sqrt(Coe[1, 2] ^ 2 * (nrow(result) - 3))
          xmax <-  1.8 * Coe[2, 1]
          r.sq <- 1 - (deviance(md) / sum((result$Dst - mean(result$Dst)) ^ 2))
        },
        error = function(e) parameter  <<- NA)
    }

    if (!is.na(xmax)){
      Predx <- seq(0, 2 * xmax, length = 1000)
      Predy <- predict(md, list(Logm = Predx))
      attr(Predy, 'gradient') <- NULL
      lst <- list(tbl = data.frame(est = round(a, 2), est.sd = round(s.d, 2),model.par = parameter),
                  result = result,
                  xmax = xmax,
                  Predx = Predx,
                  Predy = Predy)
    } else {
      lst <- list(tbl = data.frame(est=round(a, 2), est.sd = round(s.d, 2), model.par = parameter),
                  result = result)
    }
    class(lst) <- "rarestr"
    if (is.na(a)) warning("Fail to provide reliable estimators and associated s.e due to insufficient data or mismatched distribution")
    return(lst)
}
