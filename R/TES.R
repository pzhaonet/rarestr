#' Calculation of Total Expected Species base on ESa, ESb and their average value
#'
#' @importFrom Rdpack reprompt
#' @importFrom stats nls predict
#' @param x a data vector representing number of individuals for each species
#' @return a list in a self-defined class 'rarestr'. See "Details".
#' @details
#' The value returned by the \code{tes()} function in the 'rarestr' class is a list containing three parts:
#' \describe{
#'   \item{par}{A data frame of the summary of the estimated values and their standard deviations based on TESa, TESb, and TESab, and the model used in the estimation of TES, either 'logistic' or 'Weibull'.}
#'   \item{TESa}{A list of the modeled results with the TESa method.}
#'   \item{TESb}{A list of the modeled results with the TESb method.}
#' }
#' Both TESa and TESb contain five parts, including a data frame of the parameters (\code{$par}), a data frame of the simulated results (\code{$result}), a maximum x value (\code{$xmax}), a vector of the predicted x value (\code{$Predx}), and a vector of the predicted y value (\code{$Predy})
#'
#' @references \insertRef{zou2023}{rarestR}
#' @export
#' @examples
#' data(share, package = 'rarestR')
#' Output_tes <- tes(share[1,])
#' Output_tes
tes <- function(x){
  knots <- 40
  TESab <- function (x, knots = 40, method = c("a","b")){
    method <- match.arg(method, c("a", "b"))
    if (all(dim(as.matrix(x)) != 1)) {
      stop("TES only works for one sample")
    }
    x <- as.vector(x)
    if (any(x < 0, na.rm = TRUE)) {
      stop("data have negative entries")
    }
    if (any(is.na(x))) {
      x[is.na(x)] <- 0
      warning("empty data were replaced by '0' values")
    }
    if (!identical(all.equal(as.integer(x), as.vector(x)), TRUE)) {
      warning("results may be meaningless with non-integer data in the method")
    }

    nm <- seq(from = 1, to = log(sum(x)), length = knots)
    fm <- unique(floor(exp(nm)))

    result <- data.frame(value = sapply(fm, function(fm) es(x, m = fm, method = method)),
                         Logm = log(fm))

    a <- NA #Set a=NA if there is insufficient data to do the modelling
    Error_four <- FALSE #Set Error_four as FALSE
    xmax <- NA

    parameter='Weibull'
    tryCatch({
      md <- nls(value ~ SSweibull(Logm, Asym, Drop, lrc, pwr), data = result) #Use selfStart model evaluates the Weibull model for growth curve data and its gradient. Logm is the "x" value at which to evaluate the model, while Asym, Drop, lrc, and pwr are model parameters. Model expression: Asym-Drop*exp(-exp(lrc)*x^pwr)
      Coe <- summary(md)$coefficients
      a <- Coe[1, 1]
      s.d <- sqrt(Coe[1, 2]^2 * (nrow(result) - 4))
      b <- Coe[2, 1]
      c <- exp(Coe[3, 1])
      d <- Coe[4, 1]
      xmax <-  (-(log(0.1 * a/b)) / c)^(1 / d)
      }, #The 1/2 max value of x axis in plotting, at the value of y=0.9*a
      error  = function(e){Error_four <<- TRUE}
    ) #Assign TRUE to Error_four

    if (Error_four) { #If an error occur for four parameter model, then run three parameter model
      parameter <- 'logistic'
      tryCatch({
        md <- nls(value~SSlogis(Logm, Asym, xmid, scal), data = result) #Use selfStart model evaluates the logistic function and its gradient. Logm is the "x" value at which to evaluate the model, while Asym, xmid, and scal are model parameters. Model expression: Asym/(1+exp((xmid-x)/scal))
        Coe <- summary(md)$coefficients
        a <- Coe[1, 1]
        s.d <- sqrt(Coe[1, 2]^2*(nrow(result) - 3))
        xmax <-  1.8 * Coe[2, 1]},
        error = function(e){parameter <<- NA
        })
    }
    if (is.na(a)) {
      s.d <- NA
      warning("Fail to provide reliable estimators and associated s.e due to insufficient data or mismatched distribution")
    }
    if (!is.na(xmax)){
      Predx <- seq(0, 2 * xmax, length = 1000)
      Predy <- predict(md, list(Logm = Predx))
      attr(Predy, 'gradient') <- NULL
      z <- list(par = c(est = round(a, 2), est.sd = round(s.d, 2), model.par = parameter),
                result = result,
                xmax = xmax,
                Predx = Predx,
                Predy = Predy)
    } else {
      z <- list(par = c(est=a, est.sd = s.d,model.par = parameter),
                result = result)
    }
    return(z)
  }
  TESa <- TESab(x, knots = knots, method="a")
  TESb <- TESab(x, knots = knots, method="b")
  tbl <- as.data.frame(rbind(TESa = TESa$par, TESb = TESb$par))
  tbl[, 1:2] <- apply(tbl[, 1:2], 1:2, as.numeric)
  tbl[3, 1] <- round(mean(tbl[, 1]), 2)
  tbl[3, 2] <- round((sqrt(tbl[1, 2] ^ 2 + tbl[2, 2] ^ 2))/2, 2)
  rownames(tbl)[3] <- 'TESab'
  lst <- list(tbl = tbl,
              TESa = TESa,
              TESb = TESb)
  class(lst) <- "rarestr"
  return(lst)
}

#' Print the "rarestr" class
#'
#' @description This function prints the contents of a rarestr object.
#'
#' @param ... Other arguments passed to print().
#' @param x a "rarestr" object#'
#'
#' @return Print the "rarestr" class
#' @export
#' @examples
#' data(share, package = 'rarestR')
#' Output_tes <- tes(share[1,])
#' Output_tes
print.rarestr <- function(x, ...) {
  print(x[[1]], ...)
}
