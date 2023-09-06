#' Calculation of Total Expected Species base on ESa, ESb and their average value
#'
#' @param x a data vector representing number of individuals for each species
#' @param knots specifies the number of separate sample sizes of increasing value used for the calculation of ES between 1 and the endpoint, which by default is set to knots=40
#'
#' @return a list, which contains a table of the summary of the estimated values and their standard deviations based on TESa, TESb, and TESab, and the model used in the estimation of TES, either 'logistic' or 'Weibull'
#' @export
#'
#' @examples
tes <- function(x,knots=40){
  TESab <- function (x,knots=40,method=c("a","b"))
  {
    method <- match.arg(method, c("a", "b"))
    x <- as.vector(x)
    if (any(x < 0, na.rm = TRUE))
    {warning("data have negative entries");break}
    if (any(is.na(x)))
    {x [is.na(x)] <- 0; warning("empty data were replaced by '0' values")}
    if(!identical(all.equal(as.integer(x),  as.vector(x)), TRUE))
      warning("results may be meaningless with non-integer data in method")

    nm <- seq(from=1,to=log(sum(x)),length=knots)
    fm <- unique(floor(exp(nm)))

    result <- data.frame(value = sapply(fm, function(fm) ES(x,m=fm,method = method)),
                         Logm=log(fm))

    a <- NA#Set a=NA if there is insufficient data to do the modelling
    Error_four <- FALSE #Set Error_four as FALSE
    xmax <- NA

    parameter='Weibull'
    tryCatch(
      {md <- nls(value ~ SSweibull(Logm, Asym, Drop, lrc, pwr),data=result) #Use selfStart model evaluates the Weibull model for growth curve data and its gradient. Logm is the "x" value at which to evaluate the model, while Asym, Drop, lrc, and pwr are model parameters. Model expression: Asym-Drop*exp(-exp(lrc)*x^pwr)
      Coe <- summary(md)$coefficients
      a <- Coe[1,1]
      s.d <- sqrt(Coe[1,2]^2*(nrow(result)-4))
      b <- Coe[2,1]
      c <- exp(Coe[3,1])
      d <- Coe[4,1]
      xmax <-  (-(log(0.1*a/b))/c)^(1/d)},#The 1/2 max value of x axis in plotting, at the value of y=0.9*a
      error  = function(e){Error_four  <<- TRUE}
    ) #Assign TRUE to Error_four

    if(Error_four) #If an error occur for four parameter model, then run three parameter model
    {
      parameter='logistic'
      tryCatch({md <- nls(value~SSlogis(Logm, Asym, xmid, scal),data= result) #Use selfStart model evaluates the logistic function and its gradient. Logm is the "x" value at which to evaluate the model, while Asym, xmid, and scal are model parameters. Model expression: Asym/(1+exp((xmid-x)/scal))
      Coe <- summary(md)$coefficients
      a <- Coe[1,1]
      s.d <- sqrt(Coe[1,2]^2*(nrow(result)-3))
      xmax <-  1.8*Coe[2,1]},
      error  = function(e){parameter  <<- NA })
    }
    if (is.na(a))
    {
      s.d <- NA
      warning("Insufficient data to provide reliable estimators and associated s.e.")
    }
    if (!is.na(xmax)){
      Predx <- seq(0, 2*xmax, length = 1000)
      Predy <- predict(md, list(Logm = Predx))
      attr(Predy, 'gradient') <- NULL
      z <- list(par = c(est=round(a, 2), est.sd = round(s.d, 2),model.par = parameter),
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
  TESa <- TESab(x,knots=knots,method="a")
  TESb <- TESab(x,knots=knots,method="b")
  tbl <- as.data.frame(rbind(TESa = TESa$par, TESb = TESb$par))
  tbl[, 1:2] <- apply(tbl[, 1:2], 1:2, as.numeric)
  tbl[3, 1] <- round(mean(tbl[, 1]), 2)
  tbl[3, 2] <- round((sqrt(tbl[1, 2] ^ 2 + tbl[2, 2] ^ 2))/2, 2)
  rownames(tbl)[3] <- 'TESab'
  return(list(summary = tbl,
              TESa = TESa,
              TESb = TESb))
}
