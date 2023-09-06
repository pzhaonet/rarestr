#' Calculate the Total number of Expected Shared Species between two samples.
#'
#' @param x a data matrix for two samples representing two communities (plot x species)
#' @param knots specifies the number of separate sample sizes of increasing value used for the calculation of ESS between 1 and the endpoint, which by default is set to knots=40
#' @param plot a logical value that provides the fitted curve
#' @param method the method the model used, with three options available as "auto", "logistic" and "weibull", with the default set as "auto"

#' @return The asymptotic value (a), the model used in the estimation of TESS (either "Three" or "Four") and the model fit (r.sq).
#' @export
#'
#' @examples
tess <- function (x,knots=40,plot=FALSE)
{
    x <- as.matrix(x)
    if (nrow(x)!=2){warning("TESS only works for two samples");break}
    if (any(is.na(x) == TRUE))
    {x [is.na(x)] <- 0; warning("empty data were replaced by '0' values")}
    if(!identical(all.equal(as.integer(x),  as.vector(x)), TRUE))
        warning("results may be meaningless with non-integer data in method")
    if (any(x < 0, na.rm = TRUE))
        warning("results may be meaningless because data have negative entries in method")

    nm <- seq(from=1,to=log(min(rowSums(x))),length=knots)
    fm <- unique(floor(exp(nm)))

    result <- data.frame(Dst = sapply(fm, function(fm) ESS(x,m=fm,index = "ESS")),
               Logm=log(fm))

    a <- NA#Set a=NA if there is insufficient data to do the modelling
    Error_four <- FALSE #Set Error_four as FALSE
    r.sq <- NA

    parameter="Weibull"
        tryCatch({
                  md <- nls(Dst ~ SSweibull(Logm, Asym, Drop, lrc, pwr),data=result)
                  Coe <- summary(md)$coefficients
                  a <- Coe[1,1]
                  s.d <- sqrt(Coe[1,2]^2*(nrow(result)-4))
                  b <- Coe[2,1]
                  c <- exp(Coe[3,1])
                  d <- Coe[4,1]
                  xmax <-  (-(log(0.1*a/b))/c)^(1/d)#The 1/2 max value of x axis in plotting, at the value of y=0.9*a
                 r.sq <- 1 - (deviance(md)/sum((result$Dst-mean(result$Dst))^2))#Model fit
                 },
                 error  = function(e){Error_four  <<- TRUE}) #Assign TRUE to Error_four

    if(Error_four==TRUE) #If users ask for three parameter model, or if an error accur for four prarmeter model when user ask for "auto", then run three parameter
    {
        parameter="logistic"
        tryCatch({
                  md <- nls(Dst~SSlogis(Logm, Asym, xmid, scal),data= result)
                  Coe <- summary(md)$coefficients
                  a <- Coe[1,1]
                  s.d <- sqrt(Coe[1,2]^2*(nrow(result)-3))
                  xmax <-  1.8*Coe[2,1]
                  r.sq <- 1 - (deviance(md)/sum((result$Dst-mean(result$Dst))^2))#Model fit
                  },
                 error  = function(e){parameter  <<- NA })
    }
    if (is.na(a)==TRUE) {z <- list(a=a, Model.par = parameter)
                         warning("Insufficient data to provide reliable estimators and associated s.e.")}else
                         {
                             if(plot==TRUE)
                             {
                                 with(result,plot(x=Logm,y=Dst,
                                                  xlim=c(0,2*xmax),
                                                  ylim=c(0,1.2*a),
                                                  ylab="ESS",
                                                  xlab="ln(m)",bty='n'))
                                 Pred <- seq(0, 2*xmax, length = 1000)
                                 lines(Pred,
                                       predict(md, list(Logm = Pred )),
                                       col="red")
                             }
                             z <- list(a=a,a.sd = s.d,Model.par = parameter,r.sq=r.sq)
                         }
    return(z)
}

