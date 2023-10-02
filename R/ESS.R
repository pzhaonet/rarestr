#' Compute dissimilarity estimates between two samples based on Expected Species Shared (ESS)-measures, using abundance data for the species contained in each samples
#'
#' @param x a community data matrix (sample x species); sample name is the row name of the matrix
#' @param m the sample size parameter that represents the number of individuals randomly drawn from each sample, which by default is set to m=1, but can be changed according to the users' requirements. Rows with a total sample size <m will be excluded automatically from the analysis.
#' @param index the distance measure used in the calculation, as one of the four options "CNESSa", "CNESS","NESS" and "ESS", with the default set as "CNESSa"
#'
#' @return a pair-wised matrix
#' @export
#'
#' @examples
ess <- function (x,m=1,index = "CNESSa")
{
    indices <- c("CNESSa", "CNESS","NESS","ESS")
    index <- indices[match(index, indices)]
    if (m<1){stop("m must be a positive value")}
    if (m%%1!=0)warning("results may be meaningless because m is not an integer")
    x <- as.matrix(x)
    if (any(is.na(x) == TRUE))
    {x [is.na(x)] <- 0; warning("empty data were replaced by '0' values")}
    x <- x[,which(apply(x,2,sum)>0)]
    if(!identical(all.equal(as.integer(x),  as.vector(x)), TRUE))
        warning("results may be meaningless with non-integer data in method")
    if (any(x < 0, na.rm = TRUE))
        warning("results may be meaningless because data have negative entries in method")
    if (any(is.na(x) == TRUE)) x <- replace(x,is.na,0)
    Dat <- x[which(apply(x,1,sum)>=m),]
    Nrow <- nrow(Dat)
    Ncol <- ncol(Dat)
    Matrix <- matrix(nrow=Nrow,ncol=Nrow,data=0)
    for  (i in 1:Nrow)
    {
        for (j in 1: (i-1))
        {   if(j==0){next}
            Ni <- sum(Dat[i,])
            Nj <- sum(Dat[j,])
            ESSii <- sum((1 - exp(lchoose(Ni - Dat[i,], m)- lchoose(Ni, m)))*
                             (1 - exp(lchoose(Ni - Dat[i,], m)- lchoose(Ni, m))))
            ESSij <- sum((1 - exp(lchoose(Ni - Dat[i,], m)- lchoose(Ni, m)))*
                             (1 - exp(lchoose(Nj - Dat[j,], m)- lchoose(Nj, m))))
            ESSjj <- sum((1 - exp(lchoose(Nj - Dat[j,], m)- lchoose(Nj, m)))*
                             (1 - exp(lchoose(Nj - Dat[j,], m)- lchoose(Nj, m))))
            if (index == "CNESSa"){valueij <- sqrt(1-ESSij/sqrt(ESSii*ESSjj))}
            if (index == "CNESS"){valueij <- sqrt(2*(1-ESSij/sqrt(ESSii*ESSjj)))}
            if (index == "NESS"){valueij <- 2*ESSij/(ESSii+ESSjj)}
            if (index == "ESS"){valueij <- ESSij}
            Matrix[i,j]  <- valueij
        }
    }
    rownames(Matrix)=colnames(Matrix)  <- rownames(Dat)
    return(as.dist(Matrix))
}
