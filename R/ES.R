#' Calculate the Expected Species
#'
#' @importFrom Rdpack reprompt
#' @param x a data vector representing number of individuals for each species
#' @param m the sample size parameter that represents the number of individuals randomly drawn from the sample. For ESa, m can not be larger than the sample size
#' @param method the calculation approach of Expected Species used, with two options available as "a" and "b" to calculate ESa and ESb, with the default set as "a"
#' @param MARGIN a vector giving the subscripts which the function will be applied over, see '\link[base]{apply}'.
#' @return a value of Expected Species
#' @references \insertRef{zou2023}{rarestR}
#' @export
#' @examples
#' data(share, package = 'rarestR')
#' rowSums(share) #The sum size of each sample is 100, 150 and 200
#' es(share, m = 100)
#' es(share, method = "b", m = 100)
#' # When the m is larger than the total sample size, "NA" will be filled:
#' es(share, m = 150)
es <-  function(x, m, method = c("a","b"), MARGIN = 1){
  method <- match.arg(method, c("a", "b"))
  if (length(dim(x)) == 2) {
    results <- apply(x, MARGIN, function(y) es(y, m, method))
    return(results)
  }
  if (m < 1) {
    stop("m must be a positive value")
  }
  if (m %% 1 != 0) warning("results may be meaningless because m is not an integer")
  if (any(x < 0, na.rm = TRUE)) {
    stop("data have negative entries")
  }
  if (any(is.na(x))) {
    x[is.na(x)] <- 0
    warning("empty data were replaced by '0' values")
  }
  if(!identical(all.equal(as.integer(x),  as.vector(x)), TRUE)) {
    warning("results may be meaningless with non-integer data in method")
  }
  x <- as.vector(x)
  x <- x[x > 0]
  Ni <- sum(x)
  if (m > Ni){
    ESm <- NA
    warning("m can not be larger than the total sample size")
  } else if (Ni < 1) {
    ESm <- NA
    warning("total sample size < 1")
  } else if(method  == "a"){
    ESm <- sum(1 - exp(lchoose(Ni - x, m)- lchoose(Ni, m)))
  } else if(method  == "b"){
    ESm <- sum(1 - (1 - x / sum(x))^m)
  }
  return(ESm)
}
