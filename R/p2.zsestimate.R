#' Finds the MMS estimators of the type II Pareto distribution parameters
#' using the Bayesian method (and the R code) developed by
#' Zhang and Stevens (2009).
#'
#' @title Estimation of parameters for the Pareto-II distribution
#' @param x a numeric vector of observations.
#' @return
#' The list  with the following components is passed as a result:
#' \tabular{ll}{
#' \code{k} \tab	the estimated parameter of shape.\cr
#' \code{s} \tab	the estimated parameter of scale.\cr
#' }
#' @export
#' @seealso \code{\link{dpareto2}}, \code{\link{pareto2.goftest}}
#' @references
#' Zhang J., Stevens M.A., A New and Efficient Estimation Method for the Generalized Pareto Distribution, Technometrics 51(3), 2009, 316-325.\cr
pareto2.zsestimate <- function(x)
{
	n <- length(x);
	x <- sort(x);

	lx <- function(b, x)
	{
		k <- -mean(log(1-b*x));
		log(b/k)+k-1;
	}

	m <- 20+floor(sqrt(n))

	b <- w <- L <- 1/x[n]+(1-sqrt(m/((1:m)-0.5)))/3/x[floor(n/4+0.5)]

	for (i in 1:m) L[i] <- n*lx(b[i],x)

	for (i in 1:m) w[i]<- 1/sum(exp(L-L[i]))

	b <- sum(b*w);

	k <- 1/mean(log(1-b*x));
	s <- -1/b;

	if (k<=0) warning("estimated shape parameter <= 0");
	if (s<1)  warning("estimated scale parameter < 1");

	list(k=k, s=s);
}



