#' Performs goodness-of-fit test for the Pareto-II distribution
#' basing on MMSE estimates (Zhang, Stevens, 2009) and the Anderson-Darling
#' or Kolmogorov test.
#'
#' This method, proposed by Zhang and Stevens (2009), uses either the function \code{\link[ADGofTest]{ad.test}} from package \code{ADGofTest}
#' or \code{\link{ks.test}} to compute the selected test.
#' It bases on bayesian MMS estimators, see \code{\link{pareto2.zsestimate}}.
#'
#' @title Goodness-of-fit test for the Pareto-II distribution
#' @param x a numeric vector of data values.
#' @param alternative indicates the alternative hypothesis and must be one of "two.sided" (default), "less", or "greater".
#' @param method either "anderson-darling" or "kolmogorov".
#'
#' @return
#' The list of class \code{htest} with the following components is passed as a result:
#' \tabular{ll}{
#' \code{statistic} \tab	the value of the test statistic.\cr
#' \code{p.value} \tab	the p-value of the test.\cr
#' \code{alternative} \tab	a character string describing the alternative hypothesis.\cr
#' \code{method} \tab	a character string indicating what type of test was performed.\cr
#' \code{data.name} \tab	a character string giving the name(s) of the data.\cr
#' }
#' @export
#' @seealso \code{\link{dpareto2}}, \code{\link{pareto2.zsestimate}}, \code{\link{ks.test}}, \code{\link[ADGofTest]{ad.test}} from package \code{ADGofTest}
#' @references
#' Zhang J., Stevens M.A., A New and Efficient Estimation Method for the Generalized Pareto Distribution, Technometrics 51(3), 2009, 316-325.\cr
pareto2.goftest <- function(x, alternative = c("two.sided", "less", "greater"), method = c("anderson-darling", "kolmogorov"))
{
	alternative <- match.arg(alternative);
	DNAME <- deparse(substitute(x));

	method <- match.arg(method);

	x <- x[!is.na(x)];
	nx <- length(x);
	if (nx < 1L || any(x<0)) stop("incorrect 'x' data");

	params <- pareto2.zsestimate(x);

	RVAL <- switch(method,
		"anderson-darling" = ad.test(x, ppareto2, params$k, params$s),
		"kolmogorov" = ks.test(x, "ppareto2", params$k, params$s)
	);

	RVAL$method = switch(method,
		"anderson-darling" = sprintf("Anderson-Darling Goodness-of-Fit test for the Pareto-II distribution P2(%g, %g)", params$k, params$s),
		"kolmogorov" = sprintf("Kolmogorov Goodness-of-Fit test for the Pareto-II distribution P2(%g, %g)", params$k, params$s),
	);

	return(RVAL);
}






