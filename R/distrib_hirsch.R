#' The probability mass function of Hirsch's \eqn{h}-index
#' for sample of size \code{n} in an i.i.d. model with common increasing and continuous c.d.f. \eqn{F} defined on \eqn{[0,\infty)}.
#'
#' @references
#' Gagolewski M., Grzegorzewski P., S-Statistics and Their Basic Properties, In: Borgelt C. et al (Eds.), Combining Soft Computing and Statistical Methods in Data Analysis, Springer-Verlag, 2010, 281-288.\cr
#'
#' @title Distribution of the \eqn{h}-index - p.m.f.
#' @param x numeric vector.
#' @param n sample size.
#' @param cdf a continuous cumulative distribution function \eqn{F}, e.g. \code{\link{ppareto2}}.
#' @param ... optional arguments to \code{cdf}.
#' @return The value of the p.m.f. at \code{x}.
#' @export
#' @seealso \code{\link{index.h}}, \code{\link{rho.get}}, \code{\link{phirsch}}
dhirsch <- function(x, n, cdf, ...)
{
	phirsch(x+1e-9, n, cdf, ...)-phirsch(x-1e-9, n, cdf, ...)
}


#' The cumulative distribution function of Hirsch's \eqn{h}-index
#' for sample of size \code{n} in an i.i.d. model with common increasing and continuous c.d.f. \eqn{F} defined on \eqn{[0,\infty)}.
#'
#' @references
#' Gagolewski M., Grzegorzewski P., S-Statistics and Their Basic Properties, In: Borgelt C. et al (Eds.), Combining Soft Computing and Statistical Methods in Data Analysis, Springer-Verlag, 2010, 281-288.\cr
#'
#' @title Distribution of the \eqn{h}-index - c.d.f.
#' @param x numeric vector.
#' @param n sample size.
#' @param cdf a continuous cumulative distribution function \eqn{F}, e.g. \code{\link{ppareto2}}.
#' @param ... optional arguments to \code{cdf}.
#' @return The value of the c.d.f. at \code{x}.
#' @export
#' @seealso \code{\link{index.h}}, \code{\link{rho.get}}, \code{\link{dhirsch}}
phirsch <- function(x, n, cdf, ...)
{
	warn <- getOption("warn");
	options("warn"=-1);
	y <- ifelse(x>n-1e-16, 1.0,
	     ifelse(x<0, 0.0,
	     pbeta(cdf(floor(x+1), ...), n-floor(x),floor(x)+1)));
	options("warn"=warn);
	return(y);
}


