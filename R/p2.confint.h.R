#' Computes the exact two-sided confidence interval for the theoretical \eqn{h}-index
#' of a probability distribution in an \eqn{(X_1,\dots,X_n)} i.i.d. Pareto-type II
#' model with known scale parameter \eqn{s>0}.
#'
#' The confidence interval bases on the method \code{\link{pareto2.confint.rho}}.
#'
#' The \dfn{Theoretical \eqn{h}-index} for a sequence of \eqn{n} i.i.d. random variables
#' with common increasing and continuous c.d.f. \eqn{F} defined on \eqn{[0,\infty)}
#' is equal to \eqn{\lfloor n\varrho_\kappa\rfloor}{floor(n*\rho_\kappa)}, where \eqn{\rho_\kappa}
#' is the \eqn{\kappa}-index of \eqn{F} for \eqn{\kappa(x)=nx}, see \code{\link{rho.get}} for details.
#'
#' @references
#' Gagolewski M., Grzegorzewski P., S-Statistics and Their Basic Properties, In: Borgelt C. et al (Eds.),
#' Combining Soft Computing and Statistical Methods in Data Analysis, Springer-Verlag, 2010, 281-288.\cr
#'
#' @title Two-sided exact confidence interval for the theoretical h-index
#' @param h observed value of the \eqn{h}-index
#' @param s scale parameter, \eqn{s>0}.
#' @param n sample size.
#' @param conf.level confidence level; defaults 0.95.
#' @return Vector of length 2 with the computed bounds of the confidence interval.
#' @export
#' @seealso \code{\link{index.h}}, \code{\link{ppareto2}}, \code{\link{rho.get}}, \code{\link{pareto2.confint.rho}}
pareto2.confint.h <- function(h, s, n, conf.level=0.95)
{
	if (length(h) != 1 || h < 0 || h > n)
		stop("Incorrect h value!");

	if (mode(s) != "numeric" || length(s) != 1 || s <= 0) stop("'s' should be > 0");

	kappa    <- function(x) { pmax(0,pmin(1,x))*gamma; }
	gamma <- 1-conf.level;
	h <- round(h);

	if (h == 0) {
		l <- 0;
	} else {
		l <- pareto2.confint.rho.lower(h/n,kappa,s,n,1-gamma*0.5);
	}

	if (h == n) {
		u <- 1;
	} else {
		u <- pareto2.confint.rho.upper((h+1)/n-1e-9,kappa,s,n,1-gamma*0.5);
	}

	return(c(floor(l*n), floor(u*n)));
}


