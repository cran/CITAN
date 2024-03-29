% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/biblio.stats.R
\name{lbsDescriptiveStats}
\alias{lbsDescriptiveStats}
\title{Perform preliminary analysis of data in a Local Bibliometric Storage}
\usage{
lbsDescriptiveStats(
  conn,
  documentTypes = NULL,
  surveyDescription = NULL,
  which = (1L:7L),
  main = "",
  ask = (prod(par("mfcol")) < length(which) && dev.interactive()),
  ...,
  cex.caption = 1
)
}
\arguments{
\item{conn}{connection object, see \code{\link{lbsConnect}}.}

\item{documentTypes}{character vector or \code{NULL}; specifies document types to restrict to;
a combination of \code{Article}, \code{Article in Press}, \code{Book}, \code{Conference Paper},
\code{Editorial}, \code{Erratum}, \code{Letter}, \code{Note}, \code{Report}, \code{Review},
\code{Short Survey}. \code{NULL} means no restriction.}

\item{surveyDescription}{single character string or \code{NULL}; survey to restrict to, or \code{NULL} for no restriction.}

\item{which}{numeric vector with elements in 1,...,7, or \code{NULL}; plot types to be displayed.}

\item{main}{title for each plot.}

\item{ask}{logical; if \code{TRUE}, the user is asked to press return before each plot.}

\item{...}{additional graphical parameters, see \code{\link{plot.default}}.}

\item{cex.caption}{controls size of default captions.}
}
\description{
Performs preliminary analysis of data in a Local Bibliometric Storage
by creating some basic descriptive statistics (numeric and graphical).
Dataset may be restricted to any given document types
or a single survey.
}
\details{
Plot types (accessed with \code{which}):
\itemize{
	\item \code{1} --- "Document types",
	\item \code{2} --- "Publication years",
	\item \code{3} --- "Citations per document",
	\item \code{4} --- "Citations of cited documents per type",
	\item \code{5} --- "Number of pages per document type",
	\item \code{6} --- "Categories of documents" (based od source categories),
	\item \code{7} --- "Documents per author".
}

Note that this user interaction scheme is similar in behavior
to the \code{\link{plot.lm}} function.
}
\examples{
\dontrun{
conn <- lbsConnect("Bibliometrics.db");
## ...
lbsDescriptiveStats(conn, surveyDescription="Scientometrics",
   documentTypes=c("Article", "Note", "Report", "Review", "Short Survey"));
## ...
lbsDisconnect(conn);}

}
\seealso{
\code{\link{plot.default}},
\code{\link{lbsConnect}}
/internal/
/internal/
}
