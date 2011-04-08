\name{dbBiblioDescriptiveStats}
\alias{dbBiblioDescriptiveStats}
\title{Perform preliminary data analysis of data in a local bibliometric storage}
\usage{dbBiblioDescriptiveStats(con, DocumentTypes, SurveyDescription,
    which=(1L:7L), main="", ask=(prod(par("mfcol")) < length(which) &&
    dev.interactive()), ..., cex.caption=1)}
\description{Performs preliminary data analysis of data in a local bibliometric storage
by displaying some basic descriptive statistics and plots.
We may restrict ourselves to some selected document types
or a single survey.}
\details{Plot types (accessed with \code{which}):
\itemize{
\item \code{1} --- "Document types", 
\item \code{2} --- "Publication years", 
\item \code{3} --- "Citations per document",
\item \code{4} --- "Citations of cited documents per type", 
\item \code{5} --- "Number of pages per document type",
\item \code{6} --- "Categories of documents" (based od source categories), 
\item \code{7} --- "Documents per author".
}

The user interaction scheme is inspired by the \code{\link{plot.lm}} code.}
\seealso{\code{\link{plot.default}}, \code{\link{dbBiblioConnect}}}
\arguments{\item{con}{a connection object as produced by \code{\link{dbBiblioConnect}}.}
\item{DocumentTypes}{character vector or \code{NULL}; specifies document types to restrict to;
a combination of \code{Article}, \code{Article in Press}, \code{Book}, \code{Conference Paper},
\code{Editorial}, \code{Erratum}, \code{Letter}, \code{Note}, \code{Report}, \code{Review},
\code{Short Survey}. \code{NULL} means no restriction.}
\item{SurveyDescription}{single character string or \code{NULL}; survey to restrict to or \code{NULL} for no restriction.}
\item{which}{if a subset of the plots is required, specify a subset of the numbers \code{1:7}.}
\item{main}{title to each plot-in addition to default captions.}
\item{ask}{logical; if \code{TRUE} then the user is asked to press return before each plot.}
\item{...}{additional graphical parameters, see \code{\link{plot.default}}.}
\item{cex.caption}{controls the size of default captions.}}
\examples{\dontrun{con <- dbBiblioConnect("Bibliometrics.db");}
## ...
\dontrun{dbBiblioDescriptiveStats(con, SurveyDescription="Scientometrics", DocumentTypes=c("Article", "Note", "Report", "Review", "Short Survey"));}
## ...
\dontrun{dbDisconnect(con);}}
