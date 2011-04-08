\name{dbBiblioGetCitations}
\alias{dbBiblioGetCitations}
\title{Fetch citation sequences for authors in a local bibliometric storage}
\usage{dbBiblioGetCitations(con, DocumentTypes, SurveyDescription, IdAuthors,
    verbose=TRUE)}
\description{Creates citation sequences for authors in a local bibliometric storage,
that is vectors of citation counts of all the documents mapped to
selected  authors.
The results may be restricted to a given Survey (using \code{SurveyDescription}
parameter) or document types (\code{DocumentTypes}).}
\value{A list of numeric vectors is returned. Each element of the list corresponds
to a citation sequence of a different author. List \code{names} attribute are
set to authors' names. Moreover, each vector has a set \code{IdAuthor}
attribute, which uniquely identifies a corresponding record in the table \code{Biblio_Authors}.
Citation counts come together with \code{IdDocument}s (vector elements are named).

The list of citation sequences may then be used to calculate
authors' impact using \code{\link{dbBiblioAssess}} (see Examples below).}
\seealso{\code{\link{dbBiblioConnect}}, \code{\link{dbBiblioAssess}}}
\arguments{\item{con}{a connection object as produced by \code{\link{dbBiblioConnect}}.}
\item{DocumentTypes}{character vector or \code{NULL}; specifies document types to restrict to;
a combination of \code{Article}, \code{Article in Press}, \code{Book}, \code{Conference Paper},
\code{Editorial}, \code{Erratum}, \code{Letter}, \code{Note}, \code{Report}, \code{Review},
\code{Short Survey}. \code{NULL} means no restriction.}
\item{SurveyDescription}{single character string or \code{NULL}; survey to restrict to or \code{NULL} for no restriction.}
\item{IdAuthors}{numeric vector od author identifiers to restrict to (in the table \code{Biblio_Authors}) or \code{NULL} for no restriction.}
\item{verbose}{logical; \code{TRUE} to print out the progress of lengthy computations.}}
\examples{\dontrun{con <- dbBiblioConnect("Bibliometrics.db");}
## ...
\dontrun{citseq <- dbBiblioGetCitations(con,
SurveyDescription="Scientometrics", DocumentTypes="Article",
IdAuthor=c(39264,39265,39266));}
\dontrun{print(citseq);}
## $`Liu X.`                                # Author name
## 40116 34128 39122 29672 32343 32775      # IdDocument
##    11     4     1     0     0     0      # Citation count
## attr(,"IdAuthor")
## [1] 39264                                # IdAuthor
## 
## $`Xu Y.`
## 38680 38605 40035 40030 40124 39829 39745 29672 
##    30    14     8     6     6     5     3     0 
## attr(,"IdAuthor")
## [1] 39265
## 
## $`Wang Y.`
## 29992 29672 29777 32906 33858 33864 34704 
##     1     0     0     0     0     0     0 
## attr(,"IdAuthor")
## [1] 39266
\dontrun{print(dbBiblioAssess(citseq,
f=list(length, sum, index.h, index.g, function(x) index.rp(x,1),
function(x) sqrt(prod(index.lp(x,1))), function(x) sqrt(prod(index.lp(x,Inf)))),
captions=c("length", "sum", "index.h", "index.g", "index.w", "index.lp1", "index.lpInf")));}
##      Name length sum index.h index.g index.w index.lp1 index.lpInf
## 3   Xu Y.      8  72       5       8       7  8.573214    5.477226
## 2 Wang Y.      7   1       1       1       1  1.000000    1.000000
## 1  Liu X.      6  16       2       4       3  4.157609    3.316625
## ...
\dontrun{dbDisconnect(con);}}
