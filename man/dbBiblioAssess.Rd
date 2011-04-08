\name{dbBiblioAssess}
\alias{dbBiblioAssess}
\title{Calculate impact of given citation sequences}
\usage{dbBiblioAssess(citseq, f=list(length, index.h), captions=c("length",
    "index.h"), orderByColumn=2, bestRanks=20, verbose=T)}
\description{Calculates values of impact functions
for a given list given citation sequences.}
\value{A data frame in which each row corresponds to the assessment
results of the corresponding elements of the list \code{citseq}.
The first column stand for the authors' names, the second
for the valuation of \code{f[[1]]}, the third for \code{f[[2]]}, and so on.
See Examples below.}
\seealso{\code{\link{dbBiblioConnect}}, \code{\link{dbBiblioGetCitations}}}
\arguments{\item{citseq}{list of numeric vectors, e.g. output of \code{\link{dbBiblioGetCitations}}.}
\item{f}{a list of \eqn{n} functions which compute the impact of an author.
The functions must calculate their values basing on numeric
vectors passed as their first arguments.}
\item{captions}{a list of \eqn{n} descriptive captions for the functions in \code{f}.}
\item{orderByColumn}{column to sort results on. \code{1} for author
names, \code{2} for the first function in \code{f}, \code{3}
for the second, and so on.}
\item{bestRanks}{if not \code{NULL}, only a given number of authors 
with the greatest impact (for each function in \code{f}) will be included in the output.}
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
