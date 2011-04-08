\name{dbBiblioDeleteAuthorsDocuments}
\alias{dbBiblioDeleteAuthorsDocuments}
\title{Delete all authors, documents and surveys from a local bibliometric storage}
\usage{dbBiblioDeleteAuthorsDocuments(con, verbose=TRUE)}
\description{Deletes are author, document and survey information from a local bibliometric
storage.}
\details{For safety reasons, an SQL transaction opened at the beginning of the
removal process is not committed (closed) automatically.
You should do it on your own (or rollback it), see Examples below.}
\value{\code{TRUE} on success.}
\seealso{\code{\link{dbBiblioClear}}, \code{\link{dbCommit}}, \code{\link{dbRollback}}}
\arguments{\item{con}{a connection object as produced by \code{\link{dbBiblioConnect}}.}
\item{verbose}{logical; \code{TRUE} to print out the progress of database contents' removal.}}
\examples{\dontrun{con <- dbBiblioConnect("Bibliometrics.db");}
\dontrun{dbBiblioDeleteAuthorsDocuments(con);}
\dontrun{dbCommit(con);}
## ...
\dontrun{dbDisconnect(con);}}
