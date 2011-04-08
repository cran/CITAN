\name{dbBiblioClear}
\alias{dbBiblioClear}
\title{Clear a local bibliometric storage}
\usage{dbBiblioClear(con, verbose=TRUE)}
\description{Clears a local bibliometric storage by dropping all tables
named \code{Biblio_*} and all views named \code{ViewBiblio_*}.}
\details{For safety reasons, an SQL transaction  opened at the beginning of the
removal process is not committed (closed) automatically.
You should do it on your own (or rollback it), see Examples below.}
\value{\code{TRUE} on success.}
\seealso{\code{\link{dbBiblioConnect}}, \code{\link{dbBiblioCreate}},  \code{\link{Scopus_ImportSources}}, \code{\link{dbCommit}}, \code{\link{dbRollback}}, \code{\link{dbBiblioDeleteAuthorsDocuments}}}
\arguments{\item{con}{a connection object as produced by \code{\link{dbBiblioConnect}}.}
\item{verbose}{logical; \code{TRUE} to print out the progress of database contents' removal.}}
\examples{\dontrun{con <- dbBiblioConnect("Bibliometrics.db");}
\dontrun{dbBiblioClear(con);}
\dontrun{dbCommit(con);}
\dontrun{dbBiblioCreate(con);}
\dontrun{Scopus_ImportSources(con);}
## ...
\dontrun{dbDisconnect(con);}}
