% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/biblio.clear.R
\name{lbsClear}
\alias{lbsClear}
\title{Clear a Local Bibliometric Storage}
\usage{
lbsClear(conn, verbose = TRUE)
}
\arguments{
\item{conn}{database connection object, see \code{\link{lbsConnect}}.}

\item{verbose}{logical; \code{TRUE} to be more verbose.}
}
\value{
\code{TRUE} on success.
}
\description{
Clears a Local Bibliometric Storage by dropping all tables
named \code{Biblio_*} and all views named \code{ViewBiblio_*}.
}
\details{
For safety reasons, an SQL transaction opened at the beginning of the
removal process is not committed (closed) automatically.
You should do manually (or rollback it), see Examples below.
}
\examples{
\dontrun{
conn <- lbsConnect("Bibliometrics.db");
lbsClear(conn);
dbCommit(conn);
lbsCreate(conn);
Scopus_ImportSources(conn);
## ...
lbsDisconnect(conn);}

}
\seealso{
\code{\link{lbsConnect}}, \code{\link{lbsCreate}},
\code{\link{Scopus_ImportSources}}, \code{\link{lbsDeleteAllAuthorsDocuments}}
\code{\link[DBI]{dbCommit}}, \code{\link[DBI]{dbRollback}}
}
