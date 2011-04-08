\name{Scopus_ImportSources}
\alias{Scopus_ImportSources}
\title{Import SciVerse Scopus coverage information and ASJC codes to a local bibliometric storage}
\usage{Scopus_ImportSources(con, impactColumn, updateSourceIfExists=FALSE,
    verbose=T)}
\description{Imports \emph{SciVerse Scopus} covered titles and their ASJC codes to an empty local bibliometric storage.}
\details{The function should be called prior to importing any document information
to the bibliometric storage with \code{\link{dbBiblioImportDocuments}}.

If multiple sources with the same ISSN (either Print-ISSN or E-ISSN)
are found in \code{\link{Scopus_SourceList}}
and \code{updateSourceIfExists} is \code{FALSE}, then
only the first matching record is added to the table \code{Biblio_Sources}.
Additionally, a warning is generated.
Otherwise, the last non-unique record is taken into account.
Note that adding all the sources takes some time.

Only basic ASJC and \emph{SciVerse Scopus} source information
retrieved from \code{\link{Scopus_ASJC}} (stored in the table \code{Biblio_Categories})
and \code{\link{Scopus_SourceList}} (stored in tables \code{Biblio_Countries} and \code{Biblio_Sources})
will be added to the bibliometric storage.}
\value{\code{TRUE} on success.}
\seealso{\code{\link{Scopus_ASJC}}, \code{\link{Scopus_SourceList}}, \code{\link{Scopus_ReadCSV}}, \code{\link{dbBiblioConnect}}, \code{\link{dbBiblioCreate}}}
\arguments{\item{con}{a connection object as produced by \code{\link{dbBiblioConnect}}.}
\item{impactColumn}{single character value determining column name in \code{\link{Scopus_SourceList}} with values of some source impact measurements to be imported, e.g. \code{"SJR_2009"}, or \code{NULL} if no such data should be saved.}
\item{updateSourceIfExists}{logical; if \code{FALSE}, in case of sources with the same ISSN only the first will be added.}
\item{verbose}{logical; \code{TRUE} to print out the progress of lengthy computations.}}
\examples{\dontrun{con <- dbBiblioConnect("survey007.db");}
\dontrun{dbBiblioCreate(con);}
\dontrun{Scopus_ImportSources(con);}
\dontrun{dbDisconnect(con);}}
