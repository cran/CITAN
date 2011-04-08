\name{dbBiblioImportDocuments}
\alias{dbBiblioImportDocuments}
\title{Import publications to a local bibliometric storage.}
\usage{dbBiblioImportDocuments(con, data, surveyDescription="Default survey",
    originalFilename=attr(data, "filename"), excludeRows,
    updateDocumentIfExists=TRUE, doVacuum=TRUE, warnISSN=FALSE,
    warnExactDuplicates=FALSE, warnDuplicateTitles=TRUE, verbose=TRUE)}
\description{Imports publications from a special 14-column data frame to a local bibliometric storage.
Such an input may be created e.g. with \code{\link{Scopus_ReadCSV}}.}
\details{\code{data} must consist of the following 14 columns (in order). Otherwise
the process will not be executed.
\tabular{llll}{
1  \tab \code{Authors}       \tab \code{character}\tab  Author(s) name(s), comma-separated, surnames first.\cr
2  \tab \code{Title}         \tab \code{character}\tab  Document title.\cr
3  \tab \code{Year}          \tab \code{numeric}  \tab  Year of publication.\cr
4  \tab \code{SourceTitle}   \tab \code{character}\tab  Title of the source containing the document.\cr
5  \tab \code{Volume}        \tab \code{character}\tab  Volume.\cr
6  \tab \code{Issue}         \tab \code{character}\tab  Issue.\cr
7  \tab \code{ArticleNumber} \tab \code{character}\tab  Article number (identifier).\cr
8  \tab \code{PageStart}     \tab \code{numeric}  \tab  Start page; numeric.\cr
9  \tab \code{PageEnd}       \tab \code{numeric}  \tab  End page; numeric.\cr
10 \tab \code{Citations}     \tab \code{numeric}  \tab  Number of citations.\cr
11 \tab \code{UniqueId}      \tab \code{character}\tab  Unique document identifier. Documents with \code{is.na(UniqueId)} will not be added.\cr
12 \tab \code{ISSN}          \tab \code{character}\tab  ISSN of the source.\cr
13 \tab \code{Language}      \tab \code{factor}   \tab  Language of the document.\cr
14 \tab \code{DocumentType}  \tab \code{factor}   \tab  Type of the document; \dQuote{Article}, \dQuote{Article in Press},
\dQuote{Book}, \dQuote{Conference Paper}, \dQuote{Editorial}, \dQuote{Erratum},
\dQuote{Letter}, \dQuote{Note}, \dQuote{Report},
\dQuote{Review}, \dQuote{Short Survey}, or \code{NA} (other categories are interpreted as \code{NA}).\cr
}

Note that if \code{data} contains many records (>1000),
the import process may take a few minutes.

Sources (e.g. journals) are identified by ISSNs (table \code{Biblio_Sources}).
Note that generally there is no need to concern about missing ISSNs of
conference proceedings.

Each time a function is called, a new record in the table \code{Biblio_Surveys}
is created. Such surveys may be grouped using the \code{Description}
field, see \code{\link{dbBiblioCreate}}.}
\value{\code{TRUE} on success.}
\seealso{\code{\link{Scopus_ReadCSV}}, \code{\link{dbBiblioConnect}}, \code{\link{dbBiblioCreate}}}
\arguments{\item{con}{a connection object as produced by \code{\link{dbBiblioConnect}}.}
\item{data}{14 column \code{data.frame} with bibliometric entries; see above.}
\item{surveyDescription}{Description of the survey. Allows for documents grouping.}
\item{originalFilename}{Original file name, \code{attr(data, "filename")} is used by default.}
\item{excludeRows}{a numeric vector with row numbers of \code{data} to exclude or \code{NULL}}
\item{updateDocumentIfExists}{logical; if \code{TRUE}, then documents with the same \code{UniqueId} will be updated.}
\item{doVacuum}{logical; if \code{TRUE} then the SQL command \code{VACUUM}
will be executed on the database after importing data to optimize and compact the bibliometric storage.}
\item{warnISSN}{logical; if \code{TRUE} then warnings are generated if a given ISSN in not found in the table \code{Biblio_Sources}.}
\item{warnExactDuplicates}{logical; \code{TRUE} to warn if exact duplicates are found (turned off by default).}
\item{warnDuplicateTitles}{logical; \code{TRUE} to warn if documents
of the same \code{Title} but different \code{UniqueId}s are found.
This may indicate possible data misrepresentations.}
\item{verbose}{logical; \code{TRUE} to print out the progress of lengthy computations.}}
\examples{\dontrun{con <- dbBiblioConnect("Bibliometrics.db");}
## ...
\dontrun{data <- Scopus_ReadCSV("db_Polish_MATH/Poland_MATH_1987-1993.csv");}
\dontrun{dbBiblioImportDocuments(con, data, "Poland_MATH");}
## ...
\dontrun{dbDisconnect(con);}}
