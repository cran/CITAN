## This file is part of the CITAN library.
##
## Copyright 2011 Marek Gagolewski <gagolews@ibspan.waw.pl>
##
##
## CITAN is free software: you can redistribute it and/or modify
## it under the terms of the GNU Lesser General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## CITAN is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
## GNU Lesser General Public License for more details.
##
## You should have received a copy of the GNU Lesser General Public License
## along with CITAN. If not, see <http://www.gnu.org/licenses/>.

#' @include biblio.internal.R
NA



#' List of Elsevier's \emph{SciVerse Scopus} covered titles (journals, conference proceedings, book series, etc.)
#'
#' Last update: March 2011. The data file is based on the official and publicly available
#' (no permission needed-information from Elsevier) Scopus list of covered titles,
#' see \url{http://www.info.sciverse.com/documents/files/scopus-training/resourcelibrary/xls/title_list.xls}.
#'
#' This data frame consists of 30017 records.
#' It has the following columns.
#' \tabular{ll}{
#'   \code{Record} \tab Unique source identifier in \emph{SciVerse Scopus} (character string). \cr
#'   \code{Title} \tab Title of the source. \cr
#'   \code{ISSN_Print} \tab Print ISSN (8 characters). \cr
#'   \code{ISSN_E} \tab E-ISSN (8 characters). \cr
#'   \code{Status} \tab Status of the source, either \code{Active} or \code{Inactive}. \cr
#'   \code{SJR_2007} \tab SCImago Journal Rank for 2007. \cr
#'   \code{SNIP_2007} \tab Source Normalized Impact per Paper for 2007. \cr
#'   \code{SJR_2008} \tab SCImago Journal Rank for 2008. \cr
#'   \code{SNIP_2008} \tab Source Normalized Impact per Paper for 2008. \cr
#'   \code{SJR_2009} \tab SCImago Journal Rank for 2009. \cr
#'   \code{SNIP_2009} \tab Source Normalized Impact per Paper for 2009. \cr
#'   \code{OpenAccess} \tab Type of Open Access, see below. \cr
#'   \code{Type} \tab Type of the source, see below. \cr
#'   \code{Country} \tab Country of origin. \cr
#'   \code{ASJC} \tab A list of semicolon-separated ASJC classification codes, see \code{\link{Scopus_ASJC}}. \cr
#' }
#'
#' \code{OpenAccess} is one of \code{DOAJ}, \code{Not OA} (not Open Access source),
#' \code{OA but not registered}, \code{OA registered}.
#'
#' \code{Type} is one of \code{Book Series}, \code{Conference Proceedings}, \code{Journal}, \code{Trade Journal}
#'
#' The \code{data.frame} is sorted by \code{ISSN_Print} and \code{Status} (secondary criterion; \code{Active} sources first).
#'
#' @title Scopus list of covered sources
#' @name Scopus_SourceList
#' @docType data
#' @seealso \code{\link{Scopus_ASJC}}, \code{\link{Scopus_ReadCSV}}, \code{\link{Scopus_ImportSources}}
#' @author Marek Gagolewski \email{gagolews@@ibspan.waw.pl}
#' @references \url{http://www.info.sciverse.com/scopus/scopus-in-detail/facts/}\cr
#' \url{http://info.scopus.com/journalmetrics/sjr.html}\cr
#' \url{http://info.scopus.com/journalmetrics/snip.html}\cr
#' @keywords Scopus, ASJC, journal, conference, proceedings
NA


#' List of Elsevier's \emph{SciVerse Scopus} ASJC (All Science. Journals Classification)
#' source classification codes.
#'
#' Last update: March 2011. The data file is based on the official and publicly available
#' (no permission needed-information from Elsevier) \emph{SciVerse Scopus} list of covered titles,
#' see \url{http://www.info.sciverse.com/documents/files/scopus-training/resourcelibrary/xls/title_list.xls}.
#'
#' It consists of 334 ASJC 4-digit integer codes (column \code{ASJC})
#' together with their group identifiers (column \code{ASJC_Group})
#' and descriptions (column \code{Description}).
#'
#' It is used to classify journals and other sources (see \code{\link{Scopus_SourceList}}).
#'
#' @title Scopus ASJC (All Science. Journals Classification) classification codes
#' @name Scopus_ASJC
#' @docType data
#' @seealso \code{\link{Scopus_SourceList}}, \code{\link{Scopus_ReadCSV}}, \code{\link{Scopus_ImportSources}}
#' @author Marek Gagolewski \email{gagolews@@ibspan.waw.pl}
#' @references \url{http://www.info.sciverse.com/scopus/scopus-in-detail/facts/}
#' @keywords Scopus, ASJC, journal
NA





#' Reads bibliography entries from a 40-column CSV file created e.g.
#' with \emph{SciVerse Scopus} (Complete format).
#'
#' The function \code{\link{read.csv}}
#' is used to read the bibliometric database. However, you may freely modify its behavior
#' by passing further arguments (\code{...}), see the manual page of \code{\link{read.table}}
#' for details.
#'
#' The CSV file should consist of exactly 40 variables.
#' Here are their meanings (in order of appearance):
#' \enumerate{
#' \item Author name(s) (surname first; multiple names are comma-separated,
#' e.g. \dQuote{Kovalsky John, Smith B. W.}),
#' \item Document title,
#' \item Year,
#' \item Source title,
#' \item Volume.
#' \item Issue,
#' \item Article number,
#' \item Page start,
#' \item Page end,
#' \item \emph{not used},
#' \item Number of citations received,
#' \item String containing unique document identifier of the form ...id=\emph{\strong{UNIQUE_ID}}&...
#' \item \emph{not used},
#' \item \emph{not used},
#' \item \emph{not used},
#' \item \emph{not used},
#' \item \emph{not used},
#' \item \emph{not used},
#' \item \emph{not used},
#' \item \emph{not used},
#' \item \emph{not used},
#' \item \emph{not used},
#' \item \emph{not used},
#' \item \emph{not used},
#' \item \emph{not used},
#' \item \emph{not used},
#' \item \emph{not used},
#' \item \emph{not used},
#' \item \emph{not used},
#' \item \emph{not used},
#' \item \emph{not used},
#' \item Source ISSN,
#' \item \emph{not used},
#' \item \emph{not used},
#' \item \emph{not used},
#' \item \emph{not used},
#' \item Language of original document,
#' \item \emph{not used},
#' \item Document type, one of: \dQuote{Article}, \dQuote{Article in Press},
#'        \dQuote{Book}, \dQuote{Conference Paper}, \dQuote{Editorial},
#'        \dQuote{Erratum}, \dQuote{Letter}, \dQuote{Note}, \dQuote{Report},
#'        \dQuote{Review}, \dQuote{Short Survey}, or \code{NA}
#'        (other categories are interpreted as \code{NA}),
#' \item Database identifier, should be the same as the value of \code{dbIdentifier}
#'        parameter, otherwise an exception is thrown.
#' }
#'
#' Such a CSV file may be generated e.g. with \emph{SciVerse Scopus}
#' (Export format=\emph{comma separated file, .csv (e.g. Excel)},
#' Output=\emph{Complete format}). Note that the exported CSV file
#'  needs some corrections in a few cases (wrong page numbers, single
#' double quotes in string instead of two-double quotes etc.). 
#' We suggest to make them in  \dQuote{Notepad}-like applications
#' (in plain text).
#' The function tries to point out the line numbers that
#' cause potential problems. However, sometimes a support of Spreadsheet-like programs
#' could be helpful.
#'
#' @title Import bibliography entries from a 40-column CSV file.
#' @param filename the name of the file which the data are to be read from, see \code{\link{read.csv}}.
#' @param stopOnErrors logical; \code{TRUE} to stop on all potential parse errors or just warn otherwise.
#' @param dbIdentifier single character value; database identifier, helps detect parse errors, see above.
#' @param ... further arguments to be passed to \code{read.csv}.
#' @return A data frame (\code{data.frame}) containing the following 14 columns:
#' \tabular{ll}{
#' \code{Authors} \tab	Author(s) name(s), comma-separated, surnames first.\cr
#' \code{Title} \tab	Document title.\cr
#' \code{Year} \tab	Year of publication.\cr
#' \code{UniqueId} \tab	Unique document identifier.\cr
#' \code{SourceTitle} \tab	Title of the source containing the document.\cr
#' \code{Volume} \tab	Volume.\cr
#' \code{Issue} \tab	Issue.\cr
#' \code{ArticleNumber} \tab Article number (identifier).\cr
#' \code{PageStart} \tab	Start page; numeric.\cr
#' \code{PageEnd} \tab	End page; numeric.\cr
#' \code{Citations} \tab	Number of citations.\cr
#' \code{ISSN} \tab	ISSN of the source.\cr
#' \code{Language} \tab Language of the document.\cr
#' \code{DocumentType} \tab	Type of the document; see above.\cr
#' }
#' Such an object may be imported to a local bibliometric storage with \code{\link{lbsImportDocuments}}.
#' @examples
#' \dontrun{
#' conn <- lbsConnect("Bibliometrics.db");
#' ## ...
#' data <- Scopus_ReadCSV("db_Polish_MATH/Poland_MATH_1987-1993.csv");
#' lbsImportDocuments(conn, data, "Poland_MATH");
#' ## ...
#' dbDisconnect(conn);}
#' @seealso \code{\link{Scopus_ASJC}}, \code{\link{Scopus_SourceList}},
#' \code{\link{lbsConnect}},
#' \code{\link{Scopus_ImportSources}},\cr
#' \code{\link{read.table}}, \code{\link{lbsImportDocuments}}
#' @export
Scopus_ReadCSV <- function(filename, stopOnErrors=TRUE, dbIdentifier='Scopus', ...)
{
	data <- read.csv(filename, colClasses=c(
			"character", "character", "numeric",   "character", "character",
			"character", "character", "character", "character", "character",
			"numeric",   "character", "character", "character", "character",
			"character", "character", "character", "character", "character",
			"character", "character", "character", "character", "character",
			"character", "character", "character", "character", "character",
			"character", "character", "character", "character", "character",
			"character", "factor",    "character", "factor", "character"
		),
		header = T, encoding="UTF-8", fileEncoding="UTF-8",
		col.names=c(
			"Authors", "Title", "Year", "SourceTitle", "Volume",
			"Issue", "ArticleNumber", "PageStart", "PageEnd", "X10",
			"Citations", "UniqueId", "X13", "X14", "X15",
			"X16", "X17", "X18", "X19", "X20",
			"X21", "X22", "X23", "X24", "X25",
			"X26", "X27", "X28", "X29", "X30",
			"X31", "ISSN", "X33", "X34", "X35",
			"X36", "Language", "X38", "DocumentType", "X40"
		), ...);

	
	data$UniqueId <- gsub("^.*\\id=|\\&.*$", "", data$UniqueId);
	data$UniqueId[data$UniqueId == ""] <- NA;
	
	
	naUniqueId <- which(is.na(data$UniqueId));
	if (length(naUniqueId) > 0)
	{
		msg <- (sprintf("some documents do not have unique identifiers. Check line %s (or its neighborhood). \
Perhaps somethings is wrong with the end page (check for ', ' nearby).",
			naUniqueId[1]+1));
			
		if (stopOnErrors) stop(msg) else warning(msg);
	}
	
	checkUniqueId <- unique(data$UniqueId, incomparables=NA);
	if (length(checkUniqueId) != nrow(data))
	{
		msg <- (sprintf("repeating (non-unique) document identifiers at rows: %s.",
			paste((1:nrow(data))[-checkUniqueId], collapse=", ")));
			
		if (stopOnErrors) stop(msg) else warning(msg);
	}
	
	
	
	checkCitations <- which(data$Citations < 0 | data$Citations>100000);
	if (length(checkCitations) > 0)
	{
		msg <- (sprintf("something is wrong with citation counts at rows: %s.",
			paste((1:nrow(data))[-checkCitations], collapse=", ")));
			
		if (stopOnErrors) stop(msg) else warning(msg);
	}



	if (any(data$X40 != dbIdentifier))
	{
		msg <- (sprintf("source database does not match 'dbIdentifier'. This may possibly indicate a parse error. Check records: %s.",
			paste(which(data$X40 != dbIdentifier), collapse=", ")));
			
		if (stopOnErrors) stop(msg) else warning(msg);
	}
	
	
	data$PageStart[!is.na(gsub("^([[:digit:]]+)$", NA, data$PageStart))] <- NA;
	data$PageEnd  [!is.na(gsub("^([[:digit:]]+)$", NA, data$PageEnd  ))] <- NA;
	
	data$PageStart <- as.numeric(data$PageStart);
	data$PageEnd   <- as.numeric(data$PageEnd);
	
	checkPages <- which((data$PageStart<0) | (data$PageEnd<data$PageStart) | (data$PageEnd-data$PageStart>10000));
	if (length(checkPages) > 0)
	{
		msg <- (sprintf("some documents seem to have wrong page numbers. Check line %s (or its neighborhood).",
			checkPages[1]+1));
			
		if (stopOnErrors) stop(msg) else warning(msg);
	}



	data <- data[,which(substr(names(data),1,1)!="X")]; # remove unnecessary columns

	attr(data, "filename") <- filename;
	data
}


#' /internal/
.Scopus_ImportSources_Categories <- function(conn, verbose)
{
	if (verbose) cat("Importing ASJC codes into 'Biblio_Categories'... ");

	dbBeginTransaction(conn);

	for (i in 1:nrow(Scopus_ASJC))
	{
		query <- sprintf("INSERT INTO Biblio_Categories('IdCategory', 'IdCategoryGroup', 'Description')
		VALUES (%g, %g, '%s');",
			as.integer(Scopus_ASJC$ASJC[i]),
			as.integer(Scopus_ASJC$ASJC_Group[i]),
			sqlEscapeTrim(Scopus_ASJC$Description[i])
		);

		dbExecQuery(conn, query, TRUE);
	}

	if (verbose) cat(sprintf("OK, %g records added.\n", nrow(Scopus_ASJC)));
}


#' /internal/
.Scopus_ImportSources_Countries <- function(conn, verbose)
{
	if (verbose) cat("Importing country list into 'Biblio_Countries'... ");

	dbBeginTransaction(conn);

	IdCountry <- Scopus_SourceList$Country;
	levels(IdCountry) <- sqlTrim(levels(IdCountry));
	for (i in 1:length(levels(IdCountry)))
	{
		if (is.na(levels(IdCountry)[i]))
			next;

		if (levels(IdCountry)[i]=="")
			next;

		query <- sprintf("INSERT INTO Biblio_Countries('Name') VALUES ('%s');",
			sqlEscape(levels(IdCountry)[i]));
		dbExecQuery(conn, query, TRUE);

		levels(IdCountry)[i] <- dbGetQuery(conn,
			sprintf("SELECT IdCountry FROM Biblio_Countries WHERE Name='%s';",
			sqlEscape(levels(IdCountry)[i])))[1,1];
		stopifnot(!is.na(levels(IdCountry)[i]));
	}

	dbCommit(conn);

	levels(IdCountry)[levels(IdCountry)==""] <- NA;

	if (verbose) cat(sprintf("OK, %g records added.\n", length(levels(IdCountry))));

	return(IdCountry);
}




#' /internal/
.Scopus_ImportSources_Sources <- function(conn, IdCountry, Impact, updateSourceIfExists, verbose)
{
	dbBeginTransaction(conn);

	k <- 0L;
	i <- 1L;
	n <- as.integer(nrow(Scopus_SourceList));
	
	if (verbose)
	{
		cat("Importing source list into 'Biblio_Sources'... ");
		window <- CITAN:::.gtk2.progressBar(0, n, info="Importing source list into 'Biblio_Sources'... ");
	}

	while (i <= n)
	{
		query <- sprintf("INSERT %s INTO Biblio_Sources('IdSource', 'Title',
		      'ISSN_Print', 'ISSN_E', 'IsActive', 'IsOpenAccess', 'Type', 'IdCountry',
		      'Impact')
		   VALUES (%g, '%s', %s, %s, %s, %s, %s, %s, %s);",
		   ifelse(updateSourceIfExists, "OR REPLACE", "OR IGNORE"),
			as.integer(i),
			sqlEscapeTrim(Scopus_SourceList$Title[i]),
			sqlStringOrNULL(Scopus_SourceList$ISSN_Print[i]),
			sqlStringOrNULL(Scopus_SourceList$ISSN_E[i]),
			as.integer(Scopus_SourceList$Status[i] == "Active"),
			as.integer(Scopus_SourceList$OpenAccess[i] != "Not OA"),
			sqlSwitchOrNULL(Scopus_SourceList$Type[i],
			   CITAN:::.lbs_SourceTypesFull,
			   CITAN:::.lbs_SourceTypesShort
			),
			sqlNumericOrNULL(IdCountry[i]),
			sqlNumericOrNULL(Impact[i])
		);

		dbExecQuery(conn, query, TRUE);

		if (verbose) CITAN:::.gtk2.progressBar(i, n, window=window);
		
		i <- i+1L;
	};

	dbCommit(conn);



	# now check which rows were added and report missing values
	res <- dbGetQuery(conn, "SELECT IdSource FROM Biblio_Sources;");
	k <- nrow(res); # number of records added
	omitted <- (1:n)[-res[,1]];
	if (length(omitted) > 0)
		warning(
			sprintf("%g records omitted due to not unique ISSNs (conflict policy used: updateSourceIfExists==%s).\
Here are their identifiers: %s.",
			length(omitted), as.character(updateSourceIfExists), paste(omitted,collapse=","))
		);

	if (verbose) cat(sprintf("OK, %g of %g records added.\n", k, n));
}





#' /internal/
.Scopus_ImportSources_SourcesCategories <- function(conn, verbose)
{
	dbBeginTransaction(conn);


	idSources <- dbGetQuery(conn, "SELECT IdSource FROM Biblio_Sources;");
	idSources <- idSources[,1];
	n <- length(idSources);
	i <- 1;
	k <- 0;
	
	
	if (verbose)
	{
		cat("Mapping ASJC codes to sources in 'Biblio_SourcesCategories'... ");
		window <- CITAN:::.gtk2.progressBar(0, n, info="Mapping ASJC codes to sources in 'Biblio_SourcesCategories'... ");
	}
	
	while (i <= n)
	{
		src <- idSources[i];

		asjc <- strsplit(Scopus_SourceList$ASJC[src], "[[:space:]]*;[[:space:]]*")[[1]];
		if (length(asjc) == 0 || asjc[1] == "")
		{
			warning(sprintf("No ASJC for IdSource=%g.", src));
		} else
		{
			for (j in 1:length(asjc))
			{
				a <- as.integer(asjc[j]);
				if (is.na(a) || a < 1000 || a > 9999)
				{
					warning(sprintf("Incorrect ASJC '%s' for IdSource=%g.", asjc[j], src));
					next;
				}

				query <- sprintf("INSERT INTO Biblio_SourcesCategories('IdSource', 'IdCategory')
				   VALUES(%g, %g);", src, a);

				dbExecQuery(conn, query, TRUE);
				k <- k+1;
			}
		}

		if (verbose) CITAN:::.gtk2.progressBar(i,n,window=window)

		i <- i+1;
	}


	if (verbose) cat(sprintf("OK, %g records added.\n", k));

	dbCommit(conn);
}


#' Imports \emph{SciVerse Scopus} covered titles and their ASJC codes to an empty Local Bibliometric Storage (\acronym{LBS}).
#'
#' This routine should be called prior to importing any document information
#' to the LBS with the function \code{\link{lbsImportDocuments}}.
#'
#' If multiple sources with the same ISSN (either Print-ISSN or E-ISSN)
#' are found in \code{\link{Scopus_SourceList}}
#' and \code{updateSourceIfExists} is \code{FALSE}, then
#' only the first matching record is added to the table \code{Biblio_Sources}.
#' Additionally, a warning is generated.
#' Otherwise, the last non-unique record is taken into account.
#' Note that adding all the sources takes some time.
#'
#' Only basic ASJC and \emph{SciVerse Scopus} source information
#' retrieved from \code{\link{Scopus_ASJC}} (stored in the table \code{Biblio_Categories})
#' and \code{\link{Scopus_SourceList}} (stored in tables \code{Biblio_Countries} and \code{Biblio_Sources})
#' will be added to the LBS.
#'
#'
#' @title Import SciVerse Scopus coverage information and ASJC codes to a Local Bibliometric Storage
#' @param conn a connection object as produced by \code{\link{lbsConnect}}.
#' @param impactColumn single character value determining column name in \code{\link{Scopus_SourceList}} with values of some source impact measurements to be imported, e.g. \code{"SJR_2009"}, or \code{NULL} if no such data should be saved.
#' @param updateSourceIfExists logical; if \code{FALSE}, in case of sources with the same ISSN only the first will be added.
#' @param verbose logical; \code{TRUE} to inform about the progress of the process.
#' @return \code{TRUE} on success.
#' @export
#' @examples
#' \dontrun{
#' conn <- lbsConnect("Bibliometrics.db");
#' lbsCreate(conn);
#' Scopus_ImportSources(conn);
#' ## ...
#' dbDisconnect(conn);}
#' @seealso \code{\link{Scopus_ASJC}}, \code{\link{Scopus_SourceList}}, \code{\link{Scopus_ReadCSV}}, \code{\link{lbsConnect}}, \code{\link{lbsCreate}}
Scopus_ImportSources <- function(conn, impactColumn=NULL, updateSourceIfExists=FALSE, verbose=T)
{
	CITAN:::.lbsCheckConnection(conn); # will stop on invalid/dead connection


	if (is.null(impactColumn)) {
		Impact <- NA;
	} else {
		Impact <- Scopus_SourceList[impactColumn]; # stops if undefined
		if (class(Impact) != "numeric") stop("given 'impactColumn' is not numeric.");
	}

	# ----------------------------------------------------------------------


	.Scopus_ImportSources_Categories(conn, verbose)

	IdCountry <- .Scopus_ImportSources_Countries(conn, verbose)

	.Scopus_ImportSources_Sources(conn, IdCountry, Impact, updateSourceIfExists, verbose)

	.Scopus_ImportSources_SourcesCategories(conn, verbose)


	# ----------------------------------------------------------------------

	dbExecQuery(conn, "VACUUM", FALSE);



	# ----------------------------------------------------------------------

	return(TRUE);
}
