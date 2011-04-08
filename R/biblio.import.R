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


#' /internal/
.dbBiblioImportDocuments_GetSurvey <- function(con, surveyDescription, originalFilename, verbose)
{
	query <- sprintf("INSERT INTO Biblio_Surveys('Description', 'FileName', 'Timestamp')
		VALUES(%s, %s, %s)",
		sqlStringOrNULL(surveyDescription),
		sqlStringOrNULL(originalFilename),
		sqlStringOrNULL(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
	);
	dbExecQuery(con, query, TRUE);

	return(as.numeric(dbGetQuery(con, "SELECT last_insert_rowid()")[1,1]));
}


#' /internal/
.dbBiblioImportDocuments_GetIdLanguage <- function(con, data, verbose)
{
	IdLanguage <- data$Language;
	levels(IdLanguage) <- sqlTrim(levels(IdLanguage));
	for (i in 1:length(levels(IdLanguage)))
	{
		if (is.na(levels(IdLanguage)[i]))
			next;

		if (levels(IdLanguage)[i]=="")
			next;

		query <- sprintf("INSERT OR IGNORE INTO Biblio_Languages('Name')
			VALUES('%s')", sqlEscape(levels(IdLanguage)[i]));
		dbExecQuery(con, query, TRUE);

		levels(IdLanguage)[i] <- dbGetQuery(con,
			sprintf("SELECT IdLanguage FROM Biblio_Languages WHERE Name='%s';",
			sqlEscape(levels(IdLanguage)[i])))[1,1];
		stopifnot(!is.na(levels(IdLanguage)[i]));
	}

	levels(IdLanguage)[levels(IdLanguage)==""] <- NA;

	return(IdLanguage);
}



#' /internal/
.dbBiblioImportDocuments_Add_Get_idSource <- function(con, issn, warnISSN, i)
{
	if (!is.na(issn))
	{
		idSource <- dbGetQuery(con, sprintf("SELECT idSource FROM Biblio_Sources
			WHERE ISSN_Print=%s OR ISSN_E=%s",
			sqlStringOrNULL(issn),
			sqlStringOrNULL(issn)
		));

		if (nrow(idSource)==0)
		{
			if (warnISSN)
			{
				warning(sprintf("no source with ISSN='%s' found for record %g. Setting IdSource=NA.",
					issn, i));
			}
			return(NA);
		}	else {
			if (nrow(idSource)>1)
				warning(sprintf("more than one source with ISSN='%s' found for record %g. Using first.", issn, i));
			return(idSource[1,1]);
		}
	} else {
		return(NA);
	}
}


#' /internal/
.dbBiblioImportDocuments_Add_Get_BibEntry <- function(con, data_i)
{
	paste(c(
		sqlTrim(data_i$SourceTitle[1]), 
		sqlTrim(data_i$Year[1]), 
		sqlTrim(data_i$Volume[1]), 
		sqlTrim(data_i$Issue[1]), 
		sqlTrim(data_i$ArticleNumber[1]), 
		sqlTrim(data_i$PageStart[1]), 
		sqlTrim(data_i$PageEnd[1])), collapse=",");
}


#' /internal/
.dbBiblioImportDocuments_Add <- function(con, data, excludeRows, idSurvey,
	IdLanguage, i, updateDocumentIfExists, warnISSN, warnExactDuplicates,
	warnDuplicateTitles, verbose)
{
	if (any(excludeRows==i)) return(FALSE);

	issn <- data$ISSN[i];
	if (!is.na(issn) && nchar(issn) != 8)
	{
		if (nchar(issn)>0)
			warning(sprintf("incorrect ISSN='%s' for record %g. Setting NA.", issn, i));
		issn <- NA;
	}

	idSource <- .dbBiblioImportDocuments_Add_Get_idSource(con, issn, warnISSN, i);
	BibEntry <- .dbBiblioImportDocuments_Add_Get_BibEntry(con, data[i,]);



	res <- dbGetQuery(con, sprintf("SELECT IdDocument, IdSource, Title, BibEntry, Citations, Type
		FROM Biblio_Documents WHERE UniqueId='%s';", sqlEscapeTrim(data$UniqueId[i])));

	if (nrow(res) != 0)
	{
		documentExists <- TRUE;
		idDocument <- res$IdDocument[1];
		
		if (warnExactDuplicates ||  res$Title[1] != data$Title[i] || 
		                            res$BibEntry[1] != BibEntry ||
		                            res$Citations[1] != ifelse(is.finite(data$Citations[i]), data$Citations[i], 0))
		{
			warning(sprintf("source at row=%g already exists (IdSource=%g, Title='%s', Citations=%g, Type='%s'). %s.",
				i, res$IdSource[1], res$Title[1], res$Citations[1], res$Type[1],
				ifelse(updateDocumentIfExists, "Updating", "Ignoring")));
		}
	
		if (updateDocumentIfExists)
		{
		
			 # will add authors once again later (they may be different)
			dbExecQuery(con, sprintf("DELETE FROM Biblio_AuthorsDocuments WHERE IdDocument=%g;", idDocument), TRUE);
			
			
			# Update document
			query <- sprintf("UPDATE Biblio_Documents
				SET
					IdSource=%s,
					IdLanguage=%s,
					UniqueId='%s',
					Title='%s',
					BibEntry='%s',
					Year=%s,
					Pages=%s,
					Citations=%s,
					Type=%s
				WHERE IdDocument=%s;",
				
				sqlNumericOrNULL(idSource),
				sqlNumericOrNULL(IdLanguage[i]),
				sqlEscapeTrim(data$UniqueId[i]),
				sqlEscapeTrim(data$Title[i]),
				sqlEscape(BibEntry),
				sqlNumericOrNULL(data$Year[i]),
				sqlNumericOrNULL(data$PageEnd[i]-data$PageStart[i]+1),
				ifelse(is.finite(data$Citations[i]), data$Citations[i], 0),
				sqlSwitchOrNULL(data$DocumentType[i],
					CITAN:::.dbBiblio_DocumentTypesFull,
					CITAN:::.dbBiblio_DocumentTypesShort
				),
				sqlNumericOrNULL(idDocument)
			);
			dbExecQuery(con, query, TRUE);
		}
	} else {
		documentExists <- FALSE;
		
		
		# If a title is not unique, warn on demand (That may indicate non-unique-UniqueId)
		if (warnDuplicateTitles)
		{
			res <- dbGetQuery(con, sprintf("SELECT IdDocument
			FROM Biblio_Documents WHERE Title='%s';", sqlEscapeTrim(data$Title[i])));
			
			if (nrow(res)!=0)
				warning(sprintf("a document with the same title '%s' already exists (row=%g) but has different 'UniqueId'.",
					sqlTrim(data$Title[i]), i));
		}
		
		
		# Insert document
		query <- sprintf("INSERT OR FAIL INTO Biblio_Documents ('IdSource', 'IdLanguage',
			'UniqueId', 'Title', 'BibEntry', 'Year', 'Pages', 'Citations', 'Type')
			VALUES(%s, %s, '%s', '%s', '%s', %s, %s, %s, %s);",
			sqlNumericOrNULL(idSource),
			sqlNumericOrNULL(IdLanguage[i]),
			sqlEscapeTrim(data$UniqueId[i]),
			sqlEscapeTrim(data$Title[i]),
			sqlEscape(BibEntry),
			sqlNumericOrNULL(data$Year[i]),
			sqlNumericOrNULL(data$PageEnd[i]-data$PageStart[i]+1),
			ifelse(is.finite(data$Citations[i]), data$Citations[i], 0),
			sqlSwitchOrNULL(data$DocumentType[i],
				CITAN:::.dbBiblio_DocumentTypesFull,
				CITAN:::.dbBiblio_DocumentTypesShort
			)
		);
		dbExecQuery(con, query, TRUE);
		
		
		idDocument <- dbGetQuery(con, "SELECT last_insert_rowid()")[1,1];
	}
	
	
	
	query <- sprintf("INSERT INTO Biblio_DocumentsSurveys (IdDocument, IdSurvey) VALUES(%s, %s);",
		sqlNumericOrNULL(idDocument), sqlNumericOrNULL(idSurvey));
	dbExecQuery(con, query, TRUE);


	if (documentExists && !updateDocumentIfExists) return(FALSE);


	# add authors
	authors <- strsplit(data$Authors[i], "[[:space:]]*,[[:space:]]*")[[1]];
	stopifnot(length(authors)>0);
	for (j in 1:length(authors))
	{
		stopifnot(nchar(authors[j])>0);
		
		
		# Get idAuthor (and add him/her eventually)
		idAuthor <- dbGetQuery(con, sprintf("SELECT IdAuthor FROM Biblio_Authors WHERE Name=%s",
			sqlStringOrNULL(authors[j])
		));
		if (nrow(idAuthor) == 0)
		{
			dbExecQuery(con, sprintf("INSERT INTO Biblio_Authors(Name) VALUES(%s);", sqlStringOrNULL(authors[j])), TRUE);
			idAuthor <- dbGetQuery(con, "SELECT last_insert_rowid()")[1,1];
		} else {
			idAuthor <- idAuthor[1,1];
		}
		

		query <- sprintf("INSERT OR IGNORE INTO Biblio_AuthorsDocuments(IdAuthor, IdDocument)
			VALUES(%s, %s);",
			sqlNumericOrNULL(idAuthor),
			sqlNumericOrNULL(idDocument));
		dbExecQuery(con, query, TRUE);
	}
	
	return(!documentExists)
}



#' Imports publications from a special 14-column data frame to a local bibliometric storage.
#' Such an input may be created e.g. with \code{\link{Scopus_ReadCSV}}.
#'
#'
#' \code{data} must consist of the following 14 columns (in order). Otherwise
#' the process will not be executed.
#' \tabular{llll}{
#' 1  \tab \code{Authors}       \tab \code{character}\tab  Author(s) name(s), comma-separated, surnames first.\cr
#' 2  \tab \code{Title}         \tab \code{character}\tab  Document title.\cr
#' 3  \tab \code{Year}          \tab \code{numeric}  \tab  Year of publication.\cr
#' 4  \tab \code{SourceTitle}   \tab \code{character}\tab  Title of the source containing the document.\cr
#' 5  \tab \code{Volume}        \tab \code{character}\tab  Volume.\cr
#' 6  \tab \code{Issue}         \tab \code{character}\tab  Issue.\cr
#' 7  \tab \code{ArticleNumber} \tab \code{character}\tab  Article number (identifier).\cr
#' 8  \tab \code{PageStart}     \tab \code{numeric}  \tab  Start page; numeric.\cr
#' 9  \tab \code{PageEnd}       \tab \code{numeric}  \tab  End page; numeric.\cr
#' 10 \tab \code{Citations}     \tab \code{numeric}  \tab  Number of citations.\cr
#' 11 \tab \code{UniqueId}      \tab \code{character}\tab  Unique document identifier. Documents with \code{is.na(UniqueId)} will not be added.\cr
#' 12 \tab \code{ISSN}          \tab \code{character}\tab  ISSN of the source.\cr
#' 13 \tab \code{Language}      \tab \code{factor}   \tab  Language of the document.\cr
#' 14 \tab \code{DocumentType}  \tab \code{factor}   \tab  Type of the document; \dQuote{Article}, \dQuote{Article in Press},
#'        \dQuote{Book}, \dQuote{Conference Paper}, \dQuote{Editorial}, \dQuote{Erratum},
#'        \dQuote{Letter}, \dQuote{Note}, \dQuote{Report},
#'        \dQuote{Review}, \dQuote{Short Survey}, or \code{NA} (other categories are interpreted as \code{NA}).\cr
#' }
#'
#' Note that if \code{data} contains many records (>1000),
#' the import process may take a few minutes.
#'
#' Sources (e.g. journals) are identified by ISSNs (table \code{Biblio_Sources}).
#' Note that generally there is no need to concern about missing ISSNs of
#' conference proceedings.
#'
#' Each time a function is called, a new record in the table \code{Biblio_Surveys}
#' is created. Such surveys may be grouped using the \code{Description}
#' field, see \code{\link{dbBiblioCreate}}.
#'
#' @title Import publications to a local bibliometric storage.
#' @param con a connection object as produced by \code{\link{dbBiblioConnect}}.
#' @param data 14 column \code{data.frame} with bibliometric entries; see above.
#' @param surveyDescription Description of the survey. Allows for documents grouping.
#' @param originalFilename Original file name, \code{attr(data, "filename")} is used by default.
#' @param excludeRows a numeric vector with row numbers of \code{data} to exclude or \code{NULL}
#' @param updateDocumentIfExists logical; if \code{TRUE}, then documents with the same \code{UniqueId} will be updated.
#' @param doVacuum logical; if \code{TRUE} then the SQL command \code{VACUUM}
#'        will be executed on the database after importing data to optimize and compact the bibliometric storage.
#' @param warnISSN logical; if \code{TRUE} then warnings are generated if a given ISSN in not found in the table \code{Biblio_Sources}.
#' @param warnExactDuplicates logical; \code{TRUE} to warn if exact duplicates are found (turned off by default).
#' @param warnDuplicateTitles logical; \code{TRUE} to warn if documents
#'        of the same \code{Title} but different \code{UniqueId}s are found.
#'        This may indicate possible data misrepresentations.
#' @param verbose logical; \code{TRUE} to print out the progress of lengthy computations. 
#' @return  \code{TRUE} on success.
#' @seealso \code{\link{Scopus_ReadCSV}}, \code{\link{dbBiblioConnect}}, \code{\link{dbBiblioCreate}}
#' @examples
#' \dontrun{con <- dbBiblioConnect("Bibliometrics.db");}
#' ## ...
#' \dontrun{data <- Scopus_ReadCSV("db_Polish_MATH/Poland_MATH_1987-1993.csv");}
#' \dontrun{dbBiblioImportDocuments(con, data, "Poland_MATH");}
#' ## ...
#' \dontrun{dbDisconnect(con);}
#' @export
dbBiblioImportDocuments <- function(con, data, surveyDescription="Default survey",
   originalFilename=attr(data, "filename"), excludeRows=NULL,  updateDocumentIfExists=TRUE, doVacuum=TRUE,
   warnISSN=FALSE, warnExactDuplicates=FALSE, warnDuplicateTitles=TRUE, verbose=TRUE)
{
	CITAN:::.dbBiblioCheckConnection(con); # will stop on invalid/dead connection



	if (class(data) != "data.frame")
		stop("'data' is not a data.frame.");

	if (ncol(data) != 14 || any(names(data) != c("Authors", "Title", "Year", "SourceTitle", "Volume",
			"Issue", "ArticleNumber", "PageStart", "PageEnd", "Citations", "UniqueId", "ISSN", "Language", "DocumentType")))
		stop("incorrect format of 'data'.");

	if (class(data$Authors)!="character")       stop("column 'Authors' in 'data' should be 'character'.");
	if (class(data$Title)!="character")         stop("column 'Title' in 'data' should be 'character'.");
	if (class(data$Year)!="numeric")            stop("column 'Year' in 'data' should be 'numeric'.");
	if (class(data$SourceTitle)!="character")   stop("column 'SourceTitle' in 'data' should be 'character'.");
	if (class(data$Volume)!="character")        stop("column 'Volume' in 'data' should be 'character'.");
	if (class(data$Issue)!="character")         stop("column 'Issue' in 'data' should be 'character'.");
	if (class(data$ArticleNumber)!="character") stop("column 'ArticleNumber' in 'data' should be 'character'.");
	if (class(data$PageStart)!="numeric")       stop("column 'PageStart' in 'data' should be 'numeric'.");
	if (class(data$PageEnd)!="numeric")         stop("column 'PageEnd' in 'data' should be 'numeric'.");
	if (class(data$Citations)!="numeric")       stop("column 'Citations' in 'data' should be 'numeric'.");
	if (class(data$UniqueId)!="character")      stop("column 'UniqueId' in 'data' should be 'character'.");
	if (class(data$ISSN)!="character")          stop("column 'ISSN' in 'data' should be 'character'.");
	if (class(data$Language)!="factor")         stop("column 'Language' in 'data' should be 'factor'.");
	if (class(data$DocumentType)!="factor")     stop("column 'DocumentType' in 'data' should be 'factor'.");


	if (!is.null(excludeRows) && !is.numeric(excludeRows))
		stop("'excludeRows' must be numeric or NULL.");

	if (is.null(originalFilename) || is.na(originalFilename) || length(originalFilename) != 1)
		originalFilename <- "Unknown filename";

	if (is.null(surveyDescription) || is.na(surveyDescription) || length(surveyDescription) != 1)
		surveyDescription <- "Default survey";

	# -------------------------------------------------------------------

	dbBeginTransaction(con);

	idSurvey <- .dbBiblioImportDocuments_GetSurvey(con, surveyDescription, originalFilename, verbose);
	stopifnot(length(idSurvey) == 1 && is.finite(idSurvey));

	IdLanguage <- .dbBiblioImportDocuments_GetIdLanguage(con, data, verbose);
	stopifnot(length(IdLanguage) == nrow(data));
	
	if (verbose) cat(sprintf("Importing documents and their authors... "));


	i <- 1L;
	k <- 0L;
	n <- as.integer(nrow(data));
	while (i <= n)
	{
		if (.dbBiblioImportDocuments_Add(con, data, excludeRows, idSurvey, IdLanguage, i,
			updateDocumentIfExists, warnISSN, warnExactDuplicates, warnDuplicateTitles, verbose))
		{
			k <- k+1L;
		}
		
		if (verbose && (i %% 50L == 0))
			cat(sprintf(" %4.1f%%\b\b\b\b\b\b", i*100.0/n));
		i <- i+1L;
	}



	if (verbose) cat(sprintf("OK, %g of %g records added to %s/%s.\n", k, n, surveyDescription, originalFilename));

	# -------------------------------------------------------------------

	dbCommit(con);

	# -------------------------------------------------------------------


	if (doVacuum)
		dbExecQuery(con, "VACUUM", FALSE);

	return(TRUE);
}
