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




#' Creates citation sequences for authors in a local bibliometric storage,
#' that is vectors of citation counts of all the documents mapped to
#' selected  authors.
#' The results may be restricted to a given Survey (using \code{SurveyDescription}
#' parameter) or document types (\code{DocumentTypes}).
#'
#' @title Fetch citation sequences for authors in a local bibliometric storage
#' @param con a connection object as produced by \code{\link{dbBiblioConnect}}.
#' @param DocumentTypes character vector or \code{NULL}; specifies document types to restrict to;
#'    a combination of \code{Article}, \code{Article in Press}, \code{Book}, \code{Conference Paper},
#'    \code{Editorial}, \code{Erratum}, \code{Letter}, \code{Note}, \code{Report}, \code{Review},
#'    \code{Short Survey}. \code{NULL} means no restriction.
#' @param SurveyDescription single character string or \code{NULL}; survey to restrict to or \code{NULL} for no restriction.
#' @param IdAuthors numeric vector od author identifiers to restrict to (in the table \code{Biblio_Authors}) or \code{NULL} for no restriction.
#' @param verbose logical; \code{TRUE} to print out the progress of lengthy computations.
#' @return A list of numeric vectors is returned. Each element of the list corresponds
#' to a citation sequence of a different author. List \code{names} attribute are
#' set to authors' names. Moreover, each vector has a set \code{IdAuthor}
#' attribute, which uniquely identifies a corresponding record in the table \code{Biblio_Authors}.
#' Citation counts come together with \code{IdDocument}s (vector elements are named).
#'
#' The list of citation sequences may then be used to calculate
#' authors' impact using \code{\link{dbBiblioAssess}} (see Examples below).
#' @examples
#' \dontrun{con <- dbBiblioConnect("Bibliometrics.db");}
#' ## ...
#' \dontrun{citseq <- dbBiblioGetCitations(con,
#'          SurveyDescription="Scientometrics", DocumentTypes="Article",
#'          IdAuthor=c(39264,39265,39266));}
#' \dontrun{print(citseq);}
#' ## $`Liu X.`                                # Author name
#' ## 40116 34128 39122 29672 32343 32775      # IdDocument
#' ##    11     4     1     0     0     0      # Citation count
#' ## attr(,"IdAuthor")
#' ## [1] 39264                                # IdAuthor
#' ## 
#' ## $`Xu Y.`
#' ## 38680 38605 40035 40030 40124 39829 39745 29672 
#' ##    30    14     8     6     6     5     3     0 
#' ## attr(,"IdAuthor")
#' ## [1] 39265
#' ## 
#' ## $`Wang Y.`
#' ## 29992 29672 29777 32906 33858 33864 34704 
#' ##     1     0     0     0     0     0     0 
#' ## attr(,"IdAuthor")
#' ## [1] 39266
#' \dontrun{print(dbBiblioAssess(citseq,
#'    f=list(length, sum, index.h, index.g, function(x) index.rp(x,1),
#'        function(x) sqrt(prod(index.lp(x,1))), function(x) sqrt(prod(index.lp(x,Inf)))),
#'    captions=c("length", "sum", "index.h", "index.g", "index.w", "index.lp1", "index.lpInf")));}
#' ##      Name length sum index.h index.g index.w index.lp1 index.lpInf
#' ## 3   Xu Y.      8  72       5       8       7  8.573214    5.477226
#' ## 2 Wang Y.      7   1       1       1       1  1.000000    1.000000
#' ## 1  Liu X.      6  16       2       4       3  4.157609    3.316625
#' ## ...
#' \dontrun{dbDisconnect(con);}
#' @seealso \code{\link{dbBiblioConnect}}, \code{\link{dbBiblioAssess}}
#' @export
dbBiblioGetCitations <- function(con,
	DocumentTypes=NULL,
	SurveyDescription=NULL,
	IdAuthors=NULL,
	verbose=TRUE
)
{
	CITAN:::.dbBiblioCheckConnection(con); # will stop on invalid/dead connection
	
	# -----------------------------------------------------
	# Data set restrictions & subset stats
	
	SurveyDescription  <- CITAN:::.dbBiblio_PrepareRestriction_SurveyDescription(con, SurveyDescription);
	DocumentTypesShort <- CITAN:::.dbBiblio_PrepareRestriction_DocumentTypes(con, DocumentTypes);
	
	
	
	# Get subQueryWhere
	if (length(DocumentTypesShort)>0)
	{
		subQueryWhere <- sprintf("(%s)",
			paste("Type", DocumentTypesShort, sep="=", collapse=" OR "));
	} else subQueryWhere <- "1";

	if (!is.null(SurveyDescription))
		subQueryWhere <- paste(c(subQueryWhere, sprintf(" Description='%s'", SurveyDescription)), collapse=" AND ");
	
	
	
	cat("Data set restrictions:\n");
	cat(sprintf("\tSurvey:         %s.\n", ifelse(is.null(SurveyDescription), "<ALL>", SurveyDescription)));
	cat(sprintf("\tDocument types: %s.\n", ifelse(is.null(DocumentTypesShort), "<ALL>", paste(DocumentTypesShort, collapse=", "))));
	cat("\n");

	# ---------------------------------------------------------------------
	
	
	if (!is.null(IdAuthors) && (!is.numeric(IdAuthors) || any(!is.finite(IdAuthors))))
		stop("incorrect 'IdAuthors' given");
		
	if (length(IdAuthors) == 0)
	{
		query <- sprintf("
		SELECT IdAuthor
		FROM
		(
			SELECT DISTINCT Biblio_AuthorsDocuments.IdAuthor
			FROM Biblio_AuthorsDocuments
			JOIN
			(
				SELECT Biblio_Documents.IdDocument
				FROM Biblio_Documents
				JOIN ViewBiblio_DocumentsSurveys ON ViewBiblio_DocumentsSurveys.IdDocument=Biblio_Documents.IdDocument
				WHERE %s
			) AS Docs ON Docs.IdDocument=Biblio_AuthorsDocuments.IdDocument
		);",
		subQueryWhere);
		
		IdAuthors <- dbGetQuery(con, query)[,1];
		
		if (length(IdAuthors) == 0) return(list());
	}
	
	
	

	
	if (verbose) cat(sprintf("Creating citation sequences... "));
	
	
	i <- 1L;
	k <- 0L;
	n <- as.integer(length(IdAuthors));
	citseq <- list();
	length(citseq) <- n;
	
	while (i <= n)
	{
		query <- sprintf("
			SELECT DISTINCT Biblio_Authors2.Name AS Name, Biblio_Documents.IdDocument AS IdDocument, Biblio_Documents.Citations AS Citations
			FROM
			(
				SELECT Name, IdAuthor FROM Biblio_Authors WHERE IdAuthor=%s
			) AS Biblio_Authors2
			JOIN Biblio_AuthorsDocuments ON (Biblio_Authors2.IdAuthor=Biblio_AuthorsDocuments.IdAuthor)
			JOIN ViewBiblio_DocumentsSurveys ON (Biblio_AuthorsDocuments.IdDocument=ViewBiblio_DocumentsSurveys.IdDocument)
			JOIN Biblio_Documents ON (Biblio_AuthorsDocuments.IdDocument=Biblio_Documents.IdDocument)
			WHERE %s
			ORDER BY Biblio_Documents.Citations DESC",
			sqlNumericOrNULL(IdAuthors[i]),
			subQueryWhere
		);

		AuthorInfo <- dbGetQuery(con, query);
		
		if (nrow(AuthorInfo) > 0)
		{
			names(citseq)[i] <- AuthorInfo$Name[1];
			citseq[[i]] <- as.numeric(AuthorInfo$Citations);
			names(citseq[[i]]) <- as.numeric(AuthorInfo$IdDocument);
			attr(citseq[[i]], "IdAuthor") <- IdAuthors[i];
			k <- k+1L;
		}
	
		if (verbose && (i %% 50L == 0))
			cat(sprintf(" %4.1f%%\b\b\b\b\b\b", i*100.0/n));
		i <- i+1L;
	}
	
	
	if (verbose) cat(sprintf("OK, %g of %g records read.\n", k, n));
	
	return(citseq);
}
