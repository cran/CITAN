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
.dbBiblioDescriptiveStats_PrintStatsSubset <- function(con, subQueryWhere)
{
	query <- sprintf("
		SELECT COUNT(idDocument)
		FROM
		(
			SELECT DISTINCT Biblio_Documents.IdDocument
			FROM Biblio_Documents
			JOIN ViewBiblio_DocumentsSurveys ON ViewBiblio_DocumentsSurveys.IdDocument=Biblio_Documents.IdDocument
			WHERE %s
		);",
		subQueryWhere);
	res <- dbGetQuery(con, query);
	cat(sprintf("Number of documents in the selected subset: %g.\n", res[1,1]));


	query <- sprintf("
		SELECT COUNT(IdAuthor)
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
	res <- dbGetQuery(con, query);
	cat(sprintf("Number of authors in the selected subset:   %g.\n", res[1,1]));
	
	cat("\n");
}



#' /internal/
.dbBiblioDescriptiveStats_PrintSurveyStats <- function(con, subQueryWhere, split2filenames)
{
	if (!split2filenames)
	{
		query <- sprintf("
			SELECT Description AS SurveyDescription, COUNT(IdDocument) AS DocumentCount
			FROM
			(
				SELECT DISTINCT Description, ViewBiblio_DocumentsSurveys.IdDocument
				FROM ViewBiblio_DocumentsSurveys
				JOIN Biblio_Documents ON ViewBiblio_DocumentsSurveys.IdDocument=Biblio_Documents.IdDocument
				WHERE %s
			) GROUP BY Description;",
			subQueryWhere
		);
		res <- dbGetQuery(con, query);
		
		cat("Surveys:\n");
		print(res);
	} else
	{
		query <- sprintf("
			SELECT Filename, Timestamp, COUNT(IdDocument) AS DocumentCount
			FROM
			(
				SELECT DISTINCT Filename, Timestamp, ViewBiblio_DocumentsSurveys.IdDocument
				FROM ViewBiblio_DocumentsSurveys
				JOIN Biblio_Documents ON ViewBiblio_DocumentsSurveys.IdDocument=Biblio_Documents.IdDocument
				WHERE %s
			) GROUP BY Filename;",
			subQueryWhere
		);
		res <- dbGetQuery(con, query);
		
		cat("Source files in selected survey:\n");
		print(res);
	}
	
	cat("  * Note that a document may be added from many surveys/files.\n");
	cat("\n");
}





#' Performs preliminary data analysis of data in a local bibliometric storage
#' by displaying some basic descriptive statistics and plots.
#' We may restrict ourselves to some selected document types
#' or a single survey.
#'
#' Plot types (accessed with \code{which}):
#' \itemize{
#' 	\item \code{1} --- "Document types", 
#' 	\item \code{2} --- "Publication years", 
#' 	\item \code{3} --- "Citations per document",
#' 	\item \code{4} --- "Citations of cited documents per type", 
#' 	\item \code{5} --- "Number of pages per document type",
#' 	\item \code{6} --- "Categories of documents" (based od source categories), 
#' 	\item \code{7} --- "Documents per author".
#' }
#'
#' The user interaction scheme is inspired by the \code{\link{plot.lm}} code.
#'
#' @title Perform preliminary data analysis of data in a local bibliometric storage
#' @param con a connection object as produced by \code{\link{dbBiblioConnect}}.
#' @param DocumentTypes character vector or \code{NULL}; specifies document types to restrict to;
#'    a combination of \code{Article}, \code{Article in Press}, \code{Book}, \code{Conference Paper},
#'    \code{Editorial}, \code{Erratum}, \code{Letter}, \code{Note}, \code{Report}, \code{Review},
#'    \code{Short Survey}. \code{NULL} means no restriction.
#' @param SurveyDescription single character string or \code{NULL}; survey to restrict to or \code{NULL} for no restriction.
#' @param which if a subset of the plots is required, specify a subset of the numbers \code{1:7}.
#' @param main title to each plot-in addition to default captions.
#' @param ask logical; if \code{TRUE} then the user is asked to press return before each plot.
#' @param ... additional graphical parameters, see \code{\link{plot.default}}.
#' @param cex.caption controls the size of default captions.
#' @examples
#' \dontrun{con <- dbBiblioConnect("Bibliometrics.db");}
#' ## ...
#' \dontrun{dbBiblioDescriptiveStats(con, SurveyDescription="Scientometrics", DocumentTypes=c("Article", "Note", "Report", "Review", "Short Survey"));}
#' ## ...
#' \dontrun{dbDisconnect(con);}
#' @seealso \code{\link{plot.default}}, \code{\link{dbBiblioConnect}}
#' @export
dbBiblioDescriptiveStats <- function(con,
	DocumentTypes=NULL,
	SurveyDescription=NULL,
	which=(1L:7L), 
	main="",
	ask = (prod(par("mfcol")) < length(which) && dev.interactive()),
	...,
	cex.caption=1
)
{
	CITAN:::.dbBiblioCheckConnection(con); # will stop on invalid/dead connection
	
	# -----------------------------------------------------
	# Basic stats
	
	res <- dbGetQuery(con, "SELECT COUNT(idSource) FROM Biblio_Sources;");
	cat(sprintf("Number of sources in the database:   %g.\n", res[1,1]));
	
	res <- dbGetQuery(con, "SELECT COUNT(idDocument) FROM Biblio_Documents;");
	cat(sprintf("Number of documents in the database: %g.\n", res[1,1]));

	res <- dbGetQuery(con, "SELECT COUNT(idAuthor) FROM Biblio_Authors;");
	cat(sprintf("Number of authors in the database:   %g.\n", res[1,1]));
	
	cat("\n");


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



	if (length(DocumentTypesShort)>0 || !is.null(SurveyDescription))
		.dbBiblioDescriptiveStats_PrintStatsSubset(con, subQueryWhere);
		

	
	# -----------------------------------------------------
	# Survey(s) stats
	
	.dbBiblioDescriptiveStats_PrintSurveyStats(con, subQueryWhere, !is.null(SurveyDescription));
	

	
	# -----------------------------------------------------
	# User interaction scheme is based on plot.lm() code
	
	show <- rep(FALSE, 7)
	show[which] <- TRUE
	
	if (ask) {
		oask <- devAskNewPage(TRUE);
		on.exit(devAskNewPage(oask));
	}
	
	captions <- c("Document types", "Publication years", "Citations per document",
		"Citations of cited documents per type", "Number of pages per document type",
		"Categories of documents", "Documents per author");


	
	# -----------------------------------------------------
	# 1:5
	
	if (any(show[1L:5L]))
	{
		res <- dbGetQuery(con, sprintf("
			SELECT Citations, Type, Year, Pages
			FROM Biblio_Documents
			JOIN ViewBiblio_DocumentsSurveys ON Biblio_Documents.IdDocument=ViewBiblio_DocumentsSurveys.IdDocument
			WHERE %s",
			subQueryWhere));
	} else res <- NULL;
	
	
	if (show[1L])
	{
		tab <- sort(table(res$Type), dec=TRUE);
		barplot(tab, main=main, ...);
		mtext(as.graphicsAnnot(captions[1]), 3, 0.25, cex=cex.caption);
		
		cat(sprintf("%s:\n", captions[1]));
		print(tab);
		cat("\n\n");
	}
	
	if (show[2L])
	{
		tab <- table(res$Year);
		barplot(tab, main=main, ...);
		mtext(as.graphicsAnnot(captions[2]), 3, 0.25, cex=cex.caption);
		
		cat(sprintf("%s:\n", captions[2]));
		print(tab);
		cat("\n\n");
	}
	
	if (show[3L])
	{
		tab <- table(res$Citations);
		barplot(tab, main=main, ...);
		mtext(as.graphicsAnnot(captions[3]), 3, 0.25, cex=cex.caption);
		
		cat(sprintf("%s:\n", captions[3]));
		print(tab);
		cat("\n\n");
	}
	
	if (show[4L])
	{
		boxplot(res$Citations[res$Citations>0]~res$Type[res$Citations>0], log="y", main=main, ...);
		mtext(as.graphicsAnnot(captions[4]), 3, 0.25, cex=cex.caption);
	}
	
	if (show[5L])
	{
		boxplot(res$Pages[res$Pages>0 & res$Pages<500]~res$Type[res$Pages>0 & res$Pages<500], log="y", main=main, ...);
		mtext(as.graphicsAnnot(captions[5]), 3, 0.25, cex=cex.caption);
	}
	
	
	
	# -----------------------------------------------------
	# 6
	
	if (any(show[6L:6L]))
	{
		query <- sprintf(
			"SELECT DISTINCT DocInfo.IdDocument, IdCategoryGroup, DescriptionGroup
			FROM ViewBiblio_DocumentsCategories
			JOIN
			(
				SELECT Biblio_Documents.IdDocument FROM Biblio_Documents
				JOIN ViewBiblio_DocumentsSurveys ON Biblio_Documents.IdDocument=ViewBiblio_DocumentsSurveys.IdDocument
				WHERE %s
			) AS DocInfo ON DocInfo.IdDocument=ViewBiblio_DocumentsCategories.IdDocument;",
			subQueryWhere
		);
		res <- dbGetQuery(con, query)
	} else res <- NULL;

	if (show[6L])
	{
		mergepercent <- 0.01;
		tab <- table(res$Description);
		tab2 <- tab[tab>mergepercent*sum(tab)];
		tab2 <- c(tab2, "Other"=sum(tab[tab<=mergepercent*sum(tab)]));
		tab <- tab2;
		
		pie(tab, main=main, ...);
		mtext(as.graphicsAnnot(captions[6]), 3, 0.25, cex=cex.caption);
		
		cat(sprintf("%s:\n", captions[6]));
		print(tab);
		cat("\n\n");
	}
	



	# -----------------------------------------------------
	# 7
	
	if (any(show[7L:7L]))
	{
		query <- sprintf(
			"SELECT IdAuthor, COUNT(IdDocument) AS Number
			FROM
			(
				SELECT DISTINCT Biblio_AuthorsDocuments.IdAuthor, Biblio_AuthorsDocuments.IdDocument
				FROM Biblio_AuthorsDocuments
				JOIN Biblio_Documents ON Biblio_Documents.IdDocument=Biblio_AuthorsDocuments.IdDocument
				JOIN ViewBiblio_DocumentsSurveys ON ViewBiblio_DocumentsSurveys.IdDocument=Biblio_Documents.IdDocument
				WHERE %s
			)
			GROUP BY (IdAuthor)",
			subQueryWhere
		);
		res <- dbGetQuery(con, query)
	} else res <- NULL;
	
	if (show[7L])
	{
		tab <- table(res$Number);
		barplot(tab, main=main, ...);
		mtext(as.graphicsAnnot(captions[7]), 3, 0.25, cex=cex.caption);
		
		cat(sprintf("%s:\n", captions[7]));
		print(tab);
		cat("\n\n");
	}
}
