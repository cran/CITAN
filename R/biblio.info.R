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




#' Retrieves basic information on given authors.
#' 
#' @title Retrieve author information
#' @param conn a connection object as produced by \code{\link{lbsConnect}}.
#' @param idAuthors a numeric or integer vector with author identifiers (see column \code{IdAuthor} in the table \code{Biblio_Authors}).
#' @return
#' A list of \code{authorinfo} objects, that is lists with the following components:
#' \itemize{
#' \item \code{IdAuthor} --- numeric; author's identifier in the table \code{Biblio_Authors},
#' \item \code{Name} --- character; author's name.
#' }
#' @examples
#' \dontrun{
#' #' conn <- dbBiblioConnect("Bibliometrics.db");
#' ## ...
#' id <- lbsSearchAuthors(conn, c("Smith\%", "Knuth D.E.", "V_n \%"));
#' lbsGetInfoAuthors(conn, id);
#' ## ...}
#' @seealso \code{\link{lbsSearchAuthors}}, \code{\link{lbsSearchDocuments}},
#' \code{\link{lbsGetInfoDocuments}},\cr
#' \code{\link{as.character.authorinfo}}, \code{\link{print.authorinfo}}, 
#' @export
lbsGetInfoAuthors <- function(conn, idAuthors)
{
	CITAN:::.lbsCheckConnection(conn); # will stop on invalid/dead connection

		
	if (is.null(idAuthors) || (class(idAuthors) != "numeric" && class(idAuthors) != "integer"))
		stop("'idAuthors' must be a nonempty numeric vector.");
		
	query <- "SELECT IdAuthor, Name FROM Biblio_Authors WHERE 0";
	for (i in 1:length(idAuthors))
		query <- paste(query, sprintf(" OR IdAuthor=%g", idAuthors[i]), collapse="", sep="");
	
	res <- dbGetQuery(conn, query);
	out <- list();
	
	if (nrow(res) > 0)
	{
		length(out) <- nrow(res);
		for (i in 1:nrow(res))
		{
			out[[i]] <- list(IdAuthor=res[i,1], Name=res[i,2]);
			class(out[[i]]) <- "authorinfo";
		}
	}

	return(out);
}


#' Retrieves information on given documents.
#' 
#' @title Retrieve document information
#' @param conn a connection object as produced by \code{\link{lbsConnect}}.
#' @param idDocuments a numeric or integer vector with document identifiers (see column \code{IdDocument} in the table \code{Biblio_Documents}).
#' @return
#' A list of \code{docinfo} objects, that is lists with the following components:
#' \itemize{
#' \item \code{IdDocument} --- numeric; document identifier in the table \code{Biblio_Documents},
#' \item \code{Authors} --- list of \code{authorinfo} objects (see e.g. \code{\link{as.character.authorinfo}}).
#' \item \code{Title} --- title of the document,
#' \item \code{BibEntry} --- bibliographic entry,
#' \item \code{UniqueId} --- unique character identifier,
#' \item \code{Pages} --- number of pages,
#' \item \code{Citations} --- number of citations,
#' \item \code{Year} --- publication year,
#' \item \code{Type} --- document type, e.g. \code{Article} or \code{Conference Paper}.
#' }
#' @examples
#' \dontrun{
#' conn <- dbBiblioConnect("Bibliometrics.db");
#' ## ...
#' id <- lbsSearchDocuments(conn,
#' 	idAuthors=lbsSearchAuthors(conn, "Knuth\%"));
#' lbsGetInfoDocuments(conn, id);
#' ## ...}
#' @seealso \code{\link{print.docinfo}}, \code{\link{lbsSearchDocuments}},
#' \code{\link{lbsGetInfoAuthors}},\cr
#' \code{\link{as.character.authorinfo}}, \code{\link{as.character.docinfo}}
#' @export
lbsGetInfoDocuments <- function(conn, idDocuments)
{
	CITAN:::.lbsCheckConnection(conn); # will stop on invalid/dead connection

		
	if (is.null(idDocuments) || (class(idDocuments) != "numeric" && class(idDocuments) != "integer"))
		stop("'idDocuments' must be a nonempty numeric vector.");


	whexpr <- paste("IdDocument", idDocuments, sep="=", collapse=" OR ");

	query <- sprintf("SELECT IdDocument, Title, BibEntry, UniqueId, Pages, Citations, Year, Type
		FROM Biblio_Documents
		WHERE %s", whexpr);
	
	res <- dbGetQuery(conn, query);
	out <- list();
	length(out) <- nrow(res);
	
	if (nrow(res) == 0) return(out);
	
	for (i in 1:nrow(res))
	{
		res2 <- dbGetQuery(conn, sprintf("SELECT DISTINCT Biblio_Authors.IdAuthor, Name FROM
			Biblio_Authors
			JOIN Biblio_AuthorsDocuments ON Biblio_Authors.IdAuthor=Biblio_AuthorsDocuments.IdAuthor
			WHERE IdDocument=%g", res[i,1]));

		authors <- list();
		length(authors) <- nrow(res2);
		for (j in 1:nrow(res2))
		{
			authors[[j]] <- list(IdAuthor=res2[j,1], Name=res2[j,2]);
			class(authors[[j]]) <- "authorinfo";
		}
		
		doc <- list(IdDocument=res[i,1], Authors=authors, Title=res[i,2], BibEntry=res[i,3],
			UniqueId=res[i,4], Pages=res[i,5], Citations=res[i,6],
			Year=res[i,7], Type=CITAN:::.lbs_DocumentType_ShortToFull(res[i,8]));
			
		class(doc) <- "docinfo";
		
		out[[i]] <- doc;
	}
	return(out);
}


#' Converts an object of type \code{docinfo} to a character string.
#' Such an object is  returned by e.g. \code{\link{lbsGetInfoDocuments}}.
#'
#' A \code{docinfo} object is a list with the following components:
#' \itemize{
#' \item \code{IdDocument} --- numeric; document identifier in the table \code{Biblio_Documents},
#' \item \code{Authors} --- list of \code{authorinfo} objects (see e.g. \code{\link{as.character.authorinfo}}).
#' \item \code{Title} --- title of the document,
#' \item \code{BibEntry} --- bibliographic entry,
#' \item \code{UniqueId} --- unique character identifier,
#' \item \code{Pages} --- number of pages,
#' \item \code{Citations} --- number of citations,
#' \item \code{Year} --- publication year,
#' \item \code{Type} --- type of document, see \code{\link{lbsCreate}}.
#' }
#'
#' @title Coerce a docinfo object to character string
#' @param x a single object of type \code{docinfo}.
#' @param ... unused.
#' @return A character string
#' @export
#' @seealso \code{\link{lbsSearchDocuments}},
#' \code{\link{as.character.authorinfo}}, \code{\link{print.docinfo}},\cr
#' \code{\link{lbsGetInfoDocuments}}
as.character.docinfo <- function(x, ...)
{
	ret <-            sprintf("IdDocument: %g", x$IdDocument);
	ret <- paste(ret, sprintf("UniqueId:   %s", x$UniqueId), sep="\n");
	ret <- paste(ret, sprintf("Title:      %s", x$Title), sep="\n");
	ret <- paste(ret, sprintf("BibEntry:   %s", x$BibEntry), sep="\n");
	ret <- paste(ret, sprintf("Year:       %s", x$Year), sep="\n");
	ret <- paste(ret, sprintf("Type:       %s", x$Type), sep="\n");
	ret <- paste(ret, sprintf("Citations:  %s", x$Citations), sep="\n");
	ret <- paste(ret, sprintf("Authors:    %s\n",
		paste(
			sapply(x$Authors, function(y) paste(y$Name, y$IdAuthor, sep="/")),
			collapse=", ")
		), sep="\n");
	return(ret);
}

#' Prints out an object of type \code{docinfo}. Such an object is returned by e.g. \code{\link{lbsGetInfoDocuments}}.
#'
#' For more information see man page for \code{\link{as.character.docinfo}}.
#'
#' @title Print a docinfo object
#' @param x an object of type \code{docinfo}.
#' @param ... unused.
#' @export
#' @seealso \code{\link{as.character.docinfo}}, \code{\link{lbsSearchDocuments}}, \code{\link{lbsGetInfoDocuments}}
print.docinfo <- function(x, ...)
{
	cat(as.character(x));
}




#' Converts an object of type \code{authorinfo} to a character string.
#' Such an object is returned by e.g. \code{\link{lbsGetInfoAuthors}}.
#'
#' An \code{authorinfo} object  is a list with the following components:
#' \itemize{
#' \item \code{IdAuthor} --- numeric; author's identifier in the table \code{Biblio_Authors},
#' \item \code{Name} --- character; author's name.
#' }
#'
#' @title Coerce an authorinfo object to character string
#' @param x a single object of type \code{authorinfo}.
#' @param ... unused.
#' @return A character string
#' @export
#' @seealso \code{\link{print.authorinfo}}, \code{\link{lbsSearchAuthors}}, \code{\link{lbsGetInfoAuthors}}
as.character.authorinfo <- function(x, ...)
{
	ret <-            sprintf("IdAuthor: %g",   x$IdAuthor);
	ret <- paste(ret, sprintf("Name:     %s\n", x$Name), sep="\n");
	return(ret);
}

#' Prints out an object of type \code{authorinfo}. Such an object is returned by e.g. \code{\link{lbsGetInfoAuthors}}.
#'
#' For more information see man page for \code{\link{as.character.authorinfo}}.
#'
#' @title Print an authorinfo object
#' @param x an object of type \code{authorinfo}.
#' @param ... unused.
#' @export
#' @seealso \code{\link{as.character.authorinfo}}, \code{\link{lbsSearchAuthors}}, \code{\link{lbsGetInfoAuthors}}
print.authorinfo <- function(x, ...)
{
	cat(as.character(x));
}


