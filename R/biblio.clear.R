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




#' Clears a local bibliometric storage by dropping all tables
#' named \code{Biblio_*} and all views named \code{ViewBiblio_*}.
#'
#' For safety reasons, an SQL transaction  opened at the beginning of the
#' removal process is not committed (closed) automatically.
#' You should do it on your own (or rollback it), see Examples below.
#'
#' @title Clear a local bibliometric storage
#' @param con a connection object as produced by \code{\link{dbBiblioConnect}}.
#' @param verbose logical; \code{TRUE} to print out the progress of database contents' removal.
#' @examples
#' \dontrun{con <- dbBiblioConnect("Bibliometrics.db");}
#' \dontrun{dbBiblioClear(con);}
#' \dontrun{dbCommit(con);}
#' \dontrun{dbBiblioCreate(con);}
#' \dontrun{Scopus_ImportSources(con);}
#' ## ...
#' \dontrun{dbDisconnect(con);}
#' @return \code{TRUE} on success.
#' @seealso \code{\link{dbBiblioConnect}}, \code{\link{dbBiblioCreate}},  \code{\link{Scopus_ImportSources}}, \code{\link{dbCommit}}, \code{\link{dbRollback}}, \code{\link{dbBiblioDeleteAuthorsDocuments}}
#' @export
dbBiblioClear <- function(con, verbose=TRUE)
{
	CITAN:::.dbBiblioCheckConnection(con); # will stop on invalid/dead connection

	dbBeginTransaction(con);

	objects <- dbListTables(con);
	tables <- objects[substr(objects,1,7) == "Biblio_"];
	views  <- objects[substr(objects,1,11) == "ViewBiblio_"];


	if (length(tables) == 0 && length(views) == 0)
	{
		warning("database is already empty.");
		return(TRUE);
	}
	


	if (length(tables) != 0)
		for (i in 1:length(tables))
		{
			if (verbose) cat(sprintf("Dropping table '%s'... ", tables[i]));
			dbExecQuery(con, sprintf("DROP TABLE %s;", tables[i]), TRUE);
			if (verbose) cat("DONE.\n");
		}


	if (length(views) != 0)
		for (i in 1:length(views))
		{
			if (verbose) cat(sprintf("Dropping view '%s'... ", views[i]));
			dbExecQuery(con, sprintf("DROP VIEW %s;", views[i]), TRUE);
			if (verbose) cat("DONE.\n");
		}


	warning("Transaction has not been committed yet. Do-it-yourself with dbCommit(...).");

	return(TRUE);
}





#' Deletes are author, document and survey information from a local bibliometric
#' storage.
#'
#' For safety reasons, an SQL transaction opened at the beginning of the
#' removal process is not committed (closed) automatically.
#' You should do it on your own (or rollback it), see Examples below.
#'
#' @title Delete all authors, documents and surveys from a local bibliometric storage
#' @param con a connection object as produced by \code{\link{dbBiblioConnect}}.
#' @param verbose logical; \code{TRUE} to print out the progress of database contents' removal.
#' @return \code{TRUE} on success.
#' @seealso \code{\link{dbBiblioClear}}, \code{\link{dbCommit}}, \code{\link{dbRollback}}
#' @examples
#' \dontrun{con <- dbBiblioConnect("Bibliometrics.db");}
#' \dontrun{dbBiblioDeleteAuthorsDocuments(con);}
#' \dontrun{dbCommit(con);}
#' ## ...
#' \dontrun{dbDisconnect(con);}
#' @export
dbBiblioDeleteAuthorsDocuments <- function(con, verbose=TRUE)
{
	CITAN:::.dbBiblioCheckConnection(con); # will stop on invalid/dead connection
	

	if (verbose) cat(sprintf("Deleting all author and document information... "));


	dbBeginTransaction(con);
	
	dbExecQuery(con, "DELETE FROM Biblio_DocumentsSurveys", TRUE);
	dbExecQuery(con, "DELETE FROM Biblio_AuthorsDocuments", TRUE);
	dbExecQuery(con, "DELETE FROM Biblio_Surveys", TRUE);
	dbExecQuery(con, "DELETE FROM Biblio_Authors", TRUE);
	dbExecQuery(con, "DELETE FROM Biblio_Documents", TRUE);

	if (verbose) cat(sprintf("DONE.\n"));

	warning("Transaction has not been committed yet. Do-it-yourself with dbCommit(...).");

	return(TRUE);
}
