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





#' Executes an SQL query and immediately frees all allocated resources.
#'
#' This function is useful in executing queries like \code{CREATE TABLE},
#' \code{UPDATE}, \code{INSERT}, etc.
#'
#' It has its own exception handler, which prints out detailed description
#' of caught error details.
#'
#' @title Execute a query and free its resources
#' @param conn a \code{DBI} connection object.
#' @param statement a character string with the SQL statement to be executed.
#' @param rollbackOnError logical; if \code{TRUE}, then the function executes rollback on current transaction if an exception occurs.
#' @seealso \code{\link{dbSendQuery}}, \code{\link{dbClearResult}}, \code{\link{dbGetQuery}}
#' @export
dbExecQuery <- function(conn, statement, rollbackOnError=FALSE)
{
	if (!is.character(statement) || length(statement)!=1)
		stop("incorrect 'statement'");

	tryCatch(res <- dbSendQuery(conn, statement),
		error=function(err)
		{
			cat("\n\n*** SQL Exception caught ***\n\n");
			cat(sprintf("Statement: %s\n", statement));
			
			ex <- dbGetException(conn);
			
			if (rollbackOnError) dbRollback(conn);
			print(ex);
			stop("stopping on SQL exception.");
		}
	);

	dbClearResult(res);
}
