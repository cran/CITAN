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


#' Converts a single numeric variable to character string or returns \code{"NULL"}.
#'
#' @title Get nullable numeric value for use in an SQL query
#' @param value value to be processed.
#' @return The function returns a character string containing a
#' textual representation of a given numeric variable or \code{"NULL"}.
#' @export
#' @seealso \code{\link{sqlSwitchOrNULL}}, \code{\link{sqlStringOrNULL}}
sqlNumericOrNULL <- function(value)
{
	stopifnot(length(value) == 1);

	value <- as.character(as.numeric(value));
	if (is.na(value) || is.null(value))
	{
		return("NULL");
	} else {
		return(value);
	}
}



#' Returns a trimmed and single-quote-escaped (see \code{\link{sqlEscapeTrim}})
#' character string or \code{"NULL"} on empty input.
#'
#' @title Get nullable character string value for use in an SQL query
#' @param value value to be processed.
#' @return The function returns a character string containing a
#' processed input value or \code{"NULL"}.
#' @export
#' @seealso \code{\link{sqlNumericOrNULL}}, \code{\link{sqlSwitchOrNULL}}, \code{\link{sqlEscapeTrim}}
sqlStringOrNULL <- function(value)
{
	stopifnot(length(value) == 1);

	value <- sqlEscapeTrim(value);

	if (is.na(value) || is.null(value) || nchar(value)==0)
	{
		return("NULL");
	} else {
		return(paste(c("'", value, "'"), collapse=""));
	}
}


#' Given a single character string, the function
#' tries to match its value in the array \code{search}.
#' On success, a corresponding value of \code{replace} is returned.
#' Otherwise, it returns \code{"NULL"}.
#'
#' @title Switch-like nullable construct for use in an SQL query
#' @param value value to be processed.
#' @param search  character vector of length \code{n}.
#' @param replace character vector of length \code{n}.
#' @return The function returns a character string containing 
#' a value from \code{replace} that corresponds to an element of \code{search} or \code{"NULL"}.
#' @export
#' @seealso \code{\link{sqlNumericOrNULL}}, \code{\link{sqlStringOrNULL}}, \code{\link{sqlTrim}}
sqlSwitchOrNULL <- function(value, search, replace)
{
	stopifnot(length(value) == 1);
	stopifnot(length(search) == length(replace) && length(search) > 0);

	value <- sqlTrim(value);
	res <- replace[which(value == search)];

	stopifnot(length(res) <= 1);

	if (length(res) == 0) return("NULL") else return(res);
}



#' Escapes a string for use in an SQL query.
#'
#' The SQL standard specifies that single-quotes in strings are escaped by putting two single quotes in a row.
#' This function repeats the quotes using \code{\link{gsub}}.
#'
#' @title Escape a string for use in an SQL query
#' @param str a character vector where matches are sought, or an object which can be coerced by \code{as.character} to a character vector.
#' @param useBytes logical.  If \code{TRUE} the matching is done byte-by-byte rather than character-by-character.  See man page for \code{\link{gsub}}.
#' @return See 'Value' for \code{\link{gsub}}.
#' @export
#' @seealso \code{\link{gsub}}, \code{\link{sqlTrim}}, \code{\link{sqlEscapeTrim}}
sqlEscape <- function(str, useBytes=FALSE)
{
	gsub("'", "''", str, fixed=T, useBytes=useBytes);
}


#' Trims white-spaces on both sides of a string.
#'
#' This function uses \code{\link{gsub}}.
#'
#' @title Trim white-spaces on both sides of a string
#' @param str a character vector where matches are sought, or an object which can be coerced by \code{as.character} to a character vector.
#' @param useBytes logical.  If \code{TRUE} the matching is done byte-by-byte rather than character-by-character.  See man page for \code{\link{gsub}}.
#' @return See 'Value' for \code{\link{gsub}}.
#' @export
#' @seealso \code{\link{gsub}}, \code{\link{sqlEscape}}, \code{\link{sqlEscapeTrim}}
sqlTrim <- function(str, useBytes=FALSE)
{
	gsub("^[[:space:]]+|[[:space:]]+$", "", str, useBytes=useBytes)
}


#' Escapes a string for use in an SQL query and trims white-spaces on both sides.
#'
#' The SQL standard specifies that single-quotes in strings are escaped by putting two single quotes in a row.
#' This function uses \code{\link{gsub}}.
#'
#' @title Escape a string for use in an SQL query and trim white-spaces on both sides
#' @param str a character vector where matches are sought, or an object which can be coerced by \code{as.character} to a character vector.
#' @param useBytes logical.  If \code{TRUE} the matching is done byte-by-byte rather than character-by-character.  See man page for \code{\link{gsub}}.
#' @return See 'Value' for \code{\link{gsub}}.
#' @export
#' @seealso \code{\link{gsub}}, \code{\link{sqlEscape}}, \code{\link{sqlTrim}}
sqlEscapeTrim <- function(str, useBytes=FALSE)
{
	sqlEscape(sqlTrim(str, useBytes), useBytes);
}






