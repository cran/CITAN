\name{sqlStringOrNULL}
\alias{sqlStringOrNULL}
\title{Get nullable character string value for use in an SQL query}
\usage{sqlStringOrNULL(value)}
\description{Returns a trimmed and single-quote-escaped (see \code{\link{sqlEscapeTrim}})
character string or \code{"NULL"} on empty input.}
\value{The function returns a character string containing a
processed input value or \code{"NULL"}.}
\seealso{\code{\link{sqlNumericOrNULL}}, \code{\link{sqlSwitchOrNULL}}, \code{\link{sqlEscapeTrim}}}
\arguments{\item{value}{value to be processed.}}
