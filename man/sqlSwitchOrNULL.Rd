\name{sqlSwitchOrNULL}
\alias{sqlSwitchOrNULL}
\title{Switch-like nullable construct for use in an SQL query}
\usage{sqlSwitchOrNULL(value, search, replace)}
\description{Given a single character string, the function
tries to match its value in the array \code{search}.
On success, a corresponding value of \code{replace} is returned.
Otherwise, it returns \code{"NULL"}.}
\value{The function returns a character string containing 
a value from \code{replace} that corresponds to an element of \code{search} or \code{"NULL"}.}
\seealso{\code{\link{sqlNumericOrNULL}}, \code{\link{sqlStringOrNULL}}, \code{\link{sqlTrim}}}
\arguments{\item{value}{value to be processed.}
\item{search}{character vector of length \code{n}.}
\item{replace}{character vector of length \code{n}.}}
