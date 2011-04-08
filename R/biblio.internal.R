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


#' /internal/
#' stops if connection is invalid or dead
.dbBiblioCheckConnection <- function(con)
{
	if (!class(con) == "SQLiteConnection")
		stop("incorrect 'con' given");

	dbGetInfo(con); # check if con is active
}


#' /internal/
.dbBiblio_SourceTypesFull  <- c("Book Series", "Conference Proceedings", "Journal");

#' /internal/
.dbBiblio_SourceTypesShort <- c("'bs'",        "'cp'",                   "'jo'");


#' /internal/
.dbBiblio_DocumentTypesFull  <- c("Article", "Article in Press", "Book", "Conference Paper", "Editorial", "Erratum", "Letter", "Note", "Report", "Review", "Short Survey");

#' /internal/
.dbBiblio_DocumentTypesShort <- c("'ar'",    "'ip'",             "'bk'", "'cp'",             "'ed'",      "'er'",    "'le'",   "'no'", "'rp'",   "'re'",   "'sh'");



#' /internal/
.dbBiblio_PrepareRestriction_DocumentTypes <- function(con, DocumentTypes)
{
	if (is.null(DocumentTypes)) return(NULL);
	
	if (!is.character(DocumentTypes))
		stop("incorrect 'DocumentTypes' given");

	DocumentTypesShort <- character(length(DocumentTypes));
	for (i in 1:length(DocumentTypes))
		DocumentTypesShort[i] <- sqlSwitchOrNULL(DocumentTypes[i],
			CITAN:::.dbBiblio_DocumentTypesFull,
			CITAN:::.dbBiblio_DocumentTypesShort
		);
	
	incorrect <- which(DocumentTypesShort == "NULL");

	if (length(incorrect)>0)
	{
		warning(sprintf("incorrect document types: %s. Ignoring.",
			paste(DocumentTypes[incorrect], collapse=", ")));
		DocumentTypesShort <- DocumentTypesShort[-incorrect];
	}
	
	if (length(DocumentTypesShort) == 0) stop("all given document types were incorrect.");
	
	return(DocumentTypesShort);
}



#' /internal/
.dbBiblio_PrepareRestriction_SurveyDescription <- function(con, SurveyDescription)
{
	if (is.null(SurveyDescription)) return(NULL);

	if (!is.character(SurveyDescription) || length(SurveyDescription)!=1)
		stop("incorrect 'SurveyDescription' given");
		
	SurveyDescription <- sqlEscapeTrim(SurveyDescription);
	
	res <- dbGetQuery(con, sprintf("SELECT * FROM Biblio_Surveys WHERE Description='%s';",
		SurveyDescription));
	if (nrow(res) == 0) stop("Survey not found.");
	
	return(SurveyDescription);
}
