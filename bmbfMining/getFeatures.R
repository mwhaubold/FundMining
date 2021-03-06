# load library
library(XML)
library(RKEA)
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

rm(list = ls())

# define functions for extracting the features
#get release date
dateExtract <- function(noticeHTML) {

	parsedDoc <- htmlParse(noticeHTML)
	noticeDate <- xpathSApply(parsedDoc, "//strong", xmlValue)

	checkForDate = 0
	for (item in noticeDate) {
		checkCorrectDate <- substr(item, 1, 3)
		if (checkCorrectDate == "Vom") {
			noticeDate <- substr(item, 5, nchar(item))
			checkForDate = 1
			break
		}
	}

	if (checkForDate == 1) {
		noticeDate <- as.POSIXct(strptime(noticeDate, "%d. %B %Y"))
	}
	else {
		yearLocation <- grep("Im Auftrag", noticeHTML) - 3
		noticeDate <- noticeHTML[yearLocation]
	}

	return(noticeDate)
}

#get time period for notice
periodExtract <- function(noticeHTML) {
	parsedDoc <- htmlParse(noticeHTML)
	timePeriod <- xpathSApply(parsedDoc, "//span[@class='datum']", xmlValue)

	startDate <- substr(timePeriod, 1, 10)
	endDate <- substr(timePeriod, 14, nchar(timePeriod))

	startDate <- as.POSIXct(strptime(startDate, "%d.%m.%Y"))
	endDate <- as.POSIXct(strptime(endDate, "%d.%m.%Y"))

	noticeDuration <- difftime(endDate, startDate)

	return(noticeDuration)
}

#get title
titleExtract <- function(noticeHTML) {
	parsedDoc <- htmlParse(noticeHTML)
	getTitle <- xpathSApply(parsedDoc, "//div[@class='summary']", xmlValue)

	return(getTitle)
}

#get "Projektträger"
authorityExtract <- function(noticeHTML) {
	parsedDoc <- htmlParse(noticeHTML)
	checkAuthority <- xpathSApply(parsedDoc, "//div[@class='article-section']", xmlValue)

	if (length(grep("Verfahren", checkAuthority)) == 0) {
		return("none")
	}

	getMailAdress <- xpathSApply(parsedDoc, "//div[@class='article-section']/p/a", xmlValue)
	getMailAdress <- getMailAdress[grep("@", getMailAdress)]

	return(getMailAdress)
}

#get author of notice
authorExtract <- function(noticeHTML) {
	authorLocation <- grep("Im Auftrag", noticeHTML) + 1
	authorName <- noticeHTML[authorLocation]

	if (length(nchar(authorName)) != 0) {
		if (nchar(authorName) <= 6) {
			authorName <- noticeHTML[authorLocation + 1]
		}
	}

		return(authorName)
}

#get keywords from notice title
keywordExtract <- function(noticeHTML, commonWords) {
	# get overall title
	overallTitle <- titleExtract(noticeHTML)

	# remove numbers and special characters from title
	clearedTitle <- gsub("[0-9]|[[:punct:]]", "", overallTitle)

	# extract words with capital letter
	clearedTitleNouns <- gsub("\\s[a-z]+\\s", " ", clearedTitle)
	nounsSeparated <- str_split(clearedTitleNouns, "\\W")
	nounsSeparated <- do.call("rbind", nounsSeparated)

	selectKeywords <- rep(TRUE, times = length(nounsSeparated))
	for (i in 1:length(nounsSeparated)) {
		if (sum(nounsSeparated[i] == commonWords) >= 1) {
			selectKeywords[i] <- FALSE
		}
	}

	keywordsFound <- nounsSeparated[selectKeywords]
	keywordsFound <- keywordsFound[nchar(keywordsFound) != 0]
	keywordsFound <- keywordsFound[nchar(keywordsFound) > 3]

	#keywordsFound <- wordStem(keywordsFound, language = "german")

	return(keywordsFound)
}



############## MAIN ##############
# get number of files
filesAll <- list.files("F:/OwnScratch/Exchange/BMBF_Ausschreibungen/allNew")

# load common word list
commonWords <- readLines("H:/08_Code/EigeneProjekte/FundMining/commonWordList_10k.txt", encoding = "UTF-8")

noticeFeatureList <- vector("list", length(filesAll))
for (i in 1:length(filesAll)) {
	# read html document
	noticeHTML <- readLines(paste("F:/OwnScratch/Exchange/BMBF_Ausschreibungen/allNew/", filesAll[i], sep = ""))

	noticeFeatureList[[i]] <- list(overallTitle = titleExtract(noticeHTML), releaseDate = dateExtract(noticeHTML), timePeriod = periodExtract(noticeHTML), authority = authorityExtract(noticeHTML), author = authorExtract(noticeHTML), keywords = keywordExtract(noticeHTML, commonWords))
}

save(noticeFeatureList, file = "./featureList.RData")