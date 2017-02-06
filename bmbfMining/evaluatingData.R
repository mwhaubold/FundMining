# Libraries
library(ggplot2)
library(XML)
library(stringr)
library(packcircles)
library(gridExtra)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

rm(list = ls())

### functions ###
titleExtract <- function(noticeHTML) {
	parsedDoc <- htmlParse(noticeHTML)
	getTitle <- xpathSApply(parsedDoc, "//div[@class='summary']", xmlValue)

	return(getTitle)
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

### end functions ###

# load feature list
load("./featureList.RData")

numberPerYear <- c(46, 49, 63, 93, 96, 88, 120, 104, 88, 113, 83, 132, 160, 18)
years <- c(2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)

mydf1 <- data.frame(numberPerYear, years)

#barplot(numberPerYear, names.arg = years, ylim = c(0, 200), col = rgb(0, 101 / 255, 189 / 255), ylab = "Anzahl der Ausschreibungen", xlab = "Jahreszahl")
#abline(h = mean(numberPerYear))

p1 <- ggplot(mydf1, aes(x = years, y = numberPerYear)) + geom_bar(stat = "identity", fill = rgb(0, 101 / 255, 189 / 255)) + geom_text(aes(label = numberPerYear), color = "black", vjust = -1) + coord_cartesian(xlim = c(2004, 2017)) + theme_light() + labs(x = "Jahreszahl", y = "Anzahl der Ausschreibungen") + geom_hline(yintercept = mean(numberPerYear))
print(p1)

#print(months(noticeFeatureList[[1]]$releaseDate))
#print(strftime(noticeFeatureList[[1]]$releaseDate, format="%Y"))

upDaysCoded <- vector("list", length(noticeFeatureList))
for (i in 1:length(noticeFeatureList)) {
	if (typeof(noticeFeatureList[[i]]$releaseDate) == "double") {
		uploadedDay <- weekdays(noticeFeatureList[[i]]$releaseDate)
		upDaysCoded[i] <- switch(uploadedDay, "Montag" = 1, "Dienstag" = 2, "Mittwoch" = 3, "Donnerstag" = 4, "Freitag" = 5, "Samstag" = 6, "Sonntag" = 7)
	}
}

mydf2 <- do.call("rbind", upDaysCoded)
mydf2 <- data.frame(mydf2)

p2 <- ggplot(mydf2, aes(x = mydf2)) + geom_bar(fill = rgb(0, 101 / 255, 189 / 255)) + stat_bin(bins = 7, aes(y = ..count.., label = ..count..), color = "black", geom = "text", vjust = -1) + theme_light() + labs(x = "Wochentage", y = "Anzahl der Ausschreibungen") + scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7), labels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"))
print(p2)

# extract mail domain --> "Projektträger"
domainList <- vector("list", length(noticeFeatureList))
for (i in 1:length(noticeFeatureList)) {
	domainList[i] <- gsub(".*@|\\..*", "", noticeFeatureList[[i]]$authority)[1]
}

uniqueDomainList = unique(domainList)

domainTableFull <- table(as.character(domainList))
domainTable <- domainTableFull[domainTableFull > 10]
names(domainTable)[1] <- "Deutsches Elektronen Synchrotron"
names(domainTable)[2] <- "DLR"
names(domainTable)[3] <- "Projektträger Jülich"
names(domainTable)[4] <- "KIT"
names(domainTable)[5] <- "Kein Projektträger"
names(domainTable)[6] <- "VDI-TZ"
names(domainTable)[7] <- "VDI/VDE"
mydf3 <- as.data.frame(domainTable)
p3 <- ggplot(mydf3, aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", fill = rgb(0, 101 / 255, 189 / 255)) + geom_text(aes(label = Freq), color = "black", vjust = -1) + theme_light() + labs(x = "Projektträger", y = "Anzahl der Ausschreibungen")
print(p3)




ncircles <- length(domainTableFull)
limits <- c(-50, 50)
inset <- diff(limits) / 3
rmax <- 25

rawData <- data.frame(
  x = runif(ncircles, min(limits) + inset, max(limits) - inset),
  y = runif(ncircles, min(limits) + inset, max(limits) - inset),
  r = rbeta(ncircles, 1, 10) * rmax)

myX <- runif(ncircles, min(limits) + inset, max(limits) - inset)
myY <- runif(ncircles, min(limits) + inset, max(limits) - inset)
myR <- as.integer(unname(domainTableFull))
myR <- (myR/max(myR)) * rmax
mydf4 <- data.frame(myX, myY, myR)

res <- circleLayout(mydf4, limits, limits, maxiter = 1000)
dat.after <- circlePlotData(res$layout, npoints = 200)
p4 <- ggplot(dat.after) + geom_polygon(aes(x, y, group = id), lwd = 1, colour = rgb(0, 101 / 255, 189 / 255), fill = rgb(0, 101 / 255, 189 / 255), alpha = 0.1) + coord_equal(xlim = limits, ylim = limits) + theme_void() + theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
print(p4)






keywordList <- character(length = 0)
for (i in 1:length(noticeFeatureList)) {
	keywordList <- c(keywordList, noticeFeatureList[[i]]$keywords)
}

uniKeywordList <- unique(keywordList)
keywordHist <- table(keywordList)
keywordHist <- sort(keywordHist, decreasing = TRUE)

keywordDataFrame <- as.data.frame(keywordHist)
myColorPal <- c("#EABD00", "#CB5B5A", "#AC557A", "#8D4C7D", "#6B406E", "#40324F")
tumColorPal <- c("#98C6EA", "#DAD7CB", "#A2AD00", "#64A0C8", "#E37222")
wordcloud(words = keywordDataFrame$keywordList, freq = keywordDataFrame$Freq, min.freq = 2, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = tumColorPal)









commonWords <- readLines("H:/08_Code/EigeneProjekte/FundMining/commonWordList_10k.txt", encoding = "UTF-8")
for (i in 2004:2017) {
	filesInFolder <- list.files(paste("F:/OwnScratch/Exchange/BMBF_Ausschreibungen/", toString(i), sep = ""))

	keywordsPerYear <- vector("list", length(filesInFolder))
	for (ii in 1:length(filesInFolder)) {
		noticeHTML <- readLines(paste("F:/OwnScratch/Exchange/BMBF_Ausschreibungen/", toString(i), "/", filesInFolder[ii], sep = ""))

		keywordsPerYear[[ii]] <- keywordExtract(noticeHTML, commonWords)
	}
	keywordsPerYear <- do.call("rbind", keywordsPerYear)

	keywordHist <- table(keywordsPerYear)
	keywordHist <- sort(keywordHist, decreasing = TRUE)
	keywordDataFrame <- as.data.frame(keywordHist)
	tumColorPal <- c("#98C6EA", "#DAD7CB", "#A2AD00", "#64A0C8", "#E37222")
	wordcloud(words = keywordDataFrame$keywordList, freq = keywordDataFrame$Freq, min.freq = 2, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = tumColorPal)
}