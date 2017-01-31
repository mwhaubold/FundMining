# load feature list
load("./featureList.RData")

numberPerYear <- c(46, 49, 63, 93, 96, 88, 120, 104, 88, 113, 83, 132, 160, 18)
years <- c("2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")

barplot(numberPerYear, names.arg = years, ylim = c(0, 200), col = rgb(0, 101 / 255, 189 / 255), ylab = "Anzahl der Ausschreibungen", xlab = "Jahreszahl")
abline(h = mean(numberPerYear))


#print(months(noticeFeatureList[[1]]$releaseDate))
#print(weekdays(noticeFeatureList[[1]]$releaseDate))
#print(strftime(noticeFeatureList[[1]]$releaseDate, format="%Y"))

upDaysCoded <- vector("list", length(noticeFeatureList))
for (i in 1:length(noticeFeatureList)) {
	if (typeof(noticeFeatureList[[i]]$releaseDate) == "double") {
		uploadedDay <- weekdays(noticeFeatureList[[i]]$releaseDate)
		upDaysCoded[i] <- switch(uploadedDay, "Montag" = 1, "Dienstag" = 2, "Mittwoch" = 3, "Donnerstag" = 4, "Freitag" = 5, "Samstag" = 6, "Sonntag" = 7)
	}
}

