# load library
library(XML)
library(stringr)

# INPUT
basePath = "https://www.bmbf.de/foerderungen/"

# read the html code from the start page
bmbfArchiveSite = readLines(paste(basePath, "bekanntmachungen_archiv.php", sep = ""))

fileCon <- file("./bmbfArchive.html")
writeLines(bmbfArchiveSite, fileCon)
close(fileCon)

links <- getHTMLLinks(bmbfArchiveSite)

nextLinks <- unique(links[grep("bekanntmachungen_archiv-", links)])

i = 0
for (item in nextLinks) {
	bmbfYearPage = readLines(paste(basePath, item, sep = ""))

	yearLinks <- getHTMLLinks(bmbfYearPage)
	yearLinks <- unique(yearLinks[grep("bekanntmachung-", yearLinks)])

	# create folder
	#currentYear <- substr(item, 25, 28)
	#dir.create(paste("F:/OwnScratch/Exchange/BMBF_Ausschreibungen/", currentYear, sep = ""))

	for (noticeLink in yearLinks) {
		noticePage <- readLines(paste(basePath, noticeLink, sep = ""))

		#parsedDoc <- htmlParse(noticePage)
		#noticeTitle <- xpathSApply(parsedDoc, "//div[@class='summary']", xmlValue)

		# save html page
		fileCon <- file(paste("F:/OwnScratch/Exchange/BMBF_Ausschreibungen/allNew/", str_pad(toString(i), 4, pad = "0"), ".html", sep = ""))
		writeLines(noticePage, fileCon)
		close(fileCon)

		i = i + 1
	}
 }