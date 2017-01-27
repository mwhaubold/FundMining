# read the html code from the start page
thepage = readLines('https://www.bmbf.de/foerderungen/bekanntmachungen_archiv.php')

fileCon <- file("./startPage.html")
writeLines(thepage, fileCon)
close(fileCon)
