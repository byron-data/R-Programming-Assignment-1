complete <- function(directory, id = 1:332) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files.
	olddir <- getwd()
	setwd(directory)

	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used.
	n <- length(id)
	nobs <- numeric(n)
	files <- paste(formatC(id, width=3, flag="0"),".csv",sep = "")
	##count <- 0
	for(i in id) {
		##count <- count + 1
		count <- which(id==i)
		file <- read.csv(files[count])
		nobs[count] <- sum(complete.cases(file))
	}
			 
	## Return a data frame of the form:
	## id nobs
	## 1 117
	## 2 1041
	## ...
	## where 'id' is the monitor ID number and 'nobs' is the
	## number of complete cases
	setwd(olddir)
	data.frame(id, nobs)
}