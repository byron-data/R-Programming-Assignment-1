corr <- function(directory, threshold=0) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files.
	olddir <- getwd()
	setwd(directory)

	## 'threshold' is a numeric vector of length 1 indicating the
	## number of completely observed observations (on all
	## variables) required to compute the correlation between
	## nitrate and sulfate; the default is 0.
	count <- 0
	correlations <- vector(mode="numeric", length=0)
	files <- list.files(pattern="*.csv")
	for(i in files) {
		file <- read.csv(files[which(files==i)])
		cases <- sum(complete.cases(file))
		if (cases > threshold) {
			x <- file[,c("sulfate")]
			y <- file[,c("nitrate")]
			data <- data.frame(x, y)
			count <- count + 1
			correlations <- c(correlations, cor(data, use="complete.obs")[2])
		}
	}
			 
	## Return a numeric vector of correlations
	## NOTE: Do not round the result!
	setwd(olddir)
	correlations
}