# The solution to the programming task in week 4

# Helper function reading the hospital data and return a data frame
readHospital <- function(){
    data <- read.csv('./data/hospital-data.csv', colClasses="character")
}
# Helper functio reading the outcom data and return a data frame
readOutcome <- function() {
    data <- read.csv('./data/outcome-of-care-measures.csv', colClasses='character')
}

# Task 1
# ---------
# Creating a plot of the data read
plotHeartRateHist <- function(data) {
    outcome <- as.numeric(data[, 11])
    png('img/heart_rate_hist.png')
    hist(outcome)
    dev.off()
    invisible(outcome)
}


outcome <- readOutcome()
plotHeartRateHist(outcome)
