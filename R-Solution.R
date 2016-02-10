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
    colors = c("red", "yellow", "green", "violet", "orange",
   "blue", "pink", "cyan")
    png('img/heart_rate_hist.png')
    hist(outcome, col=colors, main='30-day death rates')
    dev.off()
    invisible(outcome)
}



outcome <- readOutcome()
plotHeartRateHist(outcome)
