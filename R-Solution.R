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

# Task 2
# ---------
#
# Find the best hospital in a state regarding the lowest 30-day-mortality for
# the specified type of outcome. The type can be 'heart attack', 'heart failure'
# or 'pneumonia'.
#
# Strategy: extract state, and the type of outcome from the data, apply
# complete.cases and than find the hospital with the lowest number
#
# Notes: Using a list as a map from name to column number
best <- function(state, type_of_outcome, data){
    idx <- list("heart attack"=11, "heart failure"=17, "pneumonia"=23)
}

# Running Task 1
outcome <- readOutcome()
plotHeartRateHist(outcome)
