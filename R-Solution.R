# The solution to the programming task in week 4

# Helper function reading the hospital data and return a data frame
readHospital <- function(){
    data <- read.csv('./data/hospital-data.csv', colClasses="character")
}
# Helper functio reading the outcom data and return a data frame
readOutcome <- function() {
    data <- read.csv('./data/outcome-of-care-measures.csv', colClasses='character')
}

# function that filters the given data by state and a type of data
filter_by_state_and_type <- function(idata, state, type_of_outcome){
    idx <- list("heart attack"=11, "heart failure"=17, "pneumonia"=23)
    # check if requested data type is valid, if not return with warning
    if (!type_of_outcome %in% names(idx)){
        stop("Not a valid data")
    }

    # create a filter
    a <- idx[type_of_outcome]
    filt = list('name'=2, 'state'=7, 'value'=as.numeric(a))
    # apply filters to data
    work_data <- idata[as.numeric(filt)]
    work_data[,3] <- as.numeric(work_data[,3])
    work_data <- work_data[complete.cases(work_data),]
    state_data <- work_data[work_data[,2]==state,]
    # check if filted state has data
    if (dim(state_data)[1] == 0){
        stop("Not valid state")
    }
    return(state_data)
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
    print("Histogramm saved in img/heart_rate_hist.png")
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
best <- function(state, type_of_outcome, idata){
    state_data <- try(suppressWarnings(filter_by_state_and_type(idata, state, type_of_outcome)))
    if (class(state_data)=='try-error'){return ('')}
    # sort data by colum 3
    sorted <- state_data[order(state_data[,3], decreasing=FALSE),]
    return(sorted[1,])
}

# Task 3
# ---------
#
# Find the n best hospitals in a state. If n is 'best' or 'worst', then set n to 5
rankhospital <- function(idata, state, type_of_outcome, num=5){
    state_data <- try(suppressWarnings(filter_by_state_and_type(idata, state, type_of_outcome)))
    if (is.character(num)){
        if (num == 'worst'){
            ord <- TRUE
            num <- 1:5
        }else{
            ord <- FALSE
            num <- 1:5
        }
    }else{
        ord <- FALSE
        num <- 1:num
    }
    sorted <- state_data[order(state_data[,3], state_data[,1], decreasing=ord),]
    return(sorted[num,])
}

# Running Task 1
print("Reading Data")
outcome <- readOutcome()
print("Running Task 1")
plotHeartRateHist(outcome)


# Running Task 2
print("Running Task 2")
best_hospital <- best('TX','heart failure', outcome)
print(best_hospital[1])
best_hospital <- best('MD','pneumonia', outcome)
print(best_hospital[1])
#best_hospital <- best('BB','heart attack', outcome)
print(best_hospital)

# Runnung Task 3
best_hospitals <- rankhospital(outcome, 'TX','heart failure', 'best')
print(best_hospitals)
