best <- function(state, outcome) {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data <- select(data, state, outcome)
    selectBest(data)
}

selectBest <- function(data) {
    data[,2] <- as.numeric(data[,2])
    bestValue <- min(data[,2], na.rm = TRUE)
    listBest <- subset(data, data[,2] == bestValue)[,1]
    sort(listBest)[1]
}

# Select Hospital.Name(2) and Outcome by state
select <- function(data, state, outcome) {
    outcomeCol <- getOutcomeColumnForName(outcome)
    data <- data[which(data$State == state), c(2, outcomeCol)]

    if(nrow(data) == 0) {
        stop("invalid state")
    }
    
    data
}


getOutcomeColumnForName <- function(outcome) {
    if(outcome == "heart attack") {
        return(11)
    } else if(outcome == "heart failure") {
        return(17)
    } else if(outcome == "pneumonia") {
        return(23)
    }
    
    stop("invalid outcome")
}
