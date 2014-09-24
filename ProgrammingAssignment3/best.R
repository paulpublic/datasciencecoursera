# task number 2
best <- function(state, outcome) {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data <- select(data, state, outcome)
    best <- sortOrderAndRank(data)
    best$Hospital.Name[1]
}

# task number 3
rankhospital <- function(state, outcome, num = "best") {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data <- select(data, state, outcome)
    best <- sortOrderAndRank(data)
    
    num <- if(num == "best") 1 else if(num == "worst") nrow(best) else num
    best$Hospital.Name[num]
}

sortOrderAndRank <- function(x) {
    x[,2] <- as.numeric(x[,2])
    
    x <- x[order(x[,2], x[,1]),] #sorted by rate (1st) and name (2nd)
    x <- cbind(x, 1:nrow(x)) # Add ranking
    
    colnames(x)[2] <- "Rate"
    colnames(x)[3] <- "Rank"
    
    x[!is.na(x[,2]),]
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
