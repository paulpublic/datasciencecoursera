corr <- function(directory, threshold = 0) {
    directory <- paste(directory, "/", sep = "")
    print (directory)
    allStations <- 1:332
    completeDataForStations <- complete(directory, allStations)
    selectedStations <-
        completeDataForStations[completeDataForStations[,c("nobs")] > threshold, c("id")]
    

    corrs <- vector()
    for (station in selectedStations) {
        rawStationData = readDataFromStation(directory, station)
        prunedStationData <- rawStationData[complete.cases(rawStationData), ]
        polutionCorrelation <- cor(prunedStationData["sulfate"], prunedStationData["nitrate"])
        corrs <- c(corrs, polutionCorrelation)
    }
    
    corrs
}

complete <- function(directory, id = 1:332) {
    directory <- paste(directory, "/", sep = "")
    # output = matrix(0,0,2, dimnames = list(1:2, ncol = 2, c("id", "nobs")))
    output = data.frame(0,0)
    colnames(output) <- c("id", "nobs")
    
    for(station in id) {
        stationData = readDataFromStation(directory, station)
        numberOfComplete <- sum(complete.cases(stationData))
        
        output <- rbind(output, c(station, numberOfComplete))
    }
    output[-1,]
}

readDataFromStations <- function(directory, stations) {
    fullDataFromStations <- matrix(nrow = 0, ncol = 5)
    for(station in stations) {
        fullDataFromStations <-
            rbind(fullDataFromStations, readDataFromStation(directory, station))
    }
    fullDataFromStations
}

readDataFromStation <- function(directory, stationId) {
    filename <- sprintf("%03d.csv", stationId)
    filepath <- paste(directory, filename, sep = "")
    
    data <- read.csv(filepath)
}