pollutantmean <- function(directory, pollutant, id = 1:332) {
    directory <- paste(directory, "/", sep = "")
    data <- readDataFromStations(directory, id)
    pollutantData <- data[, c(pollutant)]
    mean(pollutantData, na.rm = TRUE)
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