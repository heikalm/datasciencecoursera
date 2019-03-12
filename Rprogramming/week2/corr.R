corr <- function(directory, threshold = 0) {
        allscores <- complete(directory)
        abovethresholdid <- allscores[allscores[,"nobs"] > threshold,][,"id"]
        files_full <- list.files(directory, full.names=TRUE) #get the full file path
        files_full_subset <- files_full[abovethresholdid] #subset of only the relevant ids
        dat <- do.call(rbind, lapply(files_full_subset, read.csv)) #read from all the csv files of relevant IDs into dataframe
        
        result <- data.frame()
        for (i in abovethresholdid) {
                stationdat <- dat[dat[,"ID"] == i,] #subset a specific station based on id
                stationcorr <- cor(stationdat[,"sulfate"], stationdat[,"nitrate"], use="na.or.complete") # calculate correlation for that station
                result <- rbind(result, stationcorr)
                
        }
        colnames(result) <- "correlation"
        result[,"correlation"]
                #cor(dat[,"sulfate"], dat[,"nitrate"], use="na.or.complete")
}