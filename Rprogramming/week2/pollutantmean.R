pollutantmean <- function(directory, pollutant, id=1:332){
        files_full <- list.files(directory, full.names=TRUE) #get the full file path
        files_full_subset <- files_full[id] #subset of only the relevant ids
        dat <- do.call(rbind, lapply(files_full_subset, read.csv)) #read from all the csv files of relevant IDs into dataframe
        mean(dat[,pollutant], na.rm=TRUE) #calculate the mean of the pollutant, excluding NA values
}