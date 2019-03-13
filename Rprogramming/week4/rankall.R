rankall <- function(outcome, num = "best") {
        outcomedata <- read.csv("Rprogramming/week4/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")## Read outcome data
        
        validstates <- unique(outcomedata$State) #generate list of valid states
        validoutcomes <- c("heart attack", "heart failure", "pneumonia") #generate list of outcomes
        
        if (!(outcome %in% validoutcomes)) stop ("invalid outcome") # stop if outcome is not valid
        
        outcomecol <- c(11, 17, 23)
        columnmap <- data.frame(validoutcomes, outcomecol)
        outcomecolno <- columnmap[match(outcome, columnmap[,"validoutcomes"]),"outcomecol"] #map the outcome input to the relevant column in the data
        
        result <- data.frame()
        
        for (state in validstates[order(validstates)]) {
                stateoutcomedata <- outcomedata[outcomedata$State == state & outcomedata[,outcomecolno] != "Not Available",] #subset the data to the relevant state, only hospitals with data
                ordereddata <- stateoutcomedata[order(as.numeric(stateoutcomedata[,outcomecolno]),stateoutcomedata[,2]),] #order the data in ascending order by outcome rate, then Hospital name
                if (num == "best") {num <- 1}
                if (num == "worst") {
                        result <- rbind(result,data.frame("hospital"=tail(ordereddata[,"Hospital.Name"],1),"state"=state) )
                } else {
                        result <- rbind(result, data.frame("hospital"=ordereddata[num,"Hospital.Name"],"state"=state) )
                        
                        }
                
                
                
        }
        row.names(result) <- result$State
        return(result)
}


## For each state, find the hospital of the given rank
## Return a data frame with the hospital names and the
## (abbreviated) state name