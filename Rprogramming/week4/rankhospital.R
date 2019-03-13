rankhospital <- function(state, outcome, num = "best") {
        outcomedata <- read.csv("Rprogramming/week4/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")## Read outcome data
        
        validstates <- unique(outcomedata$State) #generate list of valid states
        validoutcomes <- c("heart attack", "heart failure", "pneumonia") #generate list of outcomes
        
        if (!(state %in% validstates)) stop ("invalid state") # stop if state is not valid
        if (!(outcome %in% validoutcomes)) stop ("invalid outcome") # stop if outcome is not valid
        
        outcomecol <- c(11, 17, 23)
        columnmap <- data.frame(validoutcomes, outcomecol)
        outcomecolno <- columnmap[match(outcome, columnmap[,"validoutcomes"]),"outcomecol"] #map the outcome input to the relevant column in the data
        
        stateoutcomedata <- outcomedata[outcomedata$State == state & outcomedata[,outcomecolno] != "Not Available",] #subset the data to the relevant state
        ordereddata <- stateoutcomedata[order(as.numeric(stateoutcomedata[,outcomecolno]),stateoutcomedata[,2]),] #order the data in ascending order by outcome rate, then Hospital name
        if (num == "best") {num <- 1}
        if (num == "worst") {return(tail(ordereddata[,"Hospital.Name"],1))
                } else {
                        return(ordereddata[num,"Hospital.Name"])}
}
## Check that state and outcome are valid
## Return hospital name in that state with the given rank
## 30-day death rate