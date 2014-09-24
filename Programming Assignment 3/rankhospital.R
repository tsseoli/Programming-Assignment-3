rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        
        data_frame <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        # check for state
        if (!(state %in% unique(data_frame[,"State"]))) {
                stop ("invalid state")
        }
        
        ## check for outcome
        if (outcome == "heart failure") {
                selCol = 17
        } else {
                if (outcome == "heart attack") {
                        selCol = 11
                } else {
                        if (outcome == "pneumonia") {
                                selCol = 23
                        } else {
                                stop ("invalid outcome")
                        }
                }
        }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        # select rows with correct state
        data_frame <- data_frame[data_frame$State == state,]
        
        # convert to numeric for sorting
        suppressWarnings(data_frame[, selCol] <- as.numeric(data_frame[, selCol]))
        
        # Omit NA
        data_frame <- na.omit(data_frame)
        # data_frame <- data_frame[!is.na(data_frame[, selCol],)
        
        # Sort
        data_frame <-data_frame[order(data_frame[,selCol], data_frame$Hospital.Name),]
        
        # convert the expressions "best" and "worst" into numerical values
        if (num == "best") {
                num <- 1
        } else {
                if (num == "worst") {
                        num <- length(data_frame[,1])
                } 
        }

        # handle "overflow"
        if (num > length(data_frame[,1])) {
                return(NA)
        } else {
                # return the hospital with corresponding rank
                return (data_frame[num, 2])
        }
        
}