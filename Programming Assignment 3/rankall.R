rankall <- function(outcome, num = "best") {
        # initialize data frame which stores the result
        df_result <- data.frame(hospital=as.character(), state=as.character())
        
        ## Read outcome data
        data_frame <- read.csv("outcome-of-care-measures.csv", 
                               colClasses = "character",
                               na.strings = "Not Available")
   
        ## Check that outcome is valid
                
        # check for outcome
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
        
        # filter rows with NAs for specific outcome
        data_frame <- data_frame[!is.na(data_frame[,selCol]),]
        
        # translate "best" to numeric value
        if (num == "best") {
                num <- 1
        }
        
        # convert to numeric for sorting
        data_frame[, selCol] <- as.numeric(data_frame[, selCol])
        
        ## For each state, find the hospital of the given rank
        
        for (strState in sort(unique(data_frame[,7]))) {
                # select rows with actual state
                stateSubset <- data_frame[data_frame$State == strState,]

                # Sort according to outcome
                stateSubset <-stateSubset[order(stateSubset[,selCol],
                                                stateSubset$Hospital.Name),]

                if (num == "worst") {
                        myNum <- nrow(stateSubset)
                } else {
                        if (num <= nrow(stateSubset)) {
                                myNum <- num
                        } else {
                                myNum <- NA
                        }
                }

                if (is.na(myNum)) {
                        strHospital <- NA
                } else {
                        strHospital <- stateSubset[myNum, 2]
                }

                df_result <- rbind(df_result,
                                   data.frame(hospital=as.character(strHospital),
                                              state=as.character(strState),
                                              row.names=strState))
         }
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        return(df_result)
}