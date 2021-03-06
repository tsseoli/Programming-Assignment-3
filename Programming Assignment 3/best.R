best <- function(state, outcome) {
        ## Read outcome data
#         data_frame <- read.csv("outcome-of-care-measures.csv")
        data_frame <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        if (!(state %in% unique(data_frame[,"State"]))) {
                stop ("invalid state")
        }
#         if (!(outcome %in% c("heart failure", "heart attack", "pneumonia"))) {
#                 stop ("invalid outcome")
#         }
#         
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


        ## Return hospital name in that state with lowest 30-day death
        ## rate
        # select rows with correct state
        data_frame <- data_frame[data_frame$State == state,]
        
        # convert to numeric for sorting
        suppressWarnings(data_frame[, selCol] <- as.numeric(data_frame[, selCol]))
        
        # Sort
        data_frame <-data_frame[order(data_frame[,selCol], data_frame$Hospital.Name),]

#         return (as.character(data_frame[1, 2]))
        # return(data_frame)
        return (data_frame[1, 2])

        
}