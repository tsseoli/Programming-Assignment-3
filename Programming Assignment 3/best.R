best <- function(state, outcome) {
        ## Read outcome data
        data_frame <- read.csv("outcome-of-care-measures.csv")
        
        ## Check that state and outcome are valid
        if (!(state %in% unique(data_frame[,"State"]))) {
                stop ("invalid state")
        }
        if (!(outcome %in% c("heart failure", "heart attack", "pneumonia"))) {
                stop ("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
}