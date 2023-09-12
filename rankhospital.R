rankhospital <- function(state, outcome, num = 'best'){
        ## Read outcome data
        
        ## Check that state and outcome are valid
        
        ## Return hospital name in that state with the give rank
        ## 30-day death rate
        
        
        data <- read.csv('outcome-of-care-measures.csv')
        
        if (!(state %in% unique(data$State))) {
                stop('invalid state')
        }
        
        if (!(outcome %in% c('heart attack', 
                             'heart failture', 
                             'pneumonia'))) {
                stop('invalid outcome')
        }
        
        # create a char vector to hold outcome hashes
        v_outcomes <- c('heart attack', 'heart failure', 'pneumonia')
        # create a char vector to hold actual outcome names
        v_col_names <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                         "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                         "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
                         )
        
        #update the outcome column names to make them more managable
        setnames(data, v_col_names, v_outcomes)
        
        # shrink the dataframe to the relevant columns
        data <- data[c('State', outcome, 'Hospital.Name')]
        
        # convert the outcome data to numbers from characters
        data[, outcome] <- as.numeric(data[, outcome])
        
        # get the relevant complete rows in the relevant state
        data <- data[ (data$State == state) & (complete.cases(data[, outcome])),
                            c('State', outcome, 'Hospital.Name')]

        data <- data[order(data[[outcome]], data$Hospital.Name),]
        
        data
}