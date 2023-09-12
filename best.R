best <- function(state, outcome) {
        ## Read outcome data
        
        ## Check that state and outcome are valid
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        # read in the data
        
        # needed to run the arrange function
        library(data.table)
        
        data <- read.csv('outcome-of-care-measures.csv', colClasses= 'character')
        
        # create a char vector to hold outcome hashes
        v_outcomes <- c('heart attack', 'heart failure', 'pneumonia')
        # create a char vector to hold actual outcome names
        v_col_names <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                         "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                         "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"         
                         )
        
        # simplify the column names to match outcome
        setnames (data, v_col_names, v_outcomes)
        
        
        # test validity of state argument
        if (!(state %in% unique(data$State))) {
                stop('invalid state')
        }
        
        # test validity of outcome  argument
        if (!(outcome %in% v_outcomes)) {
                stop('invalid outcome')
        }
        
        # convert the outcome data to numbers from characters
        data[, outcome] <- as.numeric(data[, outcome])
        
        # only select rows that are complete in the outcome column
        data <- data[(data$State == state) & complete.cases(data[,outcome]), ]
        
        # only use the 3 relevant cols State, outcome, Hospital.Name
        data <- data[ , c('State', outcome, 'Hospital.Name')]
                     
        # order the data frame by outcome and hospital.name
        data <- data[order(data[[outcome]], data$'Hospital.Name'), ]
        
        print(data[1, 'Hospital.Name'])
        
}