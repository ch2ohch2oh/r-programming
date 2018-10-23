##
## Ranking the hospital by outcome in a state
##

rankhospital <- function(state, outcome, num = "best") {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    # Check validity of outcome argument
    if(outcome == 'heart attack') {
        outcome <- 11
    } else if(outcome == 'heart failure') {
        outcome <- 17
    } else if(outcome == 'pneumonia') {
        outcome <- 23
    } else {
        stop('Invalid outcome!')
    }
    
    # Check the validity of state argument
    if(!any(data['State'] == state)) {
        stop("Invalid state!")
    }
    
    # Convert death rate to numeric and drop NAs
    data[, outcome] <- as.numeric(data[, outcome])
    data <- data[!is.na(data[, outcome]), ]
    
    # Filter states
    data <- data[data['State'] == state, ]
    
    sorted.data <- data[order(data[outcome], data['Hospital.Name']), ]
    
    # Return
    if(num == 'best') {
        return(sorted.data[1, 'Hospital.Name'])
    } else if(num == 'worst') {
        return(sorted.data[nrow(sorted.data), 'Hospital.Name'])
    } else {
        return(sorted.data[num, 'Hospital.Name'])
    }
}

rankhospital('TX', 'heart failure', 4)
rankhospital('MD', 'heart attack', 'worst')
rankhospital('MN', 'heart attack', 5000)

rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
