outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])


best <- function(state, outcome) {
    state.index <- 7
    name.index <- 2
    if(outcome == 'heart attack') {
        outcome <- 11
    } else if(outcome == 'heart failure') {
        outcome <- 17
    } else if(outcome == 'pneumonia') {
        outcome <- 23
    } else {
        stop('Invalid outcome!')
    }
    
    outcome.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcome.data[, outcome] <- as.numeric(outcome.data[, outcome])
    outcome.data <- outcome.data[, c(name.index, state.index, outcome)]
    if(!any(outcome.data[2] == state)) {
        stop("Invalid state!")
    }
    outcome.data <- outcome.data[outcome.data[2] == state, ]
    outcome.sorted <- outcome.data[order(outcome.data[, 3], outcome.data[, 1]), ]
    outcome.sorted[1, 1]
}

best('TX', 'heart attack')
best('TX', 'heart failure')
best('MD', 'heart attack')
best('MD', 'pneumonia')
best('BB', 'heart attack')
best('NY', 'hert attack')
