##
## Ranking hospitals in all states
##

rankall <- function(outcome, num = 'best') {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    # Check validity of outcome
    if(outcome == 'heart attack') {
        outcome <- 11
    } else if(outcome == 'heart failure') {
        outcome <- 17
    } else if(outcome == 'pneumonia') {
        outcome <- 23
    } else {
        stop('Invalid outcome!')
    }
    
    # Convert death rate to numeric and drop NAs
    data[, outcome] <- as.numeric(data[, outcome])
    data <- data[!is.na(data[, outcome]), ]
    
    best.hospital <- data.frame(hospital = character(), state = character())
    
    for(st in unique(data[['State']])) {
        state.data <- data[data['State'] == st & !is.na(data[outcome]), ]
        sorted.data <- state.data[order(state.data[outcome], state.data['Hospital.Name']), ]
        
        # Find the n-th hospital in the state
        hospital.name <- NA
        if(num == 'best') {
             hospital.name <- sorted.data[1, 'Hospital.Name']
        } else if(num == 'worst') {
            hospital.name <- sorted.data[nrow(sorted.data), 'Hospital.Name']
        } else {
            hospital.name <- sorted.data[num, 'Hospital.Name']
        }
        best.hospital<- rbind(best.hospital, data.frame(hospital = hospital.name, state = st))
    }
    
    # Get rid of NAs
    best.hospital[order(best.hospital['state']), ]
}

head(rankall('heart attack', 20), 10)
tail(rankall('pneumonia', 'worst'), 3)
tail(rankall('heart failure'), 10)

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
