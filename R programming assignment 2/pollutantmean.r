pollutantmean <- function(directory, pollutant, id = 1:332) {
  num <- 0
  total <- 0
  for(file in sprintf('%s/%03d.csv', directory, id)) {
    data <- read.csv(file, header = T)
    num <- num + sum(!is.na(data[, pollutant]))
    total <- total + sum(data[, pollutant], na.rm = T)
  }
  total / num
}

pollutantmean('specdata', 'sulfate', 1:332)
pollutantmean("specdata", "nitrate", 1:330)
pollutantmean("specdata", "nitrate")

