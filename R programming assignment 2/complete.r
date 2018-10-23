complete <- function(directory, id = 1:332) {
  com <- data.frame()
  for(i in id) {
    data <- read.csv(sprintf('%s/%03d.csv', directory, i), header = T)
    com <- rbind(com, 
                 c(i, sum(!is.na(data['nitrate']) & !is.na(data['sulfate']))))
  }
  colnames(com) <- c('id', 'nobs')
  com
}

complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)

