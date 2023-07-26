pollutantmean <- function(directory, pollutant, id = 1:332){
  sums <- vector("numeric", length = length(id))
  lengths <- vector("numeric", length = length(id))
  for(i in id){
    if(i >=1 && i <= 9){
      data <- read.csv(paste(directory, '/', '00',as.character(i),'.csv', sep = ''))
      goodpollutant <- complete.cases(data[, pollutant])
      completedata <- data[goodpollutant, pollutant]
      sums[i - id[1] + 1] <- sum(completedata)
      lengths[i - id[1] + 1] <- length(completedata)
    }
    else if(i >=10 && i <= 99){
      data <- read.csv(paste(directory, '/', '0',as.character(i),'.csv', sep = ''))
      goodpollutant <- complete.cases(data[, pollutant])
      completedata <- data[goodpollutant, pollutant]
      sums[i - id[1] + 1] <- sum(completedata)
      lengths[i - id[1] + 1] <- length(completedata)
    }
    else {
      data <- read.csv(paste(directory, '/', as.character(i),'.csv', sep = ''))
      goodpollutant <- complete.cases(data[, pollutant])
      completedata <- data[goodpollutant, pollutant]
      sums[i - id[1] + 1] <- sum(completedata)
      lengths[i - id[1] + 1] <- length(completedata)
    }
  }

  finalmean <- sum(sums)/sum(lengths)
  finalmean

}


#### version control test
