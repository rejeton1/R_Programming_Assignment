complete <- function(directory, id = 1:332){
  finaldata <- data.frame('id' = vector('numeric',length = length(id)), 'nobs' = vector('numeric', length = length(id)))
  for(i in id){
    if(i >=1 && i <= 9){
      data <- read.csv(paste(directory, '/', '00',as.character(i),'.csv', sep = ''))
      goodpollutant <- complete.cases(data)
      completedata <- data[goodpollutant,]
      finaldata[match(i, id),'id'] <- i
      finaldata[match(i, id),'nobs'] <- nrow(completedata)
    } else if(i >= 10 && i <= 99){
      data <- read.csv(paste(directory, '/', '0',as.character(i),'.csv', sep = ''))
      goodpollutant <- complete.cases(data)
      completedata <- data[goodpollutant,]
      finaldata[match(i, id),'id'] <- i
      finaldata[match(i, id),'nobs'] <- nrow(completedata)
    } else {
      data <- read.csv(paste(directory, '/',as.character(i),'.csv', sep = ''))
      goodpollutant <- complete.cases(data)
      completedata <- data[goodpollutant,]
      finaldata[match(i, id),'id'] <- i
      finaldata[match(i, id),'nobs'] <- nrow(completedata)
    }
  }
  finaldata
}
