corr <- function(directory, threshold = 0) {
  nele <- 0
  corrvector <- vector('numeric', nele)
  for(i in 1:332){
    if(i >=1 && i <= 9){
      data <- read.csv(paste(directory, '/', '00',as.character(i),'.csv', sep = ''))
      nobs <- nrow(data[complete.cases(data),])
      if(nobs > threshold){
        nele <- nele + 1
        corrvector[nele] <- cor(data[complete.cases(data),'sulfate'], data[complete.cases(data),'nitrate'])
      } else {
        next
      }
    } else if(i >=10 && i <= 99){
      data <- read.csv(paste(directory, '/', '0',as.character(i),'.csv', sep = ''))
      nobs <- nrow(data[complete.cases(data),])
      if(nobs > threshold){
        nele <- nele + 1
        corrvector[nele] <- cor(data[complete.cases(data),'sulfate'], data[complete.cases(data),'nitrate'])
      } else {
        next
      }
    } else {
      data <- read.csv(paste(directory, '/', as.character(i),'.csv', sep = ''))
      nobs <- nrow(data[complete.cases(data),])
      if(nobs > threshold){
        nele <- nele + 1
        corrvector[nele] <- cor(data[complete.cases(data),'sulfate'], data[complete.cases(data),'nitrate'])
      } else {
        next
      }
    }
  }

  corrvector
}


