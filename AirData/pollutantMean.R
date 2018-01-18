directory <- "specdata"
pollutant <- "nitrate"
id = 3:332
threshold = 0


## This is a function which uses a For Loop to calculate the mean value of a specified 
## air polutant from a specified directory string with multiple csv files of data.
data = numeric()
pollutantmean <- function(directory, pollutant, id) {
  
  for (i in id){
  ##Read a file
    readMonitor = read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
      ".csv", sep = ""))
                          
  ##Extract the data for the specified pollutant
    data <- c(data, readMonitor[[pollutant]])
    
  ##Garbage collection
    gc()
  }
    
  return(mean(data, na.rm = TRUE))
}

## Same function as above, but uses the lapply method
pollutantmean2 <- function(directory, pollutant, id){
  ## Create a list of the values of the selected pollutant
  data <- lapply(id, function(i) read.csv(paste(directory, "/", formatC(i, width = 3, 
            flag = "0"), ".csv", sep = ""))[[pollutant]])
  ## Calculate and return the mean of the list
  return(mean(unlist(data), na.rm = TRUE))
}

print(pollutantmean(directory, pollutant, id))
print(pollutantmean2(directory, pollutant, id))


## This is a function which uses a For Loop to read a directory of files and counts the 
## number of completely observed cases in each file. It returns a dataframe with 2
## columns where the first is the name of the file and the second is the # of cases.
complete <- function(directory, id){
  nobs = numeric()
  for (i in id){
    ## Sum the completed cases by file
    readMonitor <- read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
      ".csv", sep = ""))
    nobs <- c(nobs, sum(complete.cases(readMonitor)))
  }
  ## Return the completed cases as a dataframe
  return(data.frame(id, nobs))
}

## Same function as above using sapply
complete2 <- function(directory, id){
  ## Write a function which sums the complete cases in each document
  caseFinder <- function(i){
    readMonitor <- read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
      ".csv", sep = ""))
    sum(complete.cases(readMonitor))
  }
  ## Apply the function over the directory
  nobs <- sapply(id, caseFinder)
  ## Return the completed cases as a dataframe
  return(data.frame(id, nobs))
}

case <- (complete(directory, id))
print(complete2(directory, id))


## This is a function which returns a numeric vector containing the correlations
## between sulfate and nitrate for files in a given directory which exceed a given 
## threshold of complete cases.
corr <- function(directory, threshold=0){
  ## Find the # of completed records per file (call "complete" function)
  nComplete <- complete(directory, id)
  ## Find the completed records above the threshold
  records <- nComplete[nComplete["nobs"] > threshold,]$id
  
  corelations = numeric()
  for (i in records){
    ##Open record
    readMonitor <- read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
      ".csv", sep = ""))
    ##Extract data
    data <- readMonitor[complete.cases(readMonitor),]
    ##Find & aggregate correlations
    corelations <- c(corelations, cor(data$sulfate, data$nitrate))
  }
  return(corelations)
}