complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  #Initiate an empty data.frame with names.
  res <- data.frame(id=character(0),nobs=integer(0))
  for (i in id)
  {
    filename <- paste("./",directory,"/"
                      ,formatC(i,width=3,format="d",flag="0")
                      ,".csv"
                      ,sep="")
    csvdata <- read.csv(filename)
    nobs <- sum(complete.cases(csvdata))
    res <- rbind(res, data.frame(i,nobs))
  }
  # rename the column of data.frame
  colnames(res) <- c("id","nobs")
  return(res)
}