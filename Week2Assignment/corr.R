corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  res <- numeric(0)
  for (i in 1:332)
  {
    filename <- paste("./",directory,"/"
                      ,formatC(i,width=3,format="d",flag="0")
                      ,".csv"
                      ,sep="")
    csvdata <- read.csv(filename)
    ok <- complete.cases(csvdata)
    nd <- csvdata[ok,]
    if (sum(ok) > threshold)
    {
      co <- cor(nd$sulfate,nd$nitrate)
      res <- c(res, c(co))
    }
  }
  ## Return a numeric vector of correlations
  return(res)
}