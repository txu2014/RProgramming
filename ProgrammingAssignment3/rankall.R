rankall <- function(outcome, num = "best") {
  
  source("rankhospital.R")
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",na.strings = "Not Available")
  
  ## Check that state and outcome are valid
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) 
  {stop("invalid outcome")}
  
  ## For each state, find the hospital of the given rank
  if (exists("res")) {rm(res)}
  for (i in levels(data$State)){
    #print(i)
    if (!exists("res")) {
      res <- data.frame(hospital=rankhospital(i,outcome,num),state=i,row.names=i)
    } else{
      newrow <- data.frame(hospital=rankhospital(i,outcome,num),state=i,row.names=i)
      res <- rbind(res, newrow)
    }
  }
  return (res)

  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}