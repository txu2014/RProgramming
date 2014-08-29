rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",na.strings = "Not Available")
  
  ## Check that state and outcome are valid
  if (!state %in% data$State) 
  {stop("invalid state")}
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) 
  {stop("invalid outcome")}

  ##data preperation
  if (outcome == "heart attack") {factorname="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"}
  if (outcome == "heart failure") {factorname="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"}
  if (outcome == "pneumonia"){factorname="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"} 
  df <- data[data$State == state,] #filter state data
  df <- df[c("Hospital.Name",factorname)]
  df <- df[complete.cases(df),] #delet NAs
  colnames(df) <- c("Hospital.Name","Rate")
  res <- df[order(df$Rate, df["Hospital.Name"]),]
  res <- transform(res, Rank=c(1:length(res$Rate)))
  ## Return hospital name in that state with the given rank

  if (num=="best") {num <- 1}
  if (num=="worst") {num <-dim(res)[1]}
  if (num > dim(res)[1]) {return (NA)}
  return (as.character(res[num,]$"Hospital.Name"))
  
  
}