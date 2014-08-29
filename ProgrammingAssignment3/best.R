best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",na.strings = "Not Available")
  
  ## Check that state and outcome are valid
  if (!state %in% data$State) 
    {stop("invalid state")}
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) 
    {stop("invalid outcome")}
  ## Return hospital name in that state with lowest 30-day death
  statedata <- data[data$State == state,]
  if (outcome == "heart attack")
     {factorname="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"}
  else if (outcome == "heart failure") {
    factorname="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  }
  else {
    factorname="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } 
  #take only 2 columns
  statedata <- cbind(statedata["Hospital.Name"], statedata[factorname])
  #delete NAs
  statedata <- statedata[complete.cases(statedata),]
  #transform(statedata, factorname = as.numeric(factorname))
  min.row <- row.names(statedata)[which(statedata[factorname] == min(statedata[factorname],na.rm=T))]
  res <- as.character(statedata[min.row,"Hospital.Name"])
  return (res)
  ## rate
}
