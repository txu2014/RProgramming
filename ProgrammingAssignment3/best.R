best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",na.strings = "Not Available")
  
  ## Check that state and outcome are valid
  stopifnot (state %in% data$State)
  stopifnot (outcome %in% c("heart attack", "heart failure", "pneumonia"))
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
  statedata <- cbind(statedata["State"], statedata[factorname])
  transform(statedata, factorname = as.numeric(factorname))
  res <- statedata$"Hospital.Name"[which.min(statedata[factorname])]
  return (reorder(res))
  ## rate
}
