
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name

  
  data <- read.csv("d:/ZProgz/data/hospital/outcome-of-care-measures.csv", colClasses = "character")
  
  col_outcome <- switch(outcome,"heart attack"=11,"heart failure"=17,"pneumonia"=23)
  
# select the right colums and rows 
# Stop the program if Outcome is not existing
  
  if (!is.null(col_outcome)) {data_working<-data[,c(2, 7,col_outcome)]} else {stop("invalid outcome")}
#  if (nrow(data_outcome[data_outcome$State==state, ])!=0) {data_working <- data_outcome[data_outcome$State==state, ]} else {stop("invalid state")}
  
# Give names to colums, ensure everything is numeric and get rid of "Not Available" or NA
  colnames(data_working)<-c("hosp_name","state","days") 
  data_working$days<- suppressWarnings(sapply(data_working$days, as.numeric))
  data_working<- na.omit(data_working)
  
  # Sort the hospital by mortality rate and hospital name 
  data_working <- data_working[order(data_working$days,data_working$hosp_name),]
  
# Allocate value for "best" (and a default value for "worst")
  if (!is.numeric(num)) {num <-switch(num,"best"=1,"worst"=0)}
  
  
# Retrieve unique states and sort them by alphabetical order  
  states <- sort(unique(data_working$state))
  
  
#Function returns the hospital name for the given state at the specified rank.
  state_hospital_data <- function(state) {
    part <- data_working[data_working$state==state,] 
    if (num==0) {num = nrow(part)}
    part <- part[,c("hosp_name","state")]
    part <-part[num,]
    part$state <- state
        return(part)
  }
  
  #Apply the Function for all the states.
  state_data <- lapply(states, state_hospital_data)
  
  # Transform the list into a dataframe
  df <- data.frame(Reduce(rbind, state_data)) 
  #df <- data.frame(t(sapply(state_data,c)))
  colnames(df)<-c("hospital","state") 
 
  return(df)
}

# tests
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
