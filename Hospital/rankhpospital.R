
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate

  
  data <- read.csv("d:/ZProgz/data/hospital/outcome-of-care-measures.csv", colClasses = "character")
  
  col_outcome <- switch(outcome,"heart attack"=11,"heart failure"=17,"pneumonia"=23)
  
# select the right colums and rows 
# Stop the program if Outcome or state are not existing
  
  if (!is.null(col_outcome)) {data_outcome<-data[,c(2, 7,col_outcome)]} else {stop("invalid outcome")}
  if (nrow(data_outcome[data_outcome$State==state, ])!=0) {data_working <- data_outcome[data_outcome$State==state, ]} else {stop("invalid state")}
  
# Give names to colums, ensure everything is numeric and get rid of "Not Available" or NA
  colnames(data_working)<-c("hosp_name","state","days") 
  data_working$days<- suppressWarnings(sapply(data_working$days, as.numeric))
  data_working<- na.omit(data_working)
  
# Allocate value for "best" and worst"
  if (!is.numeric(num)) {num <-switch(num,"best"=1,"worst"=nrow(data_working))}
  
  # Sort the hospital by mortality days 
  # select the hospital at a given rank and return the name 
  hosp_name <- data_working[order(data_working$days,data_working$hosp_name),"hosp_name"]
  chosen <-hosp_name[num]
 return(chosen)
}
