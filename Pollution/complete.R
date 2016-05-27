complete <- function(directory, id = 1:332) {
  ## = "D:/ZProgz/data/specdata"
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
  All_file <- list.files(directory,full.names=TRUE)
  
  files_full<-sort(All_file)
  
  dat <- data.frame(matrix(NA, nrow = 0, ncol = 4))
  
  counter <- 1:length(id) * 0
  
  j <- 1 
  
  for (i in id) {
    dat <- read.csv(files_full[i])
    counter[j] <- sum(complete.cases(dat))
    j <- j + 1
  }
  
 result <- data.frame(id = id, nobs = counter)
  return(result)
 
} 
  

# tests
#complete("D:/ZProgz/data/specdata", 1)
#complete("D:/ZProgz/data/specdata", c(2, 4, 8, 10, 12))
#complete("D:/ZProgz/data/specdata", 30:25)
#complete("D:/ZProgz/data/specdata", 3)