pollutantmean <- function(directory, pollutant = "sulfate", id = 1:332) {
  ## = "D:/ZProgz/data/specdata"
  #pollutantmean <- function(directory, pollutant = "sulfate", id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  All_file <- list.files(directory,full.names=TRUE)
  
  files_full<-sort(All_file)
  
  dat <- data.frame(matrix(NA, nrow = 0, ncol = 4))
  
  for (i in id) {
    dat <- rbind(dat, read.csv(files_full[i]))
  }
  
  colnames(dat) <- c("D","sulfate","nitrate", "I")
  
  
  result <-   mean(dat[, pollutant][dat$I %in% id],na.rm=TRUE)
  return(result)

  
  #mean(dat[, pollutant][dat$I<=id[length(id)] & dat$I>=id[1]],na.rm=TRUE)
  
  
}



#pollutantmean("D:/ZProgz/data/specdata", "sulfate", 1:10) == 4.064
#pollutantmean("D:/ZProgz/data/specdata", "nitrate", 70:72) == 1.706
#pollutantmean("D:/ZProgz/data/specdata", "nitrate", 23) == 1.281
