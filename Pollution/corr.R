corr <- function(directory , threshold = 0) {
  ## = "D:/ZProgz/data/specdata"
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  
  All_file <- list.files(directory,full.names=TRUE)
  
  files_full<-sort(All_file)
 
  comp_id_nb <- complete(directory, 1:332)
  colnames(comp_id_nb) <- c("ID","NOBS")
 
  comp_id_thres<-comp_id_nb[, "ID"][comp_id_nb$NOBS > threshold]
  
  corr_vector <- 1:length(comp_id_thres) * 0
 
  j <- 1
  for(i in comp_id_thres) {
 
   dat <- read.csv(files_full[i])
   corr_vector[j] <- cor(dat$sulfate, dat$nitrate, use="complete.obs")
   j <- j + 1
 }
 result <- corr_vector
  return(result)   
}

# tests
#cr <- corr("D:/ZProgz/data/specdata", 150)
#head(cr)
#cr <- corr("D:/ZProgz/data/specdata", 400)
#head(cr)
#cr <- corr("D:/ZProgz/data/specdata", 5000)
#summary(cr)