
#############################
#### Actical read module ####
#### YKK - 13/08/2019    ####
####~*~*~*~*~*~*~*~*~*~*~####


actical_read <- function(workdir){
  
  ## Set working directory
  setwd(workdir)
  
  ## List .csv files
  ACTdata.files <- list.files(pattern = ".csv")
  
  
  for (i in 1:length(ACTdata.files)){
    
    ## Read in data
    ACTdata.1 <- read.csv(ACTdata.files[i], sep = ";")
    
    
    ## Start of colnames before raw data (this row contains 'Epoch#')
    colnames_startrow <- (which(ACTdata.1 == "Epoch#", arr.ind=TRUE)[1])
    
    
    ## which row precedes raw data (row before this contains '(4=vigorous)')?
    data_startrow <- (which(ACTdata.1 == "(4=vigorous)", arr.ind=TRUE)[1] + 1)
    
    
    ## Get last column of colnames
    data_colnames_lastcol <- (which(ACTdata.1 == "Event Marker", arr.ind=TRUE)[2])
    
    
    # Extract colnames
    data_colnames <- as.matrix(ACTdata.1[(colnames_startrow:(data_startrow - 1)), 1:data_colnames_lastcol])
    
    
    ## Loop for collapsing and assigning columnnames
    data_colnames_TEMP <- matrix(NA, nrow = 1, ncol = ncol(data_colnames))
    
    for(col_count in 1:ncol(data_colnames)){
      
      data_colnames_TEMP[, col_count] <- paste(data_colnames[, col_count], collapse = "")
      
    }
    data_colnames <- data_colnames_TEMP
    rm(data_colnames_TEMP)
    
    
    ## Remove header
    ACTdata.1 <- ACTdata.1[(data_startrow:nrow(ACTdata.1)), ]
    
    
    ## Select only required columns
    ACTdata.1 <- ACTdata.1[, (1:ncol(data_colnames))]
    
    
    ## Add column names
    colnames(ACTdata.1) <- data_colnames
    
    
    ## Select only columns for Date, Time, and Activity counts
    ACTdata.1 <- ACTdata.1[, c("Date", "Time", "ActivityCounts")]
    
    
    ## Column bind Date and Time into DateTime
    TEST_TEMP <- as.data.frame(ACTdata.1)
    TEST_TEMP$DateTime <- paste(TEST_TEMP$Date, TEST_TEMP$Time)
    TEST_TEMP <- TEST_TEMP[, c("DateTime", "ActivityCounts")]
    ACTdata.1 <- as.matrix(TEST_TEMP)
    rm(TEST_TEMP)
    
    
    dir.create(file.path(getwd(), "processed_data"), showWarnings = FALSE)
    old_wd <- getwd()
    setwd(file.path(getwd(), "processed_data"))
    
    write.csv(ACTdata.1, file = paste0(substr(ACTdata.files[i], 1, (nchar(ACTdata.files[i]) - 4)), "_output", i, ".csv"))
    
    setwd(old_wd)
    
  }
  
  
  
}



## test on pilot data
actical_read(workdir = "D:/Bibliotheek/Studie/PhD/Publishing/Actical Project")
