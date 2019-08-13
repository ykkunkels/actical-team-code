
##########################
#### TOSTER           ####
#### YKK - 13/08/2019 ####
####~*~*~*~*~*~*~*~*~*####


actical_TOST <- function(workdir) {
  
  ## Set working directory
  setwd(workdir)
  setwd(file.path(getwd(), "processed_data"))
  
  ## Load and/or install required packages
  if(!require('TOSTER')){install.packages('TOSTER', dep = TRUE)};library('TOSTER')
  
  ## List files
  ACT_proc_dat_files <- list.files(pattern = ".csv")
  
  ## Hardcode IDs & positions
  ACT_ppn_IDs <- c("Stefania", "Yoram", "Yugyun")
  ACT_pnn_pos <- c("ankle", "hip", "wrist")
  
  ## Remove hip data
  hip_select <- grep(ACT_pnn_pos[2], ACT_proc_dat_files, value = F)
  ACT_proc_dat_files <- ACT_proc_dat_files[(-hip_select)]
  
  ## Subset ankle data
  ankle_select <- grep(ACT_pnn_pos[1], ACT_proc_dat_files, value = F)
  ACT_proc_dat_ankle <- ACT_proc_dat_files[(ankle_select)]
  
  ## Subset wrist data
  wrist_select <- grep(ACT_pnn_pos[3], ACT_proc_dat_files, value = F)
  ACT_proc_dat_wrist <- ACT_proc_dat_files[(wrist_select)]
  
  
  
  ## Run TOST -------------------------------------------------------
  
  for (j in 1:length(ACT_ppn_IDs)){
    
    print("----------------------------------------------------------------------------------------------")
    print("")
    print(paste("Starting TOTS Analysis no. ", j));print("")
    print(paste("Testing for Equivalence with the following datasets:"))
    print(paste("No. 1: ", ACT_proc_dat_ankle[j]))
    print(paste("No. 2: ", ACT_proc_dat_wrist[j]))
    print("");print("");print("")
    
    
    ## Assign data
    dat1 <- read.csv(ACT_proc_dat_ankle[j], row.names = 1)
    dat2 <- read.csv(ACT_proc_dat_wrist[j], row.names = 1)
    
    
    ## Remove missings
    dat1 <- na.omit(dat1)
    dat2 <- na.omit(dat2)
    
    ## Initialise parameters
    m1 <- mean(dat1[, "ActivityCounts"])
    m2 <- mean(dat2[, "ActivityCounts"])
    
    sd1 <- sd(dat1[, "ActivityCounts"])
    sd2 <- sd(dat2[, "ActivityCounts"])
    
    n1 <- length(dat1[, "ActivityCounts"])
    n2 <- length(dat2[, "ActivityCounts"])
    
    low_eqbound_d <- -0.8
    high_eqbound_d <- 0.8
    
    alpha <- 0.05
    
    # Check for equal variances
    print("Testing for equal variances");print("")
    var_out <-  var.test(dat1[, "ActivityCounts"], dat2[, "ActivityCounts"], alternative = "two.sided")
    if (var_out$p.value > alpha) {
      var_equal <- TRUE
      print("Variances are equal, continuing...");print("")
    }else{
      var_equal <- FALSE
      print("Variances are NOT equal, continuing...");print("")
    }
    
    
    # Run TOST
    TOST_out <- TOSTtwo(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2,
                        low_eqbound_d = low_eqbound_d, high_eqbound_d = high_eqbound_d,
                        alpha = alpha, var.equal = var_equal)
    
    
    # mtext data names
    mtext(paste("Comparing: ", ACT_proc_dat_ankle[j], " vs. ", ACT_proc_dat_wrist[j]), line = -1)
    
    
    # Double-check interpretation
    print("");print("")
    if(max(c(TOST_out$TOST_p1, TOST_out$TOST_p2)) < alpha){
      print("INTERPRETATION SUMMARY: Wrist is equivalent to ankle")
    }else{print("INTERPRETATION SUMMARY: Wrist is NOT equivalent to ankle")}
    
    
    print("")
    print("END of this comparison, continuing to next one...")
    print("");print("");print("");print("")
    
    
  }
  
}


## test on pilot data
actical_TOST("D:/Bibliotheek/Studie/PhD/Publishing/Actical Project")


