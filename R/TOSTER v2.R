
##############################################################
#### TOSTER                                               ####
#### YKK - 06/11/2019                                     ####
#### Added log() transform to deal with unequal variances ####
####~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*####


actical_TOST <- function(workdir) {
  
  ## Set working directory
  setwd(workdir)
  # setwd(file.path(getwd(), "processed_data"))
  
  
  ## Load and/or install required packages
  if(!require('TOSTER')){install.packages('TOSTER', dep = TRUE)};library('TOSTER')
  if(!require('car')){install.packages('car', dep = TRUE)};library('car')
  
  # ## List files
  cut_files <- list.files(pattern = "_cut")
  ppns <- unique(unlist(strsplit(cut_files, "[_]"))[c(TRUE, rep(FALSE, 4))])
  
  
  
  ## Run TOST -------------------------------------------------------
  
  for (j in 1:length(ppns)){
    
    print("----------------------------------------------------------------------------------------------")
    message(cat(""))
    print(paste("Starting TOST Analysis no. ", j));message(cat(""))
    print(paste("Testing for Equivalence with the following datasets:"))
    print(paste("No. 1: ", cut_files[grep(ppns[j], cut_files)[1]]))
    print(paste("No. 2: ", cut_files[grep(ppns[j], cut_files)[2]]))
    message(cat(""));message(cat(""));message(cat(""))
    
    
    ## Assign data
    # dat1 <- read.csv(ACT_proc_dat_ankle[j], row.names = 1)
    # dat2 <- read.csv(ACT_proc_dat_wrist[j], row.names = 1)
    
    
    dat1 <- read.table(file.path(paste0(paste0(getwd(), "/", cut_files[grep("enkel", cut_files)][j], "/"),
                                        paste0(cut_files[grep("enkel", cut_files)][j], " MANAGED.txt"))))
    
    dat2 <- read.table(file.path(paste0(paste0(getwd(), "/", cut_files[grep("pols", cut_files)][j], "/"),
                                        paste0(cut_files[grep("pols", cut_files)][j], " MANAGED.txt"))))
    
    
    
    colnames(dat1) <- colnames(dat2) <- c("DateTime", "ActivityCounts")
    
    
    ## Remove missings (or replace NA with zero)
    # dat1 <- na.omit(dat1)
    # dat2 <- na.omit(dat2)
    dat1[(which(is.na(dat1)) - nrow(dat1)), "ActivityCounts"] <- 0
    dat2[(which(is.na(dat2)) - nrow(dat2)), "ActivityCounts"] <- 0
    
    ## Make datasets equal lenght (diff may occur due to removal of trailing zeroes)
    equal_lenght <- min(c(nrow(dat1), nrow(dat2)))
    dat1 <- dat1[(1:equal_lenght), ]
    dat2 <- dat2[(1:equal_lenght), ]
    
    ## Take log() of activity counts to deal with unequal variances
    dat1$ActivityCounts <- log(dat1$ActivityCounts)
    dat1[dat1$ActivityCounts == -Inf, "ActivityCounts"] <- 0
    
    dat2$ActivityCounts <- log(dat2$ActivityCounts)
    dat2[dat2$ActivityCounts == -Inf, "ActivityCounts"] <- 0
    
    
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
    print("Testing for equal variances");message(cat(""))
    var_out <-  var.test(dat1[, "ActivityCounts"], dat2[, "ActivityCounts"], alternative = "two.sided")
    if (var_out$p.value > alpha) {
      var_equal <- TRUE
      print("Variances are equal, continuing...");message(cat(""))
    }else{
      var_equal <- FALSE
      print("Variances are NOT equal, continuing...");message(cat(""))
    }
    
    print("Testing for equal variances 2 - Levene's Test");message(cat(""))
    group <- as.factor(c(rep(1, nrow(dat1)), rep(2, nrow(dat2))))
    var_out2 <- leveneTest(c(dat1[, "ActivityCounts"], dat2[, "ActivityCounts"]), group)
    
    if (var_out2$`Pr(>F)`[1] > alpha) {
      var_equal2 <- TRUE
      print("Variances are equal, continuing...");message(cat(""))
    }else{
      var_equal2 <- FALSE
      print("Variances are NOT equal, continuing...");message(cat(""))
    }
    
    
    
    
    ## Run TOST
    png(filename = paste0("TOSTplot_", j, ".png"), width = 842, height = 595)
    
    TOST_out <- TOSTtwo(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2,
                        low_eqbound_d = low_eqbound_d, high_eqbound_d = high_eqbound_d,
                        alpha = alpha, var.equal = var_equal)
    
    
    # mtext data names
    mtext(paste("Comparing: ", cut_files[grep(ppns[j], cut_files)[1]], " vs. ", cut_files[grep(ppns[j], cut_files)[2]]), line = -1)
    
    
    # Double-check interpretation
    message(cat(""));message(cat(""))
    if(max(c(TOST_out$TOST_p1, TOST_out$TOST_p2)) < alpha){
      message(cat("INTERPRETATION SUMMARY: Wrist is equivalent to ankle"))
    }else{message(cat("INTERPRETATION SUMMARY: Wrist is NOT equivalent to ankle"))}
    
    
    
    if (format(var_out$p.value, digits = 3, nsmall = 3, scientific = FALSE) <= alpha && TOST_out$TOST_p2 <= alpha) {
      combined_outcome <- "Different but equivalent (D/E): There is a difference, but it is trivial. For example, the study is overpowered"
    }
    if (format(var_out$p.value, digits = 3, nsmall = 3, scientific = FALSE)< alpha && TOST_out$TOST_p2 > alpha) {
      combined_outcome <- "Different and nonequivalent (D/NE): There is a difference—sufficient to have substantive relevance. "
    }
    if (format(var_out$p.value, digits = 3, nsmall = 3, scientific = FALSE) > alpha && TOST_out$TOST_p2 <= alpha) {
      combined_outcome <- "Not different and equivalent (ND/E): The two conditions are indistinguishable"
    }
    if (format(var_out$p.value, digits = 3, nsmall = 3, scientific = FALSE) > alpha && TOST_out$TOST_p2 > alpha) {
      combined_outcome <- "Not different but also not equivalent (ND/NE): The variability is too great relative to the effect size to interpret. For example, the study is underpowered"
    }
    cat("\n")
    message(cat("Interpretation details: The effect is ",
                combined_outcome, ".", sep = ""))
    
    
    message(cat(""))
    if(j == length(ppns)){print("All files DONE")} else{print("END of this comparison, continuing to next one...")}
    message(cat(""));message(cat(""));message(cat(""));message(cat(""))
    
    dev.off()
    
    
  }
  
}


## Runner
actical_TOST("D:/Bibliotheek/Studie/PhD/Publishing/Actical Project/data_convert/Subset periods/Managed Datasets")


