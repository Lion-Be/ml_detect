#' ------------------------------------
# simulation study --------------------
#  ------------------------------------
#' ------------------------------------

# define evaluated scenarios in simulation study
n <- c(500, 600, 700, 800, 900, 1000, 2000)
f_inc <- c(0.02, 0.05, 0.1)
f_ext <- c(0.01, 0.02, 0.05)
rounding_perc <- c(0.02)
fraud_type <- c("bbs", "stealing", "switching")
study_scenarios <- expand.grid(n, f_inc, f_ext, rounding_perc, fraud_type)
colnames(study_scenarios) <- c("n", "f_inc", "f_ext", "rounding_perc", "fraud_type")
study_scenarios$fraud_type <- as.character(study_scenarios$fraud_type)
study_scenarios[190:196,] <- NA
study_scenarios[190:196, "n"] <- c(500, 600, 700, 800, 900, 1000, 2000)
study_scenarios[190:196, "f_inc"] <- 0
study_scenarios[190:196, "f_ext"] <- 0
study_scenarios[190:196, "rounding_perc"] <- 0
study_scenarios[190:196, "fraud_type"] <- "clean"

# iterate over scenarios, store point estimates and sd
study_scenarios$perc_fraudedAll <- study_scenarios$sd <- study_scenarios$pred <-  NA

for (row in 190:196) {

  tryCatch({
    
  synth_data <- 
    gen_data(n_entities = study_scenarios$n[row], 
             eligible = rep(1000, study_scenarios$n[row]), 
             fraud_type = study_scenarios$fraud_type[row], 
             fraud_incA = study_scenarios$f_inc[row], 
             fraud_extA = study_scenarios$f_ext[row], 
             fraud_roundA = T, 
             share_roundA = study_scenarios$rounding_perc[row],
             turnout_emp = rbeta(study_scenarios$n[row], 13, 7),
             shareA_emp = rbeta(study_scenarios$n[row], 7, 5),
             n_elections = 1, 
             data_type = "full"
             ) 
  
  study_scenarios$perc_fraudedAll[row] <- synth_data$perc_fraudedAll[1]
  
  if(study_scenarios$fraud_type[row] != "clean")
    result <- 
      ml_detect(data = synth_data, 
                data_name = "synth_data",
                eligible = synth_data$eligible, 
                votes_a = synth_data$votes_a, 
                votes_b = synth_data$votes_b, 
                turnout_emp = synth_data$turnout, 
                shareA_emp = synth_data$shareA, 
                shareB_emp = synth_data$shareB, 
                fraud_incA = seq(0.01, 0.20, 0.01),
                fraud_types = c(study_scenarios$fraud_type[row], "rounding"),
                models = "randomForest", 
                ml_task="cont", 
                parallel = F) 
  
  if(study_scenarios$fraud_type[row] == "clean")
    result <- 
      ml_detect(data = synth_data, 
              data_name = "synth_data",
              eligible = synth_data$eligible, 
              votes_a = synth_data$votes_a, 
              votes_b = synth_data$votes_b, 
              turnout_emp = synth_data$turnout, 
              shareA_emp = synth_data$shareA, 
              shareB_emp = synth_data$shareB, 
              fraud_incA = seq(0.01, 0.20, 0.01),
              fraud_types = "clean",
              models = "randomForest", 
              ml_task="cont", 
              parallel = F) 
  
  study_scenarios$pred[row] <- result$`predictions for case`$perc_frauded
  study_scenarios$sd[row] <- result$`predictions for case`$`sd(yhat-y)`
  
  save(study_scenarios, file="study_scenarios.RData")
  
  }, error = function(e){
    
   
    
  }) # end tryCatch
  
}







