#' Probabilistic Detection of Election Fraud Using Machine Learning Algorithms
#' Reproduction Material
#' Lion Behrens
#' -----------------------------------------------------------------------------

#' install and load packages
packages <- c("MASS", "gaussDiff", "RColorBrewer", "truncnorm", "e1071", 
              "stringr", "dplyr", "readxl", "tikzDevice", "distantia", "doSNOW", 
              "caret", "glmnet", "class", "randomForest", "xgboost", "LaplacesDemon")

for (i in 1: length(packages)) {
  if (is.element(packages[i], installed.packages()[,1]) == FALSE) 
    install.packages(packages[i], dep = TRUE) 
}

lapply(packages, library, character.only = T)

#' source manual functions
source("_functions.R")


#' --------------------------------------
#  --- 1. synthetic data creation -------
#' --------------------------------------

  # define function 
  gen_data <- function(n_entities = 1000, eligible = rep(seq(501, 1000, 1), 2), 
                       turnout_emp=NA, shareA_emp=NA, fraud_type="clean", fraud_incA = 0, 
                       fraud_extA = 0, fraud_incB = 0, fraud_extB = 0, fraud_expo = 1.5,
                       agg_factor = 1, n_elections = 100, data_type = "full",
                       nuisance = 0.05, turnout=NA, shareA=NA, optimize_only = F) {  
    
    # n_entities = number of entities to create data for
    # eligible = number of eligible voters per entitiy
    # turnout mean = mean turnout distribution
    # turnout_sd = sd turnout distribution
    # partyA_mean = mean support party A across entities
    # partyA_sd = sd support party A across entities
    # partyB_mean = mean support party B across entities
    # partyB_sd = sd support party B across entities
    # fraud_type = type of fraud (clean, bbs, stealing, switching)
    # fraud_incA = share of n_entities with incremental fraud for partyA
    # fraud_extA = share of n_entities with extreme fraud for partyA
    # fraud_incB = share of n_entities with incremental fraud for partyB
    # fraud_extB = share of n_entities with extreme fraud for partyB
    # agg_factor = aggregation factor, n_entities/agg_factor is the number
    #              of entities data is aggregated towards
    #              no aggregation for agg_factor = 1
    # n_elections = number of elections to generate 
    # data_type = is full data frame across n_entities stored or only numerical characteristics (data_type = "num_char")
    # nuisance = nuisance parameter for rnorm() to be applied id data_type=="num_char"
    # turnout = turnout vector that was optimized for 
    # shareA = shareA vector that was optimized for
    # optimize_only = should the function only be executed to construct turnout/shareA vector?
   
    
      #' -------------------------------------------
      #  (i) optimize for shareA permutation -------
      if (is.na(turnout[1]) & is.na(shareA[1])) { 
      #' -------------------------------------------
     
        # optimize for shareA permutation such that it minimizes
        # the KL divergence between BL2_empirical and BL2_expected
     
        turnout_mean <- mean(turnout_emp[which(turnout_emp < summary(turnout_emp)[5])])
        turnout_sd <- sd(turnout_emp[which(turnout_emp < summary(turnout_emp)[5])])
        
        partyA_mean <- mean(shareA_emp[which(turnout_emp < summary(turnout_emp)[5])])
        partyA_sd <-  sd(shareA_emp[which(turnout_emp < summary(turnout_emp)[5])])
        
        turnout <- rtruncnorm(length(eligible), 0, 1, turnout_mean, turnout_sd)
        shareA <- rtruncnorm(length(eligible), 0, 1, partyA_mean, partyA_sd)
        
        n_perm <- 100
        shareA_shuffled <- matrix(NA, nrow=length(eligible), ncol=n_perm)
        KL_div <- rep(NA, n_perm)
        
        for (perm in 1:100) {
          
          # shuffle shareA vector
          shareA_shuffled[, perm] <- sample(shareA, length(shareA))
          
          # apply turnout, shareA-values to eligible vector
          votes_all <- eligible * turnout
          votes_a <- round(votes_all * shareA_shuffled[, perm], digits = 0)
          votes_b <- round(votes_all - votes_a, digits = 0)
          
          # calculate sum of KL divergences
          dis_emp2 <- table(extract_digit(votes_a, 2)) / length(votes_a)
          dis_exp2 <- benford_expected(2)
          
          dis_emplast <- table(extract_digit(votes_a, "last")) / length(votes_a)
          dis_explast <- benford_expected(3)
          
          KL_div[perm] <- LaplacesDemon::KLD(dis_emp2, dis_exp2)$mean.sum.KLD +
            LaplacesDemon::KLD(dis_emplast, dis_explast)$mean.sum.KLD
          
        } # end optimization
        
        print("optimization done.")
        ### could also minimize a multidimensional KL divergence? second and last digits?
        
        used_perm <- which.min(KL_div)
        shareA <- shareA_shuffled[, used_perm] 
       
      } # end if (is.na(turnout) & is.na(shareA))
    
        if (optimize_only) {
          out <- list(turnout, shareA)
          names(out) <- c("turnout", "shareA")
          return(out)
          break
        }
      
        
      #' -----------------------------------------------------------------
      # (ii) generate artifical data for parameter setting once ----------
      #' -----------------------------------------------------------------
       
        #'----------------------
        # clean election data
        #' ----------------------
        
          votes_all <- eligible * turnout
          votes_a <- round(votes_all * shareA, digits = 0)
          votes_b <- round(votes_all - votes_a, digits = 0)
          non_voters <- eligible - votes_all
      
          n_frauded <- 0
          
        #'----------------------------
        # manipulated data
        if (fraud_type[1] != "clean") {
        #' ---------------------------
          
            #' -------------------------------------
            # incremental fraud 
            if (fraud_incA > 0 | fraud_incB > 0) {
            #' -------------------------------------
            
              #' -----------------------------------
              # fraud in favor of partyA
              if (fraud_incA > 0) {
              #' -----------------------------------
              
                # only sample units with larger turnout values than median
                # the left part of distribution is left untouched
                # fraud_ids <- sample(which(turnout > summary(turnout)[2]), size = n_entities * fraud_incA)
                fraud_ids <- sample(1:n_entities, size = n_entities * fraud_incA)
                fraud_width <- sqrt(sd(shareA_emp[which(shareA_emp > summary(shareA_emp)[3])]))
                # fraud_width <- sqrt(sd(shareA[which(shareA>summary(shareA)[3] & shareA<summary(shareA)[5])]))
                
                # ballot box stuffing
                if (str_detect(paste(fraud_type, collapse = " "), "bbs")) {
                  share_moved <- abs(rnorm(length(fraud_ids), fraud_incA^fraud_expo, fraud_width))
                  moved_votes <- as.integer(non_voters[fraud_ids] * share_moved)
                  votes_a[fraud_ids] <- votes_a[fraud_ids] + moved_votes  
                  non_voters <- eligible - votes_a - votes_b
                  n_frauded <- n_frauded + sum(moved_votes)
                }           
                    
                # vote stealing
                if (str_detect(paste(fraud_type, collapse = " "), "stealing")) {
                  share_moved <- abs(rnorm(length(fraud_ids), fraud_incA^fraud_expo, fraud_width))
                  moved_votes <- as.integer(votes_b[fraud_ids] * share_moved)
                  votes_b[fraud_ids] <- votes_b[fraud_ids] - moved_votes 
                  non_voters <- eligible - votes_a - votes_b
                  n_frauded <- n_frauded + sum(moved_votes)
                }
                # vote switching
                if (str_detect(paste(fraud_type, collapse = " "), "switching")) { 
                  share_moved <- abs(rnorm(length(fraud_ids), fraud_incA^fraud_expo, fraud_width))
                  moved_votes <- as.integer(votes_b[fraud_ids] * share_moved)
                  votes_b[fraud_ids] <- votes_b[fraud_ids] - moved_votes
                  votes_a[fraud_ids] <- votes_a[fraud_ids] + moved_votes
                  n_frauded <- n_frauded + sum(moved_votes)
                }
                
              } # end if (fraud_incA > 0)
              
              
              #' -----------------------------------
              # fraud in favor of partyB
              if (fraud_incB > 0) {
                #' -----------------------------------
                
                fraud_ids <- sample(1:n_entities, size = n_entities * fraud_incB)
                
                # ballot box stuffing
                if (fraud_type == "bbs") {
                  share_moved <- abs(rnorm(length(fraud_ids), 0, sd(non_voters/eligible)))
                  moved_votes <- as.integer(non_voters[fraud_ids] * share_moved)
                  votes_b[fraud_ids] <- votes_b[fraud_ids] + moved_votes  
                  non_voters <- eligible - votes_a - votes_b
                  n_frauded <- n_frauded + sum(moved_votes)
                }           
                
                # vote stealing
                if (fraud_type == "stealing") {
                  share_moved <- abs(rnorm(length(fraud_ids), 0, sd(votes_a/eligible)))
                  moved_votes <- as.integer(votes_a[fraud_ids] * share_moved)
                  votes_a[fraud_ids] <- votes_a[fraud_ids] - moved_votes 
                  non_voters <- eligible - votes_a - votes_b
                  n_frauded <- n_frauded + sum(moved_votes)
                }
                # vote switching
                if (fraud_type == "switching") { 
                  share_moved <- abs(rnorm(length(fraud_ids), 0, sd(votes_a/eligible)))
                  moved_votes <- as.integer(votes_a[fraud_ids] * share_moved)
                  votes_a[fraud_ids] <- votes_a[fraud_ids] - moved_votes
                  votes_b[fraud_ids] <- votes_b[fraud_ids] + moved_votes
                  n_frauded <- n_frauded + sum(moved_votes)
                }
                
              } # end if (fraud_incB > 0)
            
            } # end if (fraud_incA > 0 | fraud_incB > 0) 
              
              
            #' -------------------------------------
            # extreme fraud 
            if (fraud_extA > 0 | fraud_extB > 0) {
            #' -------------------------------------
              
              #' -----------------------------------
              # fraud in favor of partyA
              if (fraud_extA > 0) {
              #' -----------------------------------
               
                # only sample units with larger turnout values than median
                # the left part of distribution is left untouched
                # fraud_ids <- sample(which(turnout > summary(turnout)[2]), size = n_entities * fraud_extA)
                fraud_ids <- sample(1:n_entities, size = n_entities * fraud_extA)
                fraud_width <- sqrt(sd(shareA_emp[which(shareA_emp > summary(shareA_emp)[5])]))
                # fraud_width <- sqrt(sd(shareA[which(shareA>summary(shareA)[5])]))
                
                # ballot box stuffing
                if (str_detect(paste(fraud_type, collapse = " "), "bbs")) {
                  share_moved <- 1 - abs(rnorm(length(fraud_ids), fraud_extA^fraud_expo, fraud_width))
                  moved_votes <- as.integer(non_voters[fraud_ids] * share_moved)
                  votes_a[fraud_ids] <- votes_a[fraud_ids] + moved_votes  
                  non_voters <- eligible - votes_a - votes_b
                  n_frauded <- n_frauded + sum(moved_votes)
                }           
                
                # vote stealing
                if (str_detect(paste(fraud_type, collapse = " "), "stealing")) {
                  share_moved <- 1 - abs(rnorm(length(fraud_ids), fraud_extA^fraud_expo, fraud_width))
                  moved_votes <- as.integer(votes_b[fraud_ids] * share_moved)
                  votes_b[fraud_ids] <- votes_b[fraud_ids] - moved_votes  
                  non_voters <- eligible - votes_a - votes_b
                  n_frauded <- n_frauded + sum(moved_votes)
                }
                # vote switching
                if (str_detect(paste(fraud_type, collapse = " "), "switching")) { 
                  share_moved <- 1 - abs(rnorm(length(fraud_ids), fraud_extA^fraud_expo, fraud_width))
                  moved_votes <- as.integer(votes_b[fraud_ids] * share_moved)
                  votes_b[fraud_ids] <- votes_b[fraud_ids] - moved_votes
                  votes_a[fraud_ids] <- votes_a[fraud_ids] + moved_votes
                  n_frauded <- n_frauded + sum(moved_votes)
                }
                
              } # end if (fraud_incA > 0)
              
              
              #' -----------------------------------
              # fraud in favor of partyB
              if (fraud_extB > 0) {
              #' -----------------------------------
                
                fraud_ids <- sample(1:n_entities, size = n_entities * fraud_incB)
                
                # ballot box stuffing
                if (fraud_type == "bbs") {
                  share_moved <- 1 - abs(rnorm(length(fraud_ids), 0, sd(non_voters/eligible)))
                  moved_votes <- as.integer(non_voters[fraud_ids] * share_moved)
                  votes_b[fraud_ids] <- votes_b[fraud_ids] + moved_votes  
                  non_voters <- eligible - votes_a - votes_b
                  n_frauded <- n_frauded + sum(moved_votes)
                }           
                
                # vote stealing
                if (fraud_type == "stealing") {
                  share_moved <- 1 - abs(rnorm(length(fraud_ids), 0, sd(votes_a/eligible)))
                  moved_votes <- as.integer(votes_a[fraud_ids] * share_moved)
                  votes_a[fraud_ids] <- votes_a[fraud_ids] - moved_votes  
                  non_voters <- eligible - votes_a - votes_b
                  n_frauded <- n_frauded + sum(moved_votes)
                }
                # vote switching
                if (fraud_type == "switching") { 
                  share_moved <- 1 - abs(rnorm(length(fraud_ids), 0, sd(votes_a/eligible)))
                  moved_votes <- as.integer(votes_a[fraud_ids] * share_moved)
                  votes_a[fraud_ids] <- votes_a[fraud_ids] - moved_votes
                  votes_b[fraud_ids] <- votes_b[fraud_ids] + moved_votes
                  n_frauded <- n_frauded + sum(moved_votes)
                }
                
              } # end if (fraud_extB > 0)
              
            } # end if (fraud_extA > 0 | fraud_extB > 0)
            
          } # end if fraud_type != "clean"
          
          
          
        #' ------------------------------------------------
        #  (iii) redefine variables, aggregate data -------
        #' ------------------------------------------------
          
          #' --------------------------------------------------
          # redefine variables that are affected by fraud
          #' --------------------------------------------------
            
          votes_all <- votes_a + votes_b 
          turnout <- votes_all / eligible
          turnout[which(turnout > 1)] <- 1
          non_voters <- eligible - votes_all
          
          
          #' ------------------------------------
          # construct (aggregated) dataset
          #' ------------------------------------
            
            if (agg_factor == 1)  
              data <- as.data.frame(cbind(1:n_entities, eligible, votes_all, votes_a, votes_b, 
                                          non_voters, turnout, votes_a/votes_all, votes_b/votes_all))
            
            if (agg_factor > 1) {
              
              n_entities_agg <- n_entities / agg_factor
              agg_id <- sort(rep(1:n_entities_agg, agg_factor))
              data <- as.data.frame(cbind(1:n_entities, agg_id, eligible, votes_all, votes_a, votes_b, 
                                          non_voters, turnout, votes_a/votes_all, votes_b/votes_all))
              data <- data %>% 
                group_by(agg_id) %>% 
                summarise(agg_id = mean(agg_id), 
                          eligible = sum(eligible), 
                          votes_all = sum(votes_all),
                          votes_a = sum(votes_a), 
                          votes_b = sum(votes_b), 
                          non_voters = sum(non_voters))
              data$turnout <- data$votes_all / data$eligible
              data$shareA <- data$votes_a / data$votes_all
              data$shareB <- data$votes_b / data$votes_all
              
            } 
             
          
          #' -----------------------------------------------------------
          # store whole raw (or aggregated) data across all entities
          #' -----------------------------------------------------------
         
            colnames(data) <- c("id", "eligible", "votes_total", "votes_a", "votes_b",  
                                "non_voters", "turnout", "shareA", "shareB")   
          
            if (data_type == "full") {
              if (n_elections > 1)
                print(str_c("n_elections = ", n_elections, " but full data is generated for only one election."))
              
              data_final <- data
              return(data_final)
              break
              # if full election data is requested, n_elections is forced to be 1
            } 
              
            if (data_type == "num_char") {
            
              data_char <- as.data.frame(matrix(NA, nrow=1, ncol=0))
              
              # fraud variables (y) 
              data_char$fraud <- n_frauded
              data_char$fraud[data_char$fraud > 0] <- 1
              data_char$fraud_type = fraud_type
              data_char$n_frauded <- n_frauded
              data_char$perc_frauded <- n_frauded / sum(votes_all)
              data_char$fraud_incA <- fraud_incA
              data_char$fraud_extA <- fraud_extA
              data_char$fraud_incB <- fraud_incB
              data_char$fraud_extB <- fraud_extB
              
              # numerical characteristics (X)
              data_charX <- gen_features(votes_a = data$votes_a, votes_b = data$votes_b, turnout = data$turnout, 
                                         share_A = data$shareA, share_B = data$shareB, eligible = data$eligible)
              
              data_yX <- cbind(data_char, data_charX)
     
            } # end if (data_type == "num_char")
            
            
        #' ---------------------------------------------------------------------
        #  (iv) replicate elections by n_elections, add nuisance parameter -----
        #' ---------------------------------------------------------------------
        
          # replicate election n_elections times
          data_yX <- rbind(data_yX, data_yX[rep(1, (n_elections-1)),])
        
          # add nuisance parameter
          data_yX[,9:29] <- apply(data_yX[,9:29], MARGIN = 2, rnorm, n = n_elections, sd = nuisance)
          data_final <- data_yX  
            
    # return list of generated elections      
    return(data_final)
    
        
  } # end function gen_data

  
  sim_elections <- gen_data(n_elections = 100, n_entities = 1000, fraud_type="bbs", 
                      fraud_incA = 00, fraud_extA = 0, fraud_incB = 0, fraud_extB = 0, 
                      agg_factor = 1, data_type="num_char", nuisance = 0.05)
 
    
        
#' ------------------------------------------------------------------
#  --- 2. comparison to theoretical/empirical characteristics -------
#' ------------------------------------------------------------------ 
  
  
  
    
  ### maybe there is also place for some figures simply plotting data
  ### with different fraud parameters like I did in the FLACSO presentation 
  
  ### would certainly be very helpful for me to get a feeling for how th e
  ### frauded data looks like
  
  
  
  
  #' ------------------------
  # 2.1 empirical data ------
  #' ------------------------
  
    # in general: 
    # entities with an electorate < 100 are excluded
    # entities with NAs are excluded
    # when turnout is higher than 1, is set to 1
  
    #' ---------------------------------------------
    # 2.1.0 Venezuela, recall referendum 2004 ------
    #' ---------------------------------------------
  
      #' ------------------------
      # empirical
      #' ------------------------
  
        ven04 <- read_excel("U:/PhD Electoral Fraud/Data/Venezuela/2004_referendum_revocatorio/referendum.xls",
                                skip = 34)
        # rrp_si = number of yes votes
        # rrp_no = number of no votes
        # rep200407 = number of eligible voters  
        ven04$votes_all <- ven04$rrp_si+ven04$rrp_no+ven04$rrp_nulo
        ven04$turnout <- ven04$votes_all / ven04$rep200407
        ven04$eligible <- ven04$rep200407
        ven04$share_si <- ven04$rrp_si / (ven04$rrp_si + ven04$rrp_no)
        ven04$share_no <- ven04$rrp_no / (ven04$rrp_si + ven04$rrp_no)
    
        # exclude units with an electorate < 100
        ven04 <- ven04[-which(ven04$rep200407 < 100),]
        
        # exclude units with NAs
        ven04 <- ven04[-which(is.na(ven04$share_no)),]
        
        # set turnout > 1 to 1
        ven04$turnout[which(ven04$turnout > 1)] <- 1
    
        
      #' ------------------------
      # synthetic
      #' ------------------------
    
        # find fraud_incA and fraud_extA that resembles data most closely
        opt_results <- gen_fraudvals(n_entities = nrow(ven04),
                                     eligible = ven04$eligible,
                                     turnout_mean = mean(ven04$turnout), 
                                     turnout_sd = sd(ven04$turnout), 
                                     partyA_mean = mean(ven04$share_no, na.rm=T), 
                                     partyA_sd = sd(ven04$share_no, na.rm=T), 
                                     partyB_mean = mean(ven04$share_si, na.rm=T), 
                                     partyB_sd = sd(ven04$share_si, na.rm=T),
                                     shareA = ven04$share_no, 
                                     turnout = ven04$turnout, 
                                     fraud_type = "bbs")
        
        opt_vectorsVEN <- gen_data(n_entities = nrow(ven04),
                                   eligible = ven04$eligible,
                                   turnout_mean = mean(ven04$turnout), 
                                   turnout_sd = sd(ven04$turnout), 
                                   partyA_mean = mean(ven04$share_no, na.rm=T), 
                                   partyA_sd = sd(ven04$share_no, na.rm=T), 
                                   partyB_mean = mean(ven04$share_si, na.rm=T), 
                                   partyB_sd = sd(ven04$share_si, na.rm=T),
                                   optimize_only = T)
        
        # generate synthetic data using these values
        ven04_syn <- gen_data(n_entities = nrow(ven04),
                              eligible = ven04$eligible,
                              turnout_mean = mean(ven04$turnout), 
                              turnout_sd = sd(ven04$turnout), 
                              partyA_mean = mean(ven04$share_no, na.rm=T), 
                              partyA_sd = sd(ven04$share_no, na.rm=T), 
                              partyB_mean = mean(ven04$share_si, na.rm=T), 
                              partyB_sd = sd(ven04$share_si, na.rm=T),
                              fraud_type = "bbs",
                              fraud_incA = opt_results$valsV[1, "fraud_incA"],
                              fraud_extA = opt_results$valsV[1, "fraud_extA"],
                              n_elections = 1, 
                              data_type = "full", 
                              turnout = opt_vectorsVEN$turnout, 
                              shareA = opt_vectorsVEN$shareA)
        
        
        
      #' -----------------------------------------------
      # 2.1.1 Russia, parliamentary election 2011 ------
      #' -----------------------------------------------
        
        #' ------------------------
        # empirical
        #' ------------------------
        
          ru11_1 <- read_excel("U:/PhD Electoral Fraud/Data/Russia2011_1of2.xls")
          ru11_2 <- read_excel("U:/PhD Electoral Fraud/Data/Russia2011_2of2.xls")
          ru11 <- rbind(ru11_1, ru11_2)
          ru11$votes_all <- ru11$`Number of valid ballots` + as.numeric(ru11$`Number of invalid ballots`)
          ru11$turnout <- ru11$votes_all / ru11$`Number of voters included in voters list` 
          ru11$eligible <- ru11$`Number of voters included in voters list` 
          ru11$ur <- ru11$`United Russia`
          ru11$share_ur <- ru11$ur / ru11$`Number of valid ballots`
          ru11$communist <- ru11$`Communist Party`
          ru11$share_communist <- ru11$communist / ru11$`Number of valid ballots`
         
          # exclude units with an electorate < 100
          ru11 <- ru11[-which(ru11$`Number of voters included in voters list` < 100),]
          
          # exclude units with NAs in share_ur
          ru11 <- ru11[-which(is.na(ru11$share_ur)),]
        
        
        #' ------------------------
        # synthetic
        #' ------------------------
        
          # generate synthetic data using these values
          opt_vectorsRU11 <- gen_data(n_entities = nrow(ru11),
                                    eligible = ru11$eligible,
                                    turnout_emp = ru11$turnout, 
                                    shareA_emp = ru11$share_ur,
                                    optimize_only = T)
          
          
          
        
        
        
        
      #' ----------------------------------------------
      # 2.1.2 Russia, presidential election 2012 ------
      #' ----------------------------------------------
      
        #' ------------------------
        # empirical
        #' ------------------------
        
          ru12_1 <- read_excel("U:/PhD Electoral Fraud/Data/Russia2012_1of2.xls")
          ru12_2 <- read_excel("U:/PhD Electoral Fraud/Data/Russia2012_2of2.xls")
          ru12 <- rbind(ru12_1, ru12_2)
          ru12$`Number of invalid ballots`[which(ru12$`Number of invalid ballots`=="A")] <- 0
          ru12$votes_all <- ru12$`Number of valid ballots` + as.numeric(ru12$`Number of invalid ballots`)
          ru12$turnout <- ru12$votes_all / ru12$`The number of voters included in voters list` 
          ru12$eligible <- ru12$`The number of voters included in voters list` 
          ru12$putin <- ru12$`Vladimir Putin`
          ru12$share_putin <- gsub("%", "", ru12$...31)
          ru12$share_putin <- as.numeric(ru12$share_putin) / 100
          ru12$zyuganov <- as.numeric(ru12$`Gennady Andreyevich Zyuganov`)
          ru12$share_zyuganov <- gsub("%", "", ru12$...25)
          ru12$share_zyuganov <- as.numeric(ru12$share_zyuganov) / 100
          
          # exclude units with an electorate < 100
          ru12 <- ru12[-which(ru12$`The number of voters included in voters list` < 100),]
          # exclude units with NAs in share_putin
          ru12 <- ru12[-which(is.na(ru12$share_putin)),]
      
          
        #' ------------------------
        # synthetic
        #' ------------------------
        
          # find fraud_incA and fraud_extA that resembles data most closely
          opt_resultsRU <- gen_fraudvals(n_entities = nrow(ru12),
                                       eligible = ru12$eligible,
                                       turnout_mean = mean(ru12$turnout), 
                                       turnout_sd = sd(ru12$turnout), 
                                       partyA_mean = mean(ru12$share_putin), 
                                       partyA_sd = sd(ru12$share_putin), 
                                       partyB_mean = mean(ru12$share_zyuganov), 
                                       partyB_sd = sd(ru12$share_zyuganov),
                                       shareA = ru12$share_putin, 
                                       turnout = ru12$turnout, 
                                       fraud_type = "bbs")
          
          # generate synthetic data using these values
          opt_vectorsRU12 <- gen_data(n_entities = nrow(ru12),
                                    eligible = ru12$eligible,
                                    turnout_emp = ru12$turnout, 
                                    shareA_emp = ru12$share_putin,
                               optimize_only = T)
          
          
          ru12_syn <- gen_data(n_entities = nrow(ru12),
                               eligible = ru12$eligible,
                               turnout_mean = mean(ru12$turnout), 
                               turnout_sd = sd(ru12$turnout), 
                               partyA_mean = mean(ru12$share_putin), 
                               partyA_sd = sd(ru12$share_putin), 
                               partyB_mean = mean(ru12$share_zyuganov), 
                               partyB_sd = sd(ru12$share_zyuganov),
                               fraud_type = "bbs",
                               fraud_incA = opt_results$valsV[1, "fraud_incA"],
                               fraud_extA = opt_results$valsV[1, "fraud_extA"],
                               n_elections = 1, 
                               data_type = "full")
          
          
      
      #' ----------------------------------------------
      # 2.1.3 Uganda, presidential election 2011 ------
      #' ----------------------------------------------
      
        #' ------------------------
        # empirical
        #' ------------------------
          
          uga11 <- read_excel("U:/PhD Electoral Fraud/Data/Uganda2011.xls", col_names = F)
          uga11$eligible <- uga11$...11
          
          uga11$bwanika <- as.numeric(unlist(uga11$...12))
          uga11$besigye <- as.numeric(unlist(uga11$...13))
          uga11$kamya <- as.numeric(unlist(uga11$...14))
          uga11$lubega <- as.numeric(unlist(uga11$...15))
          uga11$mao <- as.numeric(unlist(uga11$...16))
          uga11$otunnu <- as.numeric(unlist(uga11$...17))
          uga11$ssali <- as.numeric(unlist(uga11$...18))
          uga11$museveni <- as.numeric(unlist(uga11$...19))
          
          uga11$votes_all <- uga11$...20
          uga11$invalid <- as.numeric(unlist(uga11$...21))
          uga11$blanks <- as.numeric(unlist(uga11$...22))
         
          # nice check 
          # sum(c(uga11$bwanika, uga11$besigye, uga11$kamya, uga11$lubega, uga11$mao, uga11$otunnu, uga11$ssali, uga11$museveni), na.rm=T)
          # sum(uga11$votes_all, na.rm=T)
          
          uga11$turnout <- uga11$votes_all / uga11$eligible
          uga11$share_museveni <- uga11$museveni / uga11$votes_all
          uga11$share_besigye <- uga11$besigye / uga11$votes_all
          
          # exclude units with an electorate < 100
          uga11 <- uga11[-which(uga11$eligible < 100),]
          
          # exclude units with NAs in share_museveni
          uga11 <- uga11[-which(is.na(uga11$share_museveni)),]
        
           
        #' ------------------------
        # synthetic
        #' ------------------------
        
          # find fraud_incA and fraud_extA that resembles data most closely
          opt_results <- gen_fraudvals(n_entities = nrow(uga11),
                                       eligible = uga11$eligible,
                                       turnout_mean = median(uga11$turnout), 
                                       turnout_sd = sd(uga11$turnout), 
                                       partyA_mean = mean(uga11$share_museveni), 
                                       partyA_sd = sd(uga11$share_museveni), 
                                       partyB_mean = mean(uga11$share_besigye), 
                                       partyB_sd = sd(uga11$share_besigye),
                                       shareA = uga11$share_museveni, 
                                       turnout = uga11$turnout, 
                                       fraud_type = c("bbs", "switching"))
          
          # generate synthetic data using these values
          opt_vectorsUGA <- gen_data(n_entities = nrow(uga11),
                                     eligible = uga11$eligible,
                                     turnout_emp = uga11$turnout, 
                                     shareA_emp = uga11$share_museveni,
                                    optimize_only = T)
          
          uga11_syn <- gen_data(n_entities = nrow(uga11),
                               eligible = uga11$eligible,
                               turnout_mean = median(uga11$turnout), 
                               turnout_sd = sd(uga11$turnout), 
                               partyA_mean = mean(uga11$share_museveni), 
                               partyA_sd = sd(uga11$share_museveni), 
                               partyB_mean = mean(uga11$share_besigye), 
                               partyB_sd = sd(uga11$share_besigye),
                               fraud_type = c("bbs", "switching"),
                               fraud_incA = 0.35,
                               fraud_extA = 0.02,
                               n_elections = 1, 
                               data_type = "full", 
                               turnout = opt_results$`optimized vectors`$turnout, 
                               shareA = opt_results$`optimized vectors`$shareA)
          
          
      
      #' ------------------------------------------------
      # 2.1.4 Austria, parliamentary election 2008 ------
      #' ------------------------------------------------
      
        #' ------------------------
        # empirical
        #' ------------------------
        
          aus08 <- read_excel("U:/PhD Electoral Fraud/Data/Austria2008_adjusted.xls")
          aus08$turnout <- aus08$`Wahl-\nbeteil-\nigung\nin %` / 100
          aus08$eligible <- aus08$`Wahlbe- \nrechtigte`
          aus08$votes_all <- aus08$gültig
          aus08$share_spo <- aus08$`%...9`/ 100
          aus08$share_ovp <- aus08$`%...11`/ 100
          
          # exclude units with an electorate < 100
          aus08 <- aus08[-which(aus08[,3] < 100),]
      
        
        #' ------------------------
        # synthetic
        #' ------------------------
        
          # find fraud_incA and fraud_extA that resembles data most closely
          opt_results <- gen_fraudvals(n_entities = nrow(aus08),
                                       eligible = aus08$eligible,
                                       turnout_mean = mean(aus08$turnout), 
                                       turnout_sd = sd(aus08$turnout), 
                                       partyA_mean = mean(aus08$share_spo), 
                                       partyA_sd = sd(aus08$share_spo), 
                                       partyB_mean = mean(aus08$share_ovp), 
                                       partyB_sd = sd(aus08$share_ovp), 
                                       shareA = aus08$share_spo, 
                                       turnout = aus08$turnout, 
                                       fraud_type = "bbs")
          
          opt_vectorsAUS <- gen_data(n_entities = nrow(aus08),
                                     eligible = aus08$eligible,
                                     turnout_emp = aus08$turnout, 
                                     shareA_emp = aus08$share_spo,
                                     optimize_only = T)
            
          # generate synthetic data using these values
          aus08_syn <- gen_data(n_entities = nrow(aus08),
                                eligible = aus08$eligible,
                                turnout_mean = mean(aus08$turnout), 
                                turnout_sd = sd(aus08$turnout), 
                                partyA_mean = mean(aus08$share_spo), 
                                partyA_sd = sd(aus08$share_spo), 
                                partyB_mean = mean(aus08$share_ovp), 
                                partyB_sd = sd(aus08$share_ovp),
                                fraud_type = "bbs",
                                fraud_incA = opt_results$valsV[1, "fraud_incA"],
                                fraud_extA = opt_results$valsV[1, "fraud_extA"],
                                n_elections = 1, 
                                data_type = "full", 
                                turnout = opt_vectorsAUS$turnout, 
                                shareA = opt_vectorsAUS$shareA)
            
            
          
      #' ----------------------------------------------------
      # 2.1.5 Spain, European Parliament election 2019 ------
      #' ----------------------------------------------------
      
        #' ------------------------
        # empirical
        #' ------------------------
            
          esp19 <- read_excel("U:/PhD Electoral Fraud/Data/Spain_EP_2019.xlsx", skip = 5)
          esp19$turnout <- esp19$`Total votantes` / esp19$`Total censo electoral` 
          esp19$eligible <- esp19$`Total votantes`
          esp19$votes_all <- esp19$`Votos válidos`
          esp19$share_psoe <- esp19$PSOE / esp19$`Votos válidos`
          esp19$share_pp <- esp19$PP / esp19$`Votos válidos`
          
          # exclude units with an electorate < 100
          esp19 <- esp19[-which(esp19$`Total censo electoral` < 100),]
      
          
        #' ------------------------
        # synthetic
        #' ------------------------
        
          # find fraud_incA and fraud_extA that resembles data most closely
          opt_results <- gen_fraudvals(n_entities = nrow(esp19),
                                       eligible = esp19$eligible,
                                       turnout_mean = mean(esp19$turnout), 
                                       turnout_sd = sd(esp19$turnout), 
                                       partyA_mean = mean(esp19$share_psoe), 
                                       partyA_sd = sd(esp19$share_psoe), 
                                       partyB_mean = mean(esp19$share_pp), 
                                       partyB_sd = sd(esp19$share_pp),
                                       shareA = esp19$share_psoe, 
                                       turnout = esp19$turnout, 
                                       fraud_type = "bbs")
          
          # generate synthetic data using these values
          opt_vectorsESP <- gen_data(n_entities = nrow(esp19),
                                     eligible = esp19$eligible,
                                     turnout_emp = esp19$turnout, 
                                     shareA_emp = esp19$share_psoe,
                                     optimize_only = T)
          
          esp19_syn <- gen_data(n_entities = nrow(esp19),
                                eligible = esp19$eligible,
                                turnout_mean = mean(esp19$turnout), 
                                turnout_sd = sd(esp19$turnout), 
                                partyA_mean = mean(esp19$share_psoe), 
                                partyA_sd = sd(esp19$share_psoe), 
                                partyB_mean = mean(esp19$share_pp), 
                                partyB_sd = sd(esp19$share_pp),
                                fraud_type = "bbs",
                                fraud_incA = opt_results$valsV[1, "fraud_incA"],
                                fraud_extA = opt_results$valsV[1, "fraud_extA"],
                                n_elections = 1, 
                                data_type = "full", 
                                turnout = opt_vectorsESP$turnout, 
                                shareA = opt_vectorsESP$shareA)
          
          
      #' --------------------------------------------
      # 2.1.6 Finland, municipal election 2017 ------
      #' --------------------------------------------
  
        #' ------------------------
        # empirical
        #' ------------------------
          
          fin17 <- read_excel("U:/PhD Electoral Fraud/Data/Finland_municipal_2017.xlsx")
          
          fin17$`Persons entitled to vote` <- as.numeric(fin17$`Persons entitled to vote`)
          fin17$`Persons entitled to vote` <- as.numeric(gsub("[.]", "", fin17$`Persons entitled to vote`))
          
          fin17$`KOK Votes cast, total` <- as.numeric(fin17$`KOK Votes cast, total`)
          fin17$`KOK Votes cast, total` <- as.numeric(gsub("[.]", "", fin17$`KOK Votes cast, total`))
         
          fin17$`SDP Votes cast, total` <- as.numeric(fin17$`SDP Votes cast, total`)
          fin17$`SDP Votes cast, total` <- as.numeric(gsub("[.]", "", fin17$`SDP Votes cast, total`))
          
          fin17$votes_all <- as.numeric(fin17$`Persons who voted`)
          fin17$votes_all <- as.numeric(gsub("[.]", "", fin17$votes_all))
          
          fin17$turnout <- as.numeric(fin17$`Voting turnout`) / 100
          fin17$eligible <- as.numeric(fin17$`Persons entitled to vote`)
          fin17$kok_votes <- as.numeric(fin17$`KOK Votes cast, total`)
          fin17$sdp_votes <- as.numeric(fin17$`SDP Votes cast, total`)
          fin17$share_kok <- as.numeric(fin17$`KOK Proportion of all votes cast`) / 100
          fin17$share_sdp <- as.numeric(fin17$`SDP Proportion of all votes cast`) / 100
          
          # exclude units with NAs in share_kok
          fin17 <- fin17[-which(is.na(fin17$share_kok)),]
          
          
        #' ------------------------
        # synthetic
        #' ------------------------
     
          # find fraud_incA and fraud_extA that resembles data most closely
          opt_results <- gen_fraudvals(n_entities = nrow(fin17),
                                       eligible = fin17$eligible,
                                       turnout_mean = mean(fin17$turnout), 
                                       turnout_sd = sd(fin17$turnout), 
                                       partyA_mean = mean(fin17$share_kok), 
                                       partyA_sd = sd(fin17$share_kok), 
                                       partyB_mean = mean(fin17$share_sdp), 
                                       partyB_sd = sd(fin17$share_sdp),
                                       shareA = fin17$share_kok, 
                                       turnout = fin17$turnout, 
                                       fraud_type = "bbs")
          
          # generate synthetic data using these values
          opt_vectorsFIN <- gen_data(n_entities = nrow(fin17),
                                     eligible = fin17$eligible,
                                     turnout_emp = fin17$turnout, 
                                     shareA_emp = fin17$share_kok,
                                     optimize_only = T)
          
          fin17_syn <- gen_data(n_entities = nrow(fin17),
                                eligible = fin17$eligible,
                                turnout_mean = mean(fin17$turnout), 
                                turnout_sd = sd(fin17$turnout), 
                                partyA_mean = mean(fin17$share_kok), 
                                partyA_sd = sd(fin17$share_kok), 
                                partyB_mean = mean(fin17$share_sdp), 
                                partyB_sd = sd(fin17$share_sdp),
                                fraud_type = "bbs",
                                fraud_incA = opt_results$valsV[1, "fraud_incA"],
                                fraud_extA = opt_results$valsV[1, "fraud_extA"],
                                n_elections = 1, 
                                data_type = "full", 
                                turnout = opt_vectorsFIN$turnout, 
                                shareA = opt_vectorsFIN$shareA)
          
          
          
         
          
          
  #' ---------------------
  # 2.2 comparisons ------
  #' ---------------------     
    
    #' ------------------
    # 2.2.1 digits ------
    #' ------------------
    
      plot_digits_all(aus08_syn$votes_a, aus08_syn$votes_b)
      plot_digits_2last(aus08$SPÖ, aus08_syn)
      
      tikz('digit_comparisons.tex', standAlone = TRUE, width=9, height=6)
      
        par(mfrow = c(2, 3),     # 2x3 layout
            oma = c(2, 2, 0, 0), # two rows of text at the outer left and bottom margin
            mar = c(0, 2, 1, 0), # space for one row of text at ticks and to separate plots
            mgp = c(2, 1, 0),
            xpd = F)  
        
        # Russia 2012
        plot_digits_2last(ru12$putin, ru12_syn, title = "Russia 2012", ylab = "Relative Frequency",
                          y_axis = T, x_axis = F, y_labels = T)
        
        text(3.5, 0.13, "Second Digit", cex=1.5)
        text(2.1, 0.07, "Last Digit", cex=1.5)
        
        
        # Uganda 2011
        par(mar = c(0, 1, 1, 0))
        plot_digits_2last(uga11$museveni, uga11_syn, title = "Uganda 2011",
                          y_axis = F, x_axis = F, x_labels = T)
        
        # Venezuela 2004
        par(mar = c(0, 1, 1, 1))
        plot_digits_2last(ven04$rrp_no, ven04_syn, title = "Venezuela 2004",
                          y_axis = F, y_labels = F, x_axis = F, x_labels = F)
        
        # Austria 2008
        par(mar = c(2, 2, 1, 0))
        plot_digits_2last(aus08$SPÖ, aus08_syn, title = "Austria 2008", xlab = "Number",
                          ylab = "Relative Frequency", y_axis = T, y_labels = T, 
                          x_axis = T, x_labels = T)
        
        # Spain 2019
        par(mar = c(2, 1, 1, 0))
        plot_digits_2last(esp19$PSOE, esp19_syn, title = "Spain 2019", xlab = "Number",
                          y_axis = F, x_axis = T, x_labels = T)
        
        # Finland 2017
        par(mar = c(2, 1, 1, 1))
        plot_digits_2last(fin17$`KOK Votes cast, total`, fin17_syn, title = "Finland 2017", xlab = "Number",
                          y_axis = F, x_axis = T, x_labels = T)
        
        box("outer")
        
      
      dev.off()
      tools::texi2dvi('digit_comparisons.tex',pdf=T)
      system(paste(getOption('pdfviewer'),'digit_comparisons.pdf'))
      
      
      tikz('digit_comparisons_legend.tex', standAlone = TRUE, width=9, height=5)
      
        par(mfrow = c(1, 1))
        plot(1, type="n", axes = F, xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10))
        legend(5,8.5, c("First Digit Theoretical", "First Digit Empirical", "First Digit Synthetic", 
                      "Last Digit Theoretical", "Last Digit Empirical", "Last Digit Synthetic"), 
               col=c("black", "orange", "lightgrey", "black", "blue", "lightgrey"), 
               lwd=rep(2,6), lty=c(1,1,1,2,2,2), pch=c(16, 16, 16, 1, 1, 1), cex=2, bty="n")
      
      dev.off()
      tools::texi2dvi('digit_comparisons_legend.tex',pdf=T)
      system(paste(getOption('pdfviewer'),'digit_comparisons_legend.pdf'))
      
      
    #' ---------------------------------------------------------
    # 2.2.2 bivariate turnout and vote share distribution ------
    #' ---------------------------------------------------------
      
      tikz('scatter.tex', standAlone = T, width=7, height=7)
      
        par(mfrow = c(2, 2),     # 2x2 layout
            oma = c(2, 2, 0, 0), # two rows of text at the outer left and bottom margin
            mar = c(2.8, 2.8, 2.8, 1), # space for one row of text at ticks and to separate plots
            mgp = c(2, 1, 0),
            xpd = NA)  
        
        rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
        r <- rf(30)
        
        # empty image with baseline color
        x <- list()
        x$x <- seq(0,10,1)
        x$y <- seq(0,10,1)
        x$z <- matrix(rep(0,100), nrow=10, ncol=10)
        
          #' -----------------------
          # AUstria 2008
          #' -----------------------
        
            # empirical
            image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), ylab="\\% Votes for Winner", xaxt="n")    
            k <- kde2d(uga11$turnout, uga11$share_museveni, n=50)
            image(k, col=r, xlim=c(0,1), ylim=c(0,1), xaxt="n", add=T)
            text(0.23, 0.95, "Russia 2012, Empirical", col="white")
            
            # synthetic
            par(mar = c(2.8, 1.5, 2.8, 2.3))
            image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), yaxt="n", xaxt="n")    
            k <- kde2d(uga11_syn$turnout, uga11_syn$shareA, n=50)
            image(k, col=r, xlim=c(0,1), ylim=c(0,1), yaxt="n", xaxt="n", add=T)
            text(0.23, 0.95, "Russia 2012, Synthetic", col="white")
        
            
          #' -----------------------
          # Russia 2012
          #' -----------------------
            
            # empirical
            par(mar = c(2.8, 2.8, 1, 1))
            image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), ylab="\\%Votes for Winner", xlab="\\%Turnout")    
            k <- kde2d(ven04$turnout, ven04$share_no, n=50)
            image(k, col=r, xlim=c(0,1), ylim=c(0,1), add=T)
            text(0.23, 0.95, "Austria 2008, Empirical", col="white")
            
            # synthetic
            par(mar = c(2.8, 1.5, 1, 2.3))
            image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), yaxt="n", xlab="\\%Turnout")    
            k <- kde2d(ven04_syn[[1]]$turnout, ven04_syn[[1]]$shareA, n=50)
            image(k, col=r, xlim=c(0,1), ylim=c(0,1), yaxt="n", add=T)
            text(0.23, 0.95, "Austria 2008, Synthetic", col="white")
            
          
          box("outer")
        
        dev.off()
        tools::texi2dvi('scatter.tex',pdf=T)
        system(paste(getOption('pdfviewer'),'scatter.pdf'))
      
        
        
    #' ------------------------------------
    # 2.2.4 logarithmic turnout rate ------
    #' ------------------------------------
    
      par(mfrow=c(1,1))
      
      # what is the theoretical expectation here? is there one?
      
      log_turnout <- log(bbs$votes_total / (bbs$l_total - bbs$votes_total))
      log_turnoutR <- log_turnout - mean(log_turnout)
      d_log_turnoutR <- density(log_turnoutR)
      
      plot(d_log_turnoutR, lwd=2, xlab="tau - mean(tau)", main="Logarithmic Turnout Rate", col="darkgreen")  
    
    
    
      
#' ---------------------------------------------------------------------
#  --- 3. function to apply ML approach on single empirical case -------
#' --------------------------------------------------------------------- 

  ml_detect <- function(data = aus08, eligible = aus08$eligible, turnout_emp = aus08$turnout, 
                        votes_a = aus08$SPÖ, votes_b = aus08$ÖVP, shareA_emp = aus08$share_spo, 
                        shareB_emp = aus08$share_ovp, 
                        fraud_incA = seq(0.01, 0.50, 0.01), fraud_extA = seq(0.01, 0.1, 0.01),
                        fraud_types = c("bbs", "stealing", "switching"),
                        n_elections = 1, models = c("kNN", "regul_reg", "randomForest", "gradBoost"), 
                        seed=12345, parallel = T) {
    
    # data = data of empirical case 
    # eligible = vector of eligible voters across entities of empirical case
    # turnout = variable measuring turnout
    # partyA = variable measuring partyA_share
    # partyB = variable measuring partyB_share
    # fraud_incA/B = range of fraud_inc parameters to train on 
    # fraud_extA/B = range of fraud_ext parameters to train on 
    # fraud_types = fraud types to train on 
    # n_elections = number of artifical elections to create for each individual scenario
    # models = ML approaches to use for training
    
    
    
    ### so basically, I use the gen_data function here within this function 
    ### the input is just a single empirical dataset, out of which I extract the relevant
    ### parameters like turnout_mean, n_entities etc. 
    
    ### then behind the scenes, many artificial datasets are generated and frauded
    ### and models are trained. 
    ### output should summarize what was used for training and also the test errors
    ### that were yielded in the lab
    
    ### then trained models are applied to empirical dataset
    
    ### output matrix should then give the predictions (binary, mechanism, degree)
    ### for all used methods 
    
    set.seed(seed)
    
    if (class(data) != "data.frame") 
      stop("provided dataset is not a data.frame object")
    
    if (parallel) {
      cl <- makeCluster(parallel::detectCores(), type = "SOCK")
      registerDoSNOW(cl)
    }
   
    #' ------------------------------------
    #  (i) construct artifical data -------
    #' ------------------------------------
    
      # define fraud scenarios that models are trained on 
      fraud_scenarios <- as.data.frame(expand.grid(fraud_incA, fraud_extA, fraud_types))
      colnames(fraud_scenarios) <- c("fraud_incA", "fraud_extA", "type")
     
      # optimize for turnout, shareA
      opt_vecs <- gen_data(n_entities = nrow(data), 
                           eligible = eligible,
                           turnout_emp = turnout_emp, 
                           shareA_emp = shareA_emp,
                           optimize_only = T
                           )
      
      # generate n_elections artificial cases under each scenario in sim_scenarios
      Sys.time()
      for (scenario in 1:nrow(fraud_scenarios)) {
      
        output <- gen_data(n_entities = nrow(data), 
                           eligible = eligible,
                           fraud_type = fraud_scenarios[scenario, "type"],
                           fraud_incA = fraud_scenarios[scenario, "fraud_incA"],
                           fraud_extA = fraud_scenarios[scenario, "fraud_extA"],
                           fraud_incB = 0,
                           fraud_extB = 0,
                           fraud_expo = 1.5,
                           agg_factor = 1, 
                           n_elections = n_elections,
                           data_type = "num_char", 
                           shareA_emp = shareA_emp,
                           turnout = opt_vecs$turnout, 
                           shareA = opt_vecs$shareA
                           )
       
        ifelse(scenario == 1,
               sim_elections <- output, 
               sim_elections <- rbind(sim_elections, output)
               )
        
        if (scenario %% 100 == 0) 
          print(str_c("scenario: ", scenario, " out of ", nrow(fraud_scenarios), ", ", Sys.time()))
        
        } # end for scenario in 1:nrow(scenarios)
    
      # add clean scenarios
      # set up in a way that 50% of scenarios are clean, rest is frauded to different degrees
      clean_elections <- gen_data(n_entities = nrow(data), 
                                  eligible = eligible,
                                  fraud_type = "clean",
                                  agg_factor = 1, 
                                  n_elections = nrow(sim_elections),
                                  data_type = "num_char", 
                                  shareA_emp = shareA_emp,
                                  turnout = opt_vecs$turnout, 
                                  shareA = opt_vecs$shareA
      )
      
      sim_elections <- rbind(sim_elections, clean_elections)
      
    #' ----------------------------
    #  (ii) train ML models -------
    #' ----------------------------
      
        # split data into training set (used for cross-validation) and final test set (untouched)
        # stratify over y-proportions (mechanisms), 80/20 split
        train_id <- createDataPartition(sim_elections$fraud_type,
                                        times = 1,
                                        p = 0.8,
                                        list = FALSE)
        train <- sim_elections[train_id,]
        test <- sim_elections[-train_id,]
        
        # construct empty objects to store results
        test_errors <- as.data.frame(matrix(NA, nrow = 5, ncol = 3))
        rownames(test_errors) <- c("kNN", "ridge", "lasso", "randomForest", "gradBoost")
        colnames(test_errors) <- c("binary", "categorical", "RMSE")
        
        # construct blueprint confusion matrices
        confusion_binary <- list()
        confusion_cat <- list()
        
        # predictions on empirical case from trained models
        predictions <- as.data.frame(matrix(NA, nrow=5, ncol=9))
        rownames(predictions) <- c("kNN", "ridge", "lasso", "randomForest", "gradBoost")
        colnames(predictions) <- c("binary", "p(fraud|X)", "categorical", "p(bbs|X)", "p(stealing|X)", 
                                   "p(switching|X)", "p(clean|X)", "perc_frauded", "sd(yhat-y)")
        X_emp <- gen_features(votes_a, votes_b, turnout_emp, shareA_emp, shareB_emp, eligible)
        
        # define variables
        X_train <- model.matrix(fraud ~., train[,-c(2:8)])[,-1]
        X_test <- model.matrix(fraud ~., test[,-c(2:8)])[,-1]
        
        y_train_binary <- as.factor(train$fraud)
        y_test_binary <- as.factor(test$fraud)
        y_train_cat <- as.factor(train$fraud_type)
        y_test_cat <- as.factor(test$fraud_type)
        y_train_cont <- train$perc_frauded
        y_test_cont <- test$perc_frauded
        
        # define trControl settings that are agnostic to the specific method
        tr_settings <- trainControl(method = "cv", 
                                    number = 5, 
                                    repeats=NA, 
                                    search="grid")
        
        
          #' ------------------------
          # kNN
          if(is.element("kNN", models)) {
          #' ------------------------
        
            # binary 
            binary_knn <- train(
              x=X_train, y=y_train_binary, method="knn", metric="Accuracy", 
              trControl = tr_settings, 
              tuneGrid = expand.grid(k=3:20)
            )
            
            preds_knnP <- binary_knn %>% predict(X_test, type="prob")
            preds_knn <- apply(preds_knnP, 1, which.max)-1
            
            test_errors["kNN", "binary"] <- length(which(y_test_binary != preds_knn)) / length(y_test_binary)
            confusion_binary[["knn"]] <- confusionMatrix(data = as.factor(preds_knn), reference = y_test_binary)
            
            predictions["kNN", "p(fraud|X)"] <- (binary_knn %>% predict(X_emp, type="prob"))[,2]
            predictions["kNN", "binary"] <- ifelse(predictions["kNN", "p(fraud|X)"] > 0.5, "fraud", "clean")
            
            
            # categorical 
            cat_knn <- train(
              x=X_train, y=y_train_cat, method="knn", metric="Accuracy", 
              trControl = tr_settings, 
              tuneGrid = expand.grid(k=3:20)
            )
            
            preds_knnP <- cat_knn %>% predict(X_test, type="prob")
            preds_knn <- colnames(preds_knnP)[apply(preds_knnP, 1, which.max)]
            
            test_errors["kNN", "categorical"] <- length(which(y_test_cat != preds_knn)) / length(y_test_cat)
            confusion_cat[["knn"]] <- confusionMatrix(data = factor(preds_knn, levels=levels(y_test_cat)), reference = y_test_cat)
            
            predictions["kNN", "categorical"] <- names(which.max((cat_knn %>% predict(X_emp, type="prob"))))
            predictions["kNN", 4:7] <- cat_knn %>% predict(X_emp, type="prob")
            
            
            # continuous 
            cont_knn <- train(
              x=X_train, y=y_train_cont, method="knn", metric="RMSE", 
              trControl = tr_settings, 
              tuneGrid = expand.grid(k=3:20)
            )
            
            preds_knn <- cont_knn %>% predict(X_test)
            test_errors["kNN", "RMSE"] <-  RMSE(preds_knn, y_test_cont)
            
            predictions["kNN", "perc_frauded"] <- cont_knn %>% predict(X_emp)
            predictions["kNN", "sd(yhat-y)"] <- sd(preds_knn - y_test_cont)
            
          } # end kNN
        
        
          #' ------------------------
          # ridge/lasso regression 
          if(is.element("regul_reg", models)) {
          #' ------------------------
          
            # binary
            binary_regul <- train(
              x=X_train, y=y_train_binary, method="glmnet", metric="Accuracy",
              trControl = tr_settings, 
              tuneGrid = expand.grid(alpha=c(0,1), lambda=10^seq(3, -3, length=100)) 
            )
            binary_ridge <- update(binary_regul, 
                                   param=list(alpha=0, lambda=binary_regul$results$lambda[which(binary_regul$results$Accuracy[binary_regul$results$alpha==0] == max(binary_regul$results$Accuracy[binary_regul$results$alpha==0]))[1]]))
            binary_lasso <- update(binary_regul, 
                                   param=list(alpha=1, lambda=binary_regul$results$lambda[which(binary_regul$results$Accuracy[binary_regul$results$alpha==1] == max(binary_regul$results$Accuracy[binary_regul$results$alpha==1]))[1]]))
            
            preds_ridgeP <- binary_ridge %>% predict(X_test, type="prob")
            preds_ridge <- apply(preds_ridgeP, 1, which.max)-1
            
            preds_lassoP <- binary_lasso %>% predict(X_test, type="prob")
            preds_lasso <- apply(preds_lassoP, 1, which.max)-1
            
            test_errors["ridge", "binary"] <- length(which(y_test_binary != preds_ridge)) / length(y_test_binary)
            test_errors["lasso", "binary"] <- length(which(y_test_binary != preds_lasso)) / length(y_test_binary)
            
            confusion_binary[["ridge"]] <- confusionMatrix(data = as.factor(preds_ridge), reference = y_test_binary)
            confusion_binary[["lasso"]] <- confusionMatrix(data = as.factor(preds_lasso), reference = y_test_binary)
        
            predictions["ridge", "p(fraud|X)"] <- (binary_ridge %>% predict(X_emp, type="prob"))[,2]
            predictions["ridge", "binary"] <- ifelse(predictions["ridge", "p(fraud|X)"] > 0.5, "fraud", "clean")
            
            predictions["lasso", "p(fraud|X)"] <- (binary_lasso %>% predict(X_emp, type="prob"))[,2]
            predictions["lasso", "binary"] <- ifelse(predictions["lasso", "p(fraud|X)"] > 0.5, "fraud", "clean")
            
            
            # categorical
            cat_regul <- train(
              x=X_train, y=y_train_cat, method="glmnet", metric="Accuracy",
              trControl = tr_settings, 
              tuneGrid=expand.grid(alpha=c(0,1), lambda=10^seq(3, -3, length=100)) 
            )
            cat_ridge <- update(cat_regul, 
                                   param=list(alpha=0, lambda=cat_regul$results$lambda[which(cat_regul$results$Accuracy[cat_regul$results$alpha==0] == max(cat_regul$results$Accuracy[cat_regul$results$alpha==0]))[1]]))
            cat_lasso <- update(cat_regul, 
                                   param=list(alpha=1, lambda=cat_regul$results$lambda[which(cat_regul$results$Accuracy[cat_regul$results$alpha==1] == max(cat_regul$results$Accuracy[cat_regul$results$alpha==1]))[1]]))
            
            preds_ridgeP <- cat_ridge %>% predict(X_test, type="prob")
            preds_ridge <- colnames(preds_ridgeP)[apply(preds_ridgeP, 1, which.max)]
            
            preds_lassoP <- cat_lasso %>% predict(X_test, type="prob")
            preds_lasso <- colnames(preds_lassoP)[apply(preds_lassoP, 1, which.max)]
            
            test_errors["ridge", "categorical"] <- length(which(y_test_cat != preds_ridge)) / length(y_test_cat)
            test_errors["lasso", "categorical"] <- length(which(y_test_cat != preds_lasso)) / length(y_test_cat)
            
            confusion_cat[["ridge"]] <- confusionMatrix(data = as.factor(preds_ridge), reference = y_test_cat)
            confusion_cat[["lasso"]] <- confusionMatrix(data = as.factor(preds_lasso), reference = y_test_cat)
            
            predictions["ridge", "categorical"] <- names(which.max((cat_ridge %>% predict(X_emp, type="prob"))))
            predictions["ridge", 4:7] <- cat_ridge %>% predict(X_emp, type="prob")
            
            predictions["lasso", "categorical"] <- names(which.max((cat_lasso %>% predict(X_emp, type="prob"))))
            predictions["lasso", 4:7] <- cat_lasso %>% predict(X_emp, type="prob")
            
            
            # continuous
            cont_regul <- train(
              x=X_train, y=y_train_cont, method="glmnet", metric="RMSE",
              trControl = tr_settings, 
              tuneGrid=expand.grid(alpha=c(0,1), lambda=10^seq(3, -3, length=100)) 
            )
            cont_ridge <- update(cont_regul, 
                                param=list(alpha=0, lambda=cont_regul$results$lambda[which(cont_regul$results$RMSE[cont_regul$results$alpha==0] == min(cont_regul$results$RMSE[cont_regul$results$alpha==0]))[1]]))
            cont_lasso <- update(cont_regul, 
                                param=list(alpha=1, lambda=cont_regul$results$lambda[which(cont_regul$results$RMSE[cont_regul$results$alpha==1] == min(cont_regul$results$RMSE[cont_regul$results$alpha==1]))[1]]))

            preds_ridge <- cont_ridge %>% predict(X_test)
            preds_lasso <- cont_lasso %>% predict(X_test)
            
            test_errors["ridge", "RMSE"] <- RMSE(preds_ridge, y_test_cont)
            test_errors["lasso", "RMSE"] <- RMSE(preds_lasso, y_test_cont)
            
            predictions["ridge", "perc_frauded"] <- cont_ridge %>% predict(X_emp)
            predictions["ridge", "sd(yhat-y)"] <- sd(preds_ridge - y_test_cont)
          
            predictions["lasso", "perc_frauded"] <- cont_lasso %>% predict(X_emp)
            predictions["lasso", "sd(yhat-y)"] <- sd(preds_lasso - y_test_cont)
            
          } # end regul_reg
            
        
          #' ------------------------
          # random Forest
          if(is.element("randomForest", models)) {
          #' ------------------------
          
            # binary 
            binary_rf <- train(
              x=X_train, y=y_train_binary, method="rf", metric="Accuracy", 
              trControl = tr_settings, 
              tuneGrid = expand.grid(mtry=c(3:20))
            )
            
            preds_rfP <- binary_rf %>% predict(X_test, type="prob")
            preds_rf <- apply(preds_rfP, 1, which.max)-1
            
            test_errors["randomForest", "binary"] <- length(which(y_test_binary != preds_rf)) / length(y_test_binary)
            confusion_binary[["randomForest"]] <- confusionMatrix(data = as.factor(preds_rf), reference = y_test_binary)
            
            predictions["randomForest", "p(fraud|X)"] <- (binary_rf %>% predict(X_emp, type="prob"))[,2]
            predictions["randomForest", "binary"] <- ifelse(predictions["randomForest", "p(fraud|X)"] > 0.5, "fraud", "clean")
            
            
            # categorical 
            cat_rf <- train(
              x=X_train, y=y_train_cat, method="rf", metric="Accuracy", 
              trControl = tr_settings, 
              tuneGrid = expand.grid(mtry=c(3:20))
            )
            
            preds_rfP <- cat_rf %>% predict(X_test, type="prob")
            preds_rf <- colnames(preds_rfP)[apply(preds_rfP, 1, which.max)]
            
            test_errors["randomForest", "categorical"] <- length(which(y_test_cat != preds_rf)) / length(y_test_cat)
            confusion_cat[["randomForest"]] <- confusionMatrix(data = as.factor(preds_rf), reference = y_test_cat)
            
            predictions["randomForest", "categorical"] <- names(which.max((cat_rf %>% predict(X_emp, type="prob"))))
            predictions["randomForest", 4:7] <- cat_rf %>% predict(X_emp, type="prob")
            
            
            # continuous 
            cont_rf <- train(
              x=X_train, y=y_train_cont, method="rf", metric="RMSE", 
              trControl = tr_settings, 
              tuneGrid = expand.grid(mtry=c(3:20))
            )
            
            preds_rf <- cont_rf %>% predict(X_test)
            test_errors["randomForest", "RMSE"] <- RMSE(preds_rf, y_test_cont)
           
            predictions["randomForest", "perc_frauded"] <- cont_rf %>% predict(X_emp)
            predictions["randomForest", "sd(yhat-y)"] <- sd(preds_rf - y_test_cont)
            
          } # end randomForest
            
        
          #' ------------------------------------
          # gradient boosting
          if(is.element("gradBoost", models)) {
          #' ------------------------------------
          
            # binary 
            binary_boost <- train(
              x=X_train, y=y_train_binary, method="xgbTree", metric="Accuracy", 
              trControl = tr_settings, 
              tuneGrid = expand.grid(eta = c(0.05, 0.075, 0.1),
                                     nrounds = c(50, 75, 100),
                                     max_depth = 6:8,
                                     min_child_weight = 2.2,
                                     colsample_bytree = 0.4,
                                     gamma = 0,
                                     subsample = 1)
            )
            preds_boostP <- binary_boost %>% predict(X_test, type="prob")
            preds_boost <-  apply(preds_boostP, 1, which.max)-1
            
            test_errors["gradBoost", "binary"] <- length(which(y_test_binary != preds_boost)) / length(y_test_binary)
            confusion_binary[["gradBoost"]] <- confusionMatrix(data = as.factor(preds_boost), reference = y_test_binary)
            
            predictions["gradBoost", "p(fraud|X)"] <- (binary_boost %>% predict(X_emp, type="prob"))[,2]
            predictions["gradBoost", "binary"] <- ifelse(predictions["gradBoost", "p(fraud|X)"] > 0.5, "fraud", "clean")
            
            
            # categorical 
            cat_boost <- train(
              x=X_train, y=y_train_cat, method="xgbTree", metric="Accuracy", 
              trControl = tr_settings, 
              tuneGrid = expand.grid(eta = c(0.05, 0.075, 0.1),
                                     nrounds = c(50, 75, 100),
                                     max_depth = 6:8,
                                     min_child_weight = 2.2,
                                     colsample_bytree = 0.4,
                                     gamma = 0,
                                     subsample = 1)
            )
            preds_boostP <- cat_boost %>% predict(X_test, type="prob")
            preds_boost <- colnames(preds_boostP)[apply(preds_boostP, 1, which.max)]
            
            test_errors["gradBoost", "categorical"] <- length(which(y_test_cat != preds_boost)) / length(y_test_cat)
            confusion_cat[["gradBoost"]] <- confusionMatrix(data = as.factor(preds_boost), reference = y_test_cat)
            
            predictions["gradBoost", "categorical"] <- names(which.max((cat_boost %>% predict(X_emp, type="prob"))))
            predictions["gradBoost", 4:7] <- cat_boost %>% predict(X_emp, type="prob")
            
            
            # continuous 
            cont_boost <- train(
              x=X_train, y=y_train_cont, method="xgbTree", metric="RMSE", 
              trControl = tr_settings, 
              tuneGrid = expand.grid(eta = c(0.05, 0.075, 0.1),
                                     nrounds = c(50, 75, 100),
                                     max_depth = 6:8,
                                     min_child_weight = 2.2,
                                     colsample_bytree = 0.4,
                                     gamma = 0,
                                     subsample = 1)
            )
            
            preds_boost <- cont_boost %>% predict(X_test)
            test_errors["gradBoost", "RMSE"] <- RMSE(preds_boost, y_test_cont)
            
            predictions["gradBoost", "perc_frauded"] <- cont_boost %>% predict(X_emp)
            predictions["gradBoost", "sd(yhat-y)"] <- sd(preds_boost - y_test_cont)
            
          } # end gradBoost
        
        # end parallelization 
        if(parallel)
          stopCluster(cl) 
        
        #' -------------------------------
        #  (iii) save final models -------
        #' -------------------------------
        
          final_models <- list()
        
          final_models[["kNN"]][["binary"]] <- binary_knn
          final_models[["kNN"]][["categorical"]] <- cat_knn
          final_models[["kNN"]][["continuous"]] <- cont_knn
        
          final_models[["ridge"]][["binary"]] <- binary_ridge
          final_models[["ridge"]][["categorical"]] <- cat_ridge
          final_models[["ridge"]][["continuous"]] <- cont_ridge
          
          final_models[["lasso"]][["binary"]] <- binary_lasso
          final_models[["lasso"]][["categorical"]] <- cat_lasso
          final_models[["lasso"]][["continuous"]] <- cont_lasso
          
          final_models[["randomForest"]][["binary"]] <- binary_rf
          final_models[["randomForest"]][["categorical"]] <- cat_rf
          final_models[["randomForest"]][["continuous"]] <- cont_rf
          
          final_models[["gradBoost"]][["binary"]] <- binary_boost
          final_models[["gradBoost"]][["categorical"]] <- cat_boost
          final_models[["gradBoost"]][["continuous"]] <- cont_boost
        
        
        #' ---------------------------
        #  (iv) output objects -------
        #' ---------------------------
        
          if(is.element("regul_reg", models)) {
            models <- models[-which(models=="regul_reg")]
            models <- c(models, "ridge", "lasso")
          }  
          
          predictions <- predictions[models,]
        
          out_list <- list("performance in the laboratory setting, simulated data" = test_errors, 
                           "predictions for case" = predictions, 
                           "final models" = final_models)
          return(out_list)
          
  }
  
  

    


