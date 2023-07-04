#' ------------------------------------
#  1. define gen_data -----------------
#' function for synthetic data creation
#' ------------------------------------

  # define function 
  gen_data <- function(n_entities = 1000, eligible = rep(seq(501, 1000, 1), 2), 
                       fraud_type = "clean", fraud_incA = 0, fraud_extA = 0, 
                       fraud_incB = 0, fraud_extB = 0, fraud_expo = 1.5, 
                       fraud_roundA = F, share_roundA = NA, 
                       fraud_roundB = F, share_roundB = NA, 
                       agg_factor = 1, n_elections = 100, 
                       data_type = "full", nuisance = 0.05, 
                       turnout_emp=NA, shareA_emp=NA, 
                       turnout=NA, shareA=NA, optimize_only = F) {  
    
    tryCatch({
      
    # n_entities = number of entities to create data for
    # eligible = number of eligible voters per entitiy
    # fraud_type = type of fraud (clean, bbs, stealing, switching), can be multiple
    # fraud_incA = share of n_entities with incremental fraud for partyA
    # fraud_extA = share of n_entities with extreme fraud for partyA
    # fraud_incB = share of n_entities with incremental fraud for partyB
    # fraud_extB = share of n_entities with extreme fraud for partyB
    # fraud_round = T/F, indicates whether vote shares should be rounded up for partyA/B
    # share_round = share of entities that should be rounded up
    # agg_factor = aggregation factor, n_entities/agg_factor is the number
    #              of entities data is aggregated towards
    #              no aggregation for agg_factor = 1
    # n_elections = number of elections to generate 
    # data_type = is full data frame across n_entities stored or only numerical characteristics (data_type = "num_char")
    # nuisance = nuisance parameter for rnorm() to be applied id data_type=="num_char", provides variability to summary statistics
    # turnout_emp = empirical turnout vector
    # shareA_emp = empirical shareA vector (winner's vote shares)
    # turnout = turnout vector that was optimized for 
    # shareA = shareA vector that was optimized for
    # optimize_only = should the function only be executed to construct turnout/shareA vector?
    shareB_emp <- 1 - shareA_emp
    
    
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
        
        print("generating synthetic data: optimization done.")
        
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
      
          n_fraudedA <- n_fraudedB <- 0
          
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
                  n_fraudedA <- n_fraudedA + sum(moved_votes)
                }           
                    
                # vote stealing
                if (str_detect(paste(fraud_type, collapse = " "), "stealing")) {
                  share_moved <- abs(rnorm(length(fraud_ids), fraud_incA^fraud_expo, fraud_width))
                  moved_votes <- as.integer(votes_b[fraud_ids] * share_moved)
                  votes_b[fraud_ids] <- votes_b[fraud_ids] - moved_votes 
                  non_voters <- eligible - votes_a - votes_b
                  n_fraudedA <- n_fraudedA + sum(moved_votes)
                }
                # vote switching
                if (str_detect(paste(fraud_type, collapse = " "), "switching")) { 
                  share_moved <- abs(rnorm(length(fraud_ids), fraud_incA^fraud_expo, fraud_width))
                  moved_votes <- as.integer(votes_b[fraud_ids] * share_moved)
                  votes_b[fraud_ids] <- votes_b[fraud_ids] - moved_votes
                  votes_a[fraud_ids] <- votes_a[fraud_ids] + moved_votes
                  n_fraudedA <- n_fraudedA + sum(moved_votes)
                }
                
              } # end if (fraud_incA > 0)
              
              
              #' -----------------------------------
              # fraud in favor of partyB
              if (fraud_incB > 0) {
              #' -----------------------------------
                
                # only sample units with larger turnout values than median
                # the left part of distribution is left untouched
                # fraud_ids <- sample(which(turnout > summary(turnout)[2]), size = n_entities * fraud_incA)
                fraud_ids <- sample(1:n_entities, size = n_entities * fraud_incB)
                fraud_width <- sqrt(sd(shareB_emp[which(shareB_emp > summary(shareB_emp)[3])]))
                # fraud_width <- sqrt(sd(shareA[which(shareA>summary(shareA)[3] & shareA<summary(shareA)[5])]))
                
                # ballot box stuffing
                if (str_detect(paste(fraud_type, collapse = " "), "bbs")) {
                  share_moved <- abs(rnorm(length(fraud_ids), fraud_incB^fraud_expo, fraud_width))
                  moved_votes <- as.integer(non_voters[fraud_ids] * share_moved)
                  votes_b[fraud_ids] <- votes_b[fraud_ids] + moved_votes  
                  non_voters <- eligible - votes_a - votes_b
                  n_fraudedB <- n_fraudedB + sum(moved_votes)
                }           
                
                # vote stealing
                if (str_detect(paste(fraud_type, collapse = " "), "stealing")) {
                  share_moved <- abs(rnorm(length(fraud_ids), fraud_incB^fraud_expo, fraud_width))
                  moved_votes <- as.integer(votes_a[fraud_ids] * share_moved)
                  votes_a[fraud_ids] <- votes_a[fraud_ids] - moved_votes 
                  non_voters <- eligible - votes_a - votes_b
                  n_fraudedB <- n_fraudedB + sum(moved_votes)
                }
                # vote switching
                if (str_detect(paste(fraud_type, collapse = " "), "switching")) { 
                  share_moved <- abs(rnorm(length(fraud_ids), fraud_incB^fraud_expo, fraud_width))
                  moved_votes <- as.integer(votes_a[fraud_ids] * share_moved)
                  votes_a[fraud_ids] <- votes_a[fraud_ids] - moved_votes
                  votes_b[fraud_ids] <- votes_b[fraud_ids] + moved_votes
                  n_fraudedB <- n_fraudedB + sum(moved_votes)
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
                  n_fraudedA <- n_fraudedA + sum(moved_votes)
                }           
                
                # vote stealing
                if (str_detect(paste(fraud_type, collapse = " "), "stealing")) {
                  share_moved <- 1 - abs(rnorm(length(fraud_ids), fraud_extA^fraud_expo, fraud_width))
                  moved_votes <- as.integer(votes_b[fraud_ids] * share_moved)
                  votes_b[fraud_ids] <- votes_b[fraud_ids] - moved_votes  
                  non_voters <- eligible - votes_a - votes_b
                  n_fraudedA <- n_fraudedA + sum(moved_votes)
                }
                # vote switching
                if (str_detect(paste(fraud_type, collapse = " "), "switching")) { 
                  share_moved <- 1 - abs(rnorm(length(fraud_ids), fraud_extA^fraud_expo, fraud_width))
                  moved_votes <- as.integer(votes_b[fraud_ids] * share_moved)
                  votes_b[fraud_ids] <- votes_b[fraud_ids] - moved_votes
                  votes_a[fraud_ids] <- votes_a[fraud_ids] + moved_votes
                  n_fraudedA <- n_fraudedA + sum(moved_votes)
                }
                
              } # end if (fraud_incA > 0)
              
              
              #' -----------------------------------
              # fraud in favor of partyB
              if (fraud_extB > 0) {
              #' -----------------------------------
                
                # only sample units with larger turnout values than median
                # the left part of distribution is left untouched
                # fraud_ids <- sample(which(turnout > summary(turnout)[2]), size = n_entities * fraud_extA)
                fraud_ids <- sample(1:n_entities, size = n_entities * fraud_extB)
                fraud_width <- sqrt(sd(shareB_emp[which(shareB_emp > summary(shareB_emp)[5])]))
                # fraud_width <- sqrt(sd(shareA[which(shareA>summary(shareA)[5])]))
                
                # ballot box stuffing
                if (str_detect(paste(fraud_type, collapse = " "), "bbs")) {
                  share_moved <- 1 - abs(rnorm(length(fraud_ids), fraud_extB^fraud_expo, fraud_width))
                  moved_votes <- as.integer(non_voters[fraud_ids] * share_moved)
                  votes_b[fraud_ids] <- votes_b[fraud_ids] + moved_votes  
                  non_voters <- eligible - votes_a - votes_b
                  n_fraudedB <- n_fraudedB + sum(moved_votes)
                }           
                
                # vote stealing
                if (str_detect(paste(fraud_type, collapse = " "), "stealing")) {
                  share_moved <- 1 - abs(rnorm(length(fraud_ids), fraud_extB^fraud_expo, fraud_width))
                  moved_votes <- as.integer(votes_a[fraud_ids] * share_moved)
                  votes_a[fraud_ids] <- votes_a[fraud_ids] - moved_votes  
                  non_voters <- eligible - votes_a - votes_b
                  n_fraudedB <- n_fraudedB + sum(moved_votes)
                }
                # vote switching
                if (str_detect(paste(fraud_type, collapse = " "), "switching")) { 
                  share_moved <- 1 - abs(rnorm(length(fraud_ids), fraud_extB^fraud_expo, fraud_width))
                  moved_votes <- as.integer(votes_a[fraud_ids] * share_moved)
                  votes_a[fraud_ids] <- votes_a[fraud_ids] - moved_votes
                  votes_b[fraud_ids] <- votes_b[fraud_ids] + moved_votes
                  n_fraudedB <- n_fraudedB + sum(moved_votes)
                }
                
              } # end if (fraud_extB > 0)
              
            } # end if (fraud_extA > 0 | fraud_extB > 0)
          
          
            #' --------------------------------------------------
            # redefine variables that are affected by fraud
            #' --------------------------------------------------
            
            votes_all <- votes_a + votes_b 
            turnout <- votes_all / eligible
            turnout[which(turnout > 1)] <- 1
            shareA <- votes_a/votes_all
            shareA[which(shareA > 1)] <- 1
            shareB <- votes_b/votes_all
            shareB[which(shareB > 1)] <- 1
            non_voters <- eligible - votes_all
            n_fraudedAll <- n_fraudedA + n_fraudedB
            
            
            #' -----------------------------------------------------------
            # rounding votes in favor of partyA or partyB
            if ((fraud_roundA == T) & (share_roundA > 0)) {
            #' -----------------------------------------------------------
            
              # round in favor of partyA
              if (fraud_roundA) {  
                fraud_ids <- sample(which(shareA > 0.5 & shareA < 1 ), length(shareA) * share_roundA)
                shareA[fraud_ids] <- ceiling(shareA[fraud_ids] / 0.05) * 0.05 
                
                # redefine affected variables
                votes_a[fraud_ids] <- as.integer(votes_all[fraud_ids] * shareA[fraud_ids])
                votes_b <- votes_all - votes_a
                shareB[fraud_ids] <- 1-shareA[fraud_ids]
              }
              
              # round in favor of partyB
              if (fraud_roundB) {  
                fraud_ids <- sample(which(shareB > 0.5 & shareB < 1 ), length(shareB) * share_roundB)
                shareB[fraud_ids] <- ceiling(shareB[fraud_ids] / 0.05) * 0.05 
                
                # redefine affected variables
                votes_b[fraud_ids] <- as.integer(votes_all[fraud_ids] * shareB[fraud_ids])
                votes_a <- votes_all - votes_b
                shareA[fraud_ids] <- 1-shareB[fraud_ids]
              }
              
            } # end if (fraud_roundA | fraud_roundB)
          
          } # end if fraud_type != "clean"
          
          
          
        #' ----------------------------
        #  (iii) aggregate data -------
        #' ----------------------------
          
          #' --------------------------------------------------
          # redefine variables that are affected by fraud
          #' --------------------------------------------------
          
            votes_all <- votes_a + votes_b 
            turnout <- votes_all / eligible
            turnout[which(turnout > 1)] <- 1
            shareA <- votes_a/votes_all
            shareA[which(shareA > 1)] <- 1
            shareB <- votes_b/votes_all
            shareB[which(shareB > 1)] <- 1
            non_voters <- eligible - votes_all
          
          
          #' ------------------------------------
          # construct (aggregated) dataset
          #' ------------------------------------
            
            if (agg_factor == 1)  
              data <- as.data.frame(cbind(1:n_entities, eligible, votes_all, votes_a, votes_b, 
                                          non_voters, turnout, shareA, shareB))
            
            if (agg_factor > 1) {
              
              n_entities_agg <- n_entities / agg_factor
              agg_id <- sort(rep(1:n_entities_agg, agg_factor))
              data <- as.data.frame(cbind(1:n_entities, agg_id, eligible, votes_all, votes_a, votes_b, 
                                          non_voters, turnout, shareA, shareB))
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
              
              ifelse(fraud_type[1] == "clean",
                     data$perc_fraudedAll <- 0,
                     data$perc_fraudedAll <- n_fraudedAll / sum(votes_all)
                     )
              
              data_final <- data
              return(data_final)
              break
              # if full election data is requested, n_elections is forced to be 1
            } 
              
            if (data_type == "num_char") {
            
              data_char <- as.data.frame(matrix(NA, nrow=1, ncol=0))
              
              # fraud variables (y) 
              ifelse(fraud_type[1] == "clean",
                     data_char$fraud <- 0,
                     data_char$fraud <- n_fraudedAll)
              data_char$fraud[data_char$fraud > 0] <- 1
              data_char$fraud_type = fraud_type
              data_char$n_fraudedA <- n_fraudedA
              data_char$n_fraudedB <- n_fraudedB
              ifelse(fraud_type[1] == "clean",
                     data_char$n_fraudedAll <- 0,
                     data_char$n_fraudedAll <- n_fraudedAll)
              data_char$perc_fraudedA <- n_fraudedA / sum(votes_all)
              data_char$perc_fraudedB <- n_fraudedB / sum(votes_all)
              ifelse(fraud_type[1] == "clean",
                     data_char$perc_fraudedAll <- 0,
                     data_char$perc_fraudedAll <- n_fraudedAll / sum(votes_all))
              ifelse(fraud_type[1] == "clean",
                     data_char$fraud_incA <- 0,
                     data_char$fraud_incA <- fraud_incA)
              ifelse(fraud_type[1] == "clean",
                     data_char$fraud_extA <- 0,
                     data_char$fraud_extA <- fraud_extA)
              ifelse(fraud_type[1] == "clean",
                     data_char$fraud_incB <- 0,
                     data_char$fraud_incB <- fraud_incB)
              ifelse(fraud_type[1] == "clean",
                     data_char$fraud_extB <- 0,
                     data_char$fraud_extB <- fraud_extB)
              
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
          data_yX[,13:ncol(data_yX)] <- apply(data_yX[,13:ncol(data_yX)], MARGIN = 2, rnorm, n = n_elections, sd = nuisance)
          data_final <- data_yX  
            
    # return list of generated elections      
    return(data_final)
    
    }, error = function(e){
      
   
      
    }) # end tryCatch
        
  } # end function gen_data
