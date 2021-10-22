#' Probabilistic Detection of Election Fraud Using Machine Learning Algorithms
#' Reproduction Material
#' Lion Behrens
#' -----------------------------------------------------------------------------

#' install and load packages
packages <- c("MASS", "gaussDiff", "RColorBrewer", "truncnorm", "e1071", 
              "stringr", "dplyr", "readxl", "tikzDevice", "distantia")

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
                       turnout_mean = 0.7, turnout_sd = 0.1, 
                       partyA_mean = 0.6, partyA_sd = 0.1, partyB_mean = 0.4,    
                       partyB_sd = 0.1, fraud_type="clean", fraud_incA = 0, 
                       fraud_extA = 0, fraud_incB = 0, fraud_extB = 0, 
                       agg_factor = 1, n_elections = 1000, data_type = "full",
                       baseline_A = NA, baseline_B = NA) {  
    
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
    # baseline_A/B = baseline values to scale BF distribution. If not specified, function optimizes for these
  
   
    #' ----------------------------------------
    #  (i) optimize for baseline values -------
    #' ----------------------------------------
   
      # optimize for constants baseline_a and baseline_b such that 
      # they minimize distance to multivariate normal p(turnout, party share)
      val_combs <- expand.grid(seq(0,1000,10), 
                                 seq(0,1000,10))
      colnames(val_combs) <- c("baseline_a", "baseline_b")  
   
      if (is.na(baseline_A) & is.na(baseline_B)) {
        
        KL_vec <- rep(NA, nrow(val_combs))
      
        # optimize for constants baseline_a and baseline_b such that 
        # they minimize distance between simulated eligible-vector and 
        # empirical eligible-vector (euclidean distance)
        # ED_vec <- rep(NA, nrow(val_combs))
        
        for (val in 1:nrow(val_combs)) {
          
          # generate variable following Benford's law
          X_a <- 10 ^ runif(n_entities, 0, 1) 
          X_b <- 10 ^ runif(n_entities, 0, 1) 
          
          # constants for scaling
          baseline_a <- val_combs[val, "baseline_a"]
          baseline_b <- val_combs[val, "baseline_b"]
          
          # latent support rates
          l_support_a <- as.integer(baseline_a * X_a) 
          l_support_b <- as.integer(baseline_b * X_b)
          
          # construct vote totals from turnout distribution 
          turnout <- rtruncnorm(n_entities, 0, 1, turnout_mean, turnout_sd)
         
          l_total <- l_support_a + l_support_b # total latent support
          l_share_a <- l_support_a/(l_support_a+l_support_b)
          l_share_b <- 1-l_share_a
          
          votes_all <- l_total * turnout
          votes_a <- round(votes_all * l_share_a, digits = 0)
          votes_b <- round(votes_all * l_share_b, digits = 0)
          
          ### turnout, votes_a, votes_b are the final variables
          ### do they follow the characteristics that they should?
          
          ### basically: just perform my protocol for data construction 
          ### then look which baseline values minimize KL-distance between two 
          ### multivariate distributions p(turnout, shareA), where shareA
          ### is once defined via empirical values and once defined via 
          ### what is implied by baseline_a and baseline_b
          ### so I find the values of baseline_a and baseline_b that in the end
          ### resemble the empirical vote shares
          
          # define multivariate normal based on value combination chosen 
          vcov_val <- matrix(c(var(turnout), 
                               cov(turnout, l_share_a), 
                               cov(turnout, l_share_a), 
                               var(l_share_a)),
                             nrow=2, ncol=2)
                                
          mu_val <- c(mean(turnout), mean(l_share_a))
          
          # define multivariate normal based on empirical input values
          vcov_emp <- matrix(c(turnout_sd^2, 
                               0,
                               0,
                               partyA_sd^2),
                             nrow=2, ncol=2)
          mu_emp <- c(turnout_mean, partyA_mean)
          
          # calculate symmetric KL divergence
          KL_vec[val] <- normdiff(mu1 = mu_emp, sigma1 = vcov_emp,
                                  mu2 = mu_val, sigma2 = vcov_val,
                                  method="KL")
          
          # calculate euclidean distance 
          # ED_vec[val] <- distance(sort(l_total), sort(eligible), 
          #                        method="euclidean")
          
          if (val %% 1000 == 0)
            print(str_c("optimization progress: ", val, " out of ", nrow(val_combs)))
          
        } # end optimization
      } # end if (is.na(baseline_A) & is.na(baseline_B))
       
      # find value combination for baseline_a and baseline_b that 
      # minimizes summed up KL and ED distance
      # val_combs$KL_relpos <- val_combs$ED_relpos <- NA
      # for (val in 1:nrow(val_combs)) {
      #   if (length(which(sort(KL_vec) == KL_vec[val])) > 0)
      #     val_combs$KL_relpos[val] <- which(sort(KL_vec) == KL_vec[val])
      #   if (length(which(sort(ED_vec) == ED_vec[val])) > 0)
      #     val_combs$ED_relpos[val] <- which(sort(ED_vec) == ED_vec[val])
      # }
      # val_combs$dist_sum <- val_combs$KL_relpos + val_combs$ED_relpos
      # 
      # val_combs <- val_combs[-which(is.na(val_combs$dist_sum)),]
      # val_opt <- val_combs[which(val_combs$dist_sum == min(val_combs$dist_sum)),]
    
    ifelse(is.na(baseline_A) & is.na(baseline_B),
           val_opt <- val_combs[which(KL_vec == KL_vec[order(KL_vec)][1]),],
           {val_opt <- val_combs[1,]
           val_opt[1, "baseline_a"] <- baseline_A
           val_opt[1, "baseline_b"] <- baseline_B}
           )  
    
      # val_optED <- val_combs[which(ED_vec == ED_vec[order(ED_vec)][1]),]
      
   
    #' ----------------------------------------------
    # (ii) generate n=n_elections datasets ----------
    #' ----------------------------------------------
      
      data_list <- list() # empty list for generated election dataframes
      for (election in 1:n_elections) {
      
      #'----------------------
      # clean election data
      #' ----------------------
      
        # generate variable following Benford's law
        X_a <- 10 ^ runif(n_entities, 0, 1) 
        X_b <- 10 ^ runif(n_entities, 0, 1) 
        
        # constants for scaling
        baseline_a <- val_opt[1, "baseline_a"]
        baseline_b <- val_opt[1, "baseline_b"]
        
        # latent support rates
        l_support_a <- round(baseline_a * X_a, digits = 0) 
        l_support_b <- round(baseline_b * X_b, digits = 0)
        
        # construct vote totals from turnout distribution 
        turnout <- rtruncnorm(n_entities, 0, 1, turnout_mean, turnout_sd)
        
        l_total <- l_support_a + l_support_b # total latent support
        l_share_a <- l_support_a/(l_support_a+l_support_b)
        l_share_b <- 1-l_share_a
        
        votes_all <- round(l_total * turnout, digits = 0)
        votes_a <- round(votes_all * l_share_a, digits = 0)
        votes_b <- votes_all - votes_a
        non_voters <- l_total - votes_a - votes_b
    
        n_frauded <- 0
        
      #'----------------------------
      # manipulated data
      if (fraud_type != "clean") {
      #' ---------------------------
        
          #' -------------------------------------
          # incremental fraud 
          if (fraud_incA > 0 | fraud_incB > 0) {
          #' -------------------------------------
          
            #' -----------------------------------
            # fraud in favor of partyA
            if (fraud_incA > 0) {
            #' -----------------------------------
            
              fraud_ids <- sample(1:n_entities, size = n_entities * fraud_incA)
              
              # ballot box stuffing
              if (fraud_type == "bbs") {
                share_moved <- abs(rnorm(length(fraud_ids), 0, sd(non_voters/votes_all)))
                moved_votes <- as.integer(non_voters[fraud_ids] * share_moved)
                votes_a[fraud_ids] <- votes_a[fraud_ids] + moved_votes  
                non_voters <- l_total - votes_a - votes_b
                n_frauded <- n_frauded + sum(moved_votes)
              }           
                  
              # vote stealing
              if (fraud_type == "stealing") {
                share_moved <- abs(rnorm(length(fraud_ids), 0, sd(votes_b/votes_all)))
                moved_votes <- as.integer(votes_b[fraud_ids] * share_moved)
                votes_b[fraud_ids] <- votes_b[fraud_ids] - moved_votes 
                n_frauded <- n_frauded + sum(moved_votes)
              }
              # vote switching
              if (fraud_type == "switching") { 
                share_moved <- abs(rnorm(length(fraud_ids), 0, sd(votes_b/votes_all)))
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
                share_moved <- abs(rnorm(length(fraud_ids), 0, sd(non_voters/votes_all)))
                moved_votes <- as.integer(non_voters[fraud_ids] * share_moved)
                votes_b[fraud_ids] <- votes_b[fraud_ids] + moved_votes  
                non_voters <- l_total - votes_a - votes_b
                n_frauded <- n_frauded + sum(moved_votes)
              }           
              
              # vote stealing
              if (fraud_type == "stealing") {
                share_moved <- abs(rnorm(length(fraud_ids), 0, sd(votes_a/votes_all)))
                moved_votes <- as.integer(votes_a[fraud_ids] * share_moved)
                votes_a[fraud_ids] <- votes_a[fraud_ids] - moved_votes 
                n_frauded <- n_frauded + sum(moved_votes)
              }
              # vote switching
              if (fraud_type == "switching") { 
                share_moved <- abs(rnorm(length(fraud_ids), 0, sd(votes_a/votes_all)))
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
              
              fraud_ids <- sample(1:n_entities, size = n_entities * fraud_extA)
              
              # ballot box stuffing
              if (fraud_type == "bbs") {
                share_moved <- 1 - abs(rnorm(length(fraud_ids), 0, sd(non_voters/votes_all)))
                moved_votes <- as.integer(non_voters[fraud_ids] * share_moved)
                votes_a[fraud_ids] <- votes_a[fraud_ids] + moved_votes  
                non_voters <- l_total - votes_a - votes_b
                n_frauded <- n_frauded + sum(moved_votes)
              }           
              
              # vote stealing
              if (fraud_type == "stealing") {
                share_moved <- 1 - abs(rnorm(length(fraud_ids), 0, sd(votes_b/votes_all)))
                moved_votes <- as.integer(votes_b[fraud_ids] * share_moved)
                votes_b[fraud_ids] <- votes_b[fraud_ids] - moved_votes  
                n_frauded <- n_frauded + sum(moved_votes)
              }
              # vote switching
              if (fraud_type == "switching") { 
                share_moved <- 1 - abs(rnorm(length(fraud_ids), 0, sd(votes_b/votes_all)))
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
                share_moved <- 1 - abs(rnorm(length(fraud_ids), 0, sd(non_voters/votes_all)))
                moved_votes <- as.integer(non_voters[fraud_ids] * share_moved)
                votes_b[fraud_ids] <- votes_b[fraud_ids] + moved_votes  
                non_voters <- l_total - votes_a - votes_b
                n_frauded <- n_frauded + sum(moved_votes)
              }           
              
              # vote stealing
              if (fraud_type == "stealing") {
                share_moved <- 1 - abs(rnorm(length(fraud_ids), 0, sd(votes_a/votes_all)))
                moved_votes <- as.integer(votes_a[fraud_ids] * share_moved)
                votes_a[fraud_ids] <- votes_a[fraud_ids] - moved_votes  
                n_frauded <- n_frauded + sum(moved_votes)
              }
              # vote switching
              if (fraud_type == "switching") { 
                share_moved <- 1 - abs(rnorm(length(fraud_ids), 0, sd(votes_a/votes_all)))
                moved_votes <- as.integer(votes_a[fraud_ids] * share_moved)
                votes_a[fraud_ids] <- votes_a[fraud_ids] - moved_votes
                votes_b[fraud_ids] <- votes_b[fraud_ids] + moved_votes
                n_frauded <- n_frauded + sum(moved_votes)
              }
              
            } # end if (fraud_extB > 0)
            
          } # end if (fraud_extA > 0 | fraud_extB > 0)
          
        } # end if fraud_type != "clean"
        
        
        
      #' ------------------------------------------------------------
      #  (iii) redefine variables, aggregate data, store data -------
      #' ------------------------------------------------------------
        
        #' --------------------------------------------------
        # redefine variables that are affected by fraud
        #' --------------------------------------------------
          
        votes_all <- votes_a + votes_b 
        turnout <- votes_all / l_total
        non_voters <- l_total - votes_a - votes_b
        
        
        #' ------------------------------------
        # construct (aggregated) dataset
        #' ------------------------------------
          
          if (agg_factor == 1)  
            data <- as.data.frame(cbind(1:n_entities, l_total, votes_all, votes_a, votes_b, 
                                        non_voters, turnout, votes_a/votes_all, votes_b/votes_all))
          
          if (agg_factor > 1) {
            
            n_entities_agg <- n_entities / agg_factor
            agg_id <- sort(rep(1:n_entities_agg, agg_factor))
            data <- as.data.frame(cbind(1:n_entities, agg_id, l_total, votes_all, votes_a, votes_b, 
                                        non_voters, turnout, votes_a/votes_all, votes_b/votes_all))
            data <- data %>% 
              group_by(agg_id) %>% 
              summarise(agg_id = mean(agg_id), 
                        l_total = sum(l_total), 
                        votes_all = sum(votes_all),
                        votes_a = sum(votes_a), 
                        votes_b = sum(votes_b), 
                        non_voters = sum(non_voters))
            data$turnout <- data$votes_all / data$l_total
            data$share_A <- data$votes_a / data$votes_all
            data$share_B <- data$votes_b / data$votes_all
            
          } 
           
        
        #' -----------------------------------------------------------
        # store whole raw (or aggregated) data across all entities
        #' -----------------------------------------------------------
       
          colnames(data) <- c("id", "eligible", "votes_total", "votes_a", "votes_b",  
                              "non_voters", "turnout", "share_A", "share_B")   
          
          if (data_type == "full") 
            data_list[[election]] <- data
            
          
          
          if (data_type == "num_char") {
          
            #### MISSING 
            # things like chi2 statistic of BL test
            # fraction of 1 among first digit
            # skewness/kurtosis of turnout distribution
            # Wichtig: hier muss ich am Ende von gen_data() die outcome-Variable konstruieren, also n_tainted_votes
          
            data_char <- as.data.frame(matrix(NA, nrow=1, ncol=0))
            
            # fraud variables (y) 
            data_char$n_frauded <- n_frauded
            data_char$perc_frauded <- n_frauded / sum(votes_all)
            data_char$fraud_incA <- fraud_incA
            data_char$fraud_extA <- fraud_extA
            data_char$fraud_incB <- fraud_incB
            data_char$fraud_extB <- fraud_extB
            
            # numerical characteristics (X)
            
              # 1BL 
              data_char$bl1_frac1A <- length(which(extract_digit(data$votes_a, 1) == 1)) / length(data$votes_a) # partyA, fraction of '1' among first digit
              data_char$bl1_frac1B <- length(which(extract_digit(data$votes_b, 1) == 1)) / length(data$votes_b) # partyB, fraction of '1' among first digit
              data_char$bl1_meanA <- mean(extract_digit(data$votes_a, 1)) # party A, mean first digit
              data_char$bl1_meanB <- mean(extract_digit(data$votes_b, 1)) # party B, mean first digit
              data_char$bl1_chi2A <- benford_chi2(data$votes_a, 1) # party A, chi2 statistic between observed and expected shares in first digit 
              data_char$bl1_chi2B <- benford_chi2(data$votes_b, 1) # party B, chi2 statistic between observed and expected shares in first digit 
              
              # 2BL
              data_char$bl2_frac1A <- length(which(extract_digit(data$votes_a, 2) == 1)) / length(data$votes_a) # partyA, fraction of '1' among second digit
              data_char$bl2_frac1B <- length(which(extract_digit(data$votes_b, 2) == 1)) / length(data$votes_b) # partyB, fraction of '1' among second digit
              data_char$bl2_meanA <- mean(extract_digit(data$votes_a, 2)) # party A, mean second digit
              data_char$bl2_meanB <- mean(extract_digit(data$votes_b, 2)) # party B, mean second digit
              data_char$bl2_chi2A <- benford_chi2(data$votes_a, 2) # party A, chi2 statistic between observed and expected shares in second digit
              data_char$bl2_chi2B <- benford_chi2(data$votes_b, 2) # party B, chi2 statistic between observed and expected shares in second digit 
              
              # last digit
              data_char$bllast_frac1A <- length(which(extract_digit(data$votes_a, "last") == 1)) / length(data$votes_a) # partyA, fraction of '1' among last digit
              data_char$bllast_frac1B <- length(which(extract_digit(data$votes_b, "last") == 1)) / length(data$votes_b) # partyB, fraction of '1' among last digit
              data_char$bllast_meanA <- mean(extract_digit(data$votes_a, "last")) # party A, mean last digit
              data_char$bllast_meanB <- mean(extract_digit(data$votes_b, "last")) # party B, mean last digit
              data_char$bllast_chi2A <- benford_chi2(data$votes_a, "last") # party A, chi2 statistic between observed and expected shares in last digit 
              data_char$bllast_chi2B <- benford_chi2(data$votes_b, "last") # party B, chi2 statistic between observed and expected shares in last digit 
              
              # logarithmic turnout rate
              log_turnout_rate <- log(data$turnout / (data$eligible - data$turnout))
              data_char$logturnout_skew <- skewness(log_turnout_rate)
              data_char$logturnout_kurt <- kurtosis(log_turnout_rate)
              data_char$logturnout_sd <- sd(log_turnout_rate)
              
              # logarithmic vote share rates
              log_shareA_rate <- log(data$share_A / (data$eligible - data$share_A))
              data_char$logshareA_skew <- skewness(log_shareA_rate)
              data_char$logshareA_kurt <- kurtosis(log_shareA_rate)
              data_char$logshareA_sd <- sd(log_shareA_rate)
              
              log_shareB_rate <- log(data$share_B / (data$eligible - data$share_B))
              data_char$logshareB_skew <- skewness(log_shareB_rate)
              data_char$logshareB_kurt <- kurtosis(log_shareB_rate)
              data_char$logshareB_sd <- sd(log_shareB_rate)
              
            ifelse(election == 1,
                   data_list <- data_char, 
                   data_list <- rbind(data_list, data_char)
            )
            
          } # end if (data_type == "num_char")
          
       
    } # end for election in 1:n_elections
    
    # return list of generated elections      
    return(data_list)
    
        
  } # end function gen_data

  sim_elections <- gen_data(n_elections = 10, n_entities = 100, fraud_type="clean", 
                            agg_factor = 1, data_type="num_char")

  
        
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
    # 2.1.1 Venezuela, recall referendum 2004 ------
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
    
        #' -------------------------------------------------------------------------
        ### first: find fraud_incA and fraud_extA that resembles data most closely
        #' -------------------------------------------------------------------------
        
          fraud_values <- seq(0, 1, 0.01)
          fraud_values <- expand.grid(fraud_values, fraud_values)
          colnames(fraud_values) <- c("fraud_incA", "fraud_extA")
          fraud_values$euc_distV <- fraud_values$euc_distT <- NA
          # baseline_A = 870
          # baseline_B = 430
          
          for (row in 1:nrow(fraud_values)) {
            
            ven04_syn <- gen_data(n_entities = nrow(ven04),
                                  eligible = ven04$eligible,
                                  turnout_mean = mean(ven04$turnout), 
                                  turnout_sd = sd(ven04$turnout), 
                                  partyA_mean = mean(ven04$share_no, na.rm=T), 
                                  partyA_sd = sd(ven04$share_no, na.rm=T), 
                                  partyB_mean = mean(ven04$share_si, na.rm=T), 
                                  partyB_sd = sd(ven04$share_si, na.rm=T),
                                  fraud_type = "bbs",
                                  fraud_incA = fraud_values[row, "fraud_incA"],
                                  fraud_extA = fraud_values[row, "fraud_extA"],
                                  n_elections = 1, 
                                  baseline_A = 870, 
                                  baseline_B = 430
                                  
            )
            
            fraud_values[row, "euc_distV"] <- 
              distance(sort(ven04$share_no), sort(ven04_syn[[1]]$share_A), method="euclidean")
            
            fraud_values[row, "euc_distT"] <- 
              distance(sort(ven04$turnout), sort(ven04_syn[[1]]$turnout), method="euclidean")
            
            if (row %% 1000 == 0)
              print(str_c("iteration ", val, " out of ", nrow(fraud_values)))
            
          }
          
          # which fraud values to use
          ven04_valsV <- fraud_values[which(fraud_values$euc_distV == min(fraud_values$euc_distV)),]
          ven04_valsT <- fraud_values[which(fraud_values$euc_distT == min(fraud_values$euc_distT)),]
          
          
        #' -------------------------------------------------------------------------
        ### second: generate synthetic data using these values
        #' -------------------------------------------------------------------------
        
          ven04_syn <- gen_data(n_entities = nrow(ven04),
                                eligible = ven04$eligible,
                                turnout_mean = mean(ven04$turnout), 
                                turnout_sd = sd(ven04$turnout), 
                                partyA_mean = mean(ven04$share_no, na.rm=T), 
                                partyA_sd = sd(ven04$share_no, na.rm=T), 
                                partyB_mean = mean(ven04$share_si, na.rm=T), 
                                partyB_sd = sd(ven04$share_si, na.rm=T),
                                fraud_type = "bbs",
                                fraud_incA = 0.2,
                                fraud_extA = 0.0,
                                n_elections = 10)
          
        
        
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
        
          #' -------------------------------------------------------------------------
          ### first: find fraud_incA and fraud_extA that resembles data most closely
          #' -------------------------------------------------------------------------
          
            fraud_values <- seq(0, 1, 0.01)
            fraud_values <- expand.grid(fraud_values, fraud_values)
            colnames(fraud_values) <- c("fraud_incA", "fraud_extA")
            fraud_values$euc_distV <- fraud_values$euc_distT <- NA
            # baseline_A = 560
            # baseline_B = 230
            
            for (row in 1:nrow(fraud_values)) {
              
              ru12_syn <- gen_data(n_entities = nrow(ru12),
                                    eligible = ru12$eligible,
                                    turnout_mean = mean(ru12$turnout), 
                                    turnout_sd = sd(ru12$turnout), 
                                    partyA_mean = mean(ru12$share_putin, na.rm=T), 
                                    partyA_sd = sd(ru12$share_putin, na.rm=T), 
                                    partyB_mean = mean(ru12$share_zyuganov, na.rm=T), 
                                    partyB_sd = sd(ru12$share_zyuganov, na.rm=T),
                                    fraud_type = "bbs",
                                    fraud_incA = fraud_values[row, "fraud_incA"],
                                    fraud_extA = fraud_values[row, "fraud_extA"],
                                    n_elections = 1, 
                                    baseline_A = 560, 
                                    baseline_B = 230
                                    
              )
              
              fraud_values[row, "euc_distV"] <- 
                distance(sort(ru12$share_putin), sort(ru12_syn[[1]]$share_A), method="euclidean")
              
              fraud_values[row, "euc_distT"] <- 
                distance(sort(ru12$turnout), sort(ru12_syn[[1]]$turnout), method="euclidean")
              
              if (row %% 1000 == 0)
                print(str_c("iteration ", row, " out of ", nrow(fraud_values)))
              
            }
            
            # which fraud values to use
            ru12_valsV <- fraud_values[which(fraud_values$euc_distV == min(fraud_values$euc_distV)),]
            ru12_valsT <- fraud_values[which(fraud_values$euc_distT == min(fraud_values$euc_distT)),]
            
            
          #' -------------------------------------------------------------------------
          ### second: generate synthetic data using these values
          #' -------------------------------------------------------------------------
          
            ru12_syn <- gen_data(n_entities = nrow(ru12),
                                  eligible = ru12$eligible,
                                  turnout_mean = mean(ru12$turnout), 
                                  turnout_sd = sd(ru12$turnout), 
                                  partyA_mean = mean(ru12$share_putin), 
                                  partyA_sd = sd(ru12$share_putin), 
                                  partyB_mean = mean(ru12$share_zyuganov), 
                                  partyB_sd = sd(ru12$share_zyuganov),
                                  fraud_type = "bbs",
                                  fraud_incA = 0,
                                  fraud_extA = 0.04,
                                  n_elections = 10)
            
            
          
          
          
      
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
        
          #' -------------------------------------------------------------------------
          ### first: find fraud_incA and fraud_extA that resembles data most closely
          #' -------------------------------------------------------------------------
          
            fraud_values <- seq(0, 1, 0.01)
            fraud_values <- expand.grid(fraud_values, fraud_values)
            colnames(fraud_values) <- c("fraud_incA", "fraud_extA")
            fraud_values$euc_distV <- fraud_values$euc_distT <- NA
            # baseline_A = ?
            # baseline_B = ?
            
            for (row in 1:nrow(fraud_values)) {
              
              uga11_syn <- gen_data(n_entities = nrow(uga11),
                                   eligible = uga11$eligible,
                                   turnout_mean = mean(uga11$turnout), 
                                   turnout_sd = sd(uga11$turnout), 
                                   partyA_mean = mean(uga11$share_museveni, na.rm=T), 
                                   partyA_sd = sd(uga11$share_museveni, na.rm=T), 
                                   partyB_mean = mean(uga11$share_besigye, na.rm=T), 
                                   partyB_sd = sd(uga11$share_besigye, na.rm=T),
                                   fraud_type = "bbs",
                                   fraud_incA = fraud_values[row, "fraud_incA"],
                                   fraud_extA = fraud_values[row, "fraud_extA"],
                                   n_elections = 1, 
                                   baseline_A = X, # ? 
                                   baseline_B = X  # ?
                                   
              )
              
              fraud_values[row, "euc_distV"] <- 
                distance(sort(uga11$share_museveni), sort(uga11_syn[[1]]$share_A), method="euclidean")
              
              fraud_values[row, "euc_distT"] <- 
                distance(sort(uga11$turnout), sort(uga11_syn[[1]]$turnout), method="euclidean")
              
              if (row %% 1000 == 0)
                print(str_c("iteration ", row, " out of ", nrow(fraud_values)))
              
            }
            
            # which fraud values to use
            uga11_valsV <- fraud_values[which(fraud_values$euc_distV == min(fraud_values$euc_distV)),]
            uga11_valsT <- fraud_values[which(fraud_values$euc_distT == min(fraud_values$euc_distT)),]
            
            
          #' -------------------------------------------------------------------------
          ### second: generate synthetic data using these values
          #' -------------------------------------------------------------------------
          
            uga11_syn <- gen_data(n_entities = nrow(uga11),
                                 eligible = uga11$eligible,
                                 turnout_mean = mean(uga11$turnout), 
                                 turnout_sd = sd(uga11$turnout), 
                                 partyA_mean = mean(uga11$share_museveni), 
                                 partyA_sd = sd(uga11$share_museveni), 
                                 partyB_mean = mean(uga11$share_besigye), 
                                 partyB_sd = sd(uga11$share_besigye),
                                 fraud_type = "bbs",
                                 fraud_incA = X, # ?
                                 fraud_extA = X, # ?
                                 n_elections = 10)
            
          
      
      #' ------------------------------------------------
      # 2.1.4 Austria, parliamentary election 2008 ------
      #' ------------------------------------------------
      
        #' ------------------------
        # empirical
        #' ------------------------
        
          aus08 <- read_excel("U:/PhD Electoral Fraud/Data/Austria2008_adjusted.xls")
          aus08$turnout <- aus08$`Wahl-\nbeteil-\nigung\nin %` / 100
          aus08$eligible <- aus08$`Wahlbe- \nrechtigte`
          aus08$share_spo <- aus08$`%...9`/ 100
          aus08$share_ovp <- aus08$`%...11`/ 100
          
          # exclude units with an electorate < 100
          aus08 <- aus08[-which(aus08[,3] < 100),]
      
        
        #' ------------------------
        # synthetic
        #' ------------------------
        
          aus08_syn <- gen_data(n_entities = nrow(aus08),
                                eligible = aus08$eligible,
                                turnout_mean = mean(aus08$turnout), 
                                turnout_sd = sd(aus08$turnout), 
                                partyA_mean = mean(aus08$share_spo), 
                                partyA_sd = sd(aus08$share_spo), 
                                partyB_mean = mean(aus08$share_ovp), 
                                partyB_sd = sd(aus08$share_ovp),
                                fraud_type = "clean",
                                n_elections = 10
                                )
          
      #' ----------------------------------------------------
      # 2.1.5 Spain, European Parliament election 2019 ------
      #' ----------------------------------------------------
      
        #' ------------------------
        # empirical
        #' ------------------------
            
          esp19 <- read_excel("U:/PhD Electoral Fraud/Data/Spain_EP_2019.xlsx", skip = 5)
          esp19$turnout <- esp19$`Total votantes` / esp19$`Total censo electoral` 
          esp19$eligible <- esp19$`Total votantes`
          esp19$share_psoe <- esp19$PSOE / esp19$`Votos válidos`
          esp19$share_pp <- esp19$PP / esp19$`Votos válidos`
          
          # exclude units with an electorate < 100
          esp19 <- esp19[-which(esp19$`Total censo electoral` < 100),]
      
          
        #' ------------------------
        # synthetic
        #' ------------------------
        
          esp19_syn <- gen_data(n_entities = nrow(esp19),
                                eligible = aus08$eligible,
                                turnout_mean = mean(esp19$turnout), 
                                turnout_sd = sd(esp19$turnout), 
                                partyA_mean = mean(esp19$share_psoe), 
                                partyA_sd = sd(esp19$share_psoe), 
                                partyB_mean = mean(esp19$share_pp), 
                                partyB_sd = sd(esp19$share_pp),
                                fraud_type = "clean",
                                n_elections = 10
          )
          
          
          
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
     
          fin17_syn <- gen_data(n_entities = nrow(fin17),
                                eligible = fin17$eligible,
                                turnout_mean = mean(fin17$turnout), 
                                turnout_sd = sd(fin17$turnout), 
                                partyA_mean = mean(fin17$share_kok), 
                                partyA_sd = sd(fin17$share_kok), 
                                partyB_mean = mean(fin17$share_sdp), 
                                partyB_sd = sd(fin17$share_sdp),
                                fraud_type = "clean",
                                n_elections = 10
          )
          
          
          
  #' ---------------------
  # 2.2 comparisons ------
  #' ---------------------     
    
    #' -------------------------------------
    # 2.2.1 eligible voters per entity -----
    #' -------------------------------------
          
      h1 <- hist(aus08$eligible, breaks=100)
      h2 <- hist(aus08_syn[[1]]$eligible, breaks=100)
      plot(h1, col=rgb(0,0,1,1/4), 
           xlim = c(0, max(aus08$eligible, rep(seq(501, 1000, 1), 2))), 
           ylim = c(0, max(h1$counts, h2$counts)), 
           main = "Empirical vs. simulated distribution of eligible voters")  
      plot(h2, col=rgb(1,0,0,1/4), 
           xlim=c(0, max(aus08_syn[[1]]$eligible, rep(seq(501, 1000, 1), 2))), 
           add=T)
      
      
          
    #' ------------------
    # 2.2.2 digits ------
    #' ------------------
    
      plot_digits_all(aus08_syn[[1]]$votes_a, aus08_syn[[1]]$votes_b)
      plot_digits_1last(aus08$SPÖ, aus08_syn)
      
      tikz('digit_comparisons.tex', standAlone = TRUE, width=9, height=6)
      
        par(mfrow = c(2, 3),     # 2x3 layout
            oma = c(2, 2, 0, 0), # two rows of text at the outer left and bottom margin
            mar = c(0, 2, 1, 0), # space for one row of text at ticks and to separate plots
            mgp = c(2, 1, 0),
            xpd = F)  
        
        # Austria 2008
        plot_digits_1last(aus08$SPÖ, aus08_syn, title = "Austria 2008",
                          ylab = "Relative Frequency", y_axis = T, y_labels = T, 
                          x_axis = F, x_labels = F)
        
        text(3.5, 0.29, "First Digit", cex=1.5)
        text(2.1, 0.07, "Last Digit", cex=1.5)
        
        # Spain 2019
        par(mar = c(0, 1, 1, 0))
        plot_digits_1last(esp19$PSOE, esp19_syn, title = "Spain 2019",
                          y_axis = F, x_axis = F)
        
        # Finland 2017
        par(mar = c(0, 1, 1, 1))
        plot_digits_1last(fin17$`KOK Votes cast, total`, fin17_syn, title = "Finland 2017",
                          y_axis = F, x_axis = F)
        
        # Venezuela 2004
        par(mar = c(2, 2, 1, 0))
        plot_digits_1last(ven04$rrp_no, ven04_syn, title = "Venezuela 2004", ylab = "Relative Frequency", xlab = "Number",
                          y_axis = T, y_labels = T, x_axis = T, x_labels = T)
        
        # Russia 2012
        par(mar = c(2, 1, 1, 0))
        plot_digits_1last(ru12$putin, ru12_syn, title = "Russia 2012", xlab = "Number",
                          y_axis = F, x_axis = T, x_labels = T)
        
        # Country 6
        par(mar = c(2, 1, 1, 1))
        plot_digits_1last(aus08$SPÖ, aus08_syn, title = "Austria 2008", xlab = "Number",
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
    # 2.2.3 bivariate turnout and vote share distribution ------
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
            k <- kde2d(ru12$turnout, ru12$share_putin, n=50)
            image(k, col=r, xlim=c(0,1), ylim=c(0,1), xaxt="n", add=T)
            text(0.23, 0.95, "Russia 2012, Empirical", col="white")
            
            # synthetic
            par(mar = c(2.8, 1.5, 2.8, 2.3))
            image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), yaxt="n", xaxt="n")    
            k <- kde2d(ru12_syn[[1]]$turnout, ru12_syn[[1]]$share_A, n=50)
            image(k, col=r, xlim=c(0,1), ylim=c(0,1), yaxt="n", xaxt="n", add=T)
            text(0.23, 0.95, "Russia 2012, Synthetic", col="white")
        
            
          #' -----------------------
          # Russia 2012
          #' -----------------------
            
            # empirical
            par(mar = c(2.8, 2.8, 1, 1))
            image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), ylab="\\%Votes for Winner", xlab="\\%Turnout")    
            k <- kde2d(aus08$turnout, aus08$share_spo, n=50)
            image(k, col=r, xlim=c(0,1), ylim=c(0,1), add=T)
            text(0.23, 0.95, "Austria 2008, Empirical", col="white")
            
            # synthetic
            par(mar = c(2.8, 1.5, 1, 2.3))
            image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), yaxt="n", xlab="\\%Turnout")    
            k <- kde2d(aus08_syn[[1]]$turnout, aus08_syn[[1]]$share_A, n=50)
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

  ml_detection <- function(data = aus08, eligible = eligible, turnout = turnout, 
                       partyA = share_spo, partyB = share_ovp,
                       fraud_incA = seq(0.01, 0.4, 0.01), fraud_extA = seq(0.01, 0.1, 0.01),
                       fraud_incB = seq(0.01, 0.4, 0.01), fraud_extB = seq(0.01, 0.1, 0.01),
                       fraud_types = c("bbs", "stealing", "switching"),
                       n_elections = 500, models = c("ridge", "lasso"), seed=12345) {
    
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
    
    #' ------------------------------------
    #  (i) construct artifical data -------
    #' ------------------------------------
    
      # define scenarios that models are trained on 
      # set up in a way that 50% of scenarios are clean, rest is frauded to different degrees
      fraud_scenarios <- as.data.frame(expand.grid(fraud_incA, fraud_extA, fraud_incB, fraud_extB, fraud_types))
      clean_scenarios <- as.data.frame(matrix(0, nrow = nrow(fraud_scenarios), ncol = 5))
      colnames(fraud_scenarios) <- colnames(clean_scenarios) <- 
        c("fraud_incA", "fraud_extA", "fraud_incB", "fraud_extB", "type")
      clean_scenarios$type <- "clean"
      sim_scenarios <- rbind(fraud_scenarios, clean_scenarios)
     
      # generate n_elections artifical cases under each scenario in sim_scenarios
      for (scenario in 1:nrow(sim_scenarios)) {
      
        #### first: run gen_data just to determine baseline_A and baseline_B
        ## so I need to adjust the gen_data function such that I can use it only to 
        ## generate values for baseline_A and baseline_B
        
        
        
        
        #### second: create artifical data using these baselineA and baseline_B values  
        output <- gen_data(n_entities = nrow(data), 
                                  eligible = data[, deparse(substitute(eligible))],
                                  turnout_mean = mean(data[, deparse(substitute(turnout))]), 
                                  turnout_sd = sd(data[, deparse(substitute(turnout))]),
                                  partyA_mean = mean(data[, deparse(substitute(partyA))]), 
                                  partyA_mean = sd(data[, deparse(substitute(partyA))]), 
                                  partyB_mean = mean(data[, deparse(substitute(partyB))]), 
                                  partyB_mean = sd(data[, deparse(substitute(partyB))]), 
                                  fraud_type = sim_scenarios[scenario, "type"],
                                  fraud_incA = sim_scenarios[scenario, "fraud_incA"],
                                  fraud_extA = sim_scenarios[scenario, "fraud_extA"],
                                  fraud_incB = sim_scenarios[scenario, "fraud_incB"],
                                  fraud_extB = sim_scenarios[scenario, "fraud_extB"],
                                  n_elections = n_elections,
                                  data_type = "num_char",
                                  baseline_A = X, # ? 
                                  baseline_B = X # ?
                                  )
     
        ifelse(scenario == 1,
               sim_elections <- output, 
               sim_elections <- rbind(sim_elections, output)
               )
        
        }
      
      
    #' ----------------------------
    #  (ii) train ML models -------
    #' ----------------------------
    
      
      
    
    
  }
  
      
      


##### which models to use for machine learning? Models that can do classification and prediction
# kNN 
# ridge, lasso
# random Forest, BART
# neural network
 

    


