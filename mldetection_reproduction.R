#' Probabilistic Detection of Election Fraud Using Machine Learning Algorithms
#' Reproduction Material
#' Lion Behrens
#' -----------------------------------------------------------------------------

#' install and load packages
packages <- c("MASS", "gaussDiff", "truncnorm", "e1071")

for (i in 1: length(packages)) {
  if (is.element(packages[i], installed.packages()[,1]) == FALSE) 
    install.packages(packages[i], dep = TRUE) 
}

lapply(packages, library, character.only = T)

#' --------------------------------------
#  --- 1. synthetic data creation -------
#' --------------------------------------

  # define function 
  gen_data <- function(n_entities,    # number of entities to create data for
                       turnout_mean,  # mean turnout distribution
                       turnout_sd,   # sd turnout distribution
                       partyA_mean,   # mean support party A across entities
                       partyA_sd,     # sd support party A across entities
                       partyB_mean,   # mean support party B across entities
                       partyB_sd,     # mean support party B across entities
                       fraud_perc,    # overall perc of votes frauded
                       fraud_type,    # type of fraud (bbs, stealing, switching)
                       n_elections,   # number of elections to generate 
                       seed=12345) {  # seed
    
    
    #' -----------------------------------
    # optimize for baseline values -------
    #' -----------------------------------
     
      set.seed(seed)
     
      # optimize for constants baseline_a and baseline_b such that 
      # they minimize distance to multivariate normal p(turnout, party share)
      val_combs <- expand.grid(seq(0,1000,10), 
                                 seq(0,1000,10))
      colnames(val_combs) <- c("baseline_a", "baseline_b")  
      KL_vec <- rep(NA, nrow(val_combs))
      
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
        print(val)
      
      }
      
      val_opt <- val_combs[which(KL_vec == KL_vec[order(KL_vec)][1]),]
    
      
    #' --------------------
    # clean data ----------
    #' -------------------- 
    
      clean.list <- list() # empty list for clean dataframes
      
      for (election in 1:n_elections) {
        
        # generate variable following Benford's law
        X_a <- 10 ^ runif(n_entities, 0, 1) 
        X_b <- 10 ^ runif(n_entities, 0, 1) 
        
        # constants for scaling
        baseline_a <- val_opt[1, "baseline_a"]
        baseline_b <- val_opt[1, "baseline_b"]
        
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
        
        # store as dataframe in list
        # collect clean data
        clean <- as.data.frame(cbind(1:n_entities, votes_all, votes_a, votes_b, 
                                     turnout, votes_a/votes_all, votes_b/votes_all))
        colnames(clean) <- c("id", "votes_total", "votes_a", "votes_b", "turnout", 
                             "share_A", "share_B")  
        clean.list[[election]] <- clean
        
        print(election)
        
      }
     
      # store one as example
      clean <- clean.list[[1]]
      
      
      ### also generell ist das jetzt einmal aufgesetzt
      ### mal schauen, ob das theoretischen bzw. empririschen Charakteristiken 
      ### folgt. 
     
      ### an sich muss man sagen, dass ich zur Zeit die p(turnout, vote share)
      ### distribution, die approxmiert werden soll, mit off-diagonals = 0
      ### definiere. Das bedeutet, meine Daten werden wohl keinen skew
      ### in dieser Verteilung haben. Ist das in Ordnung?
      ### an sich soll es ja in sauberen Daten auch keinen *starken* skew geben.
       
        
         
        
        
        
        
    
    
        



 

    


