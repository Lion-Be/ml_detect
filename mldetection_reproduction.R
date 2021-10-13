#' Probabilistic Detection of Election Fraud Using Machine Learning Algorithms
#' Reproduction Material
#' Lion Behrens
#' -----------------------------------------------------------------------------

#' install and load packages
packages <- c("MASS", "gaussDiff", "RColorBrewer", "truncnorm", "e1071", 
              "stringr", "dplyr", "readxl")

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
  gen_data <- function(n_entities = 1000, turnout_mean = 0.7, turnout_sd = 0.1, 
                       partyA_mean = 0.6, partyA_sd = 0.1, partyB_mean = 0.4,    
                       partyB_sd = 0.1, fraud_type="clean", fraud_incA = 0, 
                       fraud_extA = 0, fraud_incB = 0, fraud_extB = 0, 
                       agg_factor = 1, n_elections = 1000, data_type = "full",
                       seed = 12345) {  
    
    # n_entities = number of entities to create data for
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
    # data_type = is full data frame across n_entities stored or only numerical characteristics (data_type = "num_char")
    # n_elections = number of elections to generate 
  
    #' ----------------------------------------
    #  (i) optimize for baseline values -------
    #' ----------------------------------------
    
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
        
        if (val %% 1000 == 0)
          print(str_c("optimization progress: ", val, " out of ", nrow(val_combs)))
        
      } # end optimization
     
      val_opt <- val_combs[which(KL_vec == KL_vec[order(KL_vec)][1]),]
    
    
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
         
        if (data_type == "full") {
        
          colnames(data) <- c("id", "l_total", "votes_total", "votes_a", "votes_b",  
                              "non_voters", "turnout", "share_A", "share_B")  
          data_list[[election]] <- data
          
        }
        
        if (data_type == "num_char") {
        
          #### MISSING 
          # things like chi2 statistic of BL test
          # fraction of 1 among first digit
          # skewness/kurtosis of turnout distribution
          # Wichtig: hier muss ich am Ende von gen_data() die outcome-Variable konstruieren, also n_tainted_votes
        
          data_char <- as.data.frame(matrix(NA, nrow=1, ncol=0))
          
          # fraud variables (y)
          data_char$n_frauded <- n_frauded
          data_char$fraud_incA <- fraud_incA
          data_char$fraud_extA <- fraud_extA
          data_char$fraud_incB <- fraud_incB
          data_char$fraud_extB <- fraud_extB
          
          # numerical characteristics (X)
          data_char$bl1_frac1A <- length(which(extract_digit(data$votes_a, 1) == 1)) / length(data$votes_a) # partyA, fraction of '1' among first digit
          data_char$bl1_frac1B <- length(which(extract_digit(data$votes_b, 1) == 1)) / length(data$votes_b) # partyA, fraction of '1' among first digit
      
          data_list[[election]] <- data_char 
          
        } # end if (data_type == "num_char")
        
       
    } # end for election in 1:n_elections
    
    # return list of generated elections      
    return(data_list)
        
  } # end function gen_data

  sim_elections <- gen_data(n_elections = 10, n_entities = 1000, fraud_type="clean", 
                            agg_factor = 1, data_type="num_char")

  
        
#' ------------------------------------------------------------------
#  --- 2. comparison to theoretical/empirical characteristics -------
#' ------------------------------------------------------------------ 
    
  #' ------------------------
  # 2.0 empirical data ------
  #' ------------------------
  
    # in general: 
    # entities with an electorate < 100 are excluded
    # entities with NAs are excluded
    # when turnout is higher than 1, it set to 1
  
  
    # -----------------------------------
    # Venezuela, recall referendum 2004  
    # -----------------------------------
     
      ven04 <- read_excel("U:/PhD Electoral Fraud/Data/Venezuela/2004_referendum_revocatorio/referendum.xls",
                              skip = 34)
      # rrp_si = number of yes votes
      # rrp_no = number of no votes
      # rep200407 = number of eligible voters  
      ven04$votes_all <- ven04$rrp_si+ven04$rrp_no+ven04$rrp_nulo
      ven04$turnout <- ven04$votes_all / ven04$rep200407
      ven04$share_si <- ven04$rrp_si / (ven04$rrp_si + ven04$rrp_no)
      ven04$share_no <- ven04$rrp_no / (ven04$rrp_si + ven04$rrp_no)
  
      # exclude units with an electorate < 100
      ven04 <- ven04[-which(ven04$rep200407 < 100),]
      
      # set turnout > 1 to 1
      ven04$turnout[which(ven04$turnout > 1)] <- 1
    
      
    # -------------------------------------
    # Russia, presidential election 2012
    # -------------------------------------
      
      ru12_1 <- read_excel("U:/PhD Electoral Fraud/Data/Russia2012_1of2.xls")
      ru12_2 <- read_excel("U:/PhD Electoral Fraud/Data/Russia2012_2of2.xls")
      ru12 <- rbind(ru12_1, ru12_2)
      ru12$`Number of invalid ballots`[which(ru12$`Number of invalid ballots`=="A")] <- 0
      ru12$votes_all <- ru12$`Number of valid ballots` + as.numeric(ru12$`Number of invalid ballots`)
      ru12$turnout <- ru12$votes_all / ru12$`The number of voters included in voters list` 
      ru12$putin <- ru12$`Vladimir Putin`
      ru12$share_putin <- ru12$putin / ru12$votes_all
      
      # exclude units with an electorate < 100
      ru12 <- ru12[-which(ru12$`The number of voters included in voters list` < 100),]
      # exclude units with NAs in share_putin
      ru12 <- ru12[-which(is.na(ru12$share_putin)),]
      
      
    # -------------------------------------
    # Uganda, presidential election 2011
    # -------------------------------------
      
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
      
      # exclude units with an electorate < 100
      uga11 <- uga11[-which(uga11$eligible < 100),]
      
      # exclude units with NAs in share_museveni
      uga11 <- uga11[-which(is.na(uga11$share_museveni)),]
     
      
    # -----------------------------------------
    # Austria, parliamentary election 2008
    # -----------------------------------------
      
      ### before I work with the data: always first check whether turnout can be constructed based on the available information
      
    
      
      
      
      
      
  #' ----------------
  # 2.1 digits ------
  #' ----------------
  
    # how to enhance plots?
    # - add empirical distributions to this
    # - add many simulated curves here to show variability of clean elections
  
  
    plot_digits(uga11$besigye, uga11$museveni)
  
  #' ----------------------------------
  # 2.2 logarithmic turnout rate ------
  #' ----------------------------------
  
    par(mfrow=c(1,1))
    
    # what is the theoretical expectation here? is there one?
    
    log_turnout <- log(bbs$votes_total / (bbs$l_total - bbs$votes_total))
    log_turnoutR <- log_turnout - mean(log_turnout)
    d_log_turnoutR <- density(log_turnoutR)
    
    plot(d_log_turnoutR, lwd=2, xlab="tau - mean(tau)", main="Logarithmic Turnout Rate", col="darkgreen")  
  
  
  
  #' -------------------------------------------------------
  # 2.3 bivariate turnout and vote share distribution ------
  #' -------------------------------------------------------
  
    ### ist there a continuous density estimated?
    ### what I want is amybe a 100 x 100 discrete histogram?
    ### maybe continuous density estimation blurrs the picture
    
    par(mfrow=c(1,1))
    
    rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
    r <- rf(32)
    
    # empty image with baseline color
    x <- list()
    x$x <- seq(0,10,1)
    x$y <- seq(0,10,1)
    x$z <- matrix(rep(0,100), nrow=10, ncol=10)
   
    # artifical data
    image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), xlab="Turnout", ylab="Vote Share Candidate A", main="Turnout and Vote Share Distribution")
    k <- kde2d(uga11$turnout, uga11$share_museveni, n=100)
    image(k, col=r, add = T)
    
    
  
  
  
  
  
  
  
  
  
        
    
    
        



 

    


