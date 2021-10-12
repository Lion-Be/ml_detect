#' Probabilistic Detection of Election Fraud Using Machine Learning Algorithms
#' Reproduction Material
#' Lion Behrens
#' -----------------------------------------------------------------------------

#' install and load packages
packages <- c("MASS", "gaussDiff", "RColorBrewer", "truncnorm", "e1071", 
              "stringr", "dplyr")

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
        
        
        #' -----------------------------------------------------------
        # store whole raw (or aggregated) data across all entities
        if (data_type == "full") {
        #' -----------------------------------------------------------
          
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
            
          colnames(data) <- c("id", "l_total", "votes_total", "votes_a", "votes_b",  
                              "non_voters", "turnout", "share_A", "share_B")  
          data_list[[election]] <- data
          
        } # end if (data_type == "full")
        
        
        
        
        
        
        #' -------------------------------------------
        # only store numerical information of data
        if (data_type == "num_char") {
        #' -------------------------------------------
        
        
          #### MISSING 
          # things like chi2 statistic of BL test
          # fraction of 1 among first digit
          # skewness/kurtosis of turnout distribution
          # Wichtig: hier muss ich am Ende von gen_data() die outcome-Variable konstruieren, also n_tainted_votes
            
           
        } # end if (data_type == "num_info")
        
        
        
        
        
        
        
        
        
       
    } # end for election in 1:n_elections
    
    # return list of generated elections      
    return(data_list)
        
  } # end function gen_data

  sim_elections <- gen_data(n_elections = 10, n_entities = 1000, fraud_type="clean", 
                            agg_factor = 20)

  # store one as example
  clean <- sim_elections[[1]]
  
  
  ### I still need to implement data aggregation 
  ### introduce additional argument: do I want the full dataframes of each election? 
  ### or is each election collapsed to one row and I only extract the features across the whole country
  ### for the ML models?
  
  
  

  # - Datensatz-Generierung und Training der ML-Modelle kann komplett separat erfolgen, oder?
  #   - Theoretisch brauche ich die kompletten Datensätze nur für die Visualisierung/Vergleich mit Theorie/empirischen Daten
  # - Für die ML-Modelle, kann ich einfach einen dataframe aufsetzen
  # - jede Zeile eine Wahl
  # - erste Hälfte der Spalten die spezifizierten Argumente in gen_data()
  # - zweite Hälfte der Spalten die ganzen Werte der Variablen, die ich zum training nutze
  # - ich könnte mir einfach 1x 1000 Wahlen aus einem Szenario simulieren und damit dann das Training aufsetzen, 
  # während die ganzen Simulation für die anderen Szenarien durchlaufen
  # 
  
  
  
  
  
  
      
      
      ### also generell ist das jetzt einmal aufgesetzt
      ### mal schauen, ob das theoretischen bzw. empririschen Charakteristiken 
      ### folgt. Fälle mit verschiedenen Fallzahlen und clean vs. fraud. 
     
      ### an sich muss man sagen, dass ich zur Zeit die p(turnout, vote share)
      ### distribution, die approxmiert werden soll, mit off-diagonals = 0
      ### definiere. Das bedeutet, meine Daten werden wohl keinen skew
      ### in dieser Verteilung haben. Ist das in Ordnung?
      ### an sich soll es ja in sauberen Daten auch keinen *starken* skew geben.
       
        
         
        
#' ------------------------------------------------------------------
#  --- 2. comparison to theoretical/empirical characteristics -------
#' ------------------------------------------------------------------ 
    
  #' ----------------
  # 2.1 digits ------
  #' ----------------
  
    # how to enhance plots?
    # - add empirical distributions to this
    # - add many simulated curves here to show variability of clean elections
  
    par(mfrow=c(1,3))
    
    # 1BL
    plot(benford_expected(1), ylab="Relative Frequency", xlab="Digit", 
         labels=F, type="o", lwd=2, ylim=c(0,0.35), main="First Digit Distribution")
    axis(1, at=1:10, labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))
    axis(2, at=seq(0, 0.35, 0.05))
    lines(c(NA, table(extract_digit(clean$votes_a, 1))/length(extract_digit(clean$votes_a, 1))), type="o",
          lwd=2, col="orange")
    lines(c(NA, table(extract_digit(clean$votes_b, 1))/length(extract_digit(clean$votes_b, 1))), type="o",
          lwd=2, col="darkred")
    
    legend(6,0.35, c("Theory", "Candidate A", "Candidate B"), 
           col=c("black", "orange", "darkred"), lwd=c(3,3,3), bty="n")
    
    # 2BL
    plot(benford_expected(2), ylab="Relative Frequency", xlab="Digit", 
         labels=F, type="o", lwd=2, ylim=c(0,0.35), main="Second Digit Distribution")
    axis(1, at=1:10, labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))
    axis(2, at=seq(0, 0.35, 0.05))
    lines(c(table(extract_digit(clean$votes_a, 2))/length(extract_digit(clean$votes_a, 2))), type="o",
          lwd=2, col="orange")
    lines(c(table(extract_digit(clean$votes_b, 2))/length(extract_digit(clean$votes_b, 2))), type="o",
          lwd=2, col="darkred")
    
    legend(6,0.35, c("Theory", "Candidate A", "Candidate B"), 
           col=c("black", "orange", "darkred"), lwd=c(3,3,3), bty="n")
    
    # Last Digit
    plot(benford_expected(3), ylab="Relative Frequency", xlab="Digit", 
         labels=F, type="o", lwd=2, ylim=c(0,0.35), main="Last Digit Distribution")
    axis(1, at=1:10, labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))
    axis(2, at=seq(0, 0.35, 0.05))
    lines(c(table(extract_digit(clean$votes_a, "last"))/length(extract_digit(clean$votes_a, "last"))), type="o",
          lwd=2, col="orange")
    lines(c(table(extract_digit(clean$votes_b, "last"))/length(extract_digit(clean$votes_b, "last"))), type="o",
          lwd=2, col="darkred")
    
    legend(6,0.35, c("Theory", "Candidate A", "Candidate B"), 
           col=c("black", "orange", "darkred"), lwd=c(3,3,3), bty="n")
    
  
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
    k <- kde2d(bbs$turnout, bbs$votes_a/bbs$votes_total, n=100)
    image(k, col=r, add = T)
    
    
  
  
  
  
  
  
  
  
  
        
    
    
        



 

    


