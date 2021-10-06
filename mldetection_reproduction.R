#' Probabilistic Detection of Election Fraud Using Machine Learning Algorithms
#' Reproduction Material
#' Lion Behrens
#' -----------------------------------------------------------------------------

#' install and load packages
packages <- c("MASS", "gaussDiff", "RColorBrewer", "truncnorm", "e1071")

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
  gen_data <- function(n_entities = 10000, turnout_mean = 0.7, turnout_sd = 0.1, 
                       partyA_mean = 0.6, partyA_sd = 0.1, partyB_mean = 0.4,    
                       partyB_sd = 0.1, fraud_type="clean", fraud_percA = 0, 
                       fraud_percB = 0, n_elections, seed=12345) {  
    
    # n_entities = number of entities to create data for
    # turnout mean = mean turnout distribution
    # turnout_sd = sd turnout distribution
    # partyA_mean = mean support party A across entities
    # partyA_sd = sd support party A across entities
    # partyB_mean = mean support party B across entities
    # partyB_sd = sd support party B across entities
    # fraud_type = type of fraud (clean, bbs, stealing, switching)
    # fraud_percA = overall perc of votes frauded in favor of partyA
    # fraud_percB = overall perc of votes frauded in favor of partyB
    # n_elections = number of elections to generate 
  
    #' ----------------------------------------
    #  (i) optimize for baseline values -------
    #' --------------------------------------
    
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
      
      } # end optimization
      
      val_opt <- val_combs[which(KL_vec == KL_vec[order(KL_vec)][1]),]
    
    
    #' ----------------------------------------------
    # (ii) generate n=n_elections datasets ----------
    #' ----------------------------------------------
      
      data_list <- list() # empty list for generated election dataframes
      for (election in 1:n_elections) {
      
      # ----------------------
      # clean election data
      # ----------------------
      
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
    
        
      # ----------------------
      # ballot box stuffing
      if (fraud_type == "bbs") {
      # ----------------------
      
        
        
        
        ### logic 
        # first: identify total number of votes that should be moved
        
        # second: identify at-risk polling stations by ordering them by the distance between the two parties. 
        # only those where candidate that we want to fraud for is loosing at
        
        # third: start frauding from below, fill up votes until party is winning then go to next polling station
        # do this up until the total number of votes that should be moved is reached  
          
        
        
      
      } # end if fraud_type == "bbs" 
      
     
      # store election data as dataframe in list
      clean <- as.data.frame(cbind(1:n_entities, votes_all, votes_a, votes_b, 
                                   l_total, turnout, votes_a/votes_all, votes_b/votes_all))
      colnames(clean) <- c("id", "votes_total", "votes_a", "votes_b", "l_total", 
                           "turnout", "share_A", "share_B")  
      clean_list[[election]] <- clean
      
      print(election)
          
    } # end for election in 1:n_elections
    
    # return list of generated elections      
    return(data_list)
        
  } # end function gen_data


  # store one as example
  clean <- data_list[[1]]
      
      
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
    lines(c(table(extract_digit(votes_a, 2))/length(extract_digit(votes_a, 2))), type="o",
          lwd=2, col="orange")
    lines(c(table(extract_digit(votes_b, 2))/length(extract_digit(votes_b, 2))), type="o",
          lwd=2, col="darkred")
    
    legend(6,0.35, c("Theory", "Candidate A", "Candidate B"), 
           col=c("black", "orange", "darkred"), lwd=c(3,3,3), bty="n")
    
    # Last Digit
    plot(benford_expected(3), ylab="Relative Frequency", xlab="Digit", 
         labels=F, type="o", lwd=2, ylim=c(0,0.35), main="Last Digit Distribution")
    axis(1, at=1:10, labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))
    axis(2, at=seq(0, 0.35, 0.05))
    lines(c(table(extract_digit(votes_a, "last"))/length(extract_digit(votes_a, "last"))), type="o",
          lwd=2, col="orange")
    lines(c(table(extract_digit(votes_b, "last"))/length(extract_digit(votes_b, "last"))), type="o",
          lwd=2, col="darkred")
    
    legend(6,0.35, c("Theory", "Candidate A", "Candidate B"), 
           col=c("black", "orange", "darkred"), lwd=c(3,3,3), bty="n")
    
  
  #' ----------------------------------
  # 2.2 logarithmic turnout rate ------
  #' ----------------------------------
  
    par(mfrow=c(1,1))
    
    # what is the theoretical expectation here? is there one?
    
    log_turnout <- log(clean$votes_total / (clean$l_total - clean$votes_total))
    log_turnoutR <- log_turnout - mean(log_turnout)
    d_log_turnoutR <- density(log_turnoutR)
    
    plot(d_log_turnoutR, lwd=2, xlab="tau - mean(tau)", main="Logarithmic Turnout Rate", col="darkgreen")  
  
  
  
  #' -------------------------------------------------------
  # 2.3 bivariate turnout and vote share distribution ------
  #' -------------------------------------------------------
  
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
    k <- kde2d(turnout, clean$votes_a/clean$votes_total, n=100)
    image(k, col=r, add = T)
    
    
  
  
  
  
  
  
  
  
  
        
    
    
        



 

    


