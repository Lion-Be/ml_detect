#' -----------------------------
#  0. baseline functions -------
#' ----------------------------- 

#' install and load packages
packages <- c("MASS", "gaussDiff", "RColorBrewer", "truncnorm", "e1071", 
              "stringr", "dplyr", "readxl", "tikzDevice", "distantia", "doSNOW", 
              "caret", "glmnet", "class", "randomForest", "xgboost", "LaplacesDemon")

for (i in 1: length(packages)) {
  if (is.element(packages[i], installed.packages()[,1]) == FALSE) 
    install.packages(packages[i], dep = TRUE) 
}

lapply(packages, library, character.only = T)


# --------------------------------------------------------------------------- #

# get expected relative frequencies by Benford's law and its generalizations
# e.g. benford(1)
# e.g. benford(2)
# e.g. benford(3)
benford_expected <- function(position){
  
  # Declarations
  # digit = the significant digit we are interest in (first digit, second digit, etc.)
  number <- seq(0,9,1)
  if (position == 1) number <- number[-1]
  number_probabilities <- rep(NA, length(number))
  
  # First digit distribution
  if (position == 1) {
    number_probabilities <- c(NA, log10(1+1/number))
  }
  
  
  # Generalization for subsequennt digits
  if (position > 1) {
    k_start <- 10^(position-2)
    k_end <- 10^(position-1)-1
    k <- seq(k_start, k_end, 1)
    
    iteration <- 0
    for (number in number) {
      iteration <- iteration + 1
      number_probabilities[iteration] <- sum(log10(1 + 1/(10*k+number)))
    }
  }
  
  # Return vector of probabilities
  return(number_probabilities)
  
}

# --------------------------------------------------------------------------- #


# extract first, second or last digit from vector of integers
# e.g. extract(x, 1)
# e.g. extract(x, 2)
# e.g. extract(x, "last")
extract_digit <- function(x, digit){
  
  # commands for digit: 1, 2, "last"
  
  if (digit == 1) 
    digits <- as.numeric(str_sub(x, 1, 1))
  
  if (digit == 2) 
    digits <- as.numeric(str_sub(x, 2, 2))
  
  if (digit == "last") 
    digits <- as.numeric(str_sub(x, -1, -1))

  return(digits)
  
}




# --------------------------------------------------------------------------- #

# get chi^2 statistic for empirical digit distribution vs. Benford expectation

benford_chi2 <- function(x, digit) {
  
  # commands for digit: 1, 2, "last"
  
  if (digit == 1)
    for (number in 1:9) {
      chi_square <- sum((((length(which(extract_digit(x, 1) == number)) / length(x)) - benford_expected(1)[number+1])^2) / benford_expected(1)[number+1])
    }
  
  if (digit == 2)
    for (number in 0:9) {
      chi_square <- sum((((length(which(extract_digit(x, 2) == number)) / length(x)) - benford_expected(2)[number+1])^2) / benford_expected(2)[number+1])
    }
  
  if (digit == "last")
    for (number in 0:9) {
      chi_square <- sum((((length(which(extract_digit(x, "last") == number)) / length(x)) - benford_expected(3)[number+1])^2) / benford_expected(3)[number+1])
    }
    
  return(chi_square)
}



# --------------------------------------------------------------------------- #

# get relative frequencies of digits from digit vector
# e.g. rel.freq(extract(x,2))

rel_freq <- function(x) {
  
  table(x) / sum(table(x))
  
}

# --------------------------------------------------------------------------- #

gen_features <- function(votes_a, votes_b, turnout, share_A, share_B, eligible) {
  
  data_char <- as.data.frame(matrix(NA, nrow=1, ncol=0))

  # 2BL
  data_char$bl2_frac1A <- length(which(extract_digit(votes_a, 2) == 1)) / length(votes_a) # partyA, fraction of '1' among second digit
  data_char$bl2_frac1B <- length(which(extract_digit(votes_b, 2) == 1)) / length(votes_b) # partyB, fraction of '1' among second digit
  data_char$bl2_meanA <- mean(extract_digit(votes_a, 2), na.rm=T) # party A, mean second digit
  data_char$bl2_meanB <- mean(extract_digit(votes_b, 2), na.rm=T) # party B, mean second digit
  data_char$bl2_chi2A <- benford_chi2(votes_a, 2) # party A, chi2 statistic between observed and expected shares in second digit
  data_char$bl2_chi2B <- benford_chi2(votes_b, 2) # party B, chi2 statistic between observed and expected shares in second digit 
  
  # last digit
  data_char$bllast_frac1A <- length(which(extract_digit(votes_a, "last") == 1)) / length(votes_a) # partyA, fraction of '1' among last digit
  data_char$bllast_frac1B <- length(which(extract_digit(votes_b, "last") == 1)) / length(votes_b) # partyB, fraction of '1' among last digit
  data_char$bllast_meanA <- mean(extract_digit(votes_a, "last")) # party A, mean last digit
  data_char$bllast_meanB <- mean(extract_digit(votes_b, "last")) # party B, mean last digit
  data_char$bllast_chi2A <- benford_chi2(votes_a, "last") # party A, chi2 statistic between observed and expected shares in last digit 
  data_char$bllast_chi2B <- benford_chi2(votes_b, "last") # party B, chi2 statistic between observed and expected shares in last digit 
  
  # characteristics of raw turnout distribution 
  data_char$turnout_skew <- e1071::skewness(turnout)
  data_char$turnout_kurt <- kurtosis(turnout)
  data_char$turnout_normdist <- LaplacesDemon::KLD(turnout, rnorm(length(turnout), mean(turnout), sd(turnout)))$mean.sum.KLD
  
  # characteristics of logarithmic turnout rate
  turnout[which(turnout <= 0)] <- 0.001
  turnout[which(turnout > 1)] <- 1
  log_turnout_rate <- log(turnout / (eligible - turnout))
  data_char$logturnout_skew <- e1071::skewness(log_turnout_rate)
  data_char$logturnout_kurt <- kurtosis(log_turnout_rate)
  data_char$logturnout_sd <- sd(log_turnout_rate)
  data_char$logturnout_normdist <- LaplacesDemon::KLD(log_turnout_rate, rnorm(length(log_turnout_rate), mean(log_turnout_rate), sd(log_turnout_rate)))$mean.sum.KLD
  
  # characteristics of vote shares 
  data_char$shareA_normdist <- LaplacesDemon::KLD(share_A, rnorm(length(share_A), mean(share_A), sd(share_A)))$mean.sum.KLD
  data_char$shareB_normdist <- LaplacesDemon::KLD(share_A, rnorm(length(share_A), mean(share_A), sd(share_A)))$mean.sum.KLD
  
  # characteristics of logarithmic vote share rates
  share_A[which(share_A <= 0)] <- 0.001
  share_A[which(share_A > 1)] <- 1
  log_shareA_rate <- log(share_A / (eligible - share_A))
  data_char$logshareA_skew <- e1071::skewness(log_shareA_rate)
  data_char$logshareA_kurt <- kurtosis(log_shareA_rate)
  data_char$logshareA_sd <- sd(log_shareA_rate)
  data_char$logshareA_normdist <- LaplacesDemon::KLD(log_shareA_rate, rnorm(length(log_shareA_rate), mean(log_shareA_rate), sd(log_shareA_rate)))$mean.sum.KLD
  
  share_B[which(share_B <= 0)] <- 0.001
  share_B[which(share_B > 1)] <- 1
  log_shareB_rate <- log(share_B / (eligible - share_B))
  data_char$logshareB_skew <- e1071::skewness(log_shareB_rate)
  data_char$logshareB_kurt <- kurtosis(log_shareB_rate)
  data_char$logshareB_sd <- sd(log_shareB_rate)
  data_char$logshareB_normdist <- LaplacesDemon::KLD(log_shareB_rate, rnorm(length(log_shareB_rate), mean(log_shareB_rate), sd(log_shareB_rate)))$mean.sum.KLD
  
  return(data_char)
  
}


# --------------------------------------------------------------------------- #

gen_fraudvals <- function(n_entities, eligible, turnout_mean, turnout_sd, 
                          partyA_mean, partyA_sd, partyB_mean, partyB_sd,
                          shareA, turnout, fraud_type="bbs") {

  # optimize for turnout, shareA
  opt_vecs <- gen_data(n_entities = n_entities,
                       eligible = eligible,
                       turnout_mean = turnout_mean, 
                       turnout_sd = turnout_sd, 
                       partyA_mean = partyA_mean, 
                       partyA_sd = partyA_sd, 
                       partyB_mean = partyB_mean, 
                       partyB_sd = partyB_sd,
                       optimize_only = T
  )  
  
  # iterate over fraud values
  fraud_values <- seq(0, 0.4, 0.01)
  fraud_values <- expand.grid(fraud_values, fraud_values)
  colnames(fraud_values) <- c("fraud_incA", "fraud_extA")
  fraud_values$euc_distV <- fraud_values$euc_distT <- NA
  
  for (val in 1:nrow(fraud_values)) {
    
    data_syn <- gen_data(n_entities = n_entities,
                          eligible = eligible,
                          turnout_mean = turnout_mean, 
                          turnout_sd = turnout_sd, 
                          partyA_mean = partyA_mean, 
                          partyA_sd = partyA_sd, 
                          partyB_mean = partyB_mean, 
                          partyB_sd = partyB_sd,
                          fraud_type = fraud_type,
                          fraud_incA = fraud_values[val, "fraud_incA"],
                          fraud_extA = fraud_values[val, "fraud_extA"],
                          n_elections = 1, 
                          data_type = "full", 
                          turnout = opt_vecs$turnout, 
                          shareA = opt_vecs$shareA
    )
    
    fraud_values[val, "euc_distV"] <- 
      distance(sort(shareA), sort(data_syn$shareA), method="euclidean")
    
    fraud_values[val, "euc_distT"] <- 
      distance(sort(turnout), sort(data_syn$turnout), method="euclidean")
    
    if (val %% 1000 == 0)
      print(str_c("iteration ", val, " out of ", nrow(fraud_values)))
    
  }
  
  # which fraud values to use
  valsV <- fraud_values[which.min(fraud_values$euc_distV),]
  valsT <- fraud_values[which.min(fraud_values$euc_distT),]
  
  out <- list(opt_vecs, valsV, valsT)
  names(out) <- c("optimized vectors", "valsV", "valsT")
  return(out)

}


# --------------------------------------------------------------------------- #

gen_cumdata <- function(votes_a, turnout, votes_all) {
  
  cum_data <- as.data.frame(cbind(votes_a, turnout))
  colnames(cum_data) <- c("votes_a", "turnout")
  cum_data <- cum_data[order(cum_data$turnout),]
  cum_data$share_cum <- NA
  votes_a[which(is.na(votes_a))] <- 0
  cum_data$share_cum <- cumsum(votes_a) / sum(votes_all)
  
  return(cum_data)
}



# --------------------------------------------------------------------------- #


plot_digits_all <- function(votes_a, votes_b) {
  
  par(mfrow=c(1,3))
  
  # 1BL
  plot(benford_expected(1), ylab="Relative Frequency", xlab="Digit", 
       labels=F, type="o", lwd=2, ylim=c(0,0.35), main="First Digit Distribution")
  axis(1, at=1:10, labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))
  axis(2, at=seq(0, 0.35, 0.05))
  lines(c(NA, table(extract_digit(votes_a, 1))/length(extract_digit(votes_a, 1))), type="o",
        lwd=2, col="orange")
  lines(c(NA, table(extract_digit(votes_b, 1))/length(extract_digit(votes_b, 1))), type="o",
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
  
}


# --------------------------------------------------------------------------- #

plot_digits_2last <- function(votes_emp, votes_syn, title=" ", ylab = " ", xlab = " ", 
                              y_axis = F, y_labels = F, x_axis = F, x_labels = F) {

  #' -------
  # 2BL
  #' -------
  # second digit theory
  par(xpd=NA)
  plot(benford_expected(2), main="", ylab = ylab, xlab = xlab, 
       cex.lab = 1.5, axes = F, bty="n", pch=16,
       type="o", lwd=1, ylim=c(0.05,0.15), labels=F)
  
  if (x_axis)
    ifelse(x_labels, 
           axis(1, at=1:10, labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"), cex.axis = 1.3), 
           axis(1, at=1:10, labels = rep(" ", 10), cex.axis = 1.3))
    
  if (y_axis)
    ifelse(y_labels, 
           axis(2, at=seq(0.05, 0.15, 0.025), labels = seq(0.05, 0.15, 0.025), cex.axis = 1.3), 
           axis(2, at=seq(0.05, 0.15, 0.025), cex.axis = 1.3))
   
  
  # grid
  par(xpd=F)
  segments(1:10, rep(0,10), 1:10, 0.15, lty = 2, col = "grey")
  segments(rep(1,10), seq(0.05, 0.15, 0.025), rep(10,10), seq(0.05, 0.15, 0.025), lty = 2, col = "grey")
  
  # second digit synthetic 
  lines(c(table(extract_digit(votes_syn, 2))/length(extract_digit(votes_syn, 2))), type="o",
        lwd=2, pch=16, col="lightgrey")
  
  # second digit empirical
  lines(c(table(extract_digit(votes_emp, 2))/length(extract_digit(votes_emp, 2))), type="o",
        lwd=2, pch=16, col="orange")
  
  # second digit theory (pronounce again)
  lines(benford_expected(2), type="o",
        lwd=2, pch=16, col="black")
  
  
  #' ------------
  # Last digit
  #' ------------
  
  # last digit synthetic 
  lines(c(table(extract_digit(votes_syn, "last"))/length(extract_digit(votes_syn, "last"))), type="o",
        lwd=2, lty=2, col="lightgrey")
  
  # last digit empirical
  lines(c(table(extract_digit(votes_emp, "last"))/length(extract_digit(votes_emp, "last"))), type="o",
        lwd=2, lty=2, col="blue")
  
  # last digit theory 
  lines(benford_expected(3), type="o",
        lwd=2, lty=2, col="black")
  
  text(2.5, 0.14, title, cex=1.5, font=2)
}
