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
# e.g. extract(x, "last)
extract_digit <- function(x, digit){
  
  # commands for digit: 1, 2, "last"
  
  
  library(benford.analysis) # for extract.digits
  library(gsubfn) # for strapply
  
  if (digit == 1) {
    digits <- extract.digits(x, number.of.digits = 1)[,2]
  }
  
  if (digit == 2) {
    digits <- extract.digits(x, number.of.digits = 2)[,2]
    digits <- strapply(as.character(digits), ".", as.numeric)
    for (i in 1:length(digits))
      digits[i] <- digits[[i]][[2]]
    digits <- unlist(digits)
  }
  
  if (digit == "last") {
    digits <- as.numeric(substr(as.character(x),
                                nchar(as.character(x)),
                                nchar(as.character(x))))
  }
  
  return(digits)
  
}

# --------------------------------------------------------------------------- #


# get relative frequencies of digits from digit vector
# e.g. rel.freq(extract(x,2))

rel_freq <- function(x) {
  
  table(x) / sum(table(x))
  
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

plot_digits_1last <- function(votes_a, syn_data, title=" ", ylab = " ", xlab = " ", 
                              y_axis = F, y_labels = F, x_axis = F, x_labels = F) {

  # -------
  # 1BL
  # -------
  # first digit theory
  par(xpd=NA)
  plot(benford_expected(1), main="", ylab = ylab, xlab = xlab, 
       axes = F, bty="n", 
       type="o", lwd=1, ylim=c(0,0.35), labels=F)
  
  if (x_axis)
    ifelse(x_labels, 
           axis(1, at=1:10, labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")), 
           axis(1, at=1:10, labels = rep(" ", 10)))
    
  if (y_axis)
    ifelse(y_labels, 
           axis(2, at=seq(0, 0.35, 0.05), labels = seq(0, 0.35, 0.05)), 
           axis(2, at=seq(0, 0.35, 0.05)))
   
  
  #grid(10, 10)
  par(xpd=F)
  abline(h = seq(0, 0.35, 0.05), lty = 2, col = "grey")
  abline(v = 1:10, lty = 2, col = "grey")
  
  # first digit synthetic 
  for (elec in 1:10) 
    lines(c(NA, table(extract_digit(syn_data[[elec]]$votes_a, 1))/length(extract_digit(syn_data[[elec]]$votes_a, 1))), type="o",
          lwd=2, col="lightgrey")
  
  # first digit empirical
  lines(c(NA, table(extract_digit(votes_a, 1))/length(extract_digit(votes_a, 1))), type="o",
        lwd=2, col="orange")
  
  # first digit theory (pronounce again)
  lines(benford_expected(1), type="o",
        lwd=2, col="black")
  
  
  # ------------
  # Last digit
  # ------------
  
  # last digit synthetic 
  for (elec in 1:10) 
    lines(c(table(extract_digit(syn_data[[elec]]$votes_a, "last"))/length(extract_digit(syn_data[[elec]]$votes_a, "last"))), type="o",
          lwd=2, lty=2, col="lightgrey")
  
  # last digit empirical
  lines(c(table(extract_digit(votes_a, "last"))/length(extract_digit(votes_a, "last"))), type="o",
        lwd=2, lty=2, col="blue")
  
  # last digit theory 
  lines(benford_expected(3), type="o",
        lwd=2, lty=2, col="black")
  
  text(2.5, 0.34, title, cex=1.5, font=2)
}
