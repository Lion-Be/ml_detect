# --------------------------------------------------------------------------- #

# get expected relative frequencies by Benford's law and its generalizations
# e.g. benford(1)
# e.g. benford(2)
# e.g. benford(3)
benford <- function(position){
  
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
extract <- function(x, digit){
  
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

rel.freq <- function(x) {
  
  table(x) / sum(table(x))
  
}




