#' Probabilistic Detection of Election Fraud Using Machine Learnign Algorithms
#' Reproduction Material
#' Lion Behrens
#' -----------------------------------------------------------------------------

#' install and load packages
packages <- c("truncnorm", "e1071")

for (i in 1: length(packages)) {
  if (is.element(packages[i], installed.packages()[,1]) == FALSE) 
    install.packages(packages[i], dep = TRUE) 
}

lapply(packages, library, character.only = T)

#' --------------------------------------
#  --- 1. synthetic data creation -------
#' --------------------------------------

  #' ------------------------------------
  #  --- 1.1 clean data -----------------
  #' ------------------------------------

    


