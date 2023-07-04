#' ---------------------------------------------
#  3. load/operationalize empirical data -------
#' --------------------------------------------- 

# in general: 
# entities with an electorate < 100 are excluded
# entities with NAs are excluded
# when turnout is higher than 1, is set to 1

  #' -------------------------------------------
  # 3.1 Venezuela, recall referendum 2004 ------
  #' -------------------------------------------
  
    #' ------------------------
    # empirical
    #' ------------------------
    
      ven04 <- read_excel("C:/Users/lionb/OneDrive/Desktop/lbehrens/PhD Electoral Fraud/Data/Venezuela/2004_referendum_revocatorio/referendum.xls",
                          skip = 34)
      ven04 <- as.data.frame(ven04)
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
  
    # find turnout, shareA vectors that resemble BL most closely
    opt_vectorsVEN <- gen_data(n_entities = nrow(ven04),
                               eligible = ven04$eligible,
                               turnout_emp = ven04$turnout, 
                               shareA_emp = ven04$share_no,
                               optimize_only = T)
    
    
    # generate synthetic data using these values
    ven04_syn <- gen_data(n_entities = nrow(ven04),
                          eligible = ven04$eligible,
                          fraud_type = "clean",
                          fraud_incA = 0,
                          fraud_extA = 0,
                          n_elections = 1, 
                          data_type = "full", 
                          turnout = opt_vectorsVEN$turnout, 
                          shareA = opt_vectorsVEN$shareA)


  #' -----------------------------------------------
  # 3.2.1 Russia, parliamentary election 2011 ------
  #' -----------------------------------------------
  
    #' ------------------------
    # empirical
    #' ------------------------
    
      ru11_1 <- read_excel("C:/Users/lionb/OneDrive/Desktop/lbehrens/PhD Electoral Fraud/Data/Russia2011_1of2.xls")
      ru11_2 <- read_excel("C:/Users/lionb/OneDrive/Desktop/lbehrens/PhD Electoral Fraud/Data/Russia2011_2of2.xls")
      ru11 <- as.data.frame(rbind(ru11_1, ru11_2))
      ru11$votes_all <- ru11$`Number of valid ballots` + as.numeric(ru11$`Number of invalid ballots`)
      ru11$turnout <- ru11$votes_all / ru11$`Number of voters included in voters list` 
      ru11$eligible <- ru11$`Number of voters included in voters list` 
      ru11$ur <- ru11$`United Russia`
      ru11$share_ur <- ru11$ur / ru11$`Number of valid ballots`
      ru11$communist <- ru11$`Communist Party`
      ru11$share_communist <- ru11$communist / ru11$`Number of valid ballots`
      
      # exclude units with an electorate < 100
      ru11 <- ru11[-which(ru11$`Number of voters included in voters list` < 100),]
      
      # exclude units with NAs in share_ur
      ru11 <- ru11[-which(is.na(ru11$share_ur)),]
    
    
    #' ------------------------
    # synthetic
    #' ------------------------
    
      # find turnout, shareA vectors that resemble BL most closely
      opt_vectorsRU11 <- gen_data(n_entities = nrow(ru11),
                                  eligible = ru11$eligible,
                                  turnout_emp = ru11$turnout, 
                                  shareA_emp = ru11$share_ur,
                                  optimize_only = T)
      
      # generate synthetic data using these values
      ru11_syn <- gen_data(n_entities = nrow(ru11),
                           eligible = ru11$eligible,
                           fraud_type = c("bbs", "switching"),
                           fraud_incA = 0.32,
                           fraud_extA = 0.10,
                           fraud_expo = 1.5,
                           fraud_roundA = T, 
                           share_roundA = 0.01,
                           n_elections = 1, 
                           data_type = "full", 
                           shareA_emp = ru11$share_ur,
                           turnout = opt_vectorsRU11$turnout, 
                           shareA = opt_vectorsRU11$shareA)
  
  
  #' ----------------------------------------------
  # 3.2.2 Russia, presidential election 2012 ------
  #' ----------------------------------------------
  
    #' ------------------------
    # empirical
    #' ------------------------
    
      ru12_1 <- read_excel("C:/Users/lionb/OneDrive/Desktop/lbehrens/PhD Electoral Fraud/Data/Russia2012_1of2.xls")
      ru12_2 <- read_excel("C:/Users/lionb/OneDrive/Desktop/lbehrens/PhD Electoral Fraud/Data/Russia2012_2of2.xls")
      ru12 <- as.data.frame(rbind(ru12_1, ru12_2))
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
    
      # find turnout, shareA vectors that resemble BL most closely
      opt_vectorsRU12 <- gen_data(n_entities = nrow(ru12),
                                  eligible = ru12$eligible,
                                  turnout_emp = ru12$turnout, 
                                  shareA_emp = ru12$share_putin,
                                  optimize_only = T)
      
      # generate synthetic data using these values
      ru12_syn <- gen_data(n_entities = nrow(ru12),
                           eligible = ru12$eligible,
                           fraud_type = c("bbs", "switching"),
                           fraud_incA = 0.35,
                           fraud_extA = 0.09,
                           fraud_expo = 1.5,
                           fraud_round = "A", 
                           share_round = 0.01,
                           n_elections = 1, 
                           data_type = "full", 
                           shareA_emp = ru12$share_putin,
                           turnout = opt_vectorsRU12$turnout, 
                           shareA = opt_vectorsRU12$shareA)


  #' ----------------------------------------------
  # 3.3 Uganda, presidential election 2011 ------
  #' ----------------------------------------------
  
    #' ------------------------
    # empirical
    #' ------------------------
    
      uga11 <- read_excel("C:/Users/lionb/OneDrive/Desktop/lbehrens/PhD Electoral Fraud/Data/Uganda2011.xls", col_names = F)
      uga11 <- as.data.frame(uga11)
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
    
      # find turnout, shareA vectors that resemble BL most closely
      opt_vectorsUGA <- gen_data(n_entities = nrow(uga11),
                                 eligible = uga11$eligible,
                                 turnout_emp = uga11$turnout, 
                                 shareA_emp = uga11$share_museveni,
                                 optimize_only = T)
      
      # generate synthetic data using these values
      uga11_syn <- gen_data(n_entities = nrow(uga11),
                            eligible = uga11$eligible,
                            fraud_type = c("bbs", "switching"),
                            fraud_incA = 0.49,
                            fraud_extA = 0.02,
                            fraud_expo = 1.5,
                            n_elections = 1, 
                            data_type = "full", 
                            shareA_emp = uga11$share_museveni,
                            turnout = opt_vectorsUGA$turnout, 
                            shareA = opt_vectorsUGA$shareA)
    

  #' ----------------------------------------------
  # 3.4 Austria, parliamentary election 2008 ------
  #' ----------------------------------------------
  
    #' ------------------------
    # empirical
    #' ------------------------
    
      aus08 <- read_excel("C:/Users/lionb/OneDrive/Desktop/lbehrens/PhD Electoral Fraud/Data/Austria2008_adjusted.xls")
      aus08 <- as.data.frame(aus08)
      aus08$turnout <- aus08$`Wahl-\nbeteil-\nigung\nin %` / 100
      aus08$eligible <- aus08$`Wahlbe- \nrechtigte`
      aus08$votes_all <- aus08$gültig
      aus08$share_spo <- aus08$`%...9`/ 100
      aus08$share_ovp <- aus08$`%...11`/ 100
      
      # exclude units with an electorate < 100
      aus08 <- aus08[-which(aus08[,3] < 100),]
    
    
    #' ------------------------
    # synthetic
    #' ------------------------
    
      # find turnout, shareA vectors that resemble BL most closely
      opt_vectorsAUS <- gen_data(n_entities = nrow(aus08),
                                 eligible = aus08$eligible,
                                 turnout_emp = aus08$turnout, 
                                 shareA_emp = aus08$share_spo,
                                 optimize_only = T)
    
      # generate synthetic data using these values
      aus08_syn <- gen_data(n_entities = nrow(aus08),
                            eligible = aus08$eligible,
                            fraud_type = c("bbs", "switching"),
                            fraud_incA = 0.6,
                            fraud_extA = 0.2,
                            fraud_incB = 0, 
                            fraud_extB = 0, 
                            fraud_roundA = T, 
                            share_roundA = 0.03, 
                            fraud_roundB = T, 
                            share_roundB = 0.03, 
                            n_elections = 1, 
                            data_type = "full", 
                            shareA_emp = aus08$share_spo,
                            turnout = opt_vectorsAUS$turnout, 
                            shareA = opt_vectorsAUS$shareA)
      
      
  #' --------------------------------------------------
  # 3.5 Spain, European Parliament election 2019 ------
  #' --------------------------------------------------
  
    #' ------------------------
    # empirical
    #' ------------------------
    
      esp19 <- read_excel("C:/Users/lionb/OneDrive/Desktop/lbehrens/PhD Electoral Fraud/Data/Spain_EP_2019.xlsx", skip = 5)
      esp19 <- as.data.frame(esp19)
      esp19$turnout <- esp19$`Total votantes` / esp19$`Total censo electoral` 
      esp19$eligible <- esp19$`Total votantes`
      esp19$votes_all <- esp19$`Votos válidos`
      esp19$share_psoe <- esp19$PSOE / esp19$`Votos válidos`
      esp19$share_pp <- esp19$PP / esp19$`Votos válidos`
      
      # exclude units with an electorate < 100
      esp19 <- esp19[-which(esp19$`Total censo electoral` < 100),]
    
    
    #' ------------------------
    # synthetic
    #' ------------------------
    
      # find turnout, shareA vectors that resemble BL most closely
      opt_vectorsESP <- gen_data(n_entities = nrow(esp19),
                                 eligible = esp19$eligible,
                                 turnout_emp = esp19$turnout, 
                                 shareA_emp = esp19$share_psoe,
                                 optimize_only = T)
    
      # generate synthetic data using these values
      esp19_syn <- gen_data(n_entities = nrow(esp19),
                            eligible = esp19$eligible,
                            fraud_type = "clean",
                            fraud_incA = 0,
                            fraud_extA = 0,
                            n_elections = 1, 
                            data_type = "full", 
                            turnout = opt_vectorsESP$turnout, 
                            shareA = opt_vectorsESP$shareA)

  #' ------------------------------------------
  # 3.6 Finland, municipal election 2017 ------
  #' ------------------------------------------
  
    #' ------------------------
    # empirical
    #' ------------------------
    
      fin17 <- read_excel("C:/Users/lionb/OneDrive/Desktop/lbehrens/PhD Electoral Fraud/Data/Finland_municipal_2017.xlsx")
      fin17 <- as.data.frame(fin17)
      
      fin17$`Persons entitled to vote` <- as.numeric(fin17$`Persons entitled to vote`)
      fin17$`Persons entitled to vote` <- as.numeric(gsub("[.]", "", fin17$`Persons entitled to vote`))
      
      fin17$`KOK Votes cast, total` <- as.numeric(fin17$`KOK Votes cast, total`)
      fin17$`KOK Votes cast, total` <- as.numeric(gsub("[.]", "", fin17$`KOK Votes cast, total`))
      
      fin17$`SDP Votes cast, total` <- as.numeric(fin17$`SDP Votes cast, total`)
      fin17$`SDP Votes cast, total` <- as.numeric(gsub("[.]", "", fin17$`SDP Votes cast, total`))
      
      fin17$votes_all <- as.numeric(fin17$`Persons who voted`)
      fin17$votes_all <- as.numeric(gsub("[.]", "", fin17$votes_all))
      
      fin17$turnout <- as.numeric(fin17$`Voting turnout`) / 100
      fin17$eligible <- as.numeric(fin17$`Persons entitled to vote`)
      fin17$kok_votes <- as.numeric(fin17$`KOK Votes cast, total`)
      fin17$sdp_votes <- as.numeric(fin17$`SDP Votes cast, total`)
      fin17$share_kok <- as.numeric(fin17$`KOK Proportion of all votes cast`) / 100
      fin17$share_sdp <- as.numeric(fin17$`SDP Proportion of all votes cast`) / 100
      
      # exclude units with NAs in share_kok or share_sdp
      fin17 <- fin17[-which(is.na(fin17$share_kok)),]
      fin17 <- fin17[-which(is.na(fin17$share_sdp)),]
      
  
    #' ------------------------
    # synthetic
    #' ------------------------
    
      # find turnout, shareA vectors that resemble BL most closely
      opt_vectorsFIN <- gen_data(n_entities = nrow(fin17),
                                 eligible = fin17$eligible,
                                 turnout_emp = fin17$turnout, 
                                 shareA_emp = fin17$share_kok,
                                 optimize_only = T)
      
      # generate synthetic data using these values
      fin17_syn <- gen_data(n_entities = nrow(fin17),
                            eligible = fin17$eligible,
                            fraud_type = "clean",
                            fraud_incA = 0,
                            fraud_extA = 0,
                            n_elections = 1, 
                            data_type = "full", 
                            turnout = opt_vectorsFIN$turnout, 
                            shareA = opt_vectorsFIN$shareA)
