#' --------------------------------------
#  5. apply ML to empirical cases -------
#' --------------------------------------

  ml_ru11 <- ml_detect(data = ru11, eligible = ru11$eligible, votes_a = ru11$ur, 
                       votes_b = ru11$communist, turnout_emp = ru11$turnout, 
                       shareA_emp = ru11$share_ur, shareB_emp = ru11$share_communist, 
                       models = "randomForest", parallel = F)
  
  ml_ru12 <- ml_detect(data = ru12, eligible = ru12$eligible, votes_a = ru12$putin, 
                       votes_b = ru12$zyuganov, turnout_emp = ru12$turnout, 
                       shareA_emp = ru12$share_putin, shareB_emp = ru12$share_zyuganov, 
                       models = "randomForest", parallel = F)  
  
  ml_uga11 <- ml_detect(data = uga11, eligible = uga11$eligible, votes_a = uga11$museveni, 
                        votes_b = uga11$besigye, turnout_emp = uga11$turnout, 
                        shareA_emp = uga11$share_museveni, shareB_emp = uga11$share_besigye, 
                        models = "randomForest", parallel = F)  
  
  ml_ven04 <- ml_detect(data = ven04, eligible = ven04$eligible, votes_a = ven04$rrp_no, 
                        votes_b = ven04$rrp_si, turnout_emp = ven04$turnout, 
                        shareA_emp = ven04$share_no, shareB_emp = ven04$share_si, 
                        models = "randomForest", parallel = F)  
  
  ml_aus08 <- ml_detect(data = aus08, eligible = aus08$eligible, votes_a = aus08$SPÖ, 
                          votes_b = aus08$ÖVP, turnout_emp = aus08$turnout, 
                          shareA_emp = aus08$share_spo, shareB_emp = aus08$share_ovp, 
                          models = "randomForest", parallel = F)
  
  ml_esp19 <- ml_detect(data = esp19, eligible = esp19$eligible, votes_a = esp19$PSOE, 
                        votes_b = esp19$PP, turnout_emp = esp19$turnout, 
                        shareA_emp = esp19$share_psoe, shareB_emp = esp19$share_pp, 
                        models = "randomForest", parallel = F)
  
  ml_fin17 <- ml_detect(data = fin17, eligible = fin17$eligible, votes_a = fin17$kok_votes, 
                        votes_b = fin17$sdp_votes, turnout_emp = fin17$turnout, 
                        shareA_emp = fin17$share_kok, shareB_emp = fin17$share_sdp, 
                        models = "randomForest", parallel = F)
  
  
  
  # generate all empirical feature values
  # -> nice for exploration which features (don't) work
  # -> I need features where fraud cases and clean cases have discrepant values
  ru11_feat <- gen_features(votes_a = ru11$ur, votes_b = ru11$communist, turnout = ru11$turnout, 
                            share_A = ru11$share_ur, share_B = ru11$share_communist, eligible = ru11$eligible)
  ru12_feat <- gen_features(votes_a = ru12$putin, votes_b = ru12$zyuganov, turnout = ru12$turnout, 
                            share_A = ru12$share_putin, share_B = ru12$share_zyuganov, eligible = ru12$eligible)
  uga11_feat <- gen_features(votes_a = uga11$museveni, votes_b = uga11$besigye, turnout = uga11$turnout, 
                             share_A = uga11$share_museveni, share_B = uga11$share_besigye, eligible = uga11$eligible)
  ven04_feat <- gen_features(votes_a = ven04$rrp_no, votes_b = ven04$rrp_si, turnout = ven04$turnout, 
                             share_A = ven04$share_no, share_B = ven04$share_si, eligible = ven04$eligible)
  aus08_feat <- gen_features(votes_a = aus08$SPÖ, votes_b = aus08$ÖVP, turnout = aus08$turnout, 
                             share_A = aus08$share_spo, share_B = aus08$share_ovp, eligible = aus08$eligible)
  esp19_feat <- gen_features(votes_a = esp19$PSOE, votes_b = esp19$PP, turnout = esp19$turnout, 
                             share_A = esp19$share_psoe, share_B = esp19$share_pp, eligible = esp19$eligible)
  fin17_feat <- gen_features(votes_a = fin17$kok_votes, votes_b = fin17$sdp_votes, turnout = fin17$turnout, 
                             share_A = fin17$share_kok, share_B = fin17$share_sdp, eligible = fin17$eligible)
  dat_features <- rbind(ru11_feat, ru12_feat, uga11_feat, ven04_feat, aus08_feat, 
                        esp19_feat, fin17_feat)
  rownames(dat_features) <- c("ru11_feat", "ru12_feat", "uga11_feat", "ven04_feat", "aus08_feat", 
                              "esp19_feat", "fin17_feat")
  
  
  # compare simulated data under empirical case to actual empirical values (like on paper)
  dev.off()
  par(mfrow=c(1,2))
  hist(ml_aus08$`simulated elections`$turnout_normdist[which(ml_aus08$`simulated elections`$fraud_type=="clean")], breaks=100, 
       main="Clean cases", xlab="turnout_normdist")
  abline(v=aus08_feat$turnout_normdist, col="darkgreen", lwd=3, lty=2)
  
  hist(ml_aus08$`simulated elections`$turnout_normdist[which(ml_aus08$`simulated elections`$fraud_type!="clean")], breaks=100,
       main="Fraud cases", xlab="turnout_normdist")
  abline(v=aus08_feat$turnout_normdist, col="darkgreen", lwd=3, lty=2)
  
  ###### diese Art von Plot und die zweite Art von Plot auf dem Papier als Funktion generalisiert definieren
  ###### für alle features erstellen. Evtl. fraud scenarios nicht bei 0, 0.01 beginnen. Sind zu ähnlich. 
  ###### Vielleicht bei 4% beginnen oder so. 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  