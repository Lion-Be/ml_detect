#' --------------------------------------
#  5. apply ML to empirical cases -------
#' --------------------------------------


  ml_ven04 <- ml_detect(data = ven04, 
                        data_name = "ven04",
                        eligible = ven04$eligible, 
                        votes_a = ven04$rrp_no, 
                        votes_b = ven04$rrp_si, 
                        turnout_emp = ven04$turnout, 
                        shareA_emp = ven04$share_no, 
                        shareB_emp = ven04$share_si, 
                        fraud_incA = seq(0.01, 0.20, 0.01),
                        fraud_types = c("bbs", "switching", "rounding"),
                        models = "randomForest",  
                        ml_task="cont", 
                        parallel = F)  


  ml_uga11 <- ml_detect(data = uga11, 
                        data_name = "uga11",
                        eligible = uga11$eligible, 
                        votes_a = uga11$museveni, 
                        votes_b = uga11$besigye, 
                        turnout_emp = uga11$turnout, 
                        shareA_emp = uga11$share_museveni, 
                        shareB_emp = uga11$share_besigye, 
                        fraud_incA = seq(0.01, 0.20, 0.01),
                        fraud_types = c("bbs", "switching", "rounding"),
                        models = "randomForest", 
                        ml_task="cont", 
                        parallel = F)  
  save(ml_uga11, file="ml_uga11.RData")
  uga11_varImp <- caret::varImp(ml_uga11$`final models`[[1]][[1]])

  
 
  ml_aus08 <- ml_detect(data = aus08, 
                        data_name = "aus08",
                        eligible = aus08$eligible, 
                        votes_a = aus08$SPÖ, 
                        votes_b = aus08$ÖVP, 
                        turnout_emp = aus08$turnout, 
                        shareA_emp = aus08$share_spo, 
                        shareB_emp = aus08$share_ovp, 
                        fraud_incA = seq(0.01, 0.20, 0.01),
                        fraud_types = c("bbs", "switching", "rounding"),
                        models = c("randomForest"),  
                        ml_task=c("cont"), 
                        parallel = F)
  save(ml_aus08, file="ml_aus08.RData")
  aus08_varImp <- caret::varImp(ml_aus08$`final models`[[1]][[1]])

  
  ml_esp19 <- ml_detect(data = esp19, 
                        data_name = "esp19",
                        eligible = esp19$eligible, 
                        votes_a = esp19$PSOE, 
                        votes_b = esp19$PP, 
                        turnout_emp = esp19$turnout, 
                        shareA_emp = esp19$share_psoe, 
                        shareB_emp = esp19$share_pp, 
                        fraud_incA = seq(0.01, 0.20, 0.01),
                        fraud_types = c("bbs", "switching", "rounding"),
                        models = "randomForest",  
                        ml_task="cont", 
                        parallel = F)
  save(ml_esp19, file="ml_esp19.RData")
  esp19_varImp <- caret::varImp(ml_esp19$`final models`[[1]][[1]])

  
  
  ml_fin17 <- ml_detect(data = fin17, 
                        data_name = "fin17", 
                        eligible = fin17$eligible, 
                        votes_a = fin17$kok_votes, 
                        votes_b = fin17$sdp_votes, 
                        turnout_emp = fin17$turnout, 
                        shareA_emp = fin17$share_kok, 
                        shareB_emp = fin17$share_sdp, 
                        fraud_incA = seq(0.01, 0.20, 0.01),
                        fraud_types = c("bbs", "switching", "rounding"),
                        models = "randomForest",  
                        ml_task="cont", parallel = F)
  save(ml_fin17, file="ml_fin17.RData")
  fin17_varImp <- caret::varImp(ml_fin17$`final models`[[1]][[1]])

  

  ml_ru11 <- ml_detect(data = ru11, 
                       data_name = "ru11",
                       eligible = ru11$eligible, 
                       votes_a = ru11$ur, 
                       votes_b = ru11$communist, 
                       turnout_emp = ru11$turnout, 
                       shareA_emp = ru11$share_ur, 
                       shareB_emp = ru11$share_communist, 
                       fraud_incA = seq(0.01, 0.20, 0.01),
                       fraud_types = c("bbs", "switching", "rounding"),
                       models = c("randomForest"), 
                       ml_task=c("cont"), 
                       parallel = F)
  save(ml_ru11, file="ml_ru11.RData")
  ru11_varImp <- caret::varImp(ml_ru11$`final models`[[1]][[1]])

  
  ml_ru12 <- ml_detect(data = ru12, 
                       data_name = "ru12",
                       eligible = ru12$eligible, 
                       votes_a = ru12$putin, 
                       votes_b = ru12$zyuganov, 
                       turnout_emp = ru12$turnout, 
                       shareA_emp = ru12$share_putin, 
                       shareB_emp = ru12$share_zyuganov, 
                       fraud_incA = seq(0.01, 0.20, 0.01),
                       fraud_types = c("bbs", "switching", "rounding"),
                       models = "randomForest", 
                       ml_task="cont", 
                       parallel = F)  
  save(ml_ru12, file="ml_ru12.RData")
  ru12_varImp <- caret::varImp(ml_ru12$`final models`[[1]][[1]])
  
  
  plot_varImp <- function(x, y_lab = "Feature (Forensic Indicator)", x_lab = "Variable Importance", 
                          plot_title = "Uganda 2011") {
    
    imp_data <- round(as.data.frame(x$importance[1:15,]), digits = 2)
    colnames(imp_data)[1] <- "varImp"
    imp_data$feature <- rownames(as.data.frame(x$importance))[1:15]
    
    p <- ggplot(data=imp_data, aes(x=feature, y=varImp)) +
      geom_bar(stat="identity", fill="steelblue") +
      geom_text(aes(label=varImp), hjust = -0.1, color="black", size=3.5)+
      theme_minimal(base_size=15) + ylab(x_lab) + xlab(y_lab) + ggtitle(plot_title) +
      coord_flip()
    
    return(p)
    
  }
  
  p_uga11 <- plot_varImp(uga11_varImp, y_lab = "", x_lab = "", plot_title = "Uganda 2011")
  p_aus08 <- plot_varImp(aus08_varImp, x_lab = "", y_lab = "", plot_title = "Austria 2008")
  p_esp19 <- plot_varImp(esp19_varImp, x_lab = "", y_lab = "Feature (Forensic Indicator)", plot_title = "Spain 2019")
  p_fin17 <- plot_varImp(fin17_varImp, y_lab = "Feature (Forensic Indicator)", 
                         x_lab = "", plot_title = "Finland 2017")
  p_ru11 <- plot_varImp(ru11_varImp, y_lab = "Feature (Forensic Indicator)", 
                        "Variable Importance", plot_title = "Russia 2011")
  p_ru12 <- plot_varImp(ru12_varImp, x_lab = "Variable Importance", 
                        y_lab = "", plot_title = "Russia 2012")
  
  library(gridExtra)
  grid.arrange(p_fin17, p_aus08, p_esp19,
               p_uga11, p_ru11, p_ru12,  
               ncol=2)
  
    
  
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
  
  
  for (col in 9:ncol(ml_aus08$`simulated elections`)) {
    
    iter <- col - 8
    
    par(mfrow=c(1,3))
    hist(ml_aus08$`simulated elections`[which(ml_aus08$`simulated elections`$fraud_type=="clean"), col], breaks=100, 
         main="Clean cases", xlab=colnames(ml_aus08$`simulated elections`)[col])
    abline(v=aus08_feat[,iter], col="darkgreen", lwd=3, lty=2)
    
    hist(ml_aus08$`simulated elections`[which(ml_aus08$`simulated elections`$fraud_type!="clean"), col], breaks=100,
         main="Fraud cases", xlab=colnames(ml_aus08$`simulated elections`)[col])
    abline(v=aus08_feat[,iter], col="darkgreen", lwd=3, lty=2)
    
    plot(ml_aus08$`simulated elections`[,"perc_frauded"], 
         ml_aus08$`simulated elections`[,col], 
         xlab="perc_frauded", ylab=colnames(ml_aus08$`simulated elections`)[col])
    
  }
  
  
  
  
  ###### diese Art von Plot und die zweite Art von Plot auf dem Papier als Funktion generalisiert definieren
  ###### für alle features erstellen. Evtl. fraud scenarios nicht bei 0, 0.01 beginnen. Sind zu ähnlich. 
  ###### Vielleicht bei 4% beginnen oder so. 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  