#' -----------------------------------------------------------------------
# 1. generate synthetic data using optimized turnout, shareA vectors -----
#' -----------------------------------------------------------------------

  ven04_syn <- gen_data(n_entities = nrow(ven04),
                        eligible = ven04$eligible,
                        turnout_mean = mean(ven04$turnout), 
                        turnout_sd = sd(ven04$turnout), 
                        partyA_mean = mean(ven04$share_no, na.rm=T), 
                        partyA_sd = sd(ven04$share_no, na.rm=T), 
                        partyB_mean = mean(ven04$share_si, na.rm=T), 
                        partyB_sd = sd(ven04$share_si, na.rm=T),
                        fraud_type = "clean",
                        fraud_incA = 0,
                        fraud_extA = 0,
                        n_elections = 1, 
                        data_type = "full", 
                        turnout = opt_vectorsVEN$turnout, 
                        shareA = opt_vectorsVEN$shareA)
  
  uga11_syn <- gen_data(n_entities = nrow(uga11),
                        eligible = uga11$eligible,
                        turnout_mean = median(uga11$turnout), 
                        turnout_sd = sd(uga11$turnout[which(uga11$turnout < median(uga11$turnout))]), 
                        partyA_mean = mean(uga11$share_museveni), 
                        partyA_sd = sd(uga11$share_museveni[which(uga11$share_museveni < median(uga11$share_museveni))]), 
                        partyB_mean = mean(uga11$share_besigye), 
                        partyB_sd = sd(uga11$share_besigye),
                        fraud_type = c("bbs", "switching"),
                        fraud_incA = 0.6,
                        fraud_extA = 0.05,
                        n_elections = 1, 
                        data_type = "full", 
                        turnout = opt_vectorsUGA$turnout, 
                        shareA = opt_vectorsUGA$shareA)
  
  ru12_syn <- gen_data(n_entities = nrow(ru12),
                       eligible = ru12$eligible,
                       turnout_mean = median(ru12$turnout), 
                       turnout_sd = sd(ru12$turnout[which(ru12$turnout < median(ru12$turnout))]), 
                       partyA_mean = median(ru12$share_putin), 
                       partyA_sd = sd(ru12$share_putin[which(ru12$share_putin < median(ru12$share_putin))]), 
                       partyB_mean = mean(ru12$share_zyuganov), 
                       partyB_sd = sd(ru12$share_zyuganov),
                       fraud_type = c("bbs", "switching"),
                       fraud_incA = 0.51,
                       fraud_extA = 0.07,
                       n_elections = 1, 
                       data_type = "full", 
                       turnout = opt_vectorsRU$turnout, 
                       shareA = opt_vectorsRU$shareA)
  
  aus08_syn <- gen_data(n_entities = nrow(aus08),
                        eligible = aus08$eligible,
                        turnout_mean = mean(aus08$turnout), 
                        turnout_sd = sd(aus08$turnout), 
                        partyA_mean = mean(aus08$share_spo), 
                        partyA_sd = sd(aus08$share_spo), 
                        partyB_mean = mean(aus08$share_ovp), 
                        partyB_sd = sd(aus08$share_ovp),
                        fraud_type = "clean",
                        fraud_incA = 0,
                        fraud_extA = 0,
                        n_elections = 1, 
                        data_type = "full", 
                        turnout = opt_vectorsAUS$turnout, 
                        shareA = opt_vectorsAUS$shareA)
  
  esp19_syn <- gen_data(n_entities = nrow(esp19),
                        eligible = esp19$eligible,
                        turnout_mean = mean(esp19$turnout), 
                        turnout_sd = sd(esp19$turnout), 
                        partyA_mean = mean(esp19$share_psoe), 
                        partyA_sd = sd(esp19$share_psoe), 
                        partyB_mean = mean(esp19$share_pp), 
                        partyB_sd = sd(esp19$share_pp),
                        fraud_type = "clean",
                        fraud_incA = 0,
                        fraud_extA = 0,
                        n_elections = 1, 
                        data_type = "full", 
                        turnout = opt_vectorsESP$turnout, 
                        shareA = opt_vectorsESP$shareA)
  
  fin17_syn <- gen_data(n_entities = nrow(fin17),
                        eligible = fin17$eligible,
                        turnout_mean = mean(fin17$turnout), 
                        turnout_sd = sd(fin17$turnout), 
                        partyA_mean = mean(fin17$share_kok), 
                        partyA_sd = sd(fin17$share_kok), 
                        partyB_mean = mean(fin17$share_sdp), 
                        partyB_sd = sd(fin17$share_sdp),
                        fraud_type = "clean",
                        fraud_incA = 0,
                        fraud_extA = 0,
                        n_elections = 1, 
                        data_type = "full", 
                        turnout = opt_vectorsFIN$turnout, 
                        shareA = opt_vectorsFIN$shareA)



#' --------------------------
# 2. plot 2d histograms -----
#' --------------------------

  par(mfrow = c(2, 2),     # 2x2 layout
      oma = c(2, 2, 0, 0), # two rows of text at the outer left and bottom margin
      mar = c(2.8, 2.8, 2.8, 1), # space for one row of text at ticks and to separate plots
      mgp = c(2, 1, 0),
      xpd = NA)  
  
  rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
  r <- rf(30)
  
  # empty image with baseline color
  x <- list()
  x$x <- seq(0,10,1)
  x$y <- seq(0,10,1)
  x$z <- matrix(rep(0,100), nrow=10, ncol=10)
  
  #' -----------------------
  # Uganda 2011
  #' -----------------------
  
    # empirical
    image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), ylab="\\% Votes for Winner", xaxt="n")    
    k <- kde2d(uga11$turnout, uga11$share_museveni, n=50)
    image(k, col=r, xlim=c(0,1), ylim=c(0,1), xaxt="n", add=T)
    text(0.23, 0.95, "Uganda 2011, Empirical", col="white")
    
    # synthetic
    par(mar = c(2.8, 1.5, 2.8, 2.3))
    image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), yaxt="n", xaxt="n")    
    k <- kde2d(uga11_syn$turnout, uga11_syn$shareA, n=50)
    image(k, col=r, xlim=c(0,1), ylim=c(0,1), yaxt="n", xaxt="n", add=T)
    text(0.23, 0.95, "Uganda 2011, Synthetic", col="white")
  
  #' -----------------------
  # Russia 2012
  #' -----------------------
  
    # empirical
    par(mar = c(2.8, 2.8, 1, 1))
    image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), ylab="\\%Votes for Winner", xlab="\\%Turnout")    
    k <- kde2d(ru12$turnout, ru12$share_putin, n=50)
    image(k, col=r, xlim=c(0,1), ylim=c(0,1), add=T)
    text(0.23, 0.95, "Russia 2012, Empirical", col="white")
    
    # synthetic
    par(mar = c(2.8, 1.5, 1, 2.3))
    image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), yaxt="n", xlab="\\%Turnout")    
    k <- kde2d(ru12_syn$turnout, ru12_syn$shareA, n=50)
    image(k, col=r, xlim=c(0,1), ylim=c(0,1), yaxt="n", add=T)
    text(0.23, 0.95, "Russia 2012, Synthetic", col="white")


#' --------------------------------------
# 3. plot univariate turnout curves -----
#' --------------------------------------

  dev.off()
  par(mfrow = c(2, 3))
  
  # Russia 2012
  d_ru12 <- density(ru12$turnout, bw=0.01)
  d_ru12_syn <- density(ru12_syn$turnout, bw=0.01)
  
  plot(d_ru12, col = "blue", lwd=1.5, main = "Russia 2012", xlab = "% Turnout",
       xlim = c(min(d_ru12$x, d_ru12_syn$x), c(max(d_ru12$x, d_ru12_syn$x))),  
       ylim = c(min(d_ru12$y, d_ru12_syn$y), c(max(d_ru12$y, d_ru12_syn$y)))) 
  lines(d_ru12_syn, col = "red", lwd=1.5)
    
  # Uganda 2011
  d_uga11 <- density(uga11$turnout, bw=0.01)
  d_uga11_syn <- density(uga11_syn$turnout, bw=0.01)
    
  plot(d_uga11, col = "blue", lwd=1.5, main = "Uganda 2011", xlab = "% Turnout", 
       xlim = c(min(d_uga11$x, d_uga11_syn$x), c(max(d_uga11$x, d_uga11_syn$x))),  
       ylim = c(min(d_uga11$y, d_uga11_syn$y), c(max(d_uga11$y, d_uga11_syn$y))))  
  lines(d_uga11_syn, col = "red", lwd=1.5)
  
  # Venezuela 2004
  d_ven04 <- density(ven04$turnout, bw=0.01)
  d_ven04_syn <- density(ven04_syn$turnout, bw=0.01)
  
  plot(d_ven04, col = "blue", lwd=1.5, main = "Venezuela 2004", xlab = "% Turnout", 
       xlim = c(min(d_ven04$x, d_ven04_syn$x), c(max(d_ven04$x, d_ven04_syn$x))),  
       ylim = c(min(d_ven04$y, d_ven04_syn$y), c(max(d_ven04$y, d_ven04_syn$y))))  
  lines(d_ven04_syn, col = "red", lwd=1.5)
  
  # Austria 2008
  d_aus08 <- density(aus08$turnout, bw=0.01)
  d_aus08_syn <- density(aus08_syn$turnout, bw=0.01)
  
  plot(d_aus08, col = "blue", lwd=1.5, main = "Austria 2008", xlab = "% Turnout", 
       xlim = c(min(d_aus08$x, d_aus08_syn$x), c(max(d_aus08$x, d_aus08_syn$x))),  
       ylim = c(min(d_aus08$y, d_aus08_syn$y), c(max(d_aus08$y, d_aus08_syn$y))))  
  lines(d_aus08_syn, col = "red", lwd=1.5)
  
  
  
 

#' -------------------------------------
# 4. plot univariate shareA curves -----
#' -------------------------------------
  
  par(mfrow = c(1, 2))
  
  # Uganda 2011
  d_uga11 <- density(uga11$share_museveni, bw=0.01)
  d_uga11_syn <- density(uga11_syn$shareA, bw=0.01)
  
  plot(d_uga11, col = "blue", lwd=1.5, main = "Uganda 2011", xlab = "% Winner", 
       xlim = c(min(d_uga11$x, d_uga11_syn$x), c(max(d_uga11$x, d_uga11_syn$x))),  
       ylim = c(min(d_uga11$y, d_uga11_syn$y), c(max(d_uga11$y, d_uga11_syn$y))))  
  lines(d_uga11_syn, col = "red", lwd=1.5)
  
  # Russia 2012
  d_ru12 <- density(ru12$share_putin, bw=0.01)
  d_ru12_syn <- density(ru12_syn$shareA, bw=0.01)
  
  plot(d_ru12, col = "blue", lwd=1.5, main = "Russia 2012", xlab = "% Winner",
       xlim = c(min(d_ru12$x, d_ru12_syn$x), c(max(d_ru12$x, d_ru12_syn$x))),  
       ylim = c(min(d_ru12$y, d_ru12_syn$y), c(max(d_ru12$y, d_ru12_syn$y)))) 
  lines(d_ru12_syn, col = "red", lwd=1.5)


#' -----------------------------------------------------
# 5. plot cumulative shareA curves against turnout -----
#' -----------------------------------------------------
  
  ven04_cum <- gen_cumdata(ven04$rrp_no, ven04$turnout, ven04$votes_all)
  ven04_syn_cum <- gen_cumdata(ven04_syn$votes_a, ven04_syn$turnout, ven04_syn$votes_total)
  
  ru12_cum <- gen_cumdata(ru12$putin, ru12$turnout, ru12$votes_all)
  ru12_syn_cum <- gen_cumdata(ru12_syn$votes_a, ru12_syn$turnout, ru12_syn$votes_total)
  
  uga11_cum <- gen_cumdata(uga11$museveni, uga11$turnout, uga11$votes_all)
  uga11_syn_cum <- gen_cumdata(uga11_syn$votes_a, uga11_syn$turnout, uga11_syn$votes_total)
  
  aus08_cum <- gen_cumdata(aus08$SPÖ, aus08$turnout, aus08$votes_all)
  aus08_syn_cum <- gen_cumdata(aus08_syn$votes_a, aus08_syn$turnout, aus08_syn$votes_total)
  
  esp19_cum <- gen_cumdata(esp19$PSOE, esp19$turnout, esp19$votes_all)
  esp19_syn_cum <- gen_cumdata(esp19_syn$votes_a, esp19_syn$turnout, esp19_syn$votes_total)
  
  fin17_cum <- gen_cumdata(fin17$kok_votes, fin17$turnout, fin17$votes_all)
  fin17_syn_cum <- gen_cumdata(fin17_syn$votes_a, fin17_syn$turnout,fin17_syn$votes_total)
  
  
  dev.off()
  par(mfrow = c(2, 3))
  
    plot(ru12_cum$turnout, ru12_cum$share_cum, type="l", col = "blue", ylim=c(0,1))
    lines(ru12_syn_cum$turnout, ru12_syn_cum$share_cum, type="l", col = "red", xlim=c(0,1))
    
    plot(uga11_cum$turnout, uga11_cum$share_cum, type="l", col = "blue", ylim=c(0,1))
    lines(uga11_syn_cum$turnout, uga11_syn_cum$share_cum, type="l", col = "red", xlim=c(0,1))
    
    plot(ven04_cum$turnout, ven04_cum$share_cum, type="l", col = "blue", ylim=c(0,1))
    lines(ven04_syn_cum$turnout, ven04_syn_cum$share_cum, type="l", col = "red", xlim=c(0,1))
    
    plot(aus08_cum$turnout, aus08_cum$share_cum, type="l", col = "blue", ylim=c(0,1))
    lines(aus08_syn_cum$turnout, aus08_syn_cum$share_cum, type="l", col = "red", xlim=c(0,1))
    
    plot(esp19_cum$turnout, esp19_cum$share_cum, type="l", col = "blue", ylim=c(0,1))
    lines(esp19_syn_cum$turnout, esp19_syn_cum$share_cum, type="l", col = "red", xlim=c(0,1))
    
    plot(fin17_cum$turnout, fin17_cum$share_cum, type="l", col = "blue", ylim=c(0,1))
    lines(fin17_syn_cum$turnout, fin17_syn_cum$share_cum, type="l", col = "red", xlim=c(0,1))
   
    
  
#' -------------------
# 6. plot digits -----
#' ------------------- 








