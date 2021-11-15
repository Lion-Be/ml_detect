#' ---------------------------------------------------------
#  4. visualize/compare empirical and synthetic data -------
#' ---------------------------------------------------------

  #' ---------------------------
  # 4.1 plot 2d histograms -----
  #' ---------------------------
  
    #' --------------------------
    # 4.1.1 fraud cases ---------
    #' --------------------------
    
      par(mfrow = c(3, 2),     # 2x2 layout
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
      # Russia 2011
      #' -----------------------
      
        # empirical
        par(mar = c(2.8, 2.8, 1, 1))
        image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), ylab="\\%Votes for Winner", xlab="\\%Turnout")    
        k <- kde2d(ru11$turnout, ru11$share_ur, n=50)
        image(k, col=r, xlim=c(0,1), ylim=c(0,1), add=T)
        text(0.23, 0.95, "Russia 2011, Empirical", col="white")
        
        # synthetic
        par(mar = c(2.8, 1.5, 1, 2.3))
        image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), yaxt="n", xlab="\\%Turnout")    
        k <- kde2d(ru11_syn$turnout, ru11_syn$shareA, n=50)
        image(k, col=r, xlim=c(0,1), ylim=c(0,1), yaxt="n", add=T)
        text(0.23, 0.95, "Russia 2011, Synthetic", col="white")
        
        
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

      
    #' --------------------------
    # 4.1.2 clean cases ---------
    #' --------------------------
      
      par(mfrow = c(3, 2),     # 2x2 layout
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
      # Austria 2008
      #' -----------------------
      
      # empirical
      image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), ylab="\\% Votes for Winner", xaxt="n")    
      k <- kde2d(aus08$turnout, aus08$share_spo, n=50)
      image(k, col=r, xlim=c(0,1), ylim=c(0,1), xaxt="n", add=T)
      text(0.23, 0.95, "Austria 2008, Empirical", col="white")
      
      # synthetic
      par(mar = c(2.8, 1.5, 2.8, 2.3))
      image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), yaxt="n", xaxt="n")    
      k <- kde2d(aus08_syn$turnout, aus08_syn$shareA, n=50)
      image(k, col=r, xlim=c(0,1), ylim=c(0,1), yaxt="n", xaxt="n", add=T)
      text(0.23, 0.95, "Austria 2008, Synthetic", col="white")
      
      
      #' -----------------------
      # Spain 2019
      #' -----------------------
      
      # empirical
      par(mar = c(2.8, 2.8, 1, 1))
      image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), ylab="\\%Votes for Winner", xlab="\\%Turnout")    
      k <- kde2d(esp19$turnout, esp19$share_psoe, n=50)
      image(k, col=r, xlim=c(0,1), ylim=c(0,1), add=T)
      text(0.23, 0.95, "Spain 2019, Empirical", col="white")
      
      # synthetic
      par(mar = c(2.8, 1.5, 1, 2.3))
      image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), yaxt="n", xlab="\\%Turnout")    
      k <- kde2d(esp19_syn$turnout, esp19_syn$shareA, n=50)
      image(k, col=r, xlim=c(0,1), ylim=c(0,1), yaxt="n", add=T)
      text(0.23, 0.95, "Spain 2019, Synthetic", col="white")
      
      
      #' -----------------------
      # Finland 2017
      #' -----------------------
      
      # empirical
      par(mar = c(2.8, 2.8, 1, 1))
      image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), ylab="\\%Votes for Winner", xlab="\\%Turnout")    
      k <- kde2d(fin17$turnout, fin17$share_kok, n=50)
      image(k, col=r, xlim=c(0,1), ylim=c(0,1), add=T)
      text(0.23, 0.95, "Finland 2017, Empirical", col="white")
      
      # synthetic
      par(mar = c(2.8, 1.5, 1, 2.3))
      image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), yaxt="n", xlab="\\%Turnout")    
      k <- kde2d(fin17_syn$turnout, fin17_syn$shareA, n=50)
      image(k, col=r, xlim=c(0,1), ylim=c(0,1), yaxt="n", add=T)
      text(0.23, 0.95, "Finland 2017, Synthetic", col="white")
      
    
  #' ----------------------------------------
  # 4.2. plot univariate turnout curves -----
  #' ----------------------------------------
  
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
    
    # Spain 2019
    d_esp19 <- density(esp19$turnout, bw=0.01)
    d_esp19_syn <- density(esp19_syn$turnout, bw=0.01)
    
    plot(d_esp19, col = "blue", lwd=1.5, main = "Spain 2019", xlab = "% Turnout", 
         xlim = c(min(d_esp19$x, d_esp19_syn$x), c(max(d_esp19$x, d_esp19_syn$x))),  
         ylim = c(min(d_esp19$y, d_esp19_syn$y), c(max(d_esp19$y, d_esp19_syn$y))))  
    lines(d_esp19_syn, col = "red", lwd=1.5)
    
    # Finland 2017
    d_fin17 <- density(fin17$turnout, bw=0.01)
    d_fin17_syn <- density(fin17_syn$turnout, bw=0.01)
    
    plot(d_fin17, col = "blue", lwd=1.5, main = "Finland 2017", xlab = "% Turnout", 
         xlim = c(min(d_fin17$x, d_fin17_syn$x), c(max(d_fin17$x, d_fin17_syn$x))),  
         ylim = c(min(d_fin17$y, d_fin17_syn$y), c(max(d_fin17$y, d_fin17_syn$y))))  
    lines(d_fin17_syn, col = "red", lwd=1.5)
   

  
  #' --------------------------------------
  # 4.3 plot univariate shareA curves -----
  #' --------------------------------------
    
    dev.off()
    par(mfrow = c(2, 3))
    
    # Russia 2012
    d_ru12 <- density(ru12$share_putin, bw=0.01)
    d_ru12_syn <- density(ru12_syn$shareA, bw=0.01)
    
    plot(d_ru12, col = "blue", lwd=1.5, main = "Russia 2012", xlab = "% Winner",
         xlim = c(min(d_ru12$x, d_ru12_syn$x), c(max(d_ru12$x, d_ru12_syn$x))),  
         ylim = c(min(d_ru12$y, d_ru12_syn$y), c(max(d_ru12$y, d_ru12_syn$y)))) 
    lines(d_ru12_syn, col = "red", lwd=1.5)
    
    # Uganda 2011
    d_uga11 <- density(uga11$share_museveni, bw=0.01)
    d_uga11_syn <- density(uga11_syn$shareA, bw=0.01)
    
    plot(d_uga11, col = "blue", lwd=1.5, main = "Uganda 2011", xlab = "% Winner", 
         xlim = c(min(d_uga11$x, d_uga11_syn$x), c(max(d_uga11$x, d_uga11_syn$x))),  
         ylim = c(min(d_uga11$y, d_uga11_syn$y), c(max(d_uga11$y, d_uga11_syn$y))))  
    lines(d_uga11_syn, col = "red", lwd=1.5)
    
    # Venezuela 2004
    d_ven04 <- density(ven04$share_no, bw=0.01)
    d_ven04_syn <- density(ven04_syn$shareA, bw=0.01)
    
    plot(d_ven04, col = "blue", lwd=1.5, main = "Venezuela 2004", xlab = "% Winner", 
         xlim = c(min(d_ven04$x, d_ven04_syn$x), c(max(d_ven04$x, d_ven04_syn$x))),  
         ylim = c(min(d_ven04$y, d_ven04_syn$y), c(max(d_ven04$y, d_ven04_syn$y))))  
    lines(d_ven04_syn, col = "red", lwd=1.5)
    
    # Austria 2008
    d_aus08 <- density(aus08$share_spo, bw=0.01)
    d_aus08_syn <- density(aus08_syn$shareA, bw=0.01)
    
    plot(d_aus08, col = "blue", lwd=1.5, main = "Austria 2008", xlab = "% Winner", 
         xlim = c(min(d_aus08$x, d_aus08_syn$x), c(max(d_aus08$x, d_aus08_syn$x))),  
         ylim = c(min(d_aus08$y, d_aus08_syn$y), c(max(d_aus08$y, d_aus08_syn$y))))  
    lines(d_aus08_syn, col = "red", lwd=1.5)
    
    # Spain 2019
    d_esp19 <- density(esp19$share_psoe, bw=0.01)
    d_esp19_syn <- density(esp19_syn$shareA, bw=0.01)
    
    plot(d_esp19, col = "blue", lwd=1.5, main = "Spain 2019", xlab = "% Winner", 
         xlim = c(min(d_esp19$x, d_esp19_syn$x), c(max(d_esp19$x, d_esp19_syn$x))),  
         ylim = c(min(d_esp19$y, d_esp19_syn$y), c(max(d_esp19$y, d_esp19_syn$y))))  
    lines(d_esp19_syn, col = "red", lwd=1.5)
    
    # Finland 2017
    d_fin17 <- density(fin17$share_kok, bw=0.01)
    d_fin17_syn <- density(fin17_syn$shareA, bw=0.01)
    
    plot(d_fin17, col = "blue", lwd=1.5, main = "Finland 2017", xlab = "% Winner", 
         xlim = c(min(d_fin17$x, d_fin17_syn$x), c(max(d_fin17$x, d_fin17_syn$x))),  
         ylim = c(min(d_fin17$y, d_fin17_syn$y), c(max(d_fin17$y, d_fin17_syn$y))))  
    lines(d_fin17_syn, col = "red", lwd=1.5)
    
  

  #' -------------------------------------------------------
  # 4.4. plot cumulative shareA curves against turnout -----
  #' -------------------------------------------------------
    
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
     
    
  
  #' --------------------
  # 4.5 plot digits -----
  #' -------------------- 
  
    #' -----------------------
    # 4.5.1 winner's party  
    #' -----------------------
      
      par(mfrow = c(2, 3),     # 2x3 layout
          oma = c(2, 2, 0, 0), # two rows of text at the outer left and bottom margin
          mar = c(0, 2, 1, 0), # space for one row of text at ticks and to separate plots
          mgp = c(2, 1, 0),
          xpd = F)  
      
      # Russia 2012
      plot_digits_2last(ru12$putin, ru12_syn$votes_a, title = "Russia 2012", ylab = "Relative Frequency",
                        y_axis = T, x_axis = F, y_labels = T)
      
      text(3.5, 0.13, "Second Digit", cex=1.5)
      text(2.1, 0.07, "Last Digit", cex=1.5)
      
      
      # Uganda 2011
      par(mar = c(0, 1, 1, 0))
      plot_digits_2last(uga11$museveni, uga11_syn$votes_a, title = "Uganda 2011",
                        y_axis = F, x_axis = F, x_labels = T)
      
      # Venezuela 2004
      par(mar = c(0, 1, 1, 1))
      plot_digits_2last(ven04$rrp_no, ven04_syn$votes_a, title = "Venezuela 2004",
                        y_axis = F, y_labels = F, x_axis = F, x_labels = F)
      
      # Austria 2008
      par(mar = c(2, 2, 1, 0))
      plot_digits_2last(aus08$SPÖ, aus08_syn$votes_a, title = "Austria 2008", xlab = "Number",
                        ylab = "Relative Frequency", y_axis = T, y_labels = T, 
                        x_axis = T, x_labels = T)
      
      # Spain 2019
      par(mar = c(2, 1, 1, 0))
      plot_digits_2last(esp19$PSOE, esp19_syn$votes_a, title = "Spain 2019", xlab = "Number",
                        y_axis = F, x_axis = T, x_labels = T)
      
      # Finland 2017
      par(mar = c(2, 1, 1, 1))
      plot_digits_2last(fin17$`KOK Votes cast, total`, fin17_syn$votes_a, title = "Finland 2017", xlab = "Number",
                        y_axis = F, x_axis = T, x_labels = T)
      
      box("outer")


    #' -----------------------
    # 4.5.2 loosers's party  
    #' -----------------------
    
      par(mfrow = c(2, 3),     # 2x3 layout
          oma = c(2, 2, 0, 0), # two rows of text at the outer left and bottom margin
          mar = c(0, 2, 1, 0), # space for one row of text at ticks and to separate plots
          mgp = c(2, 1, 0),
          xpd = F)  
      
      # Russia 2012
      plot_digits_2last(ru12$zyuganov, ru12_syn$votes_b, title = "Russia 2012", ylab = "Relative Frequency",
                        y_axis = T, x_axis = F, y_labels = T)
      
      text(3.5, 0.13, "Second Digit", cex=1.5)
      text(2.1, 0.07, "Last Digit", cex=1.5)
      
      
      # Uganda 2011
      par(mar = c(0, 1, 1, 0))
      plot_digits_2last(uga11$besigye, uga11_syn$votes_b, title = "Uganda 2011",
                        y_axis = F, x_axis = F, x_labels = T)
      
      # Venezuela 2004
      par(mar = c(0, 1, 1, 1))
      plot_digits_2last(ven04$rrp_si, ven04_syn$votes_b, title = "Venezuela 2004",
                        y_axis = F, y_labels = F, x_axis = F, x_labels = F)
      
      # Austria 2008
      par(mar = c(2, 2, 1, 0))
      plot_digits_2last(aus08$ÖVP, aus08_syn$votes_b, title = "Austria 2008", xlab = "Number",
                        ylab = "Relative Frequency", y_axis = T, y_labels = T, 
                        x_axis = T, x_labels = T)
      
      # Spain 2019
      par(mar = c(2, 1, 1, 0))
      plot_digits_2last(esp19$PP, esp19_syn$votes_b, title = "Spain 2019", xlab = "Number",
                        y_axis = F, x_axis = T, x_labels = T)
      
      # Finland 2017
      par(mar = c(2, 1, 1, 1))
      plot_digits_2last(fin17$`SDP Votes cast, total`, fin17_syn$votes_b, title = "Finland 2017", xlab = "Number",
                        y_axis = F, x_axis = T, x_labels = T)
      
      box("outer")
      
      
      # plot Russia 2011 separately
      plot_digits_2last(ru11$communist, ru11_syn$votes_b, title = "Russia 2011", ylab = "Relative Frequency",
                        y_axis = T, x_axis = F, y_labels = T)
      


