#' ----------------------
#  6. paper plots -------
#' ----------------------

 #' -----------------------------------------------
 # numerical characteristics of electoral returns
 #' -----------------------------------------------

   #' Figure 1a: second significant digits for different aggregation levels, 
   #' Spain 2019

    
      # # second aggregation level: municipo (código)
      # esp19_2 <- esp19 %>% 
      #   group_by(`Código de Municipio`) %>% 
      #   summarise(eligible = sum(eligible),
      #             PSOE = sum(PSOE), 
      #             PP = sum(PP))
      # 
      # # expected 2BL
      # plot(benford_expected(2), ylab="Relative Frequency", xlab="Digit", 
      #      labels=F, type="l", lwd=4, ylim=c(0.05, 0.15), main="")
      # axis(1, at=1:10, labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))
      # axis(2, at=seq(0.05, 0.15, 0.01))
      # 
      # # level: municipo (nombre)
      # lines(c(table(extract_digit(esp19$PSOE, 2))/length(extract_digit(esp19$PSOE, 2))), type="o",
      #       lwd=2, col="darkblue")
      # lines(c(table(extract_digit(esp19$PP, 2))/length(extract_digit(esp19$PP, 2))), type="o",
      #       lwd=2, col="lightblue")
      # 
      # lines(c(table(extract_digit(esp19_2$PSOE, 2))/length(extract_digit(esp19_2$PSOE, 2))), type="o",
      #       lwd=2, lty=2, col="darkblue")
      # lines(c(table(extract_digit(esp19_2$PP, 2))/length(extract_digit(esp19_2$PP, 2))), type="o",
      #       lwd=2, lty=2, col="lightblue")

      
    #' Figure 1a: second digits at lowest level across all elections (winner)
  
  tikz('digits.tex', standAlone = TRUE, width=12, height=6)

    par(mfrow=c(1,2),
      mar = c(4.5, 4.5, 0, 1))


      plot(benford_expected(2), ylab="Relative Frequencey", xlab="Digit", 
           main = "", frame = FALSE, type = "l", xaxt = "n", lwd=4, ylim=c(0.05, 0.15), cex.lab=2, cex.axis=1.55)
      axis(1, at=1:10, labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"), cex.lab=2, cex.axis=1.55)
      
      lines(c(table(extract_digit(aus08$SPÖ, 2))/length(extract_digit(aus08$SPÖ, 2))), type="o",
            lwd=2, lty=2, col="darkgrey")
      
      lines(c(table(extract_digit(esp19$PSOE, 2))/length(extract_digit(esp19$PSOE, 2))), type="o",
            lwd=2, lty=2, col="darkgreen")
      
      lines(c(table(extract_digit(fin17$kok_votes, 2))/length(extract_digit(fin17$kok_votes, 2))), type="o",
            lwd=2, lty=2, col="blue4")
      
      lines(c(table(extract_digit(ru11$ur, 2))/length(extract_digit(ru11$ur, 2))), type="o",
            lwd=2, lty=2, col="orange")
      
      lines(c(table(extract_digit(ru12$putin, 2))/length(extract_digit(ru12$putin, 2))), type="o",
            lwd=2, lty=2, col="brown1")
      
      lines(c(table(extract_digit(ven04$rrp_no, 2))/length(extract_digit(ven04$rrp_no, 2))), type="o",
            lwd=2, lty=2, col="darkred")
      
      lines(c(table(extract_digit(uga11$museveni, 2))/length(extract_digit(uga11$museveni, 2))), type="o",
            lwd=2, lty=2, col="darkviolet")
      
      par(mar = c(4.5, 2.8, 0, 1))
      
    #' Figure 1b: empirical second significant digits and synethtic for different degrees of fraud
    #' Russia 2012
    
      plot(benford_expected(2), ylab="Relative Frequency", xlab="Digit", 
           main = "", frame = FALSE, type = "l", xaxt = "n", lwd=4, ylim=c(0.05, 0.15), cex.lab=2, cex.axis=1.55)
      axis(1, at=1:10, labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"), cex.lab=2, cex.axis=1.55)
      
      fraud_values <- matrix(NA, ncol=2, nrow=10)
      colnames(fraud_values) <- c("fraud_incA", "fraud_extA")
      fraud_values[, "fraud_incA"] <- seq(0.05, 0.6, length.out = 10 )
      fraud_values[, "fraud_extA"] <- seq(0, 0.15, length.out = 10 )
      
      for (row in 1:nrow(fraud_values)) {
        
        ru12_syn <- gen_data(n_entities = nrow(ru12),
                             eligible = ru12$eligible,
                             fraud_type = c("bbs", "switching"),
                             fraud_incA = fraud_values[row, "fraud_incA"],
                             fraud_extA = fraud_values[row, "fraud_extA"],
                             fraud_expo = 1.5,
                             n_elections = 1, 
                             data_type = "full", 
                             shareA_emp = ru12$share_putin,
                             turnout = opt_vectorsRU12$turnout, 
                             shareA = opt_vectorsRU12$shareA)
        
        lines(c(table(extract_digit(ru12_syn$votes_b, 2))/length(extract_digit(ru12_syn$votes_b, 2))), type="o",
              lwd=2, lty=2, col="lightgrey")
        
      }
      
      # clean synthetic data
      ru12_syn <- gen_data(n_entities = nrow(ru12),
                           eligible = ru12$eligible,
                           fraud_type = c("bbs", "switching"),
                           fraud_incA = 0,
                           fraud_extA = 0,
                           fraud_expo = 1.5,
                           n_elections = 1, 
                           data_type = "full", 
                           shareA_emp = ru12$share_putin,
                           turnout = opt_vectorsRU12$turnout, 
                           shareA = opt_vectorsRU12$shareA)
      
      lines(c(table(extract_digit(ru12_syn$votes_b, 2))/length(extract_digit(ru12_syn$votes_b, 2))), type="o",
            lwd=2, lty=2, col="darkblue")
      
      
      # empirical data
      lines(c(table(extract_digit(ru12$zyuganov, 2))/length(extract_digit(ru12$zyuganov, 2))), type="o",
            lwd=2, lty=2, col="brown1")
      
      box("outer")
      
    dev.off()
    tools::texi2dvi('digits.tex',pdf=T)
    system(paste(getOption('pdfviewer'),'digits.pdf'))
    
    
    tikz('digits_legend.tex', standAlone = TRUE, width=12, height=6)
    
      par(mfrow=c(1,1))
      plot(NA, xlim=c(1,10), ylim=c(1,10), axes=F, xlab="")
      legend(5,7, legend=c("Second Digit, Theory", "Finland 2017, Empirical", "Spain 2019, Empirical", "Austria 2008, Empirical", "Russia 2011, Empirical", 
                           "Russia 2012, Empirical", "Venezuela 2004, Empirical", "Uganda 2011, Empirical", NA, "Russia 2012, Synthetic (Clean)", "Russia 2012, Synthetic (Fraud)", "Russia 2012, Empirical"),
             col=c("black", "blue4", "darkgreen", "darkgray", "brown1", "orange", "darkred", "darkviolet", NA, "darkblue", "lightgrey", "brown1"),
             lty=c(1,rep(2,7), NA, rep(2, 3)), lwd=c(4, rep(2,7), NA, rep(2,3)), pch=c(NA, rep(1,7), NA, rep(1,3)), cex=1, bty = "n")
      
    dev.off()
    tools::texi2dvi('digits_legend.tex',pdf=T)
    system(paste(getOption('pdfviewer'),'digits_legend.pdf'))
      
      
    #' Figure 2a shareA distributions, clean cases
    
    tikz('spikes_clean.tex', standAlone = TRUE, width=7, height=12)
      
      par(mfrow=c(2,1),
          mar = c(2.8, 4.5, 0, 1))
      
      d_aus08 <- density(aus08$share_spo, bw = 0.001, from = 0, to = 1, cut = TRUE, n = 1001) 
      plot(d_aus08$x, d_aus08$y, xaxt = "n", xlab = "", lwd = 1.2, ylab = "Density", main = "", frame = FALSE, type = "l", xaxt = "n", col="darkgrey", cex.lab=2, cex.axis=1.55)
      abline(v=seq(0,0.8,by = 0.05), lty=3, lwd=2, col = "grey")
      axis(1, at = seq(0,1,by=0.05), cex.lab=2, cex.axis=1.55)
      text(x=0.6, y=4.4, labels="Austria 2008", cex=2)
      
      par(mar = c(4, 4.5, 0, 1))
      
      d_fin17 <- density(fin17$share_kok, bw = 0.001, from = 0, to = 1, cut = TRUE, n = 1001) 
      plot(d_fin17$x, d_fin17$y, xaxt = "n", xlab = "Winner's vote share", lwd = 1.2, ylab = "Density", main = "", frame = FALSE, type = "l", xaxt = "n", col="darkgrey", cex.lab=2, cex.axis=1.55)
      abline(v=seq(0,0.8,by = 0.05), lty=3, lwd=2, col = "grey")
      axis(1, at = seq(0,1,by=0.05), cex.lab=2, cex.axis=1.55)
      text(x=0.6, y=6.4, labels="Finland 2017", cex=2)
      
    
    dev.off()
    tools::texi2dvi('spikes_clean.tex',pdf=T)
    system(paste(getOption('pdfviewer'),'spikes_clean.pdf'))
    
    #' Figure 2b shareA distributions, clean cases
    tikz('spikes_fraud.tex', standAlone = TRUE, width=7, height=12)
    
      par(mfrow=c(2,1),
          mar = c(2.8, 4.5, 0, 1))
      
      d_ru11 <- density(ru11$share_ur[-which(ru11$share_ur==1)], bw = 0.0001, from = 0, to = 1, cut = TRUE, n = 1001) 
      plot(d_ru11$x, d_ru11$y, xaxt = "n", xlab = "", lwd = 1.2, ylab = "Density", main = "", frame = FALSE, type = "l", xaxt = "n", col="darkgrey", cex.lab=2, cex.axis=1.55)
      abline(v=seq(0.2,1,by = 0.05), lty=3, lwd=2, col = "grey")
      axis(1, at = seq(0,1,by=0.05), cex.lab=2, cex.axis=1.55)
      # rect(0.795, 1.1, 0.955, 4.8)
      text(x=0.85, y=8.7, labels="Russia 2011", cex=2)
      
      lines(d_ru11$x[499:503], d_ru11$y[499:503],col="red")
      lines(d_ru11$x[599:603], d_ru11$y[599:603],col="red")
      lines(d_ru11$x[649:653], d_ru11$y[649:653],col="red")
      lines(d_ru11$x[749:753], d_ru11$y[749:753],col="red")
      lines(d_ru11$x[799:803], d_ru11$y[799:803],col="red")
      lines(d_ru11$x[849:853], d_ru11$y[849:853],col="red")
      lines(d_ru11$x[899:903], d_ru11$y[899:903],col="red")
      lines(d_ru11$x[949:953], d_ru11$y[949:953],col="red")
      
      
      par(mar = c(4, 4.5, 0, 1))
      
      d_ru12 <- density(ru12$share_putin[-which(ru12$share_putin==1)], bw = 0.0001, from = 0, to = 1, cut = TRUE, n = 1001) 
      plot(d_ru12$x, d_ru12$y, xaxt = "n", xlab = "Winner's vote share", lwd = 1.2, ylab = "Density", main = "", frame = FALSE, type = "l", xaxt = "n", col="darkgrey", cex.lab=2, cex.axis=1.55)
      abline(v=seq(0.2,1,by = 0.05), lty=3, lwd=2, col = "grey")
      axis(1, at = seq(0,1,by=0.05), cex.lab=2, cex.axis=1.55)
      rect(0.79, 1.1, 0.96, 8)
      text(x=0.85, y=12.7, labels="Russia 2012", cex=2)
      
      lines(d_ru12$x[699:703], d_ru12$y[699:703],col="red")
      lines(d_ru12$x[749:753], d_ru12$y[749:753],col="red")
      lines(d_ru12$x[799:803], d_ru12$y[799:803],col="red")
      lines(d_ru12$x[849:853], d_ru12$y[849:853],col="red")
      lines(d_ru12$x[899:904], d_ru12$y[899:904],col="red")
      lines(d_ru12$x[949:953], d_ru12$y[949:953],col="red")
      
    
    dev.off()
    tools::texi2dvi('spikes_fraud.tex',pdf=T)
    system(paste(getOption('pdfviewer'),'spikes_fraud.pdf'))
      
    
  #' -----------------------------------------------
  # comparison to empirical data
  #' -----------------------------------------------
  
    #' Figure: comet plots, fraud
      par(mfrow = c(2, 3),     # 2x2 layout
          oma = c(2, 2, 0, 0), # two rows of text at the outer left and bottom margin
          mgp = c(2, 1, 0),
          xpd = NA)  
      
      rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
      r <- rf(30)
      
      # empty image with baseline color
      x <- list()
      x$x <- seq(0,10,1)
      x$y <- seq(0,10,1)
      x$z <- matrix(rep(0,100), nrow=10, ncol=10)
      
      
      # Uganda 2011 empirical
      par(mar = c(1.5, 1.4, 2.5, 1))
      image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), ylab="Winner's vote share", xaxt="n", cex.lab=1.1, cex.axis=1.1)    
      k <- kde2d(uga11$turnout, uga11$share_museveni, n=50)
      image(k, col=r, xlim=c(0,1), ylim=c(0,1), xaxt="n", add=T)
      text(0.3, 1.05, "Uganda 2011, Empirical", col="black", font=1, cex=1.2)
      
      # Russia 2011 empirical
      par(mar = c(1.5, 1.2, 2.5, 1.2))
      image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), yaxt="n", xaxt="n")
      k <- kde2d(ru11$turnout, ru11$share_ur, n=50)
      image(k, col=r, xlim=c(0,1), ylim=c(0,1), add=T)
      text(0.3, 1.05, "Russia 2011, Empirical", col="black", font=1, cex=1.2)
      
      # Russia 2012 empirical
      par(mar = c(1.5, 1.2, 2.5, 1.2))
      image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), yaxt="n", xaxt="n")    
      k <- kde2d(ru12$turnout, ru12$share_putin, n=50)
      image(k, col=r, xlim=c(0,1), ylim=c(0,1), add=T)
      text(0.3, 1.05, "Russia 2012, Empirical", col="black", font=1, cex=1.2)
      
      
      # Uganda 2011 synthetic
      par(mar = c(2.5, 1.4, 1.5, 1))
      image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), ylab="Winner's vote share", xlab="Turnout", cex.lab=1.1, cex.axis=1.1)    
      k <- kde2d(uga11_syn$turnout, uga11_syn$shareA, n=50)
      image(k, col=r, xlim=c(0,1), ylim=c(0,1), yaxt="n", xaxt="n", add=T)
      text(0.3, 1.05, "Uganda 2011, Synthetic", col="black", font=1, cex=1.2)
      
      # Russia 2011 synthetic
      par(mar = c(2.5, 1.2, 1.5, 1.2))
      image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), yaxt="n", xlab="Turnout", cex.lab=1.1, cex.axis=1.1)
      k <- kde2d(ru11_syn$turnout, ru11_syn$shareA, n=50)
      image(k, col=r, xlim=c(0,1), ylim=c(0,1), yaxt="n", add=T)
      text(0.3, 1.05, "Russia 2011, Synthetic", col="black", font=1, cex=1.2)
      
      # Russia 2012 synthetic
      par(mar = c(2.5, 1.2, 1.5, 1.2))
      image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), yaxt="n", xlab="Turnout", cex.lab=1.1, cex.axis=1.1)    
      k <- kde2d(ru12_syn$turnout, ru12_syn$shareA, n=50)
      image(k, col=r, xlim=c(0,1), ylim=c(0,1), yaxt="n", add=T)
      text(0.3, 1.05, "Russia 2012, Synthetic", col="black", font=1, cex=1.2)
      
      box("outer")
    
    #' Figure: comet plots, clean
      par(mfrow = c(2, 3),     # 2x2 layout
          oma = c(2, 2, 0, 0), # two rows of text at the outer left and bottom margin
          mgp = c(2, 1, 0),
          xpd = NA)  
      
      rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
      r <- rf(30)
      
      # empty image with baseline color
      x <- list()
      x$x <- seq(0,10,1)
      x$y <- seq(0,10,1)
      x$z <- matrix(rep(0,100), nrow=10, ncol=10)
      
      
      # Austria 2008, Empirical
      par(mar = c(1.5, 1.4, 2.5, 1))
      image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), ylab="Winner's vote share", xaxt="n", cex.lab=1.1, cex.axis=1.1)    
      k <- kde2d(aus08$turnout, aus08$share_spo, n=50)
      image(k, col=r, xlim=c(0,1), ylim=c(0,1), xaxt="n", add=T)
      text(0.3, 1.05, "Austria 2008, Empirical", col="black", font=1, cex=1.2)
      
      # Spain 2019, Empirical
      par(mar = c(1.5, 1.2, 2.5, 1.2))
      image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), yaxt="n", xaxt="n")
      k <- kde2d(esp19$turnout, esp19$share_psoe, n=50)
      image(k, col=r, xlim=c(0,1), ylim=c(0,1), add=T)
      text(0.3, 1.05, "Spain 2019, Empirical", col="black", font=1, cex=1.2)
      
      # Finland 2017, Empirical
      par(mar = c(1.5, 1.2, 2.5, 1.2))
      image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), yaxt="n", xaxt="n")    
      k <- kde2d(fin17$turnout, fin17$share_kok, n=50)
      image(k, col=r, xlim=c(0,1), ylim=c(0,1), add=T)
      text(0.3, 1.05, "Finland 2017, Empirical", col="black", font=1, cex=1.2)
      
      
      # Austria 2008, Synthetic
      par(mar = c(2.5, 1.4, 1.5, 1))
      image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), ylab="Winner's vote share", xlab="Turnout", cex.lab=1.1, cex.axis=1.1)    
      k <- kde2d(aus08_syn$turnout, aus08_syn$shareA, n=50)
      image(k, col=r, xlim=c(0,1), ylim=c(0,1), yaxt="n", xaxt="n", add=T)
      text(0.32, 1.05, "Austria 2008, Synthetic", col="black", font=1, cex=1.2)
      
      # Spain 2019, Synthetic
      par(mar = c(2.5, 1.2, 1.5, 1.2))
      image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), yaxt="n", xlab="Turnout", cex.lab=1.1, cex.axis=1.1)
      k <- kde2d(esp19_syn$turnout, esp19_syn$shareA, n=50)
      image(k, col=r, xlim=c(0,1), ylim=c(0,1), yaxt="n", add=T)
      text(0.32, 1.05, "Spain 2019, Synthetic", col="black", font=1, cex=1.2)
      
      # Finland 2017, Synthetic
      par(mar = c(2.5, 1.2, 1.5, 1.2))
      image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), yaxt="n", xlab="Turnout", cex.lab=1.1, cex.axis=1.1)    
      k <- kde2d(fin17_syn$turnout, fin17_syn$shareA, n=50)
      image(k, col=r, xlim=c(0,1), ylim=c(0,1), yaxt="n", add=T)
      text(0.32, 1.05, "Finland 2017, Synthetic", col="black", font=1, cex=1.2)
      
      box("outer")
      
    #' Figure: spikes plots empirical vs. synthetic data
    
      tikz('spikes_comparison1.tex', standAlone = TRUE, width=7, height=12)
      
        par(mfrow=c(2,1),
          mar = c(2.8, 4.5, 0, 1))
        
        d_aus08 <- density(aus08$share_spo, bw = 0.001, from = 0, to = 1, cut = TRUE, n = 1001) 
        plot(d_aus08$x, d_aus08$y, xaxt = "n", xlab = "", lwd = 1.2, ylab = "Density", main = "", frame = FALSE, type = "l", xaxt = "n", col="darkgrey", cex.lab=2, cex.axis=1.55)
        abline(v=seq(0,0.8,by = 0.05), lty=3, lwd=2, col = "grey")
        axis(1, at = seq(0,1,by=0.05), cex.lab=2, cex.axis=1.55)
        text(x=0.6, y=4.4, labels="Austria 2008, Empirical",  cex=2)
        
        par(mar = c(4, 4.5, 0, 1))
        
        d_aus08_syn <- density(aus08_syn$shareA, bw = 0.001, from = 0, to = 1, cut = TRUE, n = 1001) 
        plot(d_aus08_syn$x, d_aus08_syn$y, xaxt = "n", xlab = "", lwd = 1.2, ylab = "Density", main = "", frame = FALSE, type = "l", xaxt = "n", col="darkgrey", cex.lab=2, cex.axis=1.55)
        abline(v=seq(0,0.8,by = 0.05), lty=3, lwd=2, col = "grey")
        axis(1, at = seq(0,1,by=0.05), cex.lab=2, cex.axis=1.55)
        text(x=0.6, y=4.6, labels=expression("Austria 2008, Synthetic"), cex=2)
      
      dev.off()
      tools::texi2dvi('spikes_comparison1.tex',pdf=T)
      system(paste(getOption('pdfviewer'),'spikes_comparison1.pdf'))
      
      
      
      tikz('spikes_comparison2.tex', standAlone = TRUE, width=7, height=12)
      
        par(mfrow=c(2,1),
            mar = c(2.8, 4.5, 0, 1))
        
        d_uga11 <- density(uga11$share_museveni[-which(uga11$share_museveni==1)], bw = 0.0001, from = 0, to = 1, cut = TRUE, n = 1001) 
        plot(d_uga11$x, d_uga11$y, xaxt = "n", xlab = "", lwd = 1.2, ylab = "Density", main = "", frame = FALSE, type = "l", xaxt = "n", col="darkgrey", cex.lab=2, cex.axis=1.55)
        abline(v=seq(0.2,1,by = 0.05), lty=3, lwd=2, col = "grey")
        axis(1, at = seq(0,1,by=0.05), cex.lab=2, cex.axis=1.55)
        text(x=0.3, y=9.4, labels="Uganda 2011, Empirical",  cex=2)
        
        par(mar = c(4, 4.5, 0, 1))
        
        d_uga11_syn <- density(uga11_syn$shareA, bw = 0.0001, from = 0, to = 1, cut = TRUE, n = 1001) 
        plot(d_uga11_syn$x, d_uga11_syn$y, xaxt = "n", xlab = "", lwd = 1.2, ylab = "Density", main = "", frame = FALSE, type = "l", xaxt = "n", col="darkgrey", cex.lab=2, cex.axis=1.55)
        abline(v=seq(0.2,1,by = 0.05), lty=3, lwd=2, col = "grey")
        axis(1, at = seq(0,1,by=0.05), cex.lab=2, cex.axis=1.55)
        text(x=0.3, y=10.5, labels="Uganda 2011, Synthetic", cex=2)
        
      dev.off()
      tools::texi2dvi('spikes_comparison2.tex',pdf=T)
      system(paste(getOption('pdfviewer'),'spikes_comparison2.pdf'))
     
      
      
      tikz('spikes_comparison3.tex', standAlone = TRUE, width=7, height=12)
      
        par(mfrow=c(2,1),
            mar = c(2.8, 4.5, 0, 1))
        
        d_ru11 <- density(ru11$share_ur[-which(ru11$share_ur==1)], bw = 0.0001, from = 0, to = 1, cut = TRUE, n = 1001) 
        plot(d_ru11$x, d_ru11$y, xaxt = "n", xlab = "", lwd = 1.2, ylab = "Density", main = "", frame = FALSE, type = "l", xaxt = "n", col="darkgrey", cex.lab=2, cex.axis=1.55)
        abline(v=seq(0.2,1,by = 0.05), lty=3, lwd=2, col = "grey")
        axis(1, at = seq(0,1,by=0.05), cex.lab=2, cex.axis=1.55)
        text(x=0.75, y=8.7, labels="Russia 2011, Empirical", cex=2)
        
        par(mar = c(4, 4.5, 0, 1))
        
        d_ru11_syn <- density(ru11_syn$shareA[-which(ru11_syn$shareA==1)], bw = 0.0001, from = 0, to = 1, cut = TRUE, n = 1001) 
        plot(d_ru11_syn$x, d_ru11_syn$y, xaxt = "n", xlab = "", lwd = 1.2, ylab = "Density", main = "", frame = FALSE, type = "l", xaxt = "n", col="darkgrey", cex.lab=2, cex.axis=1.55)
        abline(v=seq(0.2,1,by = 0.05), lty=3, lwd=2, col = "grey")
        axis(1, at = seq(0,1,by=0.05), cex.lab=2, cex.axis=1.55)
        text(x=0.75, y=9.1, labels="Russia 2011, Synthetic", cex=2)
        
      dev.off()
      tools::texi2dvi('spikes_comparison3.tex',pdf=T)
      system(paste(getOption('pdfviewer'),'spikes_comparison3.pdf'))
      
      
      
    #' Figure: empirical and synthetic winner's vote share curves
      
      tikz('shareA_comparison.tex', standAlone = TRUE, width=12, height=6)
      
        par(mfrow=c(1,2),
            mar = c(4.5, 4.5, 0, 1))
        
        # clean cases
        d_aus08 <- density(aus08$share_spo, bw=0.02)
        d_aus08_syn <- density(aus08_syn$shareA, bw=0.02)
        d_esp19 <- density(esp19$share_psoe, bw=0.05)
        d_esp19_syn <- density(esp19_syn$shareA, bw=0.05)
        d_fin17 <- density(fin17$share_kok, bw=0.02)
        d_fin17_syn <- density(fin17_syn$shareA, bw=0.02)
        
        plot(d_fin17, xaxt = "n", xlab = "Winner's vote share", lwd = 2, lty=1, ylab = "Density", main = "", frame = FALSE, type = "l", xaxt = "n", col="blue4", cex.lab=2, cex.axis=1.55) 
        axis(1, at = seq(0,1,by=0.05), cex.lab=2, cex.axis=1.55)
        lines(d_fin17_syn, col = "blue4", lwd=2, lty=2)
        
        lines(d_esp19, col = "darkgreen", lwd=2, lty=1)
        lines(d_esp19_syn, col = "darkgreen", lwd=2, lty=2)
        
        lines(d_aus08, col = "darkgray", lwd=2, lty=1)
        lines(d_aus08_syn, col = "darkgray", lwd=2, lty=2)
        
        par(mar = c(4.5, 2.8, 0, 1))
        
        # fraud cases
        d_ru11 <- density(ru11$share_ur, bw=0.02)
        d_ru11_syn <- density(ru11_syn$shareA, bw=0.02)
        d_ru12 <- density(ru12$share_putin, bw=0.02)
        d_ru12_syn <- density(ru12_syn$shareA, bw=0.02)
        d_uga11 <- density(uga11$share_museveni, bw=0.02)
        d_uga11_syn <- density(uga11_syn$shareA, bw=0.02)
        
        plot(d_ru12, xaxt = "n", xlab = "Winner's vote share", lwd = 2, lty=1, ylab = "", main = "", frame = FALSE, type = "l", xaxt = "n", col="brown1", cex.lab=2, cex.axis=1.55) 
        axis(1, at = seq(0,1,by=0.05), cex.lab=2, cex.axis=1.55)
        lines(d_ru12_syn, col = "brown1", lwd=2, lty=2)
        
        lines(d_ru11, col = "orange", lwd=2, lty=1)
        lines(d_ru11_syn, col = "orange", lwd=2, lty=2)
        
        lines(d_uga11, col = "darkviolet", lwd=2, lty=1)
        lines(d_uga11_syn, col = "darkviolet", lwd=2, lty=2)
        
        box("outer")
        
      dev.off()
      tools::texi2dvi('shareA_comparison.tex',pdf=T)
      system(paste(getOption('pdfviewer'),'shareA_comparison.pdf'))
     
      tikz('shareA_legend.tex', standAlone = TRUE, width=12, height=6)
        
        par(mfrow=c(1,1))
        plot(NA, xlim=c(1,10), ylim=c(1,10), axes=F, xlab="")
        legend(5,7, legend=c("Finland 2017, Empirical", "Finland 2017, Synthetic", "Spain 2019, Empirical", "Spain 2019, Synthetic", 
                             "Austria 2008, Empirical", "Austria 2008, Synthetic", "", "Russia 2011, Empirical", "Russia 2011, Synthetic",
                             "Russia 2012, Empirical", "Russia 2012, Synthetic", "Uganda 2011, Empirical", "Uganda 2011, Synthetic"),
               col=c("blue4", "blue4", "darkgreen", "darkgreen", "darkgray", "darkgray", "brown1", "brown1", "orange", "orange", "darkviolet", "darkviolet"),
               lty=c(1,2,1,2,1,2,NA,1,2,1,2,1,2), lwd=c(rep(2.5, 6), NA, rep(2.5, 6)), cex=1, bty = "n")
        
      dev.off()
      tools::texi2dvi('shareA_legend.tex',pdf=T)
      system(paste(getOption('pdfviewer'),'shareA_legend.pdf'))
   
      
      
  #' -----------------------------------------------
  # motivation for multivariate learning
  # baseline data: Austria 2008
  #' -----------------------------------------------
  
    #' --------------------------------------------------------------------
    # vote switching (digits change, turnout/shareA distributions don't)
    #' --------------------------------------------------------------------
      
      set.seed(12345)
      
      tikz('motivation1.tex', standAlone = TRUE, width=12, height=6)
        
        par(mfrow=c(1,2),
            mar = c(4.5, 4.5, 0, 1))
        
        # digit plot
        plot(benford_expected(2), ylab="Relative Frequency", xlab="Digit", 
             main = "", frame = FALSE, type = "l", xaxt = "n", lwd=4, ylim=c(0.05, 0.15), cex.lab=2, cex.axis=1.55)
        axis(1, at=1:10, labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"), cex.lab=2, cex.axis=1.55)
        
        fraud_values <- matrix(NA, ncol=2, nrow=10)
        colnames(fraud_values) <- c("fraud_incA", "fraud_extA")
        fraud_values[, "fraud_incA"] <- seq(0.01, 0.3, length.out = 10)
        fraud_values[, "fraud_extA"] <- seq(0.01, 0.1, length.out = 10)
        
        
        for (row in 1:nrow(fraud_values)) {
          
          aus08_copy <- aus08
          ids_inc <- sample(1:nrow(aus08_copy), nrow(aus08_copy) * fraud_values[row, "fraud_incA"])
          ids_ext <- sample(1:nrow(aus08_copy), nrow(aus08_copy) * fraud_values[row, "fraud_extA"])
          
          n_moved_inc <- aus08_copy$ÖVP[ids_inc] * fraud_values[row, "fraud_incA"]^1.5
          n_moved_ext <- 1 - (aus08_copy$ÖVP[ids_ext] * fraud_values[row, "fraud_extA"]^1.5)
          
          aus08_copy$SPÖ[ids_inc] <- aus08_copy$SPÖ[ids_inc] + n_moved_inc
          aus08_copy$ÖVP[ids_inc] <- aus08_copy$ÖVP[ids_inc] - n_moved_inc
          
          aus08_copy$SPÖ[ids_ext] <- aus08_copy$SPÖ[ids_ext] + n_moved_ext
          aus08_copy$ÖVP[ids_ext] <- aus08_copy$ÖVP[ids_ext] - n_moved_ext
          
          lines(c(table(extract_digit(aus08_copy$SPÖ, 2))/length(extract_digit(aus08_copy$SPÖ, 2))), type="o",
                lwd=2, lty=2, col="lightgrey")
          
        }
        
        lines(c(table(extract_digit(aus08$SPÖ, 2))/length(extract_digit(aus08$SPÖ, 2))), type="o",
              lwd=2, lty=2, col="darkblue")
        
        # turnout
        par(mar = c(4.5, 2.8, 0, 1))
       
        d_aus08 <- density(aus08$turnout, bw = 0.001, from = 0, to = 1, cut = TRUE, n = 1001) 
        plot(d_aus08$x, d_aus08$y, xaxt = "n", xlab = "Turnout", lwd = 1.2, ylab = "Density", main = "", frame = FALSE, type = "l", xaxt = "n", col="darkblue", cex.lab=2, cex.axis=1.55)
        abline(v=seq(0.4,1,by = 0.05), lty=3, lwd=2, col = "grey")
        axis(1, at = seq(0,1,by=0.05), cex.lab=2, cex.axis=1.55)
        
        box("outer")
        
      dev.off()
      tools::texi2dvi('motivation1.tex',pdf=T)
      system(paste(getOption('pdfviewer'),'motivation1.pdf'))
      
      tikz('motivation_legend.tex', standAlone = TRUE, width=12, height=6)
      
        par(mfrow=c(1,1))
        plot(NA, xlim=c(1,10), ylim=c(1,10), axes=F, xlab="")
        legend(5,7, legend=c("Second Digit, Theory", "Empirical", "Fraud (Synthetic)"),
               col=c("black", "darkblue", "darkgrey"),
               lty=c(1, 2, 1), lwd=c(4,2,2), cex=1, bty = "n")
        
      dev.off()
      tools::texi2dvi('motivation_legend.tex',pdf=T)
      system(paste(getOption('pdfviewer'),'motivation_legend.pdf'))
      
      
      
    #' ------------------------------------------------------------------------------------------
    # rounding fraud (digits don't change much, turnout/shareA distributions do)
    #' ------------------------------------------------------------------------------------------
    
      set.seed(12345)
      
      tikz('motivation2.tex', standAlone = TRUE, width=12, height=6)
        
        par(mfrow=c(1,2),
            mar = c(4.5, 4.5, 0, 1))
        
        plot(benford_expected(2), ylab="Relative Frequency", xlab="Digit", 
             main = "", frame = FALSE, type = "l", xaxt = "n", lwd=4, ylim=c(0.05, 0.15), cex.lab=2, cex.axis=1.55)
        axis(1, at=1:10, labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"), cex.lab=2, cex.axis=1.55)
        
        for (i in 1:10) {
        
          aus08_copy <- aus08
          share_round <- 0.02
          fraud_ids <- sample(which(aus08_copy$share_spo > 0.5 & aus08_copy$share_spo < 1 ), length(aus08_copy$share_spo) * share_round)
          aus08_copy$share_spo[fraud_ids] <- ceiling(aus08_copy$share_spo[fraud_ids] / 0.05) * 0.05 # if it should just be rounded up, then use ceiling instead of round
          aus08_copy$SPÖ[fraud_ids] <- as.integer(aus08_copy$votes_all[fraud_ids] *  aus08_copy$share_spo[fraud_ids])
         
          lines(c(table(extract_digit(aus08_copy$SPÖ, 2))/length(extract_digit(aus08_copy$SPÖ, 2))), type="o",
                lwd=2, lty=2, col="darkgrey")
        
        }
        
        lines(c(table(extract_digit(aus08$SPÖ, 2))/length(extract_digit(aus08$SPÖ, 2))), type="o",
              lwd=2, lty=2, col="darkblue")
        
     
        par(mar = c(4.5, 2.8, 0, 1))
        
        aus08_copy <- aus08
        share_round <- 0.02
        fraud_ids <- sample(which(aus08_copy$share_spo > 0.3 & aus08_copy$share_spo < 1 ), length(aus08_copy$share_spo) * share_round)
        aus08_copy$share_spo[fraud_ids] <- ceiling(aus08_copy$share_spo[fraud_ids] / 0.05) * 0.05 # if it should just be rounded up, then use ceiling instead of round
        
        d_aus08 <- density(aus08$share_spo, bw = 0.001, from = 0, to = 1, cut = TRUE, n = 1001) 
        plot(d_aus08$x, d_aus08$y, xaxt = "n", xlab = "Winner's vote share", lwd = 1.2, ylab = "Density", main = "", frame = FALSE, type = "l", xaxt = "n", col="darkblue", cex.lab=2, cex.axis=1.55)
        abline(v=seq(0,0.7,by = 0.05), lty=3, lwd=2, col = "grey")
        axis(1, at = seq(0,1,by=0.05), cex.lab=2, cex.axis=1.55)
        
        d_aus08_copy <- density(aus08_copy$share_spo, bw = 0.001, from = 0, to = 1, cut = TRUE, n = 1001) 
        lines(d_aus08_copy$x, d_aus08_copy$y, col="darkgrey")
        
        rect(0.32, 0, 0.52, 4.4)
        
        box("outer")
   
      dev.off()
      tools::texi2dvi('motivation2.tex',pdf=T)
      system(paste(getOption('pdfviewer'),'motivation2.pdf'))
       
      
     
    
    
    
    
    
    
    
    
    
      
  
  
  
  
  
  
  
  
  
  
  
  
  