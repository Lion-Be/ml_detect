par(mar = c(4.5, 4.5, 2, 2))  

plot(benford_expected(1), ylab="Relative Frequencey", xlab="Digit", 
     main = "", frame = FALSE, type = "l", xaxt = "n", lwd=10, ylim=c(0.05, 0.35), cex.lab=2, cex.axis=2)
axis(1, at=1:10, labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"), cex.lab=12, cex.axis=2)
abline(v=seq(0,9, by = 1), lty=3, lwd=1, col = "grey")
abline(h=seq(0,0.35,by = 0.05), lty=3, lwd=1, col = "grey")
lines(benford_expected(2), lwd=10, col="darkblue")
lines(benford_expected(3), lwd=10, col="brown1")
legend(5.5,0.3, c("First Digit", "Second Digit", "Third Digit"), col=c("black", "darkblue", "brown1"), cex=2, lwd=c(2,2,2), bty="n")



par(mar = c(4.5, 4.5, 2, 2))  

plot(benford_expected(2), ylab="Relative Frequencey", xlab="Digit", 
     main = "", frame = FALSE, type = "l", xaxt = "n", lwd=10, ylim=c(0.05, 0.15), cex.lab=2, cex.axis=2)
axis(1, at=1:10, labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"), cex.lab=2, cex.axis=2)

abline(v=seq(0,9, by = 1), lty=3, lwd=1, col = "grey")
abline(h=seq(0, 0.14,by = 0.01), lty=3, lwd=1, col = "grey")

lines(c(table(extract_digit(aus08$SPÖ, 2))/length(extract_digit(aus08$SPÖ, 2))), type="o",
      lwd=4, lty=2, col="darkgrey")

lines(c(table(extract_digit(esp19$PSOE, 2))/length(extract_digit(esp19$PSOE, 2))), type="o",
      lwd=4, lty=2, col="blue4")

lines(c(table(extract_digit(fin17$kok_votes, 2))/length(extract_digit(fin17$kok_votes, 2))), type="o",
      lwd=4, lty=2, col="darkgreen")

lines(c(table(extract_digit(ru11$ur, 2))/length(extract_digit(ru11$ur, 2))), type="o",
      lwd=4, lty=2, col="orange")

lines(c(table(extract_digit(ru12$zyuganov, 2))/length(extract_digit(ru12$zyuganov, 2))), type="o",
      lwd=4, lty=2, col="brown1")


lines(c(table(extract_digit(uga11$museveni, 2))/length(extract_digit(uga11$museveni, 2))), type="o",
      lwd=4, lty=2, col="darkviolet")


par(mfrow=c(1,1))
plot(NA, xlim=c(1,10), ylim=c(1,10), axes=F, xlab="")
legend(2,7, legend=c("Second Digit, Theory", "Finland 2017, Empirical", "Spain 2019, Empirical", "Austria 2008, Empirical", "Russia 2011, Empirical", 
                     "Russia 2012, Empirical", "Uganda 2011, Empirical"),
       col=c("black", "darkgreen", "blue4", "darkgray", "brown1", "orange", "darkviolet"),
       lty=c(1,rep(2,6)), lwd = 3, cex=2, bty = "n")


# ---------------------------------------------------------------------------- -

par(mfrow = c(1,2),     # 2x2 layout
 #   oma = c(2, 2, 0, 0), # two rows of text at the outer left and bottom margin
 #   mgp = c(2, 1, 0),
    xpd = NA)  

rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(30)

# empty image with baseline color
x <- list()
x$x <- seq(0,10,1)
x$y <- seq(0,10,1)
x$z <- matrix(rep(0,100), nrow=10, ncol=10)

#par(mar = c(1.5, 1.2, 2.5, 1.2))
image(x, col=r[1], xlim=c(0,1), ylim=c(0,1), xlab="Turnout", ylab="Winner's vote share", cex.axis=1.5, cex.lab=1.5)
k <- kde2d(esp19$turnout, esp19$share_psoe, n=50)
image(k, col=r, xlim=c(0,1), ylim=c(0,1), add=T)
text(0.35, 1.05, "Spain 2019 General Elections", col="black", font=1, cex=1.2)


# Russia 2011 empirical
#par(mar = c(1.5, 1.2, 2.5, 1.2))
image(x, col=r[1], xlim=c(0,1), ylim=c(0,1),xlab="Turnout", ylab="Winner's vote share",  cex.axis=1.5, cex.lab=1.5)
k <- kde2d(ru11$turnout, ru11$share_ur, n=50)
image(k, col=r, xlim=c(0,1), ylim=c(0,1), add=T)
text(0.4, 1.05, "Russia 2011 Legislative Elections", col="black", font=1, cex=1.2)



# --------------------------------------------------------------- #

par(mfrow = c(1,2), xpd=F)
par(mar = c(4, 4.5, 0, 1))

d_fin17 <- density(fin17$share_kok, bw = 0.001, from = 0, to = 1, cut = TRUE, n = 1001) 
plot(d_fin17$x, d_fin17$y, xaxt = "n", xlab = "Winner's vote share", lwd = 1.2, ylab = "Density", main = "", frame = FALSE, type = "l", xaxt = "n", col="darkgrey", cex.lab=1.5, cex.axis=1.5)
abline(v=seq(0,0.8,by = 0.05), lty=3, lwd=2, col = "grey")
axis(1, at = seq(0,1,by=0.05), cex.lab=1.5, cex.axis=1.5)
text(x=0.6, y=6.4, labels="Finland 2017", cex=1.5)

d_ru12 <- density(ru12$share_putin[-which(ru12$share_putin==1)], bw = 0.0001, from = 0, to = 1, cut = TRUE, n = 1001) 
plot(d_ru12$x, d_ru12$y, yaxt = "n", xaxt = "n", xlab = "Winner's vote share", lwd = 1.2, ylab = "Density", main = "", frame = FALSE, type = "l", xaxt = "n", col="darkgrey", cex.lab=1.5, cex.axis=1.5)
abline(v=seq(0.2,1,by = 0.05), lty=3, lwd=2, col = "grey")
axis(1, at = seq(0,1,by=0.05), cex.lab=1.5, cex.axis=1.5)
axis(2, at = seq(0,12,by=2), cex.lab=1.5, cex.axis=1.5)
rect(0.79, 1.1, 0.96, 8)
text(x=0.85, y=12.7, labels="Russia 2012", cex=1.5)

lines(d_ru12$x[699:703], d_ru12$y[699:703],col="red")
lines(d_ru12$x[749:753], d_ru12$y[749:753],col="red")
lines(d_ru12$x[799:803], d_ru12$y[799:803],col="red")
lines(d_ru12$x[849:853], d_ru12$y[849:853],col="red")
lines(d_ru12$x[899:904], d_ru12$y[899:904],col="red")
lines(d_ru12$x[949:953], d_ru12$y[949:953],col="red")



# ---------------------------------------------------------------- #

library(RColorBrewer)

par(mfrow = c(1,1))

l_ru11 <- scale(ru11$turnout)
d_ru11 <- density(l_ru11, bw=0.2)
d_ru12 <- density(scale(ru12$turnout), bw=0.2)
d_uga11 <- density(scale(uga11$turnout), bw=0.2)
d_aus08 <- density(scale(aus08$turnout), bw=0.2)
d_esp19 <- density(scale(esp19$turnout), bw=0.2)
d_fin17 <- density(scale(fin17$turnout), bw=0.2)

x <- seq(-4, 4, by = 0.001)
d_norm <- dnorm(x, mean = 0, sd = 1)

blue_colors <- colorRampPalette(brewer.pal(9,"Blues"))(5)

plot(d_ru11, col = blue_colors[3], main = "", lwd=3, 
     xlab = "Percentage of turned out voters (standardized)", xlim=c(-4,4), ylim=c(0,0.5), bty="n",
     cex.lab = 1.5, cex.axis=1.5)
abline(v=seq(-4,4,by = 0.5), lty=3, lwd=1, col = "grey")
abline(h=seq(0,0.5,by = 0.05), lty=3, lwd=1, col = "grey")

lines(d_ru12, col = blue_colors[4], lwd=3)
lines(d_uga11, col = blue_colors[5], lwd=3)

lines(d_aus08, col = gray.colors(n=3)[1], lwd=3)
lines(d_fin17, col = gray.colors(n=3)[2], lwd=3)
lines(d_esp19, col = gray.colors(n=3)[3], lwd=3)

lines(x=x, d_norm, lwd = 12)

rect(1.5, 0.11, 2.25, 0.22, border="red", lwd = 2)
rect(2, 0, 3.5, 0.03, border="red", lwd = 2)

par(mfrow=c(1,1))
plot(NA, xlim=c(1,10), ylim=c(1,10), axes=F, xlab="", ylab="")
legend(5,7, legend=c("N(0,1)", NA, "Austria 2008", "Finland 2017", "Spain 2019", NA, "Russia 2011", 
                     "Russia 2012", "Uganda 2011"),
       col=c("black", NA, gray.colors(n=3)[1], gray.colors(n=3)[2], gray.colors(n=3)[3], NA, blue_colors[3], blue_colors[4], blue_colors[5]), 
       lty=c(1, NA, 1, 1, 1, NA, 1, 1 ,1), lwd=2, cex=1.5, bty = "n")

