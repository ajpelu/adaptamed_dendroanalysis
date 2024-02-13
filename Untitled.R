####Figura BAI SN

setwd("/Users/guigeiz/Documents/Proyectos/LIFE Granada Regino y Antonio/Tablas y datos analisis")
####

ng<-layout(rbind(rep(0,5),c(0, 1,0, 2, 0), rep(0,5),c(0, 3,0, 4, 0), rep(0,5)), widths=c(11,30,0, 30,11), heights=c(5,25,0, 25, 5), TRUE)
layout.show(ng)
par(font=2, font.axis=2,font.lab=2, family="serif", pty="m", xaxt="n", yaxt="n", xpd=FALSE, cex=1.51, mgp=c(1.5,0.05,0), mar=c(0,0,0,0), tck=0.2, tcl=0.2, las=1,cex.lab=1.21)
#
### 1 TODOS
### 1 PIHA
piha1=read.csv("1BIO_PIHA.csv")
#
plot(piha1$year1,piha1$IBT_ag_m2, type="l", xlim=c(1950, 2022), ylim=c(0,600), xlab="Year", ylab=expression(bold(BAI~(mm~year^-1))))
##
#### 2 PIPI LOW
pipi2=read.csv("2BIO_PIPI_low.csv")
#
lines(pipi2$year1,pipi2$IBT_ag_m2, col="red")
##

#### 3 PIPI LOW 2
pipi3=read.csv("3BIO_PIPI_low2.csv")
#
lines(pipi3$year1,pipi3$IBT_ag_m2, col="red")
##
#### 4 PIPI med
pipi4=read.csv("4BIO_PIPI_med.csv")
#
lines(pipi4$year1,pipi4$IBT_ag_m2, col="red")
##

#### 5 PIPI HIGH
pipi5=read.csv("5BIO_PIPI_high.csv")
#
lines(pipi5$year1,pipi5$IBT_ag_m2, col="red")##

### 6 PINI LOW 2
pini6=read.csv("6BIO_PINI_low2.csv")
#
lines(pini6$year1,pini6$IBT_ag_m2, col="blue")##

### 7 PINI LOW-(MED)
pini7=read.csv("7BIO_PINI_low.csv")
#
lines(pini7$year1,pini7$IBT_ag_m2, col="blue")##

##
#### 8 PINI HIGH
pini8=read.csv("8BIO_PINI_high.csv")
#
lines(pini8$year1,pini8$IBT_ag_m2, col="blue")##

##
### 9 PISY LOW
pisy9=read.csv("9BIO_PISY_low.csv")
#
lines(pisy9$year1,pisy9$IBT_ag_m2, col="green")
##
### 10 PISY MED
pisy10=read.csv("10BIO_PISY_med.csv")
#
lines(pisy10$year1,pisy10$IBT_ag_m2, col="green")
#
### 11 PISY HIGH
pisy11=read.csv("11BIO_PISY_high.csv")
#
lines(pisy11$year1,pisy11$IBT_ag_m2, col="green")
#PIHA para que esté arriba
lines(piha1$year1,piha1$IBT_ag_m2, col="black")

#
par(xaxt="s", yaxt="s")
axis(1, lab=F)
axis(2)
axis(3)
axis(4, lab=F)
par(xaxt="n", yaxt="n")
#
#### PANEL 2 PINUS PINASTER

plot(pipi2$year1,pipi2$IBT_ag_m2, type="n", xlim=c(1950, 2022), ylim=c(0,600), xlab="Year", ylab=expression(bold(ABI~(g~C~m^2~year^-1))))
#PIPI LOW2
lines(pipi2$year1,pipi2$IBT_ag_m2, col="red")
##
#### 3 PIPI LOW 2
lines(pipi3$year1,pipi3$IBT_ag_m2, col="red")
##

#### 4 PIPI LOW 2
lines(pipi4$year1,pipi4$IBT_ag_m2, col="blue")
##

#### 5 PIPI HIGH
lines(pipi5$year1,pipi5$IBT_ag_m2, col="green")
##
par(xaxt="s", yaxt="s")
axis(1, lab=F)
axis(2, lab=F)
axis(3)
axis(4)
par(xaxt="n", yaxt="n")
#

####PANEL 3 PINUS NIGRA
plot(pini6$year,pini6$BAI, type="n", xlim=c(1950, 2022), ylim=c(0,600), xlab="Year", ylab=expression(bold(BAI~(mm~year^-1))))
### 6 PINI LOW 2
lines(pini6$year1,pini6$IBT_ag_m2, col="red")##
##
### 7 PINI LOW-(MED)

lines(pini7$year1,pini7$IBT_ag_m2, col="blue")##

#### 8 PINI HIGH
lines(pini8$year1,pini8$IBT_ag_m2, col="green")##

##
par(xaxt="s", yaxt="s")
axis(1)
axis(2)
axis(3, lab=F)
axis(4, lab=F)
par(xaxt="n", yaxt="n")
#

#### PANEL 4 PINUS SYLVESTRIS
plot(pisy9$year1,pisy9$IBT_ag_m2, type="n", xlim=c(1950, 2022), ylim=c(0,600), xlab="Year", ylab=expression(bold(BAI~(mm~year^-1))))

### 9 PISY LOW
lines(pisy9$year1,pisy9$IBT_ag_m2, col="red")
##
### 10 PISY MED
#
lines(pisy10$year1,pisy10$IBT_ag_m2, col="blue")
#
### 11 PISY HIGH
#
lines(pisy11$year1,pisy11$IBT_ag_m2, col="green")
#
#
par(xaxt="s", yaxt="s")
axis(1)
axis(2, lab=F)
axis(3, lab=F)
axis(4)
par(xaxt="n", yaxt="n", xpd=NA)
#
text(1947, -88, "Year", cex=1.21)
text(1860, 636, expression(bold(ABI~(g~C~m^2~year^-1))), srt=90, cex=1.21)
legend(1872, 1250, c("PIHA",  "PIPI", "PINI", "PISY"), col=c("black", "red", "blue", "green"), border=NA, ncol=1, cex=1, bty="o", bg=NA, lwd=2)
legend(1950, 1221, c("Low", "Medium",  "High"), col=c("red","blue", "green"), border=NA, ncol=1, cex=1, bty="o", bg=NA, lwd=2)
text(2011, 1221, expression(bolditalic("Pinus pinaster")), cex=1.21)
legend(1919, 575, c("Low", "Medium", "High"), col=c("red", "blue", "green"), border=NA, ncol=1, cex=1, bty="o", bg=NA, lwd=2)
text(1883, 575, expression(bolditalic("Pinus nigra")), cex=1.21)
legend(1950, 575, c("Low", "Medium", "High"), col=c("red", "blue", "green"), border=NA, ncol=1, cex=1, bty="o", bg=NA, lwd=2)
text(2010, 575, expression(bolditalic("Pinus sylvestris")), cex=1.21)
#
