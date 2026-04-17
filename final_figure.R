
#load libraries
library(car)
library(readxl)
library(ggplot2)
library(factoextra)
library(vegan)
library(lme4)

##Figure 1
dat=read_excel("all_data.xlsx","Species_traits")
dat=data.frame(dat)
dat=dat[,c(1:11)]
colnames(dat)= c("species","lma","ldmc","lnc","la","ht","seed","stem","savanna","origin","type")

pdf("PRSB-B-Fig1.pdf",width = 11, height = 8)

par(mfrow=c(2,3)) #rows,columns
par(oma=c(15,3.5,3.5,3.5)) #outer margin (bottom, left top, right)
par(mar=c(1.5, 6, 0, 0)) #margin around panel (bottom, left top, right)
#par(mgp=c(2,2,0)) #spacing between plotting region and (axis title, axis labels,axis line)

dat$type <- factor(dat$type, levels=c("BLS Native","BLS Invasive","FLS Native","FLS Invasive"))
#re-order type based on order required for plotting

colsav<-c(adjustcolor("white",alpha=0.75),adjustcolor("white",alpha=0.75),adjustcolor("darkgray",alpha=1), adjustcolor("darkgray",alpha=1))

#####LMA
boxplot(dat$lma~ dat$type, data = dat, lwd = 1,las=2, cex.lab=1, outline=F, ann=F, xaxt="n", 
        col=colsav, ylim=c(0,0.008))
axis(1, at=c(1,2,3,4), labels=F, cex.axis=1, las=1)
stripchart(dat$lma~ dat$type, vertical = TRUE, data = dat,
           pch=21, cex=2, col="black", bg=adjustcolor("gray15", alpha=.05), lwd=1,
           method = "jitter", add = TRUE, jitter=0.155)
mtext(expression(paste("Leaf Mass per Area (g",plain('\u00b7'),"cm"^-2,")")), side = 2, outer = F, cex = 1.2, line = 3.5, col = "black")
mtext(expression(paste("S:", italic(" ns"),", O:", italic(" ns"),", S x O:", italic(" ns"))),
      side=1, outer = F, cex = 0.8, line=-1,  col = "black")
mtext("(a)", side=3, line=-2, adj=0.95, cex=1.5)

#####LDMC
boxplot(dat$ldmc~ dat$type, data = dat, lwd = 1,las=2, cex.lab=1, outline=F, ann=F, xaxt="n", 
        col=colsav, ylim=c(-0.05,0.85))
axis(1, at=c(1,2,3,4), labels=F, cex.axis=1, las=1)
stripchart(dat$ldmc~ dat$type, vertical = TRUE, data = dat,
           pch=21, cex=2, col="black", bg=adjustcolor("gray15", alpha=.05), lwd=1,
           method = "jitter", add = TRUE, jitter=0.155)
mtext(expression(paste("Leaf Dry Matter Content (g",plain('\u00b7'),"g"^-1,")")), side = 2, outer = F, cex = 1.2, line = 3,col = "black")
mtext(expression(paste("S:", italic(" ns"),", O:", italic(" p < 0.001"),", S x O:", italic(" ns"))),
      side=1, outer = F, cex = 0.8, line=-1,  col = "black")
mtext("(b)", side=3, line=-2, adj=0.95, cex=1.5)

#####LNC
boxplot(dat$lnc~ dat$type, data = dat, lwd = 1,las=2, cex.lab=1, outline=F, 
        ann=F, xaxt="n", ylim=c(0.1,5.2), col=colsav)
axis(1, at=c(1,2,3,4), labels=F, cex.axis=1, las=1)
stripchart(dat$lnc~ dat$type, vertical = TRUE, data = dat,
           pch=21, cex=2, col="black", bg=adjustcolor("gray15", alpha=.05), lwd=1,
           method = "jitter", add = TRUE, jitter=0.155)
mtext("Leaf Nitrogen Content (%)", side = 2, outer = F, cex = 1.2, line = 3,col = "black")
mtext(expression(paste("S:", italic(" ns"),", O:", italic(" p < 0.001"),", S x O:", italic(" ns"))),
      side=1, outer = F, cex = 0.8, line=-1,  col = "black")
mtext("(c)", side=3, line=-2, adj=0.95, cex=1.5)

points(0.9,4.8, pch=22, cex=3, col=c("black"), bg=adjustcolor("white", alpha=.5), lwd=1)
points(0.9,4.325, pch=22, cex=3, col=c("black"), bg=adjustcolor("black", alpha=.5), lwd=1)
mtext("BLS", side=3, line=-2.2, adj=0.2)
mtext("FLS", side=3, line=-3.7, adj=0.2)

#####LA
boxplot(dat$la~ dat$type, data = dat, lwd = 1,las=2, cex.lab=1, outline=F, 
        ann=F, xaxt="n", yaxt="n", log="y", ylim=c(0.075,175), col=colsav)
stripchart(dat$la~ dat$type, vertical = TRUE, data = dat,
           pch=21, cex=2, col="black", bg=adjustcolor("gray15", alpha=.05), lwd=1,
           method = "jitter", add = TRUE, jitter=0.155)
mtext(expression(paste("Leaf Size (cm"^2,")")), side = 2, outer = F, cex = 1.2, line = 3,col = "black")
axis(1, at=c(1,2,3,4), labels=F, cex.axis=1, las=1)
marks=c(0.1,0.5,2,10,50,200)
axis(2, at=marks, labels=format(marks,scientific=FALSE), cex.axis=1, las=1)
mtext(side=1, at=1, "BLS\nNative", line=2.5);mtext(side=1, at=2, "BLS\nInvasive", line=2.5)
mtext(side=1, at=3, "FLS\nNative", line=2.5);mtext(side=1, at=4, "FLS\nInvasive", line=2.5)
mtext(expression(paste("S:", italic(" ns"),", O:", italic(" p < 0.01"),", S x O:", italic(" ns"))),
      side=1, outer = F, cex = 0.8, line=-1,  col = "black")
mtext("(d)", side=3, line=-2, adj=0.95, cex=1.5)
mtext(side=1, "Savanna Type / Origin", line=4.5, cex=1.2)

#####HT
boxplot(dat$ht~ dat$type, data = dat, lwd = 1,las=2, cex.lab=1, outline=F, 
        ann=F, xaxt="n", log="y", ylim=c(1.5,250), col=colsav)
axis(1, at=c(1,2,3,4), labels=F, cex.axis=1, las=1)
stripchart(dat$ht~ dat$type, vertical = TRUE, data = dat,
           pch=21, cex=2, col="black", bg=adjustcolor("gray15", alpha=.05), lwd=1,
           method = "jitter", add = TRUE, jitter=0.155)
mtext("Height (cm)", side = 2, outer = F, cex = 1.2, line = 3,col = "black")
axis(1, at=c(1,2,3,4), labels=F, cex.axis=1, las=1)
mtext(side=1, at=1, "BLS\nNative", line=2.5);mtext(side=1, at=2, "BLS\nInvasive", line=2.5)
mtext(side=1, at=3, "FLS\nNative", line=2.5);mtext(side=1, at=4, "FLS\nInvasive", line=2.5)
mtext(expression(paste("S:", italic(" p < 0.01"),", O:", italic(" p < 0.001"),", S x O:", italic(" ns"))),
      side=1, outer = F, cex = 0.8, line=-1,  col = "black")
mtext("(e)", side=3, line=-2, adj=0.95, cex=1.5)
mtext(side=1, "Savanna Type / Origin", line=4.5, cex=1.2)

#####SEED vol
boxplot(dat$seed~ dat$type, data = dat, lwd = 1,las=2, cex.lab=1, outline=F, 
        ann=F, xaxt="n", yaxt="n", log="y", ylim=c(0.04,2600), col=colsav)
axis(1, at=c(1,2,3,4), labels=F, cex.axis=1, las=1)
marks=c(0.1,1,10,100,1000)
axis(2, at=marks, labels=format(marks,scientific=FALSE), cex.axis=1, las=1)

stripchart(dat$seed~ dat$type, vertical = TRUE, data = dat,
           pch=21, cex=2, col="black", bg=adjustcolor("gray15", alpha=.05), lwd=1,
           method = "jitter", add = TRUE, jitter=0.155)
mtext(expression(paste("Seed Volume (mm"^3,")")), side = 2, outer = F, cex = 1.2, line = 3,col = "black")
axis(1, at=c(1,2,3,4), labels=F, cex.axis=1, las=1)
mtext(side=1, at=1, "BLS\nNative", line=2.5);mtext(side=1, at=2, "BLS\nInvasive", line=2.5)
mtext(side=1, at=3, "FLS\nNative", line=2.5);mtext(side=1, at=4, "FLS\nInvasive", line=2.5)
mtext(expression(paste("S:", italic(" ns"),", O:", italic("ns"),", S x O:", italic(" ns"))),
      side=1, outer = F, cex = 0.8, line=-1,  col = "black")
mtext("(f)", side=3, line=-2, adj=0.95, cex=1.5)
mtext(side=1, "Savanna Type / Origin", line=4.75, cex=1.2)

dev.off()

#Statistics
#for type-III anova using 'car' package
testlma<-Anova(lm(lma~savanna*origin, data=dat, contrasts=list(savanna=contr.sum, origin=contr.sum)), type=3)
testldmc<-Anova(lm(ldmc~savanna*origin, data=dat, contrasts=list(savanna=contr.sum, origin=contr.sum)), type=3)
testlnc<-Anova(lm(lnc~savanna*origin, data=dat, contrasts=list(savanna=contr.sum, origin=contr.sum)), type=3)
testla<-Anova(lm(la~savanna*origin, data=dat, contrasts=list(savanna=contr.sum, origin=contr.sum)), type=3)
testht<-Anova(lm(ht~savanna*origin, data=dat, contrasts=list(savanna=contr.sum, origin=contr.sum)), type=3)
dat$seed=log10(dat$seed)
testseed<-Anova(lm(seed~savanna*origin, data=dat, contrasts=list(savanna=contr.sum, origin=contr.sum)), type=3)



#Figure 2#

df=read_excel("all_data.xlsx","Community_weighted_trait")
df=data.frame(df)
colnames(df)=c("Site","invasion","savanna","Place","seed","ldmc","lma","la","ht","stem","lnc","invader")
pdf("CWM-no-color3.pdf",width=11, height=8)

par(mfrow=c(2,6)) #rows,columns
par(oma=c(15,3,4,4)) #outer margin (bottom, left top, right)
layout(matrix(1:12, nrow=2, ncol=6, byrow=TRUE), widths=c(1,1.4,1,1.4,1,1.4,1,1.4,1,1.4,1,1.4))

remBLS<-subset(df, df$savanna=="BLS" & df$invasion=="Remnant")
remFLS<-subset(df, df$savanna=="FLS" & df$invasion=="Remnant" & df$invader=="no")

FLSinv<-subset(df, df$savanna=="FLS" & df$invasion!="Uninvaded" & df$invasion!="Remnant" & df$invader=="yes")
FLSnat<-subset(df, df$savanna=="FLS" & df$invasion!="Remnant" & df$invader=="no")
BLSnat<-subset(df, df$savanna=="BLS" & df$invasion!="Remnant")

#FOR LMA_____________________________________________________________________________________
#FOR REMANANT BLS___________________________________________________________________
dat<-remBLS
par(mar=c(1.5, 6, 0, 0)) #margin around panel (bottom, left top, right)
plot(NULL, ylim=c(0.00275, 0.00475), xlim=c(0,2), ann=F, xaxt="n", yaxt="n")

LMA_mean<-mean(dat$lma); LMA_SD<-sd(dat$lma); LMA_n<-length(dat$lma)
LMA_SEM<-LMA_SD/sqrt(LMA_n)

par(new=TRUE)
points(0.80,LMA_mean, pch=21, cex=2.5, col="black", bg=adjustcolor("white", alpha=1), lwd=1)
arrows(0.80,LMA_mean-LMA_SEM, 0.80,LMA_mean+ LMA_SEM,angle=90, code=3, length=0.125, lwd=1, col="black")

#FOR REMANANT FLS___________________________________________________________________
par(new=TRUE)
dat<-remFLS

axis(1, at=c(1), labels=F, cex.axis=1, las=0)
axis(2, cex.axis=1.1, las=0)
mtext(expression(paste("Leaf Mass per Area (g",plain('\u00b7'),"cm"^-2,")")), side=2, outer=F, cex=1.2, line=2.5, col="black")

LMA_mean<-mean(dat$lma); LMA_SD<-sd(dat$lma); LMA_n<-length(dat$lma)
LMA_SEM<-LMA_SD/sqrt(LMA_n)

par(new=TRUE)
points(1.2,LMA_mean, pch=21, cex=2.5, col="black", bg=adjustcolor("black", alpha=.5), lwd=1)
arrows(1.2,LMA_mean-LMA_SEM, 1.2,LMA_mean+ LMA_SEM,angle=90, code=3, length=0.125, lwd=1, col="black")

#FOR MAIN BLS Native only___________________________________________________________________
dat<-BLSnat
dat$invasion<-factor(dat$invasion, levels=c("Uninvaded", "low", "Medium", "High"))
par(mar=c(1.5, 0.5, 0, 0)) #margin around panel (bottom, left top, right)
plot(NULL, ylim=c(0.00275, 0.00475), xlim=c(0.5,4.5), ann=F, xaxt="n", yaxt="n")
mtext("(a)", side=3, line=-2, adj=0.95, cex=1.5)
axis(1, at=c(1,2,3,4), labels=F, cex.axis=1, las=1)

LMA_mean<-aggregate(dat$lma~ dat$invasion, dat, FUN = "mean")
LMA_SD<-aggregate(dat$lma~ dat$invasion, FUN = "sd")
LMA_n<-aggregate(dat$lma~ dat$invasion, FUN = "length")
LMA_SEM<-LMA_SD[,2]/sqrt(LMA_n[,2])

par(new=TRUE)
x=c(0.8,1.8,2.8,3.8); y=LMA_mean[,2]
points(x,y, pch=21, cex=2.5, col=c("black"), bg=adjustcolor("white", alpha=.5), lwd=1)
arrows(0.8,LMA_mean[1,2]-LMA_SEM[1], 0.8,LMA_mean[1,2]+ LMA_SEM[1],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(1.8,LMA_mean[2,2]-LMA_SEM[2], 1.8,LMA_mean[2,2]+ LMA_SEM[2],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(2.8,LMA_mean[3,2]-LMA_SEM[3], 2.8,LMA_mean[3,2]+ LMA_SEM[3],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(3.8,LMA_mean[4,2]-LMA_SEM[4], 3.8,LMA_mean[4,2]+ LMA_SEM[4],angle=90, code=3, length=0.125, lwd=1, col="black")

arrows(0.8,LMA_mean[1,2],1.8,LMA_mean[2,2],angle=0, code=3, length=0, lwd=1, col="black")
arrows(1.8,LMA_mean[2,2],2.8,LMA_mean[3,2],angle=0, code=3, length=0, lwd=1, col="black")
arrows(2.8,LMA_mean[3,2],3.8,LMA_mean[4,2],angle=90, code=3, length=0, lwd=1, col="black")

#FOR MAIN FLS Native only___________________________________________________________________
dat<-FLSnat
dat$invasion<-factor(dat$invasion, levels=c("Uninvaded", "low", "Medium", "High"))
par(new=TRUE)

LMA_mean<-aggregate(dat$lma~ dat$invasion, dat, FUN = "mean")
LMA_SD<-aggregate(dat$lma~ dat$invasion, FUN = "sd")
LMA_n<-aggregate(dat$lma~ dat$invasion, FUN = "length")
LMA_SEM<-LMA_SD[,2]/sqrt(LMA_n[,2])

par(new=TRUE)
points(LMA_mean , pch=21, cex=2.5, col=c("black"), bg=adjustcolor("black", alpha=.5), lwd=1)
arrows(1,LMA_mean[1,2]-LMA_SEM[1], 1,LMA_mean[1,2]+ LMA_SEM[1],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(2,LMA_mean[2,2]-LMA_SEM[2], 2,LMA_mean[2,2]+ LMA_SEM[2],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(3,LMA_mean[3,2]-LMA_SEM[3], 3,LMA_mean[3,2]+ LMA_SEM[3],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(4,LMA_mean[4,2]-LMA_SEM[4], 4,LMA_mean[4,2]+ LMA_SEM[4],angle=90, code=3, length=0.125, lwd=1, col="black")

arrows(1,LMA_mean[1,2],2,LMA_mean[2,2],angle=90, code=3, length=0, lwd=1, col="black")
arrows(2,LMA_mean[2,2],3,LMA_mean[3,2],angle=90, code=3, length=0, lwd=1, col="black")
arrows(3,LMA_mean[3,2],4,LMA_mean[4,2],angle=90, code=3, length=0, lwd=1, col="black")

#FOR MAIN FLS with invasive___________________________________________________________________
dat<-FLSinv
dat$invasion<-factor(dat$invasion, levels=c("low", "Medium", "High"))
par(new=TRUE)

LMA_mean<-aggregate(dat$lma~ dat$invasion, dat, FUN = "mean")
LMA_SD<-aggregate(dat$lma~ dat$invasion, FUN = "sd")
LMA_n<-aggregate(dat$lma~ dat$invasion, FUN = "length")
LMA_SEM<-LMA_SD[,2]/sqrt(LMA_n[,2])

par(new=TRUE)
x=c(2.2,3.2,4.2); y=LMA_mean[,2]
points(x,y, pch=22, cex=2.5, col=c("black"), bg=adjustcolor("black", alpha=.5), lwd=1)
arrows(2.2,LMA_mean[1,2]-LMA_SEM[1], 2.2,LMA_mean[1,2]+ LMA_SEM[1],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(3.2,LMA_mean[2,2]-LMA_SEM[2], 3.2,LMA_mean[2,2]+ LMA_SEM[2],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(4.2,LMA_mean[3,2]-LMA_SEM[3], 4.2,LMA_mean[3,2]+ LMA_SEM[3],angle=90, code=3, length=0.125, lwd=1, col="black")

arrows(2.2,LMA_mean[1,2],3.2,LMA_mean[2,2],angle=90, lty=2, code=3, length=0, lwd=1, col="black")
arrows(3.2,LMA_mean[2,2],4.2,LMA_mean[3,2],angle=90, lty=2, code=3, length=0, lwd=1, col="black")

#plot LDMC________________________________________________________________________
#FOR REMANANT BLS___________________________________________________________________
dat<-remBLS
par(mar=c(1.5, 6, 0, 0)) #margin around panel (bottom, left top, right)
plot(NULL, ylim=c(0.2,0.6), xlim=c(0,2), ann=F, xaxt="n", yaxt="n")

ldmc_mean<-mean(dat$ldmc); ldmc_SD<-sd(dat$ldmc); ldmc_n<-length(dat$ldmc)
ldmc_SEM<-ldmc_SD/sqrt(ldmc_n)

par(new=TRUE)
points(0.80,ldmc_mean, pch=21, cex=2.5, col="black", bg=adjustcolor("white", alpha=1), lwd=1)
arrows(0.80,ldmc_mean-ldmc_SEM, 0.80,ldmc_mean+ ldmc_SEM,angle=90, code=3, length=0.125, lwd=1, col="black")

#FOR REMANANT FLS___________________________________________________________________
par(new=TRUE)
dat<-remFLS

axis(1, at=c(1), labels=F, cex.axis=1, las=1)
axis(2, cex.axis=1, las=1)
mtext(expression(paste("Leaf Dry Matter Content (g",plain('\u00b7'),"g"^-1,")")), side=2, outer=F, cex=1.2, line=2.5, col="black")

ldmc_mean<-mean(dat$ldmc); ldmc_SD<-sd(dat$ldmc); ldmc_n<-length(dat$ldmc)
ldmc_SEM<-ldmc_SD/sqrt(ldmc_n)

par(new=TRUE)
points(1.2,ldmc_mean, pch=21, cex=2.5, col="black", bg=adjustcolor("black", alpha=.5), lwd=1)
arrows(1.2,ldmc_mean-ldmc_SEM, 1.2,ldmc_mean+ ldmc_SEM,angle=90, code=3, length=0.125, lwd=1, col="black")

#FOR MAIN BLS NATIVE ONLY___________________________________________________________________
dat<-BLSnat
dat$invasion<-factor(dat$invasion, levels=c("Uninvaded", "low", "Medium", "High"))
par(mar=c(1.5, 0.5, 0, 0)) #margin around panel (bottom, left top, right)
plot(NULL, ylim=c(0.2,0.6), xlim=c(0.5,4.5), ann=F, xaxt="n", yaxt="n")
mtext("(b)", side=3, line=-2, adj=0.95, cex=1.5)
axis(1, at=c(1,2,3,4), labels=F, cex.axis=1, las=1)

ldmc_mean<-aggregate(dat$ldmc~ dat$invasion, dat, FUN = "mean")
ldmc_SD<-aggregate(dat$ldmc~ dat$invasion, FUN = "sd")
ldmc_n<-aggregate(dat$ldmc~ dat$invasion, FUN = "length")
ldmc_SEM<-ldmc_SD[,2]/sqrt(ldmc_n[,2])

par(new=TRUE)
x=c(0.8,1.8,2.8,3.8); y=ldmc_mean[,2]
points(x,y, pch=21, cex=2.5, col=c("black"), bg=adjustcolor("white", alpha=.5), lwd=1)
arrows(0.8,ldmc_mean[1,2]-ldmc_SEM[1], 0.8,ldmc_mean[1,2]+ ldmc_SEM[1],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(1.8,ldmc_mean[2,2]-ldmc_SEM[2], 1.8,ldmc_mean[2,2]+ ldmc_SEM[2],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(2.8,ldmc_mean[3,2]-ldmc_SEM[3], 2.8,ldmc_mean[3,2]+ ldmc_SEM[3],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(3.8,ldmc_mean[4,2]-ldmc_SEM[4], 3.8,ldmc_mean[4,2]+ ldmc_SEM[4],angle=90, code=3, length=0.125, lwd=1, col="black")

arrows(0.8,ldmc_mean[1,2],1.8,ldmc_mean[2,2],angle=0, code=3, length=0, lwd=1, col="black")
arrows(1.8,ldmc_mean[2,2],2.8,ldmc_mean[3,2],angle=0, code=3, length=0, lwd=1, col="black")
arrows(2.8,ldmc_mean[3,2],3.8,ldmc_mean[4,2],angle=90, code=3, length=0, lwd=1, col="black")

#FOR MAIN FLS Native only___________________________________________________________________
dat<-FLSnat
dat$invasion<-factor(dat$invasion, levels=c("Uninvaded", "low", "Medium", "High"))
par(new=TRUE)

ldmc_mean<-aggregate(dat$ldmc~ dat$invasion, dat, FUN = "mean")
ldmc_SD<-aggregate(dat$ldmc~ dat$invasion, FUN = "sd")
ldmc_n<-aggregate(dat$ldmc~ dat$invasion, FUN = "length")
ldmc_SEM<-ldmc_SD[,2]/sqrt(ldmc_n[,2])

par(new=TRUE)
points(ldmc_mean , pch=21, cex=2.5, col=c("black"), bg=adjustcolor("black", alpha=.5), lwd=1)
arrows(1,ldmc_mean[1,2]-ldmc_SEM[1], 1,ldmc_mean[1,2]+ ldmc_SEM[1],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(2,ldmc_mean[2,2]-ldmc_SEM[2], 2,ldmc_mean[2,2]+ ldmc_SEM[2],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(3,ldmc_mean[3,2]-ldmc_SEM[3], 3,ldmc_mean[3,2]+ ldmc_SEM[3],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(4,ldmc_mean[4,2]-ldmc_SEM[4], 4,ldmc_mean[4,2]+ ldmc_SEM[4],angle=90, code=3, length=0.125, lwd=1, col="black")

arrows(1,ldmc_mean[1,2],2,ldmc_mean[2,2],angle=90, code=3, length=0, lwd=1, col="black")
arrows(2,ldmc_mean[2,2],3,ldmc_mean[3,2],angle=90, code=3, length=0, lwd=1, col="black")
arrows(3,ldmc_mean[3,2],4,ldmc_mean[4,2],angle=90, code=3, length=0, lwd=1, col="black")

#FOR MAIN FLS INVASIVE___________________________________________________________________
dat<-FLSinv
dat$invasion<-factor(dat$invasion, levels=c("low", "Medium", "High"))
par(new=TRUE)

ldmc_mean<-aggregate(dat$ldmc~ dat$invasion, dat, FUN = "mean")
ldmc_SD<-aggregate(dat$ldmc~ dat$invasion, FUN = "sd")
ldmc_n<-aggregate(dat$ldmc~ dat$invasion, FUN = "length")
ldmc_SEM<-ldmc_SD[,2]/sqrt(ldmc_n[,2])

par(new=TRUE)
x=c(2.2,3.2,4.2); y=ldmc_mean[,2]
points(x,y, pch=22, cex=2.5, col=c("black"), bg=adjustcolor("black", alpha=.5), lwd=1)
arrows(2.2,ldmc_mean[1,2]-ldmc_SEM[1], 2.2,ldmc_mean[1,2]+ ldmc_SEM[1],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(3.2,ldmc_mean[2,2]-ldmc_SEM[2], 3.2,ldmc_mean[2,2]+ ldmc_SEM[2],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(4.2,ldmc_mean[3,2]-ldmc_SEM[3], 4.2,ldmc_mean[3,2]+ ldmc_SEM[3],angle=90, code=3, length=0.125, lwd=1, col="black")

arrows(2.2,ldmc_mean[1,2],3.2,ldmc_mean[2,2],angle=90, lty=2, code=3, length=0, lwd=1, col="black")
arrows(3.2,ldmc_mean[2,2],4.2,ldmc_mean[3,2],angle=90, lty=2, code=3, length=0, lwd=1, col="black")

#plot LNC________________________________________________________________________
#FOR REMANANT BLS___________________________________________________________________
dat<-remBLS
par(mar=c(1.5, 6, 0, 0)) #margin around panel (bottom, left top, right)
plot(NULL, ylim=c(1, 3), xlim=c(0,2), ann=F, xaxt="n", yaxt="n")

lnc_mean<-mean(dat$lnc); lnc_SD<-sd(dat$lnc); lnc_n<-length(dat$lnc)
lnc_SEM<-lnc_SD/sqrt(lnc_n)

par(new=TRUE)
points(0.80,lnc_mean, pch=21, cex=2.5, col="black", bg=adjustcolor("white", alpha=1), lwd=1)
arrows(0.80,lnc_mean-lnc_SEM, 0.80,lnc_mean+ lnc_SEM,angle=90, code=3, length=0.125, lwd=1, col="black")

#FOR REMANANT FLS___________________________________________________________________
par(new=TRUE)
dat<-remFLS

axis(1, at=c(1), labels=F, cex.axis=1, las=1)
axis(2, cex.axis=1, las=1)
mtext("Leaf Nitrogen Content (%)", side=2, outer=F, cex=1.2, line=2.5, col="black")

lnc_mean<-mean(dat$lnc); lnc_SD<-sd(dat$lnc); lnc_n<-length(dat$lnc)
lnc_SEM<-lnc_SD/sqrt(lnc_n)

par(new=TRUE)
points(1.2,lnc_mean, pch=21, cex=2.5, col="black", bg=adjustcolor("black", alpha=.5), lwd=1)
arrows(1.2,lnc_mean-lnc_SEM, 1.2,lnc_mean+ lnc_SEM,angle=90, code=3, length=0.125, lwd=1, col="black")

#FOR MAIN BLS NATIVE ONLY___________________________________________________________________
dat<-BLSnat
dat$invasion<-factor(dat$invasion, levels=c("Uninvaded", "low", "Medium", "High"))
par(mar=c(1.5, 0.5, 0, 0)) #margin around panel (bottom, left top, right)
plot(NULL, ylim=c(1,3), xlim=c(0.5,4.5), ann=F, xaxt="n", yaxt="n")
mtext("(c)", side=3, line=-2, adj=0.95, cex=1.5)
axis(1, at=c(1,2,3,4), labels=F, cex.axis=1, las=1)

points(0.75,2.95, pch=21, cex=2.5, col=c("black"), bg=adjustcolor("white", alpha=.5), lwd=1)
points(0.75,2.8, pch=21, cex=2.5, col=c("black"), bg=adjustcolor("black", alpha=.5), lwd=1)
points(0.75,2.65, pch=22, cex=2.5, col=c("black"), bg=adjustcolor("black", alpha=.5), lwd=1)
segments(0.45, 2.95, 1.05, 2.95); mtext("BLS (Native)", side=3, line=-1.9, adj=.35)
segments(0.45, 2.8, 1.05, 2.8); mtext("FLS (Native)", side=3, line=-3.1, adj=0.35)
segments(0.45, 2.65, 1.05, 2.65, lty=2.25)
mtext("FLS (N + I)", side=3, line=-4.5, adj=0.34)

lnc_mean<-aggregate(dat$lnc~ dat$invasion, dat, FUN = "mean")
lnc_SD<-aggregate(dat$lnc~ dat$invasion, FUN = "sd")
lnc_n<-aggregate(dat$lnc~ dat$invasion, FUN = "length")
lnc_SEM<-lnc_SD[,2]/sqrt(lnc_n[,2])

par(new=TRUE)
x=c(0.8,1.8,2.8,3.8); y=lnc_mean[,2]
points(x,y, pch=21, cex=2.5, col=c("black"), bg=adjustcolor("white", alpha=.5), lwd=1)
arrows(0.8,lnc_mean[1,2]-lnc_SEM[1], 0.8,lnc_mean[1,2]+ lnc_SEM[1],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(1.8,lnc_mean[2,2]-lnc_SEM[2], 1.8,lnc_mean[2,2]+ lnc_SEM[2],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(2.8,lnc_mean[3,2]-lnc_SEM[3], 2.8,lnc_mean[3,2]+ lnc_SEM[3],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(3.8,lnc_mean[4,2]-lnc_SEM[4], 3.8,lnc_mean[4,2]+ lnc_SEM[4],angle=90, code=3, length=0.125, lwd=1, col="black")

arrows(0.8,lnc_mean[1,2],1.8,lnc_mean[2,2],angle=0, code=3, length=0, lwd=1, col="black")
arrows(1.8,lnc_mean[2,2],2.8,lnc_mean[3,2],angle=0, code=3, length=0, lwd=1, col="black")
arrows(2.8,lnc_mean[3,2],3.8,lnc_mean[4,2],angle=90, code=3, length=0, lwd=1, col="black")

#FOR MAIN FLS Native only___________________________________________________________________
dat<-FLSnat
dat$invasion<-factor(dat$invasion, levels=c("Uninvaded", "low", "Medium", "High"))
par(new=TRUE)

lnc_mean<-aggregate(dat$lnc~ dat$invasion, dat, FUN = "mean")
lnc_SD<-aggregate(dat$lnc~ dat$invasion, FUN = "sd")
lnc_n<-aggregate(dat$lnc~ dat$invasion, FUN = "length")
lnc_SEM<-lnc_SD[,2]/sqrt(lnc_n[,2])

par(new=TRUE)
points(lnc_mean , pch=21, cex=2.5, col=c("black"), bg=adjustcolor("black", alpha=.5), lwd=1)
arrows(1,lnc_mean[1,2]-lnc_SEM[1], 1,lnc_mean[1,2]+ lnc_SEM[1],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(2,lnc_mean[2,2]-lnc_SEM[2], 2,lnc_mean[2,2]+ lnc_SEM[2],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(3,lnc_mean[3,2]-lnc_SEM[3], 3,lnc_mean[3,2]+ lnc_SEM[3],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(4,lnc_mean[4,2]-lnc_SEM[4], 4,lnc_mean[4,2]+ lnc_SEM[4],angle=90, code=3, length=0.125, lwd=1, col="black")

arrows(1,lnc_mean[1,2],2,lnc_mean[2,2],angle=90, code=3, length=0, lwd=1, col="black")
arrows(2,lnc_mean[2,2],3,lnc_mean[3,2],angle=90, code=3, length=0, lwd=1, col="black")
arrows(3,lnc_mean[3,2],4,lnc_mean[4,2],angle=90, code=3, length=0, lwd=1, col="black")

#FOR MAIN FLS WITH INVASIVE___________________________________________________________________
dat<-FLSinv
dat$invasion<-factor(dat$invasion, levels=c("Uninvaded", "low", "Medium", "High"))
par(new=TRUE)

lnc_mean<-aggregate(dat$lnc~ dat$invasion, dat, FUN = "mean")
lnc_SD<-aggregate(dat$lnc~ dat$invasion, FUN = "sd")
lnc_n<-aggregate(dat$lnc~ dat$invasion, FUN = "length")
lnc_SEM<-lnc_SD[,2]/sqrt(lnc_n[,2])

par(new=TRUE)
x=c(2.2,3.2,4.2); y=lnc_mean[,2]
points(x,y, pch=22, cex=2.5, col=c("black"), bg=adjustcolor("black", alpha=.5), lwd=1)
arrows(2.2,lnc_mean[1,2]-lnc_SEM[1], 2.2,lnc_mean[1,2]+ lnc_SEM[1],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(3.2,lnc_mean[2,2]-lnc_SEM[2], 3.2,lnc_mean[2,2]+ lnc_SEM[2],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(4.2,lnc_mean[3,2]-lnc_SEM[3], 4.2,lnc_mean[3,2]+ lnc_SEM[3],angle=90, code=3, length=0.125, lwd=1, col="black")

arrows(2.2,lnc_mean[1,2],3.2,lnc_mean[2,2],angle=90, lty=2, code=3, length=0, lwd=1, col="black")
arrows(3.2,lnc_mean[2,2],4.2,lnc_mean[3,2],angle=90, lty=2, code=3, length=0, lwd=1, col="black")

#plot LA________________________________________________________________________
#FOR REMANANT BLS___________________________________________________________________
dat<-remBLS
par(mar=c(1.5, 6, 0, 0)) #margin around panel (bottom, left top, right)
plot(NULL, ylim=c(1,40), xlim=c(0,2), ann=F, xaxt="n", yaxt="n", log="y")

la_mean<-mean(dat$la); la_SD<-sd(dat$la); la_n<-length(dat$la)
la_SEM<-la_SD/sqrt(la_n)

par(new=TRUE)
points(0.80,la_mean, pch=21, cex=2.5, col="black", bg=adjustcolor("white", alpha=1), lwd=1)
arrows(0.80,la_mean-la_SEM, 0.80,la_mean+ la_SEM,angle=90, code=3, length=0.125, lwd=1, col="black")

#FOR REMANANT FLS___________________________________________________________________
par(new=TRUE)
dat<-remFLS

axis(1, at=c(1), cex.axis=1, las=1, labels=c("Rem"), cex.axis=1.45)
marks=c(1,2,5,10, 20, 40)
axis(2, at=marks, labels=format(marks,scientific=FALSE), cex.axis=1, las=1)

mtext(expression(paste("Leaf Size (cm"^2,")")), side=2, outer=F, cex=1.2, line=2.5, col="black")

la_mean<-mean(dat$la); la_SD<-sd(dat$la); la_n<-length(dat$la)
la_SEM<-la_SD/sqrt(la_n)

par(new=TRUE)
points(1.2,la_mean, pch=21, cex=2.5, col="black", bg=adjustcolor("black", alpha=.5), lwd=1)
arrows(1.2,la_mean-la_SEM, 1.2,la_mean+ la_SEM,angle=90, code=3, length=0.125, lwd=1, col="black")

#FOR MAIN BLS NATIVE ONLY___________________________________________________________________
dat<-BLSnat
dat$invasion<-factor(dat$invasion, levels=c("Uninvaded", "low", "Medium", "High"))
par(mar=c(1.5, 0.5, 0, 0)) #margin around panel (bottom, left top, right)
plot(NULL, ylim=c(1,40), xlim=c(0.5,4.5), ann=F, xaxt="n", yaxt="n", log="y")
mtext("(d)", side=3, line=-2, adj=0.95, cex=1.5)
axis(1, at=c(1,2,3,4), labels=c("Un-I","Low","Med","Hi"), cex.axis=1.45, las=1)
mtext("Invasion Gradient", side=1, line=3, adj=-0.25, cex=1.2)

la_mean<-aggregate(dat$la~ dat$invasion, dat, FUN = "mean")
la_SD<-aggregate(dat$la~ dat$invasion, FUN = "sd")
la_n<-aggregate(dat$la~ dat$invasion, FUN = "length")
la_SEM<-la_SD[,2]/sqrt(la_n[,2])

par(new=TRUE)
x=c(0.8,1.8,2.8,3.8); y=la_mean[,2]
points(x,y, pch=21, cex=2.5, col=c("black"), bg=adjustcolor("white", alpha=.5), lwd=1)
arrows(0.8,la_mean[1,2]-la_SEM[1], 0.8,la_mean[1,2]+ la_SEM[1],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(1.8,la_mean[2,2]-la_SEM[2], 1.8,la_mean[2,2]+ la_SEM[2],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(2.8,la_mean[3,2]-la_SEM[3], 2.8,la_mean[3,2]+ la_SEM[3],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(3.8,la_mean[4,2]-la_SEM[4], 3.8,la_mean[4,2]+ la_SEM[4],angle=90, code=3, length=0.125, lwd=1, col="black")

arrows(0.8,la_mean[1,2],1.8,la_mean[2,2],angle=0, code=3, length=0, lwd=1, col="black")
arrows(1.8,la_mean[2,2],2.8,la_mean[3,2],angle=0, code=3, length=0, lwd=1, col="black")
arrows(2.8,la_mean[3,2],3.8,la_mean[4,2],angle=90, code=3, length=0, lwd=1, col="black")

#FOR MAIN FLS Native only___________________________________________________________________
dat<-FLSnat
dat$invasion<-factor(dat$invasion, levels=c("Uninvaded", "low", "Medium", "High"))
par(new=TRUE)

la_mean<-aggregate(dat$la~ dat$invasion, dat, FUN = "mean")
la_SD<-aggregate(dat$la~ dat$invasion, FUN = "sd")
la_n<-aggregate(dat$la~ dat$invasion, FUN = "length")
la_SEM<-la_SD[,2]/sqrt(la_n[,2])

par(new=TRUE)
points(la_mean , pch=21, cex=2.5, col=c("black"), bg=adjustcolor("black", alpha=.5), lwd=1)
arrows(1,la_mean[1,2]-la_SEM[1], 1,la_mean[1,2]+ la_SEM[1],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(2,la_mean[2,2]-la_SEM[2], 2,la_mean[2,2]+ la_SEM[2],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(3,la_mean[3,2]-la_SEM[3], 3,la_mean[3,2]+ la_SEM[3],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(4,la_mean[4,2]-la_SEM[4], 4,la_mean[4,2]+ la_SEM[4],angle=90, code=3, length=0.125, lwd=1, col="black")

arrows(1,la_mean[1,2],2,la_mean[2,2],angle=90, code=3, length=0, lwd=1, col="black")
arrows(2,la_mean[2,2],3,la_mean[3,2],angle=90, code=3, length=0, lwd=1, col="black")
arrows(3,la_mean[3,2],4,la_mean[4,2],angle=90, code=3, length=0, lwd=1, col="black")

#FOR MAIN FLS WITH INVASIVE___________________________________________________________________
dat<-FLSinv
dat$invasion<-factor(dat$invasion, levels=c("Uninvaded", "low", "Medium", "High"))
par(new=TRUE)

la_mean<-aggregate(dat$la~ dat$invasion, dat, FUN = "mean")
la_SD<-aggregate(dat$la~ dat$invasion, FUN = "sd")
la_n<-aggregate(dat$la~ dat$invasion, FUN = "length")
la_SEM<-la_SD[,2]/sqrt(la_n[,2])

par(new=TRUE)
x=c(2.2,3.2,4.2); y=la_mean[,2]
points(x,y, pch=22, cex=2.5, col=c("black"), bg=adjustcolor("black", alpha=.5), lwd=1)
arrows(2.2,la_mean[1,2]-la_SEM[1], 2.2,la_mean[1,2]+ la_SEM[1],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(3.2,la_mean[2,2]-la_SEM[2], 3.2,la_mean[2,2]+ la_SEM[2],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(4.2,la_mean[3,2]-la_SEM[3], 4.2,la_mean[3,2]+ la_SEM[3],angle=90, code=3, length=0.125, lwd=1, col="black")

arrows(2.2,la_mean[1,2],3.2,la_mean[2,2],angle=90, lty=2, code=3, length=0, lwd=1, col="black")
arrows(3.2,la_mean[2,2],4.2,la_mean[3,2],angle=90, lty=2, code=3, length=0, lwd=1, col="black")

#plot Height________________________________________________________________________
#FOR REMANANT BLS___________________________________________________________________
dat<-remBLS
par(mar=c(1.5, 6, 0, 0)) #margin around panel (bottom, left top, right)
plot(NULL, ylim=c(20,50), xlim=c(0,2), ann=F, xaxt="n", yaxt="n", log="y")

ht_mean<-mean(dat$ht); ht_SD<-sd(dat$ht); ht_n<-length(dat$ht)
ht_SEM<-ht_SD/sqrt(ht_n)

par(new=TRUE)
points(0.80,ht_mean, pch=21, cex=2.5, col="black", bg=adjustcolor("white", alpha=1), lwd=1)
arrows(0.80,ht_mean-ht_SEM, 0.80,ht_mean+ ht_SEM,angle=90, code=3, length=0.125, lwd=1, col="black")

#FOR REMANANT FLS___________________________________________________________________
par(new=TRUE)
dat<-remFLS

axis(1, at=c(1), labels=c("Rem"), cex.axis=1.45, las=1)
axis(2, cex.axis=1, las=1)
mtext("Height (cm)", side=2, outer=F, cex=1.2, line=2.5, col="black")

ht_mean<-mean(dat$ht); ht_SD<-sd(dat$ht); ht_n<-length(dat$ht)
ht_SEM<-ht_SD/sqrt(ht_n)

par(new=TRUE)
points(1.2,ht_mean, pch=21, cex=2.5, col="black", bg=adjustcolor("black", alpha=.5), lwd=1)
arrows(1.2,ht_mean-ht_SEM, 1.2,ht_mean+ ht_SEM,angle=90, code=3, length=0.125, lwd=1, col="black")

#FOR MAIN BLS NATIVE ONLY___________________________________________________________________
dat<-BLSnat
dat$invasion<-factor(dat$invasion, levels=c("Uninvaded", "low", "Medium", "High"))
par(mar=c(1.5, 0.5, 0, 0)) #margin around panel (bottom, left top, right)
plot(NULL, ylim=c(20,50), xlim=c(0.5,4.5), ann=F, xaxt="n", yaxt="n", log="y")
mtext("(e)", side=3, line=-2, adj=0.95, cex=1.5)
axis(1, at=c(1,2,3,4), labels=c("Un-I","Low","Med","Hi"), cex.axis=1.45, las=1)
mtext("Invasion Gradient", side=1, line=3, adj=-0.25, cex=1.2)

ht_mean<-aggregate(dat$ht~ dat$invasion, dat, FUN = "mean")
ht_SD<-aggregate(dat$ht~ dat$invasion, FUN = "sd")
ht_n<-aggregate(dat$ht~ dat$invasion, FUN = "length")
ht_SEM<-ht_SD[,2]/sqrt(ht_n[,2])

par(new=TRUE)
x=c(0.8,1.8,2.8,3.8); y=ht_mean[,2]
points(x,y, pch=21, cex=2.5, col=c("black"), bg=adjustcolor("white", alpha=.5), lwd=1)
arrows(0.8,ht_mean[1,2]-ht_SEM[1], 0.8,ht_mean[1,2]+ ht_SEM[1],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(1.8,ht_mean[2,2]-ht_SEM[2], 1.8,ht_mean[2,2]+ ht_SEM[2],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(2.8,ht_mean[3,2]-ht_SEM[3], 2.8,ht_mean[3,2]+ ht_SEM[3],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(3.8,ht_mean[4,2]-ht_SEM[4], 3.8,ht_mean[4,2]+ ht_SEM[4],angle=90, code=3, length=0.125, lwd=1, col="black")

arrows(0.8,ht_mean[1,2],1.8,ht_mean[2,2],angle=0, code=3, length=0, lwd=1, col="black")
arrows(1.8,ht_mean[2,2],2.8,ht_mean[3,2],angle=0, code=3, length=0, lwd=1, col="black")
arrows(2.8,ht_mean[3,2],3.8,ht_mean[4,2],angle=90, code=3, length=0, lwd=1, col="black")

#FOR MAIN FLS Native only___________________________________________________________________
dat<-FLSnat
dat$invasion<-factor(dat$invasion, levels=c("Uninvaded", "low", "Medium", "High"))
par(new=TRUE)

ht_mean<-aggregate(dat$ht~ dat$invasion, dat, FUN = "mean")
ht_SD<-aggregate(dat$ht~ dat$invasion, FUN = "sd")
ht_n<-aggregate(dat$ht~ dat$invasion, FUN = "length")
ht_SEM<-ht_SD[,2]/sqrt(ht_n[,2])

par(new=TRUE)
points(ht_mean , pch=21, cex=2.5, col=c("black"), bg=adjustcolor("black", alpha=.5), lwd=1)
arrows(1,ht_mean[1,2]-ht_SEM[1], 1,ht_mean[1,2]+ ht_SEM[1],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(2,ht_mean[2,2]-ht_SEM[2], 2,ht_mean[2,2]+ ht_SEM[2],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(3,ht_mean[3,2]-ht_SEM[3], 3,ht_mean[3,2]+ ht_SEM[3],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(4,ht_mean[4,2]-ht_SEM[4], 4,ht_mean[4,2]+ ht_SEM[4],angle=90, code=3, length=0.125, lwd=1, col="black")

arrows(1,ht_mean[1,2],2,ht_mean[2,2],angle=90, code=3, length=0, lwd=1, col="black")
arrows(2,ht_mean[2,2],3,ht_mean[3,2],angle=90, code=3, length=0, lwd=1, col="black")
arrows(3,ht_mean[3,2],4,ht_mean[4,2],angle=90, code=3, length=0, lwd=1, col="black")

#FOR MAIN FLS WITH INVASIVE___________________________________________________________________
dat<-FLSinv
dat$invasion<-factor(dat$invasion, levels=c("Uninvaded", "low", "Medium", "High"))
par(new=TRUE)

ht_mean<-aggregate(dat$ht~ dat$invasion, dat, FUN = "mean")
ht_SD<-aggregate(dat$ht~ dat$invasion, FUN = "sd")
ht_n<-aggregate(dat$ht~ dat$invasion, FUN = "length")
ht_SEM<-ht_SD[,2]/sqrt(ht_n[,2])

par(new=TRUE)
x=c(2.2,3.2,4.2); y=ht_mean[,2]
points(x,y, pch=22, cex=2.5, col=c("black"), bg=adjustcolor("black", alpha=.5), lwd=1)
arrows(2.2,ht_mean[1,2]-ht_SEM[1], 2.2,ht_mean[1,2]+ ht_SEM[1],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(3.2,ht_mean[2,2]-ht_SEM[2], 3.2,ht_mean[2,2]+ ht_SEM[2],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(4.2,ht_mean[3,2]-ht_SEM[3], 4.2,ht_mean[3,2]+ ht_SEM[3],angle=90, code=3, length=0.125, lwd=1, col="black")

arrows(2.2,ht_mean[1,2],3.2,ht_mean[2,2],angle=90, lty=2, code=3, length=0, lwd=1, col="black")
arrows(3.2,ht_mean[2,2],4.2,ht_mean[3,2],angle=90, lty=2, code=3, length=0, lwd=1, col="black")

#plot Seed________________________________________________________________________
#FOR REMANANT BLS___________________________________________________________________
dat<-remBLS
par(mar=c(1.5, 6, 0, 0)) #margin around panel (bottom, left top, right)
plot(NULL, ylim=c(8,420), xlim=c(0,2), ann=F, xaxt="n", yaxt="n", log="y")

seed_mean<-mean(dat$seed); seed_SD<-sd(dat$seed); seed_n<-length(dat$seed)
seed_SEM<-seed_SD/sqrt(seed_n)

par(new=TRUE)
points(0.80,seed_mean, pch=21, cex=2.5, col="black", bg=adjustcolor("white", alpha=1), lwd=1)
arrows(0.80,seed_mean-seed_SEM, 0.80,seed_mean+ seed_SEM,angle=90, code=3, length=0.125, lwd=1, col="black")

#FOR REMANANT FLS___________________________________________________________________
par(new=TRUE)
dat<-remFLS

axis(1, at=c(1), labels=c("Rem"), cex.axis=1.45, las=1)
marks=c(10,20,50,100,200,400)
axis(2, at=marks, labels=format(marks,scientific=FALSE), cex.axis=1, las=1)

mtext(expression(paste("Seed Volume (mm"^3,")")), side=2, outer=F, cex=1.2, line=2.5, col="black")

seed_mean<-mean(dat$seed); seed_SD<-sd(dat$seed); seed_n<-length(dat$seed)
seed_SEM<-seed_SD/sqrt(seed_n)

par(new=TRUE)
points(1.2,seed_mean, pch=21, cex=2.5, col="black", bg=adjustcolor("black", alpha=.5), lwd=1)
arrows(1.2,seed_mean-seed_SEM, 1.2,seed_mean+ seed_SEM,angle=90, code=3, length=0.125, lwd=1, col="black")

#FOR MAIN BLS NATIVE ONLY___________________________________________________________________
dat<-BLSnat
dat$invasion<-factor(dat$invasion, levels=c("Uninvaded", "low", "Medium", "High"))
par(mar=c(1.5, 0.5, 0, 0)) #margin around panel (bottom, left top, right)
plot(NULL, ylim=c(8,420), xlim=c(0.5,4.5), ann=F, xaxt="n", yaxt="n", log="y")
mtext("(f)", side=3, line=-2, adj=0.95, cex=1.5)
axis(1, at=c(1,2,3,4), labels=c("Un-I","Low","Med","Hi"), cex.axis=1.45, las=1)
mtext("Invasion Gradient", side=1, line=3, adj=-0.25, cex=1.2)

seed_mean<-aggregate(dat$seed~ dat$invasion, dat, FUN = "mean")
seed_SD<-aggregate(dat$seed~ dat$invasion, FUN = "sd")
seed_n<-aggregate(dat$seed~ dat$invasion, FUN = "length")
seed_SEM<-seed_SD[,2]/sqrt(seed_n[,2])

par(new=TRUE)
x=c(0.8,1.8,2.8,3.8); y=seed_mean[,2]
points(x,y, pch=21, cex=2.5, col=c("black"), bg=adjustcolor("white", alpha=.5), lwd=1)
arrows(0.8,seed_mean[1,2]-seed_SEM[1], 0.8,seed_mean[1,2]+ seed_SEM[1],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(1.8,seed_mean[2,2]-seed_SEM[2], 1.8,seed_mean[2,2]+ seed_SEM[2],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(2.8,seed_mean[3,2]-seed_SEM[3], 2.8,seed_mean[3,2]+ seed_SEM[3],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(3.8,seed_mean[4,2]-seed_SEM[4], 3.8,seed_mean[4,2]+ seed_SEM[4],angle=90, code=3, length=0.125, lwd=1, col="black")

arrows(0.8,seed_mean[1,2],1.8,seed_mean[2,2],angle=0, code=3, length=0, lwd=1, col="black")
arrows(1.8,seed_mean[2,2],2.8,seed_mean[3,2],angle=0, code=3, length=0, lwd=1, col="black")
arrows(2.8,seed_mean[3,2],3.8,seed_mean[4,2],angle=90, code=3, length=0, lwd=1, col="black")

#FOR MAIN FLS Native only___________________________________________________________________
dat<-FLSnat
dat$invasion<-factor(dat$invasion, levels=c("Uninvaded", "low", "Medium", "High"))
par(new=TRUE)

seed_mean<-aggregate(dat$seed~ dat$invasion, dat, FUN = "mean")
seed_SD<-aggregate(dat$seed~ dat$invasion, FUN = "sd")
seed_n<-aggregate(dat$seed~ dat$invasion, FUN = "length")
seed_SEM<-seed_SD[,2]/sqrt(seed_n[,2])

par(new=TRUE)
points(seed_mean , pch=21, cex=2.5, col=c("black"), bg=adjustcolor("black", alpha=.5), lwd=1)
arrows(1,seed_mean[1,2]-seed_SEM[1], 1,seed_mean[1,2]+ seed_SEM[1],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(2,seed_mean[2,2]-seed_SEM[2], 2,seed_mean[2,2]+ seed_SEM[2],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(3,seed_mean[3,2]-seed_SEM[3], 3,seed_mean[3,2]+ seed_SEM[3],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(4,seed_mean[4,2]-seed_SEM[4], 4,seed_mean[4,2]+ seed_SEM[4],angle=90, code=3, length=0.125, lwd=1, col="black")

arrows(1,seed_mean[1,2],2,seed_mean[2,2],angle=90, code=3, length=0, lwd=1, col="black")
arrows(2,seed_mean[2,2],3,seed_mean[3,2],angle=90, code=3, length=0, lwd=1, col="black")
arrows(3,seed_mean[3,2],4,seed_mean[4,2],angle=90, code=3, length=0, lwd=1, col="black")

#FOR MAIN FLS WITH INVASIVE___________________________________________________________________
dat<-FLSinv
dat$invasion<-factor(dat$invasion, levels=c("Uninvaded", "low", "Medium", "High"))
par(new=TRUE)

seed_mean<-aggregate(dat$seed~ dat$invasion, dat, FUN = "mean")
seed_SD<-aggregate(dat$seed~ dat$invasion, FUN = "sd")
seed_n<-aggregate(dat$seed~ dat$invasion, FUN = "length")
seed_SEM<-seed_SD[,2]/sqrt(seed_n[,2])

par(new=TRUE)
x=c(2.2,3.2,4.2); y=seed_mean[,2]
points(x,y, pch=22, cex=2.5, col=c("black"), bg=adjustcolor("black", alpha=.5), lwd=1)
arrows(2.2,seed_mean[1,2]-seed_SEM[1], 2.2,seed_mean[1,2]+ seed_SEM[1],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(3.2,seed_mean[2,2]-seed_SEM[2], 3.2,seed_mean[2,2]+ seed_SEM[2],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(4.2,seed_mean[3,2]-seed_SEM[3], 4.2,seed_mean[3,2]+ seed_SEM[3],angle=90, code=3, length=0.125, lwd=1, col="black")

arrows(2.2,seed_mean[1,2],3.2,seed_mean[2,2],angle=90, lty=2, code=3, length=0, lwd=1, col="black")
arrows(3.2,seed_mean[2,2],4.2,seed_mean[3,2],angle=90, lty=2, code=3, length=0, lwd=1, col="black")

dev.off()


###Statistic
#change in community weighted means of native community across invasion gradient between savannas
df=read_excel("all_data.xlsx","Community_weighted_trait")
df=data.frame(df)
df=subset(df,df$invader.included %in% "no")
colnames(df)=c("Site","invasion","savanna","Place","seed","ldmc","lma","la","ht","stem","lnc","invader")

df=subset(df,!df$invasion %in% "Remnant")
options(contrasts = c("contr.sum", "contr.poly"))
m <- lmer(seed~savanna*invasion +      ### seed can be replaced by other traits in this model
            (1|Place), 
          REML=TRUE,
          data = df)
Anova(m,"III")


##change in community weighted means of native community and entire community (N+I) of community across invasion gradient in FLS

df=read_excel("all_data.xlsx","Community_weighted_trait")
df=data.frame(df)
df=subset(df,df$Savannahtype %in% "FLS")
colnames(df)=c("Site","invasion","savanna","Place","seed","ldmc","lma","la","ht","stem","lnc","invader")

df=subset(df,!df$invasion %in% c("Remnant","Uninvaded"))
options(contrasts = c("contr.sum", "contr.poly"))
m <- lmer(seed~invasion*invader +      ### seed can be replaced by other traits in this model
            (1|Place), 
          REML=TRUE,
          data = df)
Anova(m,"III")


#####FIGURE 3####

##BLS Native


dat=read_excel("all_data.xlsx","Community_weighted_trait")
dat=data.frame(dat)
dat=subset(dat,dat$Savannahtype %in% "BLS")
dat$newcol=dat$Plot
dat=dat[,c(1:11,13)]

dat$ldmc_mean.cwm=(dat$ldmc_mean.cwm)^2
dat$la_meancwm=log(dat$la_meancwm)
dat$Stem_diameter_cwm=log(dat$Stem_diameter_cwm)
dat$lma_meancwm=log(dat$lma_meancwm)
colnames(dat)=c("Site","Plot","Savannahtype","Place","Seed.Vol","LDMC","LMA","Leaf.Size","Height","Stem.Dia","Nitrogen","newcol")
pca=prcomp(dat[,c(5:11)],scale=T)
scores=pca$x
mydata=data.frame(scores[,c(1,2)])
ggdata <- data.frame(mydata, Cluster=dat$newcol)
ggdata$Cluster <- factor(ggdata$Cluster, levels = c("Remnant", "Uninvaded", "low", "Medium", "High"))

habillage <- as.character(ggdata$Cluster)
BN= fviz_pca_biplot(pca,
                    geom.ind = c("point","text"), pointsize=4, pointshape=21,
                    col.ind = "black", alpha.ind=0.8, fill.ind=habillage,
                    palette=c("blue","turquoise","purple","chocolate","orange"),
                    geom.var=c("arrow", "text"), col.var="black", alpha.var = 1,
                    label="var", labelsize=5,
                    mean.point=F, repel=T)+
  ggtitle(NULL)+
  scale_y_continuous(limits=c(-7,3.75))+
  # scale_x_continuous(limits=c(-4.5,3.75))+
  stat_ellipse(data=ggdata,aes(x=PC1, y=PC2, fill=factor(Cluster)),lwd=0.3,
               color="black", geom="polygon", level=0.68, alpha=0.2, lty = 1)+
  annotate("text", x=1.9, y=-7, label= "BLS Native", size=7.5) +
  annotate("text", x=3.25, y=3.75, label= "(a)", size = 7.5) + 
  annotate("segment", x=0.351436133, y=0.800773691, xend=0.953483109466667, yend=0.180009402,
           colour = "black", size=1, alpha=1)+
  annotate("segment", x=0.953483109466667, y=0.180009402, xend=0.493999117133333, yend=0.0573812305333333,
           colour = "black", size=1, alpha=1)+
  annotate("segment", x=0.493999117133333, y=0.0573812305333333, xend=-0.3056129044, yend=-0.0585557984666667,
           colour = "black", size=1, alpha=1)+  
  annotate("segment", x=-0.3056129044, y=-0.0585557984666667, xend=-1.49330545493333, yend=-0.979608525133333,
           colour = "black", size=1, alpha=1, arrow=arrow())+ 
  theme(axis.text=element_text(size=12, color="black"),
        axis.title=element_text(size=14,color="black"),
        panel.border=element_rect(color = "gray10", fill=NA),
        plot.margin=unit(c(2,1.75,0.5,1), "cm"), #top, right, bottom, left
        legend.position = c(1.5,0.5),  # Position legend outside the plot area
        legend.text = element_text(size = 18), 
        legend.key.size = unit(2, "lines"),
        legend.box.spacing = unit(2, "cm"),  # Removes space around the legend box
        legend.margin = margin(0,0,0,0),
        legend.title = element_text(size = 14, face = "plain", color = "black")
  )+
  guides(color="none", fill=guide_legend(title="", override.aes=list(color=NA)))

##Statistics- PERMANOVA
group=dat$Plot
dissimilarity_matrix <- vegdist(scores, method = "euclidean")
permanova_result <- adonis2(dissimilarity_matrix ~ group, data = data.frame(group), permutations = 999)
permanova_result


##FLS Native

dat=read_excel("all_data.xlsx","Community_weighted_trait")
dat=data.frame(dat)
dat=subset(dat,dat$Savannahtype %in% "FLS")
dat$newcol=dat$Plot
dat=subset(dat,dat$invader.included %in% "no")
dat=dat[,c(1:11,13)]

dat$seed.volume.cwm=log(dat$seed.volume.cwm)
dat$ldmc_mean.cwm=(dat$ldmc_mean.cwm)^2
dat$la_meancwm=log(dat$la_meancwm)
dat$Stem_diameter_cwm=log(dat$Stem_diameter_cwm)
dat$lma_meancwm=log(dat$lma_meancwm)
colnames(dat)=c("Site","Plot","Savannahtype","Place","Seed.Vol","LDMC","LMA","Leaf.Size","Height","Stem.Dia","Nitrogen","newcol")
pca=prcomp(dat[,c(5:11)],scale=T)
scores=pca$x
mydata=data.frame(scores[,c(1,2)])
ggdata <- data.frame(mydata, Cluster=dat$newcol)
ggdata$Cluster <- factor(ggdata$Cluster, levels = c("Remnant", "Uninvaded", "low", "Medium", "High"))

habillage <- as.character(ggdata$Cluster)
FN= fviz_pca_biplot(pca,
                    geom.ind = c("point","text"), pointsize=4, pointshape=21,
                    col.ind = "black", alpha.ind=0.8, fill.ind=habillage,
                    palette=c("blue","turquoise","purple","chocolate","orange"),
                    geom.var=c("arrow", "text"), col.var="black", alpha.var = 1,
                    label="var", labelsize=5,
                    mean.point=F, repel=T)+
  ggtitle(NULL)+
  scale_y_continuous(limits=c(-7,3.75))+
  stat_ellipse(data=ggdata,aes(x=PC1, y=PC2, fill=factor(Cluster)),lwd=0.3,
               color="black", geom="polygon", level=0.68, alpha=0.2, lty = 1)+
  annotate("text", x=1.9, y=-7, label= "FLS Native", size=7.5) +
  annotate("text", x=3.25, y=3.75, label= "(b)", size = 7.5) + 
  annotate("segment", x=0.351436133, y=0.800773691, xend=0.953483109466667, yend=0.180009402,
           colour = "black", size=1, alpha=1)+
  annotate("segment", x=0.953483109466667, y=0.180009402, xend=0.493999117133333, yend=0.0573812305333333,
           colour = "black", size=1, alpha=1)+
  annotate("segment", x=0.493999117133333, y=0.0573812305333333, xend=-0.3056129044, yend=-0.0585557984666667,
           colour = "black", size=1, alpha=1)+  
  annotate("segment", x=-0.3056129044, y=-0.0585557984666667, xend=-1.49330545493333, yend=-0.979608525133333,
           colour = "black", size=1, alpha=1, arrow=arrow())+ 
  theme(axis.text=element_text(size=12, color="black"),
        axis.title=element_text(size=14,color="black"),
        panel.border=element_rect(color = "gray10", fill=NA),
        plot.margin=unit(c(2,1.75,0.5,1), "cm"), #top, right, bottom, left
        legend.position = c(1.5,0.5),  # Position legend outside the plot area
        legend.text = element_text(size = 18), 
        legend.key.size = unit(2, "lines"),
        legend.box.spacing = unit(2, "cm"),  # Removes space around the legend box
        legend.margin = margin(0,0,0,0),
        legend.title = element_text(size = 14, face = "plain", color = "black")
  )+
  #  coord_fixed(ratio = 1)+
  guides(color="none", fill=guide_legend(title="", override.aes=list(color=NA)))

##Statistics- PERMANOVA
group=dat$Plot
dissimilarity_matrix <- vegdist(scores, method = "euclidean")
permanova_result <- adonis2(dissimilarity_matrix ~ group, data = data.frame(group), permutations = 999)
permanova_result



##FLS All
dat=read_excel("all_data.xlsx","Community_weighted_trait")
dat=data.frame(dat)
dat=subset(dat,dat$Savannahtype %in% "FLS")
dat$newcol=dat$Plot
dat=subset(dat,dat$invader.included %in% "yes")
dat=dat[,c(1:11,13)]
dat$seed.volume.cwm=log(dat$seed.volume.cwm)
dat$ldmc_mean.cwm=(dat$ldmc_mean.cwm)^2
dat$la_meancwm=log(dat$la_meancwm)
dat$Stem_diameter_cwm=log(dat$Stem_diameter_cwm)
dat$lma_meancwm=log(dat$lma_meancwm)
colnames(dat)=c("Site","Plot","Savannahtype","Place","Seed.Vol","LDMC","LMA","Leaf.Size","Height","Stem.Dia","Nitrogen","newcol")
pca=prcomp(dat[,c(5:11)],scale=T)

scores=pca$x
mydata=data.frame(scores[,c(1,2)])
ggdata <- data.frame(mydata, Cluster=dat$newcol)
ggdata$Cluster <- factor(ggdata$Cluster, levels = c("Remnant", "Uninvaded", "low", "Medium", "High"))

habillage <- as.character(ggdata$Cluster)
FA= fviz_pca_biplot(pca,
                    geom.ind = c("point","text"), pointsize=4, pointshape=21,
                    col.ind = "black", alpha.ind=0.8, fill.ind=habillage,
                    palette=c("blue","turquoise","purple","chocolate","orange"),
                    geom.var=c("arrow", "text"), col.var="black", alpha.var = 1,
                    label="var", labelsize=5,
                    mean.point=F, repel=T)+
  ggtitle(NULL)+
  scale_y_continuous(limits=c(-7,3.75))+
  stat_ellipse(data=ggdata,aes(x=PC1, y=PC2, fill=factor(Cluster)),lwd=0.3,
               color="black", geom="polygon", level=0.68, alpha=0.2, lty = 1)+
  annotate("text", x=1.9, y=-7, label= "FLS (N+I)", size=7.5) +
  annotate("text", x=3.25, y=3.75, label= "(c)", size = 7.5) + 
  annotate("segment", x=0.351436133, y=0.800773691, xend=0.953483109466667, yend=0.180009402,
           colour = "black", size=1, alpha=1)+
  annotate("segment", x=0.953483109466667, y=0.180009402, xend=0.493999117133333, yend=0.0573812305333333,
           colour = "black", size=1, alpha=1)+
  annotate("segment", x=0.493999117133333, y=0.0573812305333333, xend=-0.3056129044, yend=-0.0585557984666667,
           colour = "black", size=1, alpha=1)+  
  annotate("segment", x=-0.3056129044, y=-0.0585557984666667, xend=-1.49330545493333, yend=-0.979608525133333,
           colour = "black", size=1, alpha=1, arrow=arrow())+ 
  theme(axis.text=element_text(size=12, color="black"),
        axis.title=element_text(size=14,color="black"),
        panel.border=element_rect(color = "gray10", fill=NA),
        plot.margin=unit(c(2,1.75,0.5,1), "cm"), #top, right, bottom, left
        legend.position = c(1.5,0.5),  # Position legend outside the plot area
        legend.text = element_text(size = 18), 
        legend.key.size = unit(2, "lines"),
        legend.box.spacing = unit(2, "cm"),  # Removes space around the legend box
        legend.margin = margin(0,0,0,0),
        legend.title = element_text(size = 14, face = "plain", color = "black")
  )+
  guides(color="none", fill=guide_legend(title="", override.aes=list(color=NA)))


##Statistics- PERMANOVA
group=dat$Plot
dissimilarity_matrix <- vegdist(scores, method = "euclidean")
permanova_result <- adonis2(dissimilarity_matrix ~ group, data = data.frame(group), permutations = 999)
permanova_result




###Figure 4

pdf("PD-NRI2.pdf",width=8, height=11)

par(mfrow=c(1,4)) #rows,columns
par(oma=c(55,5,5,5)) #outer margin (bottom, left top, right)
layout(matrix(1:4, nrow=1, ncol=4, byrow=TRUE), widths=c(0.75,1.4,0.95,1.4))

#FOR FPD
#FOR REMANANT BLS___________________________________________________________________
df<-read_excel("all_data.xlsx","Phylogenetic_diversity")


remBLS<-subset(df, df$savanna=="BLS" & df$invasion=="Remnant")
remFLS<-subset(df, df$savanna=="FLS" & df$invasion=="Remnant" & df$invader=="no")

FLSinv<-subset(df, df$savanna=="FLS" & df$invasion!="Remnant" & df$invader=="yes")
FLSnat<-subset(df, df$savanna=="FLS" & df$invasion!="Remnant" & df$invader=="no")
BLSnat<-subset(df, df$savanna=="BLS" & df$invasion!="Remnant")

dat<-remBLS
par(mar=c(1.5, 4, 0, 0)) #margin around panel (bottom, left top, right)
plot(NULL, ylim=c(200,1600), xlim=c(0.5,1.5), ann=F, xaxt="n", yaxt="n")

fpd_mean<-mean(dat$fpd); fpd_SD<-sd(dat$fpd); fpd_n<-length(dat$fpd)
fpd_SEM<-fpd_SD/sqrt(fpd_n)

par(new=TRUE)
points(0.80,fpd_mean, pch=21, cex=2.5, col="black", bg=adjustcolor("white", alpha=1), lwd=1)
arrows(0.80,fpd_mean-fpd_SEM, 0.80,fpd_mean+ fpd_SEM,angle=90, code=3, length=0.125, lwd=1, col="black")

#FOR REMANANT FLS___________________________________________________________________
par(new=TRUE)
dat<-remFLS

axis(1, at=c(1), labels=F, cex.axis=1)
axis(2, cex.axis=1, las=0, at=c(250,500,750,1000,1250,1500,1750))

mtext(expression(paste("Faith's PD")), side=2, outer=F, cex=1.2, line=2.5, col="black")
axis(1, at=c(1), labels=c("Rem"), cex.axis=1.45, las=0)

fpd_mean<-mean(dat$fpd); fpd_SD<-sd(dat$fpd); fpd_n<-length(dat$fpd)
fpd_SEM<-fpd_SD/sqrt(fpd_n)

par(new=TRUE)
points(1.2,fpd_mean, pch=21, cex=2.5, col="black", bg=adjustcolor("black", alpha=.5), lwd=1)
arrows(1.2,fpd_mean-fpd_SEM, 1.2,fpd_mean+ fpd_SEM,angle=90, code=3, length=0.125, lwd=1, col="black")

#FOR MAIN BLS Native only___________________________________________________________________
dat<-BLSnat
dat$invasion<-factor(dat$invasion, levels=c("Uninvaded", "Low", "Medium", "High"))
par(mar=c(1.5, 0.5, 0, 0)) #margin around panel (bottom, left top, right)
plot(NULL, ylim=c(200,1600), xlim=c(0.5,4.5), ann=F, xaxt="n", yaxt="n")
mtext("(a)", side=3, line=-2, adj=0.95, cex=1.5)
axis(1, at=c(1,2,3,4), labels=F, cex.axis=1, las=0)
mtext("Invasion Gradient", side=1, line=3.25, adj=-0.25, cex=1.2)
axis(1, at=c(1,2,3,4), labels=c("Un-I","Low","Med","Hi"), cex.axis=1.45, las=0)

fpd_mean<-aggregate(dat$fpd~ dat$invasion, dat, FUN = "mean")
fpd_SD<-aggregate(dat$fpd~ dat$invasion, FUN = "sd")
fpd_n<-aggregate(dat$fpd~ dat$invasion, FUN = "length")
fpd_SEM<-fpd_SD[,2]/sqrt(fpd_n[,2])

par(new=TRUE)
x=c(0.8,1.8,2.8,3.8); y=fpd_mean[,2]
points(x,y, pch=21, cex=2.5, col=c("black"), bg=adjustcolor("white", alpha=.5), lwd=1)
arrows(0.8,fpd_mean[1,2]-fpd_SEM[1], 0.8,fpd_mean[1,2]+ fpd_SEM[1],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(1.8,fpd_mean[2,2]-fpd_SEM[2], 1.8,fpd_mean[2,2]+ fpd_SEM[2],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(2.8,fpd_mean[3,2]-fpd_SEM[3], 2.8,fpd_mean[3,2]+ fpd_SEM[3],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(3.8,fpd_mean[4,2]-fpd_SEM[4], 3.8,fpd_mean[4,2]+ fpd_SEM[4],angle=90, code=3, length=0.125, lwd=1, col="black")

arrows(0.8,fpd_mean[1,2],1.8,fpd_mean[2,2],angle=0, code=3, length=0, lwd=1, col="black")
arrows(1.8,fpd_mean[2,2],2.8,fpd_mean[3,2],angle=0, code=3, length=0, lwd=1, col="black")
arrows(2.8,fpd_mean[3,2],3.8,fpd_mean[4,2],angle=90, code=3, length=0, lwd=1, col="black")

#FOR MAIN FLS Native only___________________________________________________________________
dat<-FLSnat
dat$invasion<-factor(dat$invasion, levels=c("Uninvaded", "Low", "Medium", "High"))
par(new=TRUE)

fpd_mean<-aggregate(dat$fpd~ dat$invasion, dat, FUN = "mean")
fpd_SD<-aggregate(dat$fpd~ dat$invasion, FUN = "sd")
fpd_n<-aggregate(dat$fpd~ dat$invasion, FUN = "length")
fpd_SEM<-fpd_SD[,2]/sqrt(fpd_n[,2])

par(new=TRUE)
points(fpd_mean , pch=21, cex=2.5, col=c("black"), bg=adjustcolor("black", alpha=.5), lwd=1)
arrows(1,fpd_mean[1,2]-fpd_SEM[1], 1,fpd_mean[1,2]+ fpd_SEM[1],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(2,fpd_mean[2,2]-fpd_SEM[2], 2,fpd_mean[2,2]+ fpd_SEM[2],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(3,fpd_mean[3,2]-fpd_SEM[3], 3,fpd_mean[3,2]+ fpd_SEM[3],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(4,fpd_mean[4,2]-fpd_SEM[4], 4,fpd_mean[4,2]+ fpd_SEM[4],angle=90, code=3, length=0.125, lwd=1, col="black")

arrows(1,fpd_mean[1,2],2,fpd_mean[2,2],angle=90, code=3, length=0, lwd=1, col="black")
arrows(2,fpd_mean[2,2],3,fpd_mean[3,2],angle=90, code=3, length=0, lwd=1, col="black")
arrows(3,fpd_mean[3,2],4,fpd_mean[4,2],angle=90, code=3, length=0, lwd=1, col="black")

#FOR MAIN FLS with invasive___________________________________________________________________
dat<-FLSinv
dat$invasion<-factor(dat$invasion, levels=c("Uninvaded", "Low", "Medium", "High"))
par(new=TRUE)

fpd_mean<-aggregate(dat$fpd~ dat$invasion, dat, FUN = "mean")
fpd_SD<-aggregate(dat$fpd~ dat$invasion, FUN = "sd")
fpd_n<-aggregate(dat$fpd~ dat$invasion, FUN = "length")
fpd_SEM<-fpd_SD[,2]/sqrt(fpd_n[,2])

par(new=TRUE)
x=c(1.2,2.2,3.2,4.2); y=fpd_mean[,2]
points(x,y, pch=22, cex=2.5, col=c("black"), bg=adjustcolor("black", alpha=.5), lwd=1)
arrows(1.2,fpd_mean[1,2]-fpd_SEM[1], 1.2,fpd_mean[1,2]+ fpd_SEM[1],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(2.2,fpd_mean[2,2]-fpd_SEM[2], 2.2,fpd_mean[2,2]+ fpd_SEM[2],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(3.2,fpd_mean[3,2]-fpd_SEM[3], 3.2,fpd_mean[3,2]+ fpd_SEM[3],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(4.2,fpd_mean[4,2]-fpd_SEM[4], 4.2,fpd_mean[4,2]+ fpd_SEM[4],angle=90, code=3, length=0.125, lwd=1, col="black")

arrows(1.2,fpd_mean[1,2],2.2,fpd_mean[2,2],angle=90, code=3, length=0, lwd=1, col="black", lty=2)
arrows(2.2,fpd_mean[2,2],3.2,fpd_mean[3,2],angle=90, code=3, length=0, lwd=1, col="black", lty=2)
arrows(3.2,fpd_mean[3,2],4.2,fpd_mean[4,2],angle=90, code=3, length=0, lwd=1, col="black", lty=2)

#plot NRI
#FOR REMANANT BLS___________________________________________________________________
df<-read_excel("all_data.xlsx","Phylogenetic_diversity")

remBLS<-subset(df, df$savanna=="BLS" & df$invasion=="Remnant" & df$invader=="no")
remFLS<-subset(df, df$savanna=="FLS" & df$invasion=="Remnant" & df$invader=="no")

FLSinv<-subset(df, df$savanna=="FLS" & df$invasion!="Remnant" & df$invader=="yes")
FLSnat<-subset(df, df$savanna=="FLS" & df$invasion!="Remnant" & df$invader=="no")
BLSnat<-subset(df, df$savanna=="BLS" & df$invasion!="Remnant" & df$invader=="no")

dat<-remBLS
par(mar=c(1.5, 6, 0, 0)) #margin around panel (bottom, left top, right)
plot(NULL, ylim=c(-0.5, 6), xlim=c(0.5,1.5), ann=F, xaxt="n", yaxt="n")

NRI_mean<-mean(dat$NRI); NRI_SD<-sd(dat$NRI); NRI_n<-length(dat$NRI)
NRI_SEM<-NRI_SD/sqrt(NRI_n)

par(new=TRUE)
points(0.80,NRI_mean, pch=21, cex=2.5, col="black", bg=adjustcolor("white", alpha=1), lwd=1)
arrows(0.80,NRI_mean-NRI_SEM, 0.80,NRI_mean+ NRI_SEM,angle=90, code=3, length=0.125, lwd=1, col="black")

#FOR REMANANT FLS___________________________________________________________________
par(new=TRUE)
dat<-remFLS

axis(1, at=c(1), labels=F, cex.axis=1)
axis(2, cex.axis=1, las=1)
mtext(expression(paste("Net Relatedness Index (NRI)-PD")), side=2, outer=F, cex=1.2, line=2.5, col="black")
axis(1, at=c(1), labels=c("Rem"), cex.axis=1.45, las=0)

NRI_mean<-mean(dat$NRI); NRI_SD<-sd(dat$NRI); NRI_n<-length(dat$NRI)
NRI_SEM<-NRI_SD/sqrt(NRI_n)

par(new=TRUE)
points(1.2,NRI_mean, pch=21, cex=2.5, col="black", bg=adjustcolor("black", alpha=.5), lwd=1)
arrows(1.2,NRI_mean-NRI_SEM, 1.2,NRI_mean+ NRI_SEM,angle=90, code=3, length=0.125, lwd=1, col="black")

#FOR MAIN BLS Native only___________________________________________________________________
dat<-BLSnat
dat$invasion<-factor(dat$invasion, levels=c("Uninvaded", "Low", "Medium", "High"))
par(mar=c(1.5, 0.5, 0, 0)) #margin around panel (bottom, left top, right)
plot(NULL, ylim=c(-0.5, 6), xlim=c(0.5,4.5), ann=F, xaxt="n", yaxt="n")
mtext("(b)", side=3, line=-2, adj=0.95, cex=1.5)
axis(1, at=c(1,2,3,4), labels=F, cex.axis=1, las=0)
mtext("Invasion Gradient", side=1, line=3, adj=-0.25, cex=1.2)
axis(1, at=c(1,2,3,4), labels=c("Un-I","Low","Med","Hi"), cex.axis=1.45, las=0)

NRI_mean<-aggregate(dat$NRI~ dat$invasion, dat, FUN = "mean")
NRI_SD<-aggregate(dat$NRI~ dat$invasion, FUN = "sd")
NRI_n<-aggregate(dat$NRI~ dat$invasion, FUN = "length")
NRI_SEM<-NRI_SD[,2]/sqrt(NRI_n[,2])

par(new=TRUE)
x=c(0.8,1.8,2.8,3.8); y=NRI_mean[,2]
points(x,y, pch=21, cex=2.5, col=c("black"), bg=adjustcolor("white", alpha=.5), lwd=1)
arrows(0.8,NRI_mean[1,2]-NRI_SEM[1], 0.8,NRI_mean[1,2]+ NRI_SEM[1],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(1.8,NRI_mean[2,2]-NRI_SEM[2], 1.8,NRI_mean[2,2]+ NRI_SEM[2],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(2.8,NRI_mean[3,2]-NRI_SEM[3], 2.8,NRI_mean[3,2]+ NRI_SEM[3],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(3.8,NRI_mean[4,2]-NRI_SEM[4], 3.8,NRI_mean[4,2]+ NRI_SEM[4],angle=90, code=3, length=0.125, lwd=1, col="black")

arrows(0.8,NRI_mean[1,2],1.8,NRI_mean[2,2],angle=0, code=3, length=0, lwd=1, col="black")
arrows(1.8,NRI_mean[2,2],2.8,NRI_mean[3,2],angle=0, code=3, length=0, lwd=1, col="black")
arrows(2.8,NRI_mean[3,2],3.8,NRI_mean[4,2],angle=90, code=3, length=0, lwd=1, col="black")

#FOR MAIN FLS Native only___________________________________________________________________
dat<-FLSnat
dat$invasion<-factor(dat$invasion, levels=c("Uninvaded", "Low", "Medium", "High"))
par(new=TRUE)

NRI_mean<-aggregate(dat$NRI~ dat$invasion, dat, FUN = "mean")
NRI_SD<-aggregate(dat$NRI~ dat$invasion, FUN = "sd")
NRI_n<-aggregate(dat$NRI~ dat$invasion, FUN = "length")
NRI_SEM<-NRI_SD[,2]/sqrt(NRI_n[,2])

par(new=TRUE)
points(NRI_mean , pch=21, cex=2.5, col=c("black"), bg=adjustcolor("black", alpha=.5), lwd=1)
arrows(1,NRI_mean[1,2]-NRI_SEM[1], 1,NRI_mean[1,2]+ NRI_SEM[1],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(2,NRI_mean[2,2]-NRI_SEM[2], 2,NRI_mean[2,2]+ NRI_SEM[2],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(3,NRI_mean[3,2]-NRI_SEM[3], 3,NRI_mean[3,2]+ NRI_SEM[3],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(4,NRI_mean[4,2]-NRI_SEM[4], 4,NRI_mean[4,2]+ NRI_SEM[4],angle=90, code=3, length=0.125, lwd=1, col="black")

arrows(1,NRI_mean[1,2],2,NRI_mean[2,2],angle=90, code=3, length=0, lwd=1, col="black")
arrows(2,NRI_mean[2,2],3,NRI_mean[3,2],angle=90, code=3, length=0, lwd=1, col="black")
arrows(3,NRI_mean[3,2],4,NRI_mean[4,2],angle=90, code=3, length=0, lwd=1, col="black")

#FOR MAIN FLS with invasive___________________________________________________________________
dat<-FLSinv
dat$invasion<-factor(dat$invasion, levels=c("Uninvaded", "Low", "Medium", "High"))
par(new=TRUE)

NRI_mean<-aggregate(dat$NRI~ dat$invasion, dat, FUN = "mean")
NRI_SD<-aggregate(dat$NRI~ dat$invasion, FUN = "sd")
NRI_n<-aggregate(dat$NRI~ dat$invasion, FUN = "length")
NRI_SEM<-NRI_SD[,2]/sqrt(NRI_n[,2])

par(new=TRUE)
x=c(1.2,2.2,3.2,4.2); y=NRI_mean[,2]
points(x,y, pch=22, cex=2.5, col=c("black"), bg=adjustcolor("black", alpha=.5), lwd=1)
arrows(1.2,NRI_mean[1,2]-NRI_SEM[1], 1.2,NRI_mean[1,2]+ NRI_SEM[1],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(2.2,NRI_mean[2,2]-NRI_SEM[2], 2.2,NRI_mean[2,2]+ NRI_SEM[2],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(3.2,NRI_mean[3,2]-NRI_SEM[3], 3.2,NRI_mean[3,2]+ NRI_SEM[3],angle=90, code=3, length=0.125, lwd=1, col="black")
arrows(4.2,NRI_mean[4,2]-NRI_SEM[4], 4.2,NRI_mean[4,2]+ NRI_SEM[4],angle=90, code=3, length=0.125, lwd=1, col="black")

arrows(1.2,NRI_mean[1,2],2.2,NRI_mean[2,2],angle=90, code=3, length=0, lwd=1, col="black", lty=2)
arrows(2.2,NRI_mean[2,2],3.2,NRI_mean[3,2],angle=90, code=3, length=0, lwd=1, col="black", lty=2)
arrows(3.2,NRI_mean[3,2],4.2,NRI_mean[4,2],angle=90, code=3, length=0, lwd=1, col="black", lty=2)

#Legend
points(0.75,6, pch=21, cex=2.5, col=c("black"), bg=adjustcolor("white", alpha=.5), lwd=1)
points(0.75,5.5, pch=21, cex=2.5, col=c("black"), bg=adjustcolor("black", alpha=.5), lwd=1)
points(0.75,5, pch=22, cex=2.5, col=c("black"), bg=adjustcolor("black", alpha=.5), lwd=1)
segments(0.45, 6, 1.05, 6); mtext("BLS (Native)", side=3, line=-1.5, adj=0.35)
segments(0.45, 5.5, 1.05, 5.5); mtext("FLS (Native)", side=3, line=-3, adj=0.35)
segments(0.45, 5, 1.05, 5, lty=2)
mtext("FLS (N + I)", side=3, line=-4.5, adj=0.32)


dev.off()

#Statistics
## Phylogenetic diversity (Faith's/NRI PD) change with invasion gradient across savannas
df<-read_excel("all_data.xlsx","Phylogenetic_diversity")
df=data.frame(df)
df=subset(df,df$invader %in% "no")
df=subset(df,!df$invasion %in% "Remnant")

options(contrasts = c("contr.sum", "contr.poly"))
m <- lmer(NRI~savanna*invasion +      ### fpd can be replaced by NRI
            (1|place), 
          REML=TRUE,
          data = df)
Anova(m,"III")


#### Phylogenetic diversity (Faith's/NRI PD) of native only and entire community change for invaded communities with invasion
library(readxl)
df<-read_excel("all_data.xlsx","Phylogenetic_diversity")
df=data.frame(df)
df=subset(df,df$savanna %in% "FLS")

options(contrasts = c("contr.sum", "contr.poly"))
df=subset(df,!df$invasion %in% c("Remnant"))
m <- lmer(NRI~invader*invasion +      ### fpd can be replaced by NRI
            (1|place), 
          REML=TRUE,
          data = df)
Anova(m,"III")

