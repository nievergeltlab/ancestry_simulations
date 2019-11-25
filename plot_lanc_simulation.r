#Make a header for hte output file
echo ""afeffectsize" "eureffectsize" "afmaf" "eurmaf" "admixturegroup" "N" "prevalence" "power_newanc" "power_oldanc"" > header.txt 
#concatenate all simulation files from the output directory and add the header
cat sims_null/power500sims*.txt | cat header.txt  - > null_powersims_100k_highprev.txt

#Now plot the stuff using R
R

library(plyr)
library(plotrix)

#Aggregate simulations
powergivenn <-ddply(powergivenn1, ~ afeffectsize + eureffectsize +afmaf + eurmaf+ admixturegroup + N +prevalence  ,colwise(mean,c("power_oldanc","power_newanc"),na.rm=T ))

##Matching effect size between AAM and EUR, over different MAFs
efparm='matching'
afmafparm='varying'
eurmafparm='same'
admixtureparm=080
prevalenceparm=010

pdf(paste(efparm,'_',afmafparm,'_',eurmafparm,'_',admixtureparm,'_',prevalenceparm, '.pdf',sep=''),8,4)
for (maf in c(0.1,0.2,0.3,0.4))
{
 d1 <- subset(powergivenn,eureffectsize == "matching"&afmaf==maf&eurmaf=="same"&admixturegroup==0.8&prevalence==0.1)
 d1 <- d1[order(d1$afeffectsize),]

 plot(d1$afeffectsize,100*d1$power_newanc/100,type='l',lwd=2,cex.axis=1.25,cex.lab=1.45,xlab="Odds Ratio", ylab="Power",lty=2,ylim=c(0,100),main=paste("Overall MAF set to", maf))
 lines(d1$afeffectsize,100*d1$power_old/100,type='l',lwd=2,col='darkgrey',lty=1)
 legend("topleft",legend=c("12000 Cases, LANC","12000 Cases, GLOB" ), col=c('black','darkgrey'),lty=c(2,1),bty='n',cex=.5, pt.cex = .4)

}
dev.off()

#Null European effect, varying MAF
efparm='null'
afmafparm='varying'
eurmafparm='same'
admixtureparm=080
prevalenceparm=010

pdf(paste(efparm,'_',afmafparm,'_',eurmafparm,'_',admixtureparm,'_',prevalenceparm, '.pdf',sep=''),8,4)
for (maf in c(0.1,0.2,0.3,0.4))
{
 d1 <- subset(powergivenn,eureffectsize == "null"&afmaf==maf&eurmaf=="same"&admixturegroup==0.8&prevalence==0.1)
 d1 <- d1[order(d1$afeffectsize),]

 plot(d1$afeffectsize,100*d1$power_newanc/100,type='l',lwd=2,cex.axis=1.25,cex.lab=1.45,xlab="Odds Ratio", ylab="Power",lty=2,ylim=c(0,100),main=paste("Overall MAF set to", maf))
 lines(d1$afeffectsize,100*d1$power_old/100,type='l',lwd=2,col='darkgrey',lty=1)
 legend("topleft",legend=c("12000 Cases, LANC","12000 Cases, GLOB" ), col=c('black','darkgrey'),lty=c(2,1),bty='n',cex=.5, pt.cex = .4)

}
dev.off()

##Vary relative european effect strength
afmafparm='020'
eurmafparm='same'
admixtureparm=080
prevalenceparm=010

pdf(paste('varying','_',afmafparm,'_',eurmafparm,'_',admixtureparm,'_',prevalenceparm, '.pdf',sep=''),8,4)
for (efparm in c("euronly", "matching"  ,  "null", "stronger" ,  "weaker"))
{
 d1 <- subset(powergivenn,eureffectsize == efparm &afmaf==0.2&eurmaf=="same"&admixturegroup==0.8&prevalence==0.1)
 d1 <- d1[order(d1$afeffectsize),]

 plot(d1$afeffectsize,100*d1$power_newanc/100,type='l',lwd=2,cex.axis=1.25,cex.lab=1.45,xlab="Odds Ratio", ylab="Power",lty=2,ylim=c(0,100),main=paste("Effect size in europeans:", efparm))
 lines(d1$afeffectsize,100*d1$power_old/100,type='l',lwd=2,col='darkgrey',lty=1)
 legend("topleft",legend=c("12000 Cases, LANC","12000 Cases, GLOB" ), col=c('black','darkgrey'),lty=c(2,1),bty='n',cex=.5, pt.cex = .4)

}
dev.off()

##vary  admixture and effect
afmafparm='020'
eurmafparm='same'
prevalenceparm=010

pdf(paste('varyingef','_',afmafparm,'_',eurmafparm,'_',"varyingadmix",'_',prevalenceparm, '.pdf',sep=''),8,4)
for (efparm in c("euronly", "matching"  ,  "null", "stronger" ,  "weaker"))
{
 for (admixparm in c(0.8, 0.5))
 {
 d1 <- subset(powergivenn,eureffectsize == efparm &afmaf==0.2&eurmaf=="same"&admixturegroup==admixparm&prevalence==0.1)
 d1 <- d1[order(d1$afeffectsize),]

 plot(d1$afeffectsize,100*d1$power_newanc/100,type='l',lwd=2,cex.axis=1.25,cex.lab=1.45,xlab="Odds Ratio", ylab="Power",lty=2,ylim=c(0,100),main=paste("Effect size in europeans:", efparm, ".Admixture rate:", admixparm))
 lines(d1$afeffectsize,100*d1$power_old/100,type='l',lwd=2,col='darkgrey',lty=1)
 legend("topleft",legend=c("12000 Cases, LANC","12000 Cases, GLOB" ), col=c('black','darkgrey'),lty=c(2,1),bty='n',cex=.5, pt.cex = .4)
 } 
}
dev.off()



##vary  prevalence and effect
efparm='varyingef'
afmafparm=0.2
eurmafparm='same'
admixparm=0.8
prevalenceparm=varying

pdf(paste(efparm,'_',afmafparm,'_',eurmafparm,'_',admixtureparm,'_',prevalenceparm, '.pdf',sep=''),8,4)
for (efparm in c("euronly", "matching"  ,  "null", "stronger" ,  "weaker"))
{
 for (prevalenceparm in c(0.1,0.2))
 {
 d1 <- subset(powergivenn,eureffectsize == efparm &afmaf==afmafparm&eurmaf==eurmafparm&admixturegroup==admixparm&prevalence==prevalenceparm)
 d1 <- d1[order(d1$afeffectsize),]

 plot(d1$afeffectsize,100*d1$power_newanc/100,type='l',lwd=2,cex.axis=1.25,cex.lab=1.45,xlab="Odds Ratio", ylab="Power",lty=2,ylim=c(0,100),main=paste("Effect size in europeans:", efparm, ". Prevalence:", prevalenceparm))
 lines(d1$afeffectsize,100*d1$power_old/100,type='l',lwd=2,col='darkgrey',lty=1)
 legend("topleft",legend=c("12000 Cases, LANC","12000 Cases, GLOB" ), col=c('black','darkgrey'),lty=c(2,1),bty='n',cex=.5, pt.cex = .4)
 } 
}
dev.off()



 ##vary  maf
efparm='varyingef'
afmafparm=0.2
eurmafparm='same'
admixparm=0.8
prevalenceparm=0.2

pdf(paste(efparm,'_',afmafparm,'_',eurmafparm,'_',admixtureparm,'_',prevalenceparm, '.pdf',sep=''),8,4)
for (efparm in c("euronly", "matching"  ,  "null", "stronger" ,  "weaker"))
{
 for (eurmafparm in c("larger", "smaller","same"))
 {
 d1 <- subset(powergivenn,eureffectsize == efparm &afmaf==afmafparm&eurmaf==eurmafparm&admixturegroup==admixparm&prevalence==prevalenceparm)
 d1 <- d1[order(d1$afeffectsize),]

 plot(d1$afeffectsize,100*d1$power_newanc/100,type='l',lwd=2,cex.axis=1.25,cex.lab=1.45,xlab="Odds Ratio", ylab="Power",lty=2,ylim=c(0,100),main=paste("Effect size in europeans:", efparm, ". Maf difference:", eurmafparm))
 lines(d1$afeffectsize,100*d1$power_old/100,type='l',lwd=2,col='darkgrey',lty=1)
 legend("topleft",legend=c("12000 Cases, LANC","12000 Cases, GLOB" ), col=c('black','darkgrey'),lty=c(2,1),bty='n',cex=.5, pt.cex = .4)
 } 
}
dev.off()



 
 


#Graph w/o green lne
pdf('powersimg2_scen1_v2.pdf',5.5,4.5)

plot(efseq,powergivenn4000[,1]/10,type='l',lwd=2,cex.axis=1.15,cex.lab=1.45,xlab="Odds Ratio", ylab="Power",lty=2,ylim=c(0,110),yaxt='n',bty='l')

abline(a=80,b=0,lty=3)
axis(2,c(0,20,40,60,80,100),cex.axis=1.15) #jesus christ you fuck jkust tra
lines(efseq,powergivenn4000[,3]/10,type='l',lwd=2,col='black',lty=1)

lines(efseq,powergivenn12000[,1]/10,type='l',lwd=2,col='blue',lty=2)
lines(efseq,powergivenn12000[,3]/10,type='l',lwd=2,col='blue',lty=1)

lines(efseq,powergivenn13[,1]/10,type='l',lwd=2,col='red',lty=2)
lines(efseq,powergivenn13[,3]/10,type='l',lwd=2,col='red',lty=1)

legend("topleft",legend=c("12000 Cases, LANC, MAF20%","12000 Cases, GLOB" ,"12000 Cases, LANC, MAF10% AFR, 30% EUR" ,"12000 Cases, GLOB" ,"4000 Cases, LANC, MAF20%" ,"4000 Cases, GLOB" ), col=rep(c('blue','blue','red','red','black','black'),3),lty=rep(c(2,1),3),bty='n',cex=.5, pt.cex = .4)

dev.off()



#Graph with black and blue (grant). Put green color first in legend
pdf('powersimg2_scen2.pdf',5.5,4.5)
 par(mar=c(5, 4, 4, 2) + 0.5)
plot(efseq,powergivenn4000[,1]/10,type='l',lwd=2,cex.axis=1.4,cex.lab=1.8,xlab="", ylab="",lty=2,ylim=c(0,110),yaxt='n',bty='l')

mtext(side=1,"Odds Ratio",cex=1.8,line=2.6)
mtext(side=2,"Power",cex=1.8,line=2.6)
 
 
abline(a=80,b=0,lty=3)
axis(2,c(0,20,60,100),cex.axis=1.4) 
axis(2,c(40,80),cex.axis=1.4) 

lines(efseq,powergivenn4000[,3]/10,type='l',lwd=2,col='black',lty=1)

lines(efseq,powergivenn12000[,1]/10,type='l',lwd=2,col='blue',lty=2)
lines(efseq,powergivenn12000[,3]/10,type='l',lwd=2,col='blue',lty=1)

lines(efseq,powergivenn13[,1]/10,type='l',lwd=2,col='green',lty=2)
lines(efseq,powergivenn13[,3]/10,type='l',lwd=2,col='green',lty=1)
par(xpd=TRUE)
legend(x=1.035,y=130,legend=c("LANC, MAF10% AFR, 30% EUR" ,"GLOB, MAF10% AFR, 30% EUR"  ,"LANC, MAF20%","GLOB, MAF20%" ,"LANC, MAF20%" ,"GLOB, MAF 20%" ), col=rep(c('green','green','blue','blue','black','black'),3),lty=rep(c(2,1),3),bty='n',cex=.85, pt.cex = .85)
#legend("topleft",legend=c("12000 Cases, LANC, MAF10% AFR, 30% EUR" ,"12000 Cases, GLOB"  ,"12000 Cases, LANC, MAF20%","12000 Cases, GLOB" ,"4000 Cases, LANC, MAF20%" ,"4000 Cases, GLOB" ), col=rep(c('green','green','blue','blue','black','black'),3),lty=rep(c(2,1),3),bty='n',cex=.5, pt.cex = .4)

dev.off()

#Graph with red and green

pdf('powersimg2_scen3_v2.pdf',5.5,4.5)

plot(efseq,powergivenn4000[,1]/10,type='l',lwd=2,cex.axis=1.15,cex.lab=1.45,xlab="Odds Ratio", ylab="Power",lty=2,ylim=c(0,110),yaxt='n',bty='l',col='white')
axis(2,c(0,20,40,60,80,100),cex.axis=1.15) 

abline(a=80,b=0,lty=3)
lines(efseq,powergivenn24[,1]/10,type='l',lwd=2,col='green',lty=2)
lines(efseq,powergivenn24[,3]/10,type='l',lwd=2,col='green',lty=1)

lines(efseq,powergivenn13[,1]/10,type='l',lwd=2,col='red',lty=2)
lines(efseq,powergivenn13[,3]/10,type='l',lwd=2,col='red',lty=1)

legend("topleft",legend=c("12000 Cases, LANC, MAF20% AFR, 40% EUR","12000 Cases, GLOB" ,"12000 Cases, LANC, MAF10% AFR, 30% EUR" ,"12000 Cases, GLOB" ), col=rep(c('green','green','red','red'),3),lty=rep(c(2,1),2),bty='n',cex=.5, pt.cex = .4)

dev.off()




#graph all 
pdf('powersimg2_202020401030.pdf',5.5,4.5)

plot(efseq,powergivenn4000[,1]/10,type='l',lwd=2,cex.axis=1.15,cex.lab=1.45,xlab="Odds Ratio", ylab="Power",lty=2,ylim=c(0,110),yaxt='n',bty='l')
axis(2,c(0,20,40,60,80,100),cex.axis=1.15) #jesus christ you fuck jkust tra
lines(efseq,powergivenn4000[,3]/10,type='l',lwd=2,col='black',lty=1)
abline(h=0)

lines(efseq,powergivenn12000[,1]/10,type='l',lwd=2,col='blue',lty=2)
lines(efseq,powergivenn12000[,3]/10,type='l',lwd=2,col='blue',lty=1)

lines(efseq,powergivenn24[,1]/10,type='l',lwd=2,col='red',lty=2)
lines(efseq,powergivenn24[,3]/10,type='l',lwd=2,col='red',lty=1)

lines(efseq,powergivenn13[,1]/10,type='l',lwd=2,col='green',lty=2)
lines(efseq,powergivenn13[,3]/10,type='l',lwd=2,col='green',lty=1)
legend("topleft",legend=c("12000 Cases, asaMap","12000 Cases, standard" ), col=rep(c('red','pink'),3),lty=c(2,1),bty='n')


dev.off()


#Incomplete test code

#Read simulation results files
powergivenn1 <- read.table('null_powersims_100k_highprev.txt',header=T,stringsAsFactors=F)

#Difference in power 
powergivenn1$dif <- powergivenn1$power_newanc - powergivenn1$power_oldanc

#Function for returning mean and SD
meansd <- function(x)
{
 return(t(c(mean(x),std.error(x))))
 }
 
#Aggregate, over all simulations, average values
powergivenn <- aggregate(cbind(power_oldanc,power_newanc,dif) ~afeffectsize + eureffectsize +afmaf + eurmaf+ admixturegroup + N +prevalence ,data=powergivenn1,FUN=meansd)
#... correc this here
#names(powergivenn)[8:13] <- c("mean_power_oldmethod","sd_power_oldmethod","mean_power_newmethod","sd_power_newmethod","mean_power_difference","sd_power_difference")

#This makes the names crummy. Variables ending in .1 are the mean and the .2 is the sd

#Mean power using standard method
powergivenn$pomean <- powergivenn$power_oldanc[,1]
#SD of mean power using standard method - SDs are not quite correct, because they havent been divided by the number of reptitions in a given sim
powergivenn$posd <- powergivenn$power_oldanc[,2]
#Mean power using our method
powergivenn$pnmean <- powergivenn$power_newanc[,1]
#SD power using standard method
powergivenn$pnsd <- powergivenn$power_newanc[,2]

#Mean Power difference between methods
powergivenn$difmean <- powergivenn$dif[,1]
#SD Power difference between methods
powergivenn$difsd <- powergivenn$dif[,2]


#data needs to be split between old and new

powergivenn$name <- paste(powergivenn$afmaf,powergivenn$eurmaf,powergivenn$admixturegroup,powergivenn$prevalence)

barCenters <- barplot( height= powergivenn$difmean, names.arg= powergivenn$name, horiz=TRUE,beside=TRUE ,border=F,las=1)
segments(powergivenn$difmean - 1.96*powergivenn$difsd, y0= barCenters ,  
         powergivenn$difmean + 1.96*powergivenn$difsd ,y1= barCenters ,lwd = 2.5)

arrows(d2$CI_lower[6:1], y0= barCenters[6:1],  
       d2$CI_upper[6:1],y1= barCenters[6:1], lwd = 2.5, angle = 90,
       code = 3, length = 0.05)
       
#raw False positive rate
barCenters <- barplot( height= powergivenn$pomean, names.arg= powergivenn$name, horiz=TRUE,beside=TRUE ,border=F,las=1)
segments(powergivenn$pomean - 1.96*powergivenn$posd, y0= barCenters ,  
         powergivenn$pomean + 1.96*powergivenn$posd ,y1= barCenters ,lwd = 2.5)
#Note that every repetition contains many semi dependent sims.. so the sd bars are overstated here.
write.table(powergivenn,'null_simulation_significancerate_100k_highprev.txt')

#PLot the power difference at different scenarios

barplot(dif ~ afmaf + eurmaf , data=powergivenn_alt)

