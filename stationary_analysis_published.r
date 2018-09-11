#Script to analyze the FFT data from stationary videos
library(agricolae)

#for hand generated
#set working directory
setwd("D:/PhD/Videos/stationary_output")

#read final stationary data
finaldf <- read.table("stationaryfinal.txt", header=TRUE)
finaldf$PlantingDate <- as.factor(finaldf$PlantingDate)
finaldf$Crop <- as.factor(finaldf$Crop)

#remove from all videos in planting date as they were zeros in matlab analysis.
# StellarND_6row_1A 
# Conion_2row_2B
# Celebration_6row_3A
# ND021052_oat_4B
finaldf<-finaldf[!(finaldf$Line=="StellarND" & finaldf$PlantingDate==1 & finaldf$Rep =="A"),]
finaldf<-finaldf[!(finaldf$Line=="Conion" & finaldf$PlantingDate==2 & finaldf$Rep =="B"),]
finaldf<-finaldf[!(finaldf$Line=="Celebration" & finaldf$PlantingDate==3 & finaldf$Rep =="A"),]
finaldf<-finaldf[!(finaldf$Line=="ND021052" & finaldf$PlantingDate==4 & finaldf$Rep =="B"),]



#analysis
lmMeanFreq <- lm(MeanFreq ~ Line + VideoDate + Position + Rep + PlantingDate + Line*PlantingDate, data = finaldf)
summary(aov(lmMeanFreq)) # no significant line*position interaction
summary(lmMeanFreq)
lmMedFreq <- lm(MedianFreq ~ Line + VideoDate + Position + Rep + PlantingDate + Line*PlantingDate, data = finaldf)
summary(aov(lmMedFreq)) # no significant line*position interaction
summary(lmMedFreq)

lmHz0.9 <- lm(Hz0.9 ~ Line + VideoDate + Position + Rep + PlantingDate + Line*PlantingDate, data = finaldf)
summary(aov(lmHz0.9)) # no significant line*position interaction
summary(lmHz0.9)
lmHz1.1 <- lm(Hz1.1 ~ Line + VideoDate + Position + Rep + PlantingDate + Line*PlantingDate, data = finaldf)
summary(aov(lmHz1.1)) # no significant line*position interaction
summary(lmHz1.1)
lmHz1.3 <- lm(Hz1.3 ~ Line + VideoDate + Position + Rep + PlantingDate + Line*PlantingDate, data = finaldf)
summary(aov(lmHz1.3)) # no significant line*position interaction
summary(lmHz1.3)
lmHz1.5 <- lm(Hz1.5 ~ Line + VideoDate + Position + Rep + PlantingDate + Line*PlantingDate, data = finaldf)
summary(aov(lmHz1.5)) # no significant line*position interaction
summary(lmHz1.5)
lmHz1.7 <- lm(Hz1.7 ~ Line + VideoDate + Position + Rep + PlantingDate + Line*PlantingDate, data = finaldf)
summary(aov(lmHz1.7)) # no significant line*position interaction
summary(lmHz1.7)


#lsd tests
lsdMeanFreq <- LSD.test(aov(lmMeanFreq),"Line",p.adj="bonferroni")
lsdMeanFreqVd <- LSD.test(aov(lmMeanFreq),"VideoDate",p.adj="bonferroni")
lsdHz0.9 <- LSD.test(aov(lmHz0.9),"Line",p.adj="bonferroni")
lsdHz1.1 <- LSD.test(aov(lmHz1.1),"Line",p.adj="bonferroni")
lsdHz1.3 <- LSD.test(aov(lmHz1.3),"Line",p.adj="bonferroni")
lsdHz1.5 <- LSD.test(aov(lmHz1.5),"Line",p.adj="bonferroni")
lsdHz1.7 <- LSD.test(aov(lmHz1.7),"Line",p.adj="bonferroni")
lsdHz1.1vd <- LSD.test(aov(lmHz1.1),"VideoDate",p.adj="bonferroni")
lsdHz1.3vd <- LSD.test(aov(lmHz1.3),"VideoDate",p.adj="bonferroni")
lsdHz1.1pd <- LSD.test(aov(lmHz1.1),"PlantingDate",p.adj="bonferroni")
lsdHz1.3pd <- LSD.test(aov(lmHz1.3),"PlantingDate",p.adj="bonferroni")

#histograms of peak frequencies from plot output files
setwd("D:/PhD/Videos/stationary_p1_noFFT/FLY07_10_17_p1")
gopher <- read.table("GOPHER_oat_1A.txt",header=FALSE)
IL <- read.table("IL078721_oat_1B.txt",header=FALSE)
pinnacle <- read.table("Pinnacle_2row_1B.txt",header=FALSE)
linkert <- read.table("Linkert_wheat_1B.txt",header=FALSE)
#filter out low and high frequencies, as done for calculating the mean frequency in the stationary organization no FFT script
gopher <- subset(gopher, gopher$V1 >= 0.5 & gopher$V1 <4.9)
IL <- subset(IL, IL$V1 >= 0.5 & IL$V1 <4.9)
pinnacle <- subset(pinnacle, pinnacle$V1 >= 0.5 & pinnacle$V1 <4.9)
linkert <- subset(linkert, linkert$V1 >= 0.5 & linkert$V1 <4.9)

#create histograms, add line at the mean freq of each
hist(linkert$V1,breaks=seq(0.5,4.9,by=0.2),ylim=c(0,30),axes=FALSE,labels=FALSE,col="dodgerblue3",main="'Linkert' Wheat")
axis(1, at=seq(0.5,4.9,by=0.2))
axis(2, at=seq(0,30,by=5))
abline(v=1.39,col="red",lwd=2)

hist(pinnacle$V1,breaks=seq(0.5,4.9,by=0.2),ylim=c(0,30),axes=FALSE,labels=FALSE,col="dodgerblue3",main="'Pinnacle' 2-Row Barley")
axis(1, at=seq(0.5,4.9,by=0.2))
axis(2, at=seq(0,30,by=5))
abline(v=1.33,col="red",lwd=2)

hist(IL$V1,breaks=seq(0.5,4.9,by=0.2),ylim=c(0,30),axes=FALSE,labels=FALSE,col="dodgerblue3",main="'IL078721' Oat")
axis(1, at=seq(0.5,4.9,by=0.2))
axis(2, at=seq(0,30,by=5))
abline(v=1.39,col="red",lwd=2)

hist(gopher$V1,breaks=seq(0.5,4.9,by=0.2),ylim=c(0,30),axes=FALSE,labels=FALSE,col="dodgerblue3",main="'Gopher' Oat")
axis(1, at=seq(0.5,4.9,by=0.2))
axis(2, at=seq(0,30,by=5))
abline(v=1.24,col="red",lwd=2)
