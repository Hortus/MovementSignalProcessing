#Script to analyze the FFT data from stationary videos
library(agricolae)

#for hand generated
#set working directory
setwd("D:/.../signals")

#read final stationary data
finaldf <- read.table("stationaryfinal_noFFT.txt", header=TRUE)
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



#linear model analysis
lmMeanFreq <- lm(MeanFreq ~ VideoDate + Line + PlantingDate + Line/Rep + Line*PlantingDate + (Line/Rep)*PlantingDate, data = finaldf)
#re worked model after committee meeting with correct rep nesting
lmMeanFreq <- lm(MeanFreq ~ VideoDate + Line + PlantingDate + PlantingDate/Rep + Line*PlantingDate + (PlantingDate/Rep)*Line, data = finaldf)
#Apply this to each video date.  
jul10 <- subset(finaldf,VideoDate=="07_10_17")
jul11 <- subset(finaldf,VideoDate=="07_11_17")
jul12 <- subset(finaldf,VideoDate=="07_12_17")
jul13 <- subset(finaldf,VideoDate=="07_13_17")
jul17 <- subset(finaldf,VideoDate=="07_17_17")

#models for each video date. Substitute the response Hz1.1 and Hz1.3 for MeanFreq to analyze amplitude.
#jul10
lmMeanFreq10 <- lm(Hz1.1 ~ Line + PlantingDate + PlantingDate/Position + Line*PlantingDate + (PlantingDate/Position)*Line, data = jul10)
modaov <- anova(lmMeanFreq10) #run anova
#Extract Mean squares, degrees of freedom
meansq <- modaov$`Mean Sq` #get vector of mean sq. Line, PlantingDate,rep(PlantingDate),LinexPlantingDate,Linexrep(PlantingDate)
df <- modaov$Df #get vector of df. Line, PlantingDate,rep(PlantingDate),LinexPlantingDate,Linexrep(PlantingDate)
LineMS <- meansq[1]
PdMS <- meansq[2]
repPdMS <- meansq[3]
LinexPdMS <- meansq[4]
LinexrepPdMS <- meansq[5]
MSE <- meansq[6]
LineDF <- df[1]
PdDF <- df[2]
repPdDF <- df[3]
LinexPdDF <- df[4]
LinexrepPdDF <- df[5]
DFE <- df[6]
#Run F tests
#get f values
LineF <- (LineMS+LinexrepPdMS)/LinexrepPdMS
PdF <- (repPdMS+PdMS)/repPdMS
repPdF <- (repPdMS)/MSE
LinexPdF <- (repPdMS+LinexPdMS)/repPdMS
LinexrepPdF <- (LinexrepPdMS)/MSE
#test f values with pf function and df
LineP <- pf(LineF,LineDF,LinexrepPdDF,lower.tail=FALSE)
PdP <- pf(PdF,PdDF,repPdDF,lower.tail=FALSE)
repPdP <- pf(repPdF,repPdDF,DFE,lower.tail = FALSE)
LinexPdP <- pf(LinexPdF,LinexPdDF,LinexrepPdDF,lower.tail=FALSE)
LinexrepPdP <- pf(LinexrepPdF,LinexrepPdDF,DFE,lower.tail = FALSE)
Pvals <- c(LineP,PdP,repPdP,LinexPdP,LinexrepPdP)

#jul11
lmMeanFreq11 <- lm(Hz1.1 ~ Line + PlantingDate + PlantingDate/Position + Line*PlantingDate + (PlantingDate/Position)*Line, data = jul11)
modaov <- anova(lmMeanFreq11) #run anova
#Extract Mean squares, degrees of freedom
meansq <- modaov$`Mean Sq` #get vector of mean sq. Line, PlantingDate,rep(PlantingDate),LinexPlantingDate,Linexrep(PlantingDate)
df <- modaov$Df #get vector of df. Line, PlantingDate,rep(PlantingDate),LinexPlantingDate,Linexrep(PlantingDate)
LineMS <- meansq[1]
PdMS <- meansq[2]
repPdMS <- meansq[3]
LinexPdMS <- meansq[4]
LinexrepPdMS <- meansq[5]
MSE <- meansq[6]
LineDF <- df[1]
PdDF <- df[2]
repPdDF <- df[3]
LinexPdDF <- df[4]
LinexrepPdDF <- df[5]
DFE <- df[6]
#Run F tests
#get f values
LineF <- (LineMS+LinexrepPdMS)/LinexrepPdMS
PdF <- (repPdMS+PdMS)/repPdMS
repPdF <- (repPdMS)/MSE
LinexPdF <- (repPdMS+LinexPdMS)/repPdMS
LinexrepPdF <- (LinexrepPdMS)/MSE
#test f values with pf function and df
LineP <- pf(LineF,LineDF,LinexrepPdDF,lower.tail=FALSE)
PdP <- pf(PdF,PdDF,repPdDF,lower.tail=FALSE)
repPdP <- pf(repPdF,repPdDF,DFE,lower.tail = FALSE)
LinexPdP <- pf(LinexPdF,LinexPdDF,LinexrepPdDF,lower.tail=FALSE)
LinexrepPdP <- pf(LinexrepPdF,LinexrepPdDF,DFE,lower.tail = FALSE)
Pvals <- c(LineP,PdP,repPdP,LinexPdP,LinexrepPdP)

#jul12
lmMeanFreq12 <- lm(Hz1.1 ~ Line + PlantingDate + PlantingDate/Position + Line*PlantingDate + (PlantingDate/Position)*Line, data = jul12)
modaov <- anova(lmMeanFreq12) #run anova
#Extract Mean squares, degrees of freedom
meansq <- modaov$`Mean Sq` #get vector of mean sq. Line, PlantingDate,rep(PlantingDate),LinexPlantingDate,Linexrep(PlantingDate)
df <- modaov$Df #get vector of df. Line, PlantingDate,rep(PlantingDate),LinexPlantingDate,Linexrep(PlantingDate)
LineMS <- meansq[1]
PdMS <- meansq[2]
repPdMS <- meansq[3]
LinexPdMS <- meansq[4]
LinexrepPdMS <- meansq[5]
MSE <- meansq[6]
LineDF <- df[1]
PdDF <- df[2]
repPdDF <- df[3]
LinexPdDF <- df[4]
LinexrepPdDF <- df[5]
DFE <- df[6]
#Run F tests
#get f values
LineF <- (LineMS+LinexrepPdMS)/LinexrepPdMS
PdF <- (repPdMS+PdMS)/repPdMS
repPdF <- (repPdMS)/MSE
LinexPdF <- (repPdMS+LinexPdMS)/repPdMS
LinexrepPdF <- (LinexrepPdMS)/MSE
#test f values with pf function and df
LineP <- pf(LineF,LineDF,LinexrepPdDF,lower.tail=FALSE)
PdP <- pf(PdF,PdDF,repPdDF,lower.tail=FALSE)
repPdP <- pf(repPdF,repPdDF,DFE,lower.tail = FALSE)
LinexPdP <- pf(LinexPdF,LinexPdDF,LinexrepPdDF,lower.tail=FALSE)
LinexrepPdP <- pf(LinexrepPdF,LinexrepPdDF,DFE,lower.tail = FALSE)
Pvals <- c(LineP,PdP,repPdP,LinexPdP,LinexrepPdP)

#jul13
lmMeanFreq13 <- lm(Hz1.1 ~ Line + PlantingDate + PlantingDate/Position + Line*PlantingDate + (PlantingDate/Position)*Line, data = jul13)
modaov <- anova(lmMeanFreq13) #run anova
#Extract Mean squares, degrees of freedom
meansq <- modaov$`Mean Sq` #get vector of mean sq. Line, PlantingDate,rep(PlantingDate),LinexPlantingDate,Linexrep(PlantingDate)
df <- modaov$Df #get vector of df. Line, PlantingDate,rep(PlantingDate),LinexPlantingDate,Linexrep(PlantingDate)
LineMS <- meansq[1]
PdMS <- meansq[2]
repPdMS <- meansq[3]
LinexPdMS <- meansq[4]
LinexrepPdMS <- meansq[5]
MSE <- meansq[6]
LineDF <- df[1]
PdDF <- df[2]
repPdDF <- df[3]
LinexPdDF <- df[4]
LinexrepPdDF <- df[5]
DFE <- df[6]
#Run F tests
#get f values
LineF <- (LineMS+LinexrepPdMS)/LinexrepPdMS
PdF <- (repPdMS+PdMS)/repPdMS
repPdF <- (repPdMS)/MSE
LinexPdF <- (repPdMS+LinexPdMS)/repPdMS
LinexrepPdF <- (LinexrepPdMS)/MSE
#test f values with pf function and df
LineP <- pf(LineF,LineDF,LinexrepPdDF,lower.tail=FALSE)
PdP <- pf(PdF,PdDF,repPdDF,lower.tail=FALSE)
repPdP <- pf(repPdF,repPdDF,DFE,lower.tail = FALSE)
LinexPdP <- pf(LinexPdF,LinexPdDF,LinexrepPdDF,lower.tail=FALSE)
LinexrepPdP <- pf(LinexrepPdF,LinexrepPdDF,DFE,lower.tail = FALSE)
Pvals <- c(LineP,PdP,repPdP,LinexPdP,LinexrepPdP)

#jul17
lmMeanFreq17 <- lm(Hz1.3 ~ Line + PlantingDate + PlantingDate/Position + Line*PlantingDate + (PlantingDate/Position)*Line, data = jul17)
modaov <- anova(lmMeanFreq17) #run anova
#Extract Mean squares, degrees of freedom
meansq <- modaov$`Mean Sq` #get vector of mean sq. Line, PlantingDate,rep(PlantingDate),LinexPlantingDate,Linexrep(PlantingDate)
df <- modaov$Df #get vector of df. Line, PlantingDate,rep(PlantingDate),LinexPlantingDate,Linexrep(PlantingDate)
LineMS <- meansq[1]
PdMS <- meansq[2]
repPdMS <- meansq[3]
LinexPdMS <- meansq[4]
LinexrepPdMS <- meansq[5]
MSE <- meansq[6]
LineDF <- df[1]
PdDF <- df[2]
repPdDF <- df[3]
LinexPdDF <- df[4]
LinexrepPdDF <- df[5]
DFE <- df[6]
#Run F tests
#get f values
LineF <- (LineMS+LinexrepPdMS)/LinexrepPdMS
PdF <- (repPdMS+PdMS)/repPdMS
repPdF <- (repPdMS)/MSE
LinexPdF <- (repPdMS+LinexPdMS)/repPdMS
LinexrepPdF <- (LinexrepPdMS)/MSE
#test f values with pf function and df
LineP <- pf(LineF,LineDF,LinexrepPdDF,lower.tail=FALSE)
PdP <- pf(PdF,PdDF,repPdDF,lower.tail=FALSE)
repPdP <- pf(repPdF,repPdDF,DFE,lower.tail = FALSE)
LinexPdP <- pf(LinexPdF,LinexPdDF,LinexrepPdDF,lower.tail=FALSE)
LinexrepPdP <- pf(LinexrepPdF,LinexrepPdDF,DFE,lower.tail = FALSE)
Pvals <- c(LineP,PdP,repPdP,LinexPdP,LinexrepPdP)




#lsd tests. USE FDR correction.  THen go back and do each model for each video date.  Line can remain
#mean frequency
lsdMeanFreq10 <- LSD.test(aov(lmMeanFreq10),"Line",p.adj="fdr") 
lsdMeanFreq11 <- LSD.test(aov(lmMeanFreq11),"Line",p.adj="fdr")
lsdMeanFreq12 <- LSD.test(aov(lmMeanFreq12),"Line",p.adj="fdr")
lsdMeanFreq13 <- LSD.test(aov(lmMeanFreq13),"Line",p.adj="fdr")
lsdMeanFreq17 <- LSD.test(aov(lmMeanFreq17),"Line",p.adj="fdr")
lsdMeanFreq10pd <- LSD.test(aov(lmMeanFreq10),"PlantingDate",p.adj="fdr") 
lsdMeanFreq11pd <- LSD.test(aov(lmMeanFreq11),"PlantingDate",p.adj="fdr")
lsdMeanFreq12pd <- LSD.test(aov(lmMeanFreq12),"PlantingDate",p.adj="fdr")
lsdMeanFreq13pd <- LSD.test(aov(lmMeanFreq13),"PlantingDate",p.adj="fdr")
lsdMeanFreq17pd <- LSD.test(aov(lmMeanFreq17),"PlantingDate",p.adj="fdr")


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
