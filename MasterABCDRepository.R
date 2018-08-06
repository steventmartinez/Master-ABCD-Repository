
###mADE CHANGE
library(ggplot2)
library(readr)
library(dplyr)

#Read Files into R
Screener<-read.csv("/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/abcd_screen01_2017_01_29.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
Screener<-Screener %>% slice(-1)
MRINBack<-read.csv("/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/abcd_mrinback01_2017_01_29.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
MRINBack<-MRINBack %>% slice(-1)
RecMem<-read.csv("/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/mribrec01_2017_01_29.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
RecMem<-RecMem %>% slice(-1)
RAChecklist<-read.csv("/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/abcd_ra01_2017_01_29.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
RAChecklist<-RAChecklist %>% slice(-1)
PKSADS<-read.csv("/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/abcd_ksad01_2017_01_29.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
PKSADS<-PKSADS %>% slice(-1)
YKSADS<-read.csv("/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/abcd_ksad501_2017_01_29.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
YKSADS<-YKSADS %>% slice(-1)
Pearson<-read.csv("/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/absd_ps01_2017_01_29.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
Pearson<-Pearson %>% slice(-1)
FamilyHistory1<-read.csv("/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/fhxp101_2017_01_29.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
FamilyHistory1<-FamilyHistory1 %>% slice(-1)
FamilyHistory2<-read.csv("/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/fhxp201_2017_01_29.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
FamilyHistory2<-FamilyHistory2 %>% slice(-1)
Demographics<-read.csv("/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/pdem01_2017_01_29.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
Demographics<-Demographics %>% slice(-1)
ASR<-read.csv("/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/pasr01_2018_02_09.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
ASR<-ASR %>% slice(-1)
Sleep<-read.csv("/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/abcd_sds01_2017_01_29.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
Sleep<-Sleep %>% slice(-1)
Physical<-read.csv("/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/abcd_yrb01_2017_01_29.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
Physical<-Physical %>% slice(-1)
CBCL<-read.csv("/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/abcd_cbcl01_2017_01_29.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
CBCL<-CBCL %>% slice(-1)
ScannerIDs<-read.csv("/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/abcd_betnet01_2017_01_29.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
ScannerIDs<-ScannerIDs %>% slice(-1)
Siblings<-read.csv("/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/acspsw01_2017_01_29.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
Siblings<-Siblings %>% slice(-1)

#Merge Data
mergedata<-merge(Screener, MRINBack, by, by.x="subjectkey", by.y="subjectkey", sort=TRUE, all=TRUE)
write.csv(mergedata,file="/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv")
ABCD<-read.csv('/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv', header =TRUE, fill=TRUE, sep =',')
mergedata<-merge(mergedata, RecMem, by, by.x="subjectkey", by.y="subjectkey", sort=TRUE, all=TRUE)
write.csv(mergedata,file="/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv")
ABCD<-read.csv('/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv', header =TRUE, fill=TRUE, sep =',')
mergedata<-merge(mergedata, RAChecklist, by, by.x="subjectkey", by.y="subjectkey", sort=TRUE, all=TRUE)
write.csv(mergedata,file="/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv")
ABCD<-read.csv('/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv', header =TRUE, fill=TRUE, sep =',')
mergedata<-merge(mergedata, Demographics, by, by.x="subjectkey", by.y="subjectkey", sort=TRUE, all=TRUE)
write.csv(mergedata,file="/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv")
ABCD<-read.csv('/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv', header =TRUE, fill=TRUE, sep =',')
mergedata<-merge(mergedata, YKSADS, by, by.x="subjectkey", by.y="subjectkey", sort=TRUE, all=TRUE)
write.csv(mergedata,file="/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv")
ABCD<-read.csv('/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv', header =TRUE, fill=TRUE, sep =',')
mergedata<-merge(mergedata, PKSADS, by, by.x="subjectkey", by.y="subjectkey", sort=TRUE, all=TRUE)
write.csv(mergedata,file="/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv")
ABCD<-read.csv('/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv', header =TRUE, fill=TRUE, sep =',')
mergedata<-merge(mergedata, ASR, by, by.x="subjectkey", by.y="subjectkey", sort=TRUE, all=TRUE)
write.csv(mergedata,file="/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv")
ABCD<-read.csv('/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv', header =TRUE, fill=TRUE, sep =',')
mergedata<-merge(mergedata, FamilyHistory1, by, by.x="subjectkey", by.y="subjectkey", sort=TRUE, all=TRUE)
write.csv(mergedata,file="/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv")
ABCD<-read.csv('/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv', header =TRUE, fill=TRUE, sep =',')
mergedata<-merge(mergedata, FamilyHistory2, by, by.x="subjectkey", by.y="subjectkey", sort=TRUE, all=TRUE)
write.csv(mergedata,file="ABCD.csv")
ABCD<-read.csv('/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv', header =TRUE, fill=TRUE, sep =',')
mergedata<-merge(mergedata, Pearson, by, by.x="subjectkey", by.y="subjectkey", sort=TRUE, all=TRUE)
write.csv(mergedata,file="/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv")
ABCD<-read.csv('/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv', header =TRUE, fill=TRUE, sep =',')
mergedata<-merge(mergedata, Sleep, by, by.x="subjectkey", by.y="subjectkey", sort=TRUE, all=TRUE)
write.csv(mergedata,file="/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv")
ABCD<-read.csv('/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv', header =TRUE, fill=TRUE, sep =',')
mergedata<-merge(mergedata, Physical, by, by.x="subjectkey", by.y="subjectkey", sort=TRUE, all=TRUE)
write.csv(mergedata,file="/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv")
ABCD<-read.csv('/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv', header =TRUE, fill=TRUE, sep =',')
mergedata<-merge(mergedata, CBCL, by, by.x="subjectkey", by.y="subjectkey", sort=TRUE, all=TRUE)
write.csv(mergedata,file="/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv")
ABCD<-read.csv('/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv', header =TRUE, fill=TRUE, sep =',')
mergedata<-merge(mergedata, ScannerIDs, by, by.x="subjectkey", by.y="subjectkey", sort=TRUE, all=TRUE)
write.csv(mergedata,file="/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv")
ABCD<-read.csv('/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv', header =TRUE, fill=TRUE, sep =',')
mergedata<-merge(mergedata, Siblings, by, by.x="subjectkey", by.y="subjectkey", sort=TRUE, all=TRUE)
write.csv(mergedata,file="/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv")
ABCD<-read.csv('/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCD.csv', header =TRUE, fill=TRUE, sep =',')
View(ABCD)

#Subsetting
Autism<-subset(ABCD, scrn_asd =1)
Seizures<-subset(ABCD, scrn_medcond_other =1)
#Incidental
#Sibling
#If NDARs with NA, then exclude it that way
#DPrime 3 SD away from the mean if higher
#DPrime 3 SD away from the mean if lower
#TotalAcc 3 SD away from the mean if higher
#TotalAcc 3 SD away from mean if higher

#DPrime Mean
ABCD$DPrimeMean<-rowMeans(cbind(as.numeric(ABCD$beh_rec_negface_dprime),as.numeric(ABCD$beh_rec_posface_dprime),as.numeric(ABCD$beh_rec_neutface_dprime),as.numeric(ABCD$beh_rec_place_dprime)),na.rm=TRUE, dims=1)
ABCD$DPrimeMean[is.nan(ABCD$DPrimeMean)]<-NA
ABCD$DPrimeMean<-as.numeric(ABCD$DPrimeMean)

#0-Back Mean
ABCD$ZeroBackMean<-rowMeans(cbind(as.numeric(ABCD$bn_block_0_back_total_acc)),na.rm=TRUE, dims=1)
ABCD$ZeroBackMean[is.nan(ABCD$ZeroBackMean)]<-NA
ABCD$ZeroBackMean<-as.numeric(ABCD$ZeroBackMean)

#2-BackMean
ABCD$TwoBackMean<-rowMeans(cbind(as.numeric(ABCD$bn_block_2_back_total_acc)),na.rm=TRUE, dims=1)
ABCD$TwoBackMean[is.nan(ABCD$TwoBackMean)]<-NA
ABCD$TwoBackMean<-as.numeric(ABCD$TwoBackMean)

#TotalAccMean
ABCD$TotalAccMean<-rowMeans(cbind(as.numeric(ABCD$beh_nback_all_total_acc)),na.rm=TRUE, dims=1)
ABCD$TotalAccMean[is.nan(ABCD$TotalAccMean)]<-NA
ABCD$TotalAccMean<-as.numeric(ABCD$TotalAccMean)

#ZeroBackAndTwoBackMean
ABCD$ZeroBackTwoBackMean<-rowMeans(cbind(as.numeric(ABCD$bn_block_0_back_total_acc),as.numeric(ABCD$bn_block_2_back_total_acc)),na.rm=TRUE, dims=1)
ABCD$ZeroBackTwoBackMean[is.nan(ABCD$ZeroBackTwoBackMean)]<-NA
ABCD$ZeroBackTwoBackMean<-as.numeric(ABCD$ZeroBackTwoBackMean)

#Finalization
ABCD_clean <- ABCD[ which(ABCD$scrn_asd==0 & ABCD$scrn_medcond_other==0 & ABCD$DPrimeMean>=(mean(ABCD$DPrimeMean, na.rm=TRUE) - 3*sd(ABCD$DPrimeMean, na.rm=TRUE)) & ABCD$DPrimeMean<=(mean(ABCD$DPrimeMean, na.rm=TRUE) + 3*sd(ABCD$DPrimeMean, na.rm=TRUE)) & ABCD$ZeroBackMean>=(mean(ABCD$ZeroBackMean, na.rm=TRUE)-3*sd(ABCD$ZeroBackMean, na.rm=TRUE)) & ABCD$ZeroBackMean<=(mean(ABCD$ZeroBackMean, na.rm=TRUE)+3*sd(ABCD$ZeroBackMean, na.rm=TRUE)) & ABCD$TwoBackMean>=(mean(ABCD$TwoBackMean, na.rm=TRUE)-3*sd(ABCD$TwoBackMean, na.rm=TRUE)) & ABCD$TwoBackMean<=(mean(ABCD$TwoBackMean, na.rm=TRUE)+3*sd(ABCD$TwoBackMean, na.rm=TRUE))), ]
View(ABCD_clean)

#For the histogram for 0-back vs 2-back total acc %
qplot(ABCD_clean$ZeroBackMean, geom="histogram", xlab="AllTotalAcc", fill=I("black"), col=I("red"))
qplot(ABCD_clean$TwoBackMean, geom="histogram", xlab="AllTotalAcc", fill=I("black"), col=I("red"))

#Histograms
hist(ABCD_clean$TwoBackMean)
hist(ABCD_clean$ZeroBackMean)
hist(ABCD_clean$TotalAccMean)
hist(ABCD_clean$DPrimeMean)

#Scatterplots
ggplot(ABCD_clean, aes(x=TwoBackMean, y=DPrimeMean)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred")
ggplot(ABCD_clean, aes(x=ZeroBackMean, y=DPrimeMean)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred")
ggplot(ABCD_clean, aes(x=TotalAccMean, y=DPrimeMean)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred")

#To remove an environment: rm(list=ls())

#Add column for average of Zero Back and TwoBack (Use the same structure as finding the mean for DPrime)
#Add file for Scanner Types
#Add file for siblings
#Baseline data workshop on abcd-workshop.ucsd.edu
#Play around with subsetting if able to decode sibling structure