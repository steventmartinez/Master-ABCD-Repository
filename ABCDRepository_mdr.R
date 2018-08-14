library(ggplot2)
library(readr)
library(dplyr)

dataDir<-"/Users/monica/Documents/Projects/ABCD/NDAR_downloads/"
outputDir<-"/Users/monica/Documents/Projects/ABCD/R_scripts"

######### Read Files into R #########
Screener<-read.delim(paste(dataDir,"Questionnaire/abcd_screen01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
Screener<-Screener %>% slice(-1)

MRINBack<-read.delim(paste(dataDir,"TaskBased/abcd_mrinback01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
MRINBack<-MRINBack %>% slice(-1)

RecMem<-read.delim(paste(dataDir,"TaskBased/mribrec01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
RecMem<-RecMem %>% slice(-1)

MRISST<-read.delim(paste(dataDir,"TaskBased/abcd_sst01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
MRISST<-MRISST %>% slice(-1)

MRIMID<-read.delim(paste(dataDir,"TaskBased/abcd_mid01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
MRIMID<-MRIMID %>% slice(-1)

RAChecklist<-read.delim(paste(dataDir,"Questionnaire/abcd_ra01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
RAChecklist<-RAChecklist %>% slice(-1)

PKSADS<-read.delim(paste(dataDir,"Diagnostic/abcd_ksad01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
PKSADS<-PKSADS %>% slice(-1)

YKSADS<-read.delim(paste(dataDir,"Diagnostic/abcd_ksad501.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
YKSADS<-YKSADS %>% slice(-1)

Pearson<-read.delim(paste(dataDir,"Cognitive/abcd_ps01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
Pearson<-Pearson %>% slice(-1)

FamilyHistory1<-read.delim(paste(dataDir,"MedHistory/fhxp101.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
FamilyHistory1<-FamilyHistory1 %>% slice(-1)

FamilyHistory2<-read.delim(paste(dataDir,"MedHistory/fhxp201.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
FamilyHistory2<-FamilyHistory2 %>% slice(-1)

Demographics<-read.delim(paste(dataDir,"Demographics/pdem01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
Demographics<-Demographics %>% slice(-1)

ASR<-read.delim(paste(dataDir,"Questionnaire/pasr01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
ASR<-ASR %>% slice(-1)

Sleep<-read.delim(paste(dataDir,"Sleep/abcd_sds01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
Sleep<-Sleep %>% slice(-1)

Physical<-read.delim(paste(dataDir,"Activity/abcd_yrb01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
Physical<-Physical %>% slice(-1)

CBCL<-read.delim(paste(dataDir,"Behavior/abcd_cbcl01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
CBCL<-CBCL %>% slice(-1)

CBCLS<-read.delim(paste(dataDir,"Behavior/abcd_cbcls01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
CBCLS<-CBCLS %>% slice(-1)

ScannerIDs<-read.delim(paste(dataDir,"EvaluatedData/abcd_betnet01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
ScannerIDs<-ScannerIDs %>% slice(-1)

Siblings<-read.delim(paste(dataDir,"Socioeconomic/acspsw01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
Siblings<-Siblings %>% slice(-1)

######### Merge Data #########
sheets<-c("RecMem", "MRISST", "MRIMID", "RAChecklist", "Demographics", "YKSADS", "PKSADS", "ASR", "FamilyHistory1", "FamilyHistory2", "Pearson", "Sleep", "Physical", "CBCL", "CBCLS", "ScannerIDs", "Siblings")
  
mergedata<-merge(Screener, MRINBack, by, by.x="subjectkey", by.y="subjectkey", sort=TRUE, all=TRUE)
write.csv(mergedata,file=paste(outputDir,"ABCD.csv",sep=""))

for (s in sheets) {
  tmp<-get(s)
  ABCD<-read.csv(paste(outputDir,"ABCD.csv",sep=""), header =TRUE, fill=TRUE, sep =',')
  mergedata<-merge(mergedata, tmp, by, by.x="subjectkey", by.y="subjectkey", sort=TRUE, all=TRUE)
  write.csv(mergedata,file=paste(outputDir,"ABCD.csv",sep=""))
}

ABCD<-read.csv(paste(outputDir,"ABCD.csv",sep=""), header =TRUE, fill=TRUE, sep =',')
#View(ABCD)

#Subsetting
Autism<-subset(ABCD, scrn_asd =1)
Seizures<-subset(ABCD, scrn_medcond_other =1)
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
#View(ABCD_clean)

#For the histogram for 0-back vs 2-back total acc %
#qplot(ABCD_clean$ZeroBackMean, geom="histogram", xlab="AllTotalAcc", fill=I("black"), col=I("red"))
#qplot(ABCD_clean$TwoBackMean, geom="histogram", xlab="AllTotalAcc", fill=I("black"), col=I("red"))

#Histograms
#hist(ABCD_clean$TwoBackMean)
#hist(ABCD_clean$ZeroBackMean)
#hist(ABCD_clean$TotalAccMean)
#hist(ABCD_clean$DPrimeMean)

#Scatterplots
#ggplot(ABCD_clean, aes(x=TwoBackMean, y=DPrimeMean)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred")
#ggplot(ABCD_clean, aes(x=ZeroBackMean, y=DPrimeMean)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred")
#ggplot(ABCD_clean, aes(x=TotalAccMean, y=DPrimeMean)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred")

ggplot(ABCD_clean, aes(x=beh_sst_ssrt_mean_total, y=DPrimeMean)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred")
ggplot(ABCD_clean, aes(x=beh_sst_ssrt_mean_total, y=beh_mid_money_total)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred")
ggplot(ABCD_clean, aes(x=DPrimeMean, y=beh_mid_money_total)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred")

ggplot(ABCD_clean, aes(x=DPrimeMean, y=cbcl_scr_dsm5_adhd_t)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred")
ggplot(ABCD_clean, aes(x=beh_sst_ssrt_mean_total, y=cbcl_scr_dsm5_adhd_t)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred")
ggplot(ABCD_clean, aes(x=beh_mid_money_total, y=cbcl_scr_dsm5_adhd_t)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred")

#To remove an environment: rm(list=ls())

#Add column for average of Zero Back and TwoBack (Use the same structure as finding the mean for DPrime)
#Add file for Scanner Types
#Add file for siblings
#Baseline data workshop on abcd-workshop.ucsd.edu
#Play around with subsetting if able to decode sibling structure
