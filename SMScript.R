library(ggplot2)
library(readr)
library(dplyr)
library(corrplot)
library(psych)

dataDir<-"/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/"
outputDir<-"/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/output"

######### Read Files into R #########
Screener<-read.delim(paste(dataDir, "abcd_screen01_2017_01_29.tsv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
Screener<-Screener %>% slice(-1)

MRINBack<-read.delim(paste(dataDir,"abcd_mrinback01_2017_01_29.tsv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
MRINBack<-MRINBack %>% slice(-1)

RecMem<-read.delim(paste(dataDir,"mribrec01_2017_01_29.tsv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
RecMem<-RecMem %>% slice(-1)

MRISST<-read.delim(paste(dataDir,"abcd_sst01_2017_01_29.tsv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
MRISST<-MRISST %>% slice(-1)

MRIMID<-read.delim(paste(dataDir,"abcd_mid01_2017_01_29.tsv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
MRIMID<-MRIMID %>% slice(-1)

RAChecklist<-read.delim(paste(dataDir,"abcd_ra01_2017_01_29.tsv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
RAChecklist<-RAChecklist %>% slice(-1)

PKSADS<-read.delim(paste(dataDir,"abcd_ksad01_2017_01_29.tsv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
PKSADS<-PKSADS %>% slice(-1)

YKSADS<-read.delim(paste(dataDir,"abcd_ksad501_2017_01_29.tsv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
YKSADS<-YKSADS %>% slice(-1)

Pearson<-read.delim(paste(dataDir,"absd_ps01_2017_01_29.tsv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
Pearson<-Pearson %>% slice(-1)

NeuroCog<-read.delim(paste(dataDir,"abcd_tbss01_2017_01_29.tsv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
NeuroCog<-NeuroCog %>% slice(-1)

FamilyHistory1<-read.delim(paste(dataDir,"fhxp101_2017_01_29.tsv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
FamilyHistory1<-FamilyHistory1 %>% slice(-1)

FamilyHistory2<-read.delim(paste(dataDir,"fhxp201_2017_01_29.tsv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
FamilyHistory2<-FamilyHistory2 %>% slice(-1)

Demographics<-read.delim(paste(dataDir,"pdem01_2017_01_29.tsv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
Demographics<-Demographics %>% slice(-1)

ASR<-read.delim(paste(dataDir,"pasr01_2018_02_09.tsv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
ASR<-ASR %>% slice(-1)

Sleep<-read.delim(paste(dataDir,"abcd_sds01_2017_01_29.tsv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
Sleep<-Sleep %>% slice(-1)

Physical<-read.delim(paste(dataDir,"abcd_yrb01_2017_01_29.tsv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
Physical<-Physical %>% slice(-1)

CBCL<-read.delim(paste(dataDir,"abcd_cbcl01_2017_01_29.tsv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
CBCL<-CBCL %>% slice(-1)

CBCLS<-read.delim(paste(dataDir,"abcd_cbcls01_2017_01_29.tsv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
CBCLS<-CBCLS %>% slice(-1)

ScannerIDs<-read.delim(paste(dataDir,"abcd_betnet01_2017_01_29.tsv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
ScannerIDs<-ScannerIDs %>% slice(-1)

Siblings<-read.delim(paste(dataDir,"acspsw01_2017_01_29.tsv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
Siblings<-Siblings %>% slice(-1)

Incidental<-read.delim(paste(dataDir,"abcd_mrfindings01_2017_01_29.tsv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
Incidental<-Incidental %>% slice(-1)


######### Merge Data #########
sheets<-c("RecMem", "MRISST", "MRIMID", "RAChecklist", "Demographics", "YKSADS", "PKSADS", "ASR", "NeuroCog", "FamilyHistory1", "FamilyHistory2", "Pearson", "Sleep", "Physical", "CBCL", "CBCLS", "ScannerIDs", "Siblings", "Incidental")

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

#CreatingNewCoumns
ABCD$Single<-ABCD$Siblings
ABCD$Sibs<-ABCD$Siblings
ABCD$Twins<-ABCD$Siblings

#Subsetting
Autism<-subset(ABCD, scrn_asd =0)
Seizures<-subset(ABCD, scrn_medcond_other =0)
Single<-subset(ABCD, rel_relationship =0)
Sibs<-subset(ABCD, rel_relationship= 1)
Twins<-subset(ABCD, rel_relationship= 2)
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

#Standard Error of Means
describe(ABCD_clean$DPrimeMean, type=2)
describe(ABCD_clean$ZeroBackMean, type=2)
describe(ABCD_clean$TwoBackMean, type=2)
describe(ABCD_clean$TotalAccMean, type=2)
describe(ABCD_clean$ZeroBackTwoBackMean, type=2)

#For the histogram for 0-back vs 2-back total acc %
#qplot(ABCD_clean$ZeroBackMean, geom="histogram", xlab="AllTotalAcc", fill=I("black"), col=I("red"))
#qplot(ABCD_clean$TwoBackMean, geom="histogram", xlab="AllTotalAcc", fill=I("black"), col=I("red"))

#Histograms
qplot(ABCD_clean$TwoBackMean, geom="histogram", main="2 Back Accuracy", xlab="2BackMean", fill=I("green"), col=I("blue"), alpha=I(.2))
qplot(ABCD_clean$ZeroBackMean, geom="histogram", main="0 Back Accuracy", xlab="0BackMean", fill=I("green"), col=I("blue"), alpha=I(.2))
qplot(ABCD_clean$TotalAccMean, geom="histogram", main="Total Accuracy", xlab="TotalAccMean", fill=I("green"), col=I("blue"), alpha=I(.2))
qplot(ABCD_clean$DPrimeMean, geom="histogram", main="DPrime", xlab="DPrimeMean", fill=I("green"), col=I("blue"), alpha=I(.2))
qplot(ABCD_clean$rel_relationship, geom="histogram", main="Siblings", xlab="Siblings", fill=I("green"), col=I("blue"), alpha=I(.2))
#hist(Single$rel_relationship)
#hist(Sibs$rel_relationship)
#hist(Twins$rel_relationship)

#Scatterplots
ggplot(ABCD_clean, aes(x=TwoBackMean, y=DPrimeMean)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred") + stat_cor()
ggplot(ABCD_clean, aes(x=ZeroBackMean, y=DPrimeMean)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred") + stat_cor()
ggplot(ABCD_clean, aes(x=TotalAccMean, y=DPrimeMean)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred") + stat_cor()

#ggplot(ABCD_clean, aes(x=beh_sst_ssrt_mean_total, y=DPrimeMean)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred") + stat_cor()
#ggplot(ABCD_clean, aes(x=beh_sst_ssrt_mean_total, y=beh_mid_money_total)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred") + stat_cor()
#ggplot(ABCD_clean, aes(x=DPrimeMean, y=beh_mid_money_total)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred") + stat_cor()

#ggplot(ABCD_clean, aes(x=DPrimeMean, y=cbcl_scr_dsm5_adhd_t)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred") + stat_cor()
#ggplot(ABCD_clean, aes(x=beh_sst_ssrt_mean_total, y=cbcl_scr_dsm5_adhd_t)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred") + stat_cor()
#ggplot(ABCD_clean, aes(x=beh_mid_money_total, y=cbcl_scr_dsm5_adhd_t)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred") + stat+cor()

#To remove an environment: rm(list=ls())
#This resests the graphs environment: dev.off()

#CorrMatrix
CorrMatrix<-subset(ABCD_clean, select = c(beh_nback_all_total_acc, bn_block_2_back_total_acc, bn_block_0_back_total_acc, DPrimeMean, nihtbx_list_agecorrected, nihtbx_picture_agecorrected, beh_sst_ssrt_mean_total, beh_mid_money_total, pea_wiscv_tss, pea_ravlt_sd_trial_vi_tc, pea_ravlt_ld_trial_vii_tc))
#CorrMatrix<-rename(CorrMatrix, c("beh_nback_all_total_acc"="NBackTotalAcc", "bn_block_2_back_total_acc"="2BackTotalAcc", "bn_block_0_back_total_acc"="0BackTotalAcc", "nihtbx_list_agecorrected"="ListSortSS", "nihtbx_picture_agecorrected"="PictureSequenceSS", "beh_sst_ssrt_mean_total"="SSTMeanRT", "beh_mid_money_total"="MIDTotalEarnings", "pea_wiscv_tss"="MatrixReasoningScaledScore", "pea_ravlt_sd_trial_vi_tc"="RAVLT_Trial6", "pea_ravlt_ld_trial_vii_tc"="RAVLT_Trial7"))
Matrix<-cor(CorrMatrix, use="complete.obs")
corrplot(Matrix)
#Compare these with the original one
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(Matrix, type="full")
corrplot(Matrix, type="upper", order="hclust", tl.col="black", tl.srt=1)

#RAVLT: pea_ravlt_sd_trial_vi_tc + pea_ravlt_sd_trial_vii_tc
RAVLT<-subset(ABCD_clean, select = c(pea_ravlt_sd_trial_i_tc, pea_ravlt_sd_trial_ii_tc, pea_ravlt_sd_trial_iii_tc, pea_ravlt_sd_trial_iv_tc, pea_ravlt_sd_trial_v_tc)) 
Trial1_5<-colMeans(RAVLT, na.rm=TRUE)
              
#Add column for average of Zero Back and TwoBack (Use the same structure as finding the mean for DPrime)
#Add file for Scanner Types
#Add file for siblings
#Baseline data workshop on abcd-workshop.ucsd.edu
#Play around with subsetting if able to decode sibling structure

#####To find standard error use: describe(ABCD_clean$rel_relationship, type=2) 
#To make a new data frame? Or column? single <- ABCD_clean[ABCD_clean$rel_relationship==0,]
#To see what's in that column: max(single$rel_relationship)
#To make a new column?? ABCD_clean$newcolumn <- 1
#To see the first few rows of a column: head(ABCD_clean)
#To get correlation coefficient and p-values: stat_cor()