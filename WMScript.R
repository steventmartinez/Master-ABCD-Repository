library(ggplot2)
library(VennDiagram)
library(readr)
library(dplyr)
library(corrplot)
library(psych)
library(ggpubr)
library(RColorBrewer)

#To remove an environment: rm(list=ls())
#This resets the graphs environment: dev.off()
To write csv: write.csv(mydata, "mydata.csv")
#To make a new data frame? Or column? single <- ABCD_clean[ABCD_clean$rel_relationship==0,]
To see what's in that column: max(single$rel_relationship)
#To make a new column?? ABCD_clean$newcolumn <- 1
To see the first few rows of a column: head(ABCD_clean)
To get correlation coefficient and p-values: stat_cor()
To count NA values: sum(is.na(df$col))

dataDir<-"/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCDRelease/"
outputDir<-"/Users/stevenmartinez/Desktop/NeuralSignaturePaper/ABCDDataFiles/ABCDMasterFiles/ABCDRelease/output"

mysum <- function(x)sum(x,na.rm = any(!is.na(x)))
mymean <- function(x)mean(x,na.rm = any(!is.na(x)))

######### Read Files In ############
Screener<-read.delim(paste(dataDir, "abcd_screen01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)

MRINBack<-read.delim(paste(dataDir,"abcd_mrinback02.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)

RecMem<-read.delim(paste(dataDir,"mribrec02.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)

MRISST<-read.delim(paste(dataDir,"abcd_sst02.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)

MRIMID<-read.delim(paste(dataDir,"abcd_mid02.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)

RAChecklist<-read.delim(paste(dataDir,"abcd_ra01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)

Pearson<-read.delim(paste(dataDir,"abcd_ps01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)

NeuroCog<-read.delim(paste(dataDir,"abcd_tbss01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)

Demographics<-read.delim(paste(dataDir,"pdem02.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)

ScannerIDs<-read.delim(paste(dataDir,"abcd_mri01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)

Siblings<-read.delim(paste(dataDir,"acspsw02.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)

Incidental<-read.delim(paste(dataDir,"abcd_mrfindings01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)

CashChoice<-read.delim(paste(dataDir,"cct01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)

LittleMan<-read.delim(paste(dataDir,"lmtp201.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)

###### Unique To Avoid Duplicates ######
Screener     <- unique(subset(Screener, select = c(subjectkey, scrn_asd, scrn_medcond_other, scrn_epls, scrn_seizure)))

MRINBack     <- unique(subset(MRINBack, select = c(subjectkey, tfmri_nback_beh_switchflag, tfmri_nback_beh_performflag, tfmri_nb_all_beh_ctotal_mrt, tfmri_nb_all_beh_ctotal_stdrt, tfmri_nb_all_beh_c0b_rate, tfmri_nb_all_beh_c0bnf_rate, tfmri_nb_all_beh_c0bngf_rate, tfmri_nb_all_beh_c0bp_rate, tfmri_nb_all_beh_c0bpf_rate, tfmri_nb_all_beh_c2b_rate, tfmri_nb_all_beh_c2bnf_rate, tfmri_nb_all_beh_c2bngf_rate, tfmri_nb_all_beh_c2bp_rate, tfmri_nb_all_beh_c2bpf_rate, tfmri_nb_all_beh_cnf_rate, tfmri_nb_all_beh_cngf_rate, tfmri_nb_all_beh_cpf_rate, tfmri_nb_all_beh_cplace_rate, tfmri_nb_all_beh_ctotal_rate)))

RecMem       <- unique(subset(RecMem, select = c(subjectkey, tfmri_rec_beh_switchflag, tfmri_rec_all_beh_posface_br, tfmri_rec_all_beh_posf_dpr, tfmri_rec_all_beh_neutface_br, tfmri_rec_all_beh_neutf_dp, tfmri_rec_all_beh_negface_br, tfmri_rec_all_beh_negf_dp, tfmri_rec_all_beh_place_br, tfmri_rec_all_beh_place_dp)))

MRISST       <- unique(subset(MRISST, select = c(subjectkey, tfmri_sst_beh_switchflag, tfmri_sst_beh_performflag, tfmri_sst_all_beh_crgo_rt, tfmri_sst_all_beh_crgo_mrt, tfmri_sst_all_beh_crgo_stdrt, tfmri_sst_all_beh_crlg_rt, tfmri_sst_all_beh_incrgo_rt, tfmri_sst_all_beh_incrlg_rt, tfmri_sst_all_beh_nrgo_rt, tfmri_sst_all_beh_crs_rt, tfmri_sst_all_beh_incrs_rt, tfmri_sst_all_beh_ssds_rt, tfmri_sst_all_beh_tot_mssd, tfmri_sst_all_beh_total_meanrt)))

MRIMID       <- unique(subset(MRIMID, select = c(subjectkey, tfmri_mid_beh_switchflag, tfmri_mid_beh_performflag, tfmri_mid_all_beh_srwpfb_rate, tfmri_mid_all_beh_lrwpfb_rate, tfmri_mid_all_beh_slpfb_rate, tfmri_mid_all_beh_llpfb_rate, tfmri_mid_all_beh_ntpfb_rate, tfmri_mid_all_beh_t_earnings)))

RAChecklist  <- unique(subset(RAChecklist, select = c(subjectkey, ra_scan_check_list_rcom, ra_scan_cl_mid_scan_lap, ra_scan_check_list_vemorc, ra_scan_cl_nbac_scan_lap, ra_scan_check_list_sstrc, ra_scan_cl_sst_scan_lap)))

Pearson      <- unique(subset(Pearson, select = c(subjectkey, pea_wiscv_tss, pea_ravlt_sd_trial_i_tc, pea_ravlt_sd_trial_ii_tc, pea_ravlt_sd_trial_iii_tc, pea_ravlt_sd_trial_iv_tc, pea_ravlt_sd_trial_v_tc, pea_ravlt_sd_trial_i_tr, pea_ravlt_sd_trial_ii_tr, pea_ravlt_sd_trial_iii_tr, pea_ravlt_sd_trial_iv_tr, pea_ravlt_sd_trial_v_tr, pea_ravlt_sd_trial_i_ti, pea_ravlt_sd_trial_ii_ti, pea_ravlt_sd_trial_iii_ti, pea_ravlt_sd_trial_iv_ti, pea_ravlt_sd_trial_v_ti, pea_ravlt_sd_listb_tc, pea_ravlt_sd_listb_tr, pea_ravlt_sd_listb_ti, pea_ravlt_sd_trial_vi_tc, pea_ravlt_sd_trial_vi_tr, pea_ravlt_sd_trial_vi_ti, pea_ravlt_ld_trial_vii_tc, pea_ravlt_ld_trial_vii_tr, pea_ravlt_ld_trial_vii_ti)))

NeuroCog     <- unique(subset(NeuroCog, select = c(subjectkey, nihtbx_picvocab_uncorrected, nihtbx_flanker_uncorrected, nihtbx_list_uncorrected, nihtbx_cardsort_uncorrected, nihtbx_pattern_uncorrected, nihtbx_picture_uncorrected, nihtbx_reading_uncorrected, nihtbx_fluidcomp_uncorrected, nihtbx_cryst_uncorrected, nihtbx_totalcomp_uncorrected)))

Demographics <- unique(subset(Demographics, select = c(subjectkey, interview_age, gender)))

ScannerIDs    <- unique(subset(ScannerIDs, select = c(subjectkey, mri_info_deviceserialnumber)))

Siblings       <- unique(subset(Siblings, select = c(subjectkey, rel_relationship, rel_family_id)))

Incidental    <-unique(subset(Incidental, select = c(subjectkey, mrif_score)))

CashChoice   <- unique(subset(CashChoice, select = c(subjectkey, cash_choice_task)))

LittleMan    <- unique(subset(LittleMan, select = c(subjectkey, lmt_scr_efficiency, lmt_scr_perc_correct, lmt_scr_rt_correct)))

######### Numeric ##########

Demographics[, 2]                  <- sapply(Demographics[, 2], as.numeric)
NeuroCog[, 2:ncol(NeuroCog)] <- sapply(NeuroCog[, 2:ncol(NeuroCog)], as.numeric)
Pearson[, 2:ncol(Pearson)]         <- sapply(Pearson[, 2:ncol(Pearson)], as.numeric)
CashChoice[, 2]                    <- sapply(CashChoice[, 2], as.numeric)
LittleMan[, 2:ncol(LittleMan)]     <- sapply(LittleMan[, 2:ncol(LittleMan)], as.numeric)
MRINBack[, 4:ncol(MRINBack)]             <- sapply(MRINBack[, 4:ncol(MRINBack)], as.numeric)
RecMem[, 3:ncol(RecMem)]           <- sapply(RecMem[, 3:ncol(RecMem)], as.numeric)
MRISST[, 4:ncol(MRISST)]                 <- sapply(MRISST[, 4:ncol(MRISST)], as.numeric)
MRIMID[, 4:ncol(MRIMID)]                 <- sapply(MRIMID[, 4:ncol(MRIMID)], as.numeric)
Screener[, 2]                      <- sapply(Screener[, 2], as.numeric)
RAChecklist[, 2:ncol(RAChecklist)]         <- sapply(RAChecklist[, 2:ncol(RAChecklist)], as.numeric)
Siblings[,2:ncol(Siblings)]            <- sapply(Siblings[, 2:ncol(Siblings)], as.numeric)


########## RAVLT ###########

Pearson$pea_ravlt_sd_trial_itov_tc <- apply(Pearson[c('pea_ravlt_sd_trial_i_tc', 'pea_ravlt_sd_trial_ii_tc', 'pea_ravlt_sd_trial_iii_tc', 'pea_ravlt_sd_trial_iv_tc', 'pea_ravlt_sd_trial_v_tc')], 1, mysum)
Pearson$pea_ravlt_sd_trial_itov_tr <- apply(Pearson[c('pea_ravlt_sd_trial_i_tr', 'pea_ravlt_sd_trial_ii_tr', 'pea_ravlt_sd_trial_iii_tr', 'pea_ravlt_sd_trial_iv_tr', 'pea_ravlt_sd_trial_v_tr')], 1, mysum)
Pearson$pea_ravlt_sd_trial_itov_ti <- apply(Pearson[c('pea_ravlt_sd_trial_i_ti', 'pea_ravlt_sd_trial_ii_ti', 'pea_ravlt_sd_trial_iii_ti', 'pea_ravlt_sd_trial_iv_ti', 'pea_ravlt_sd_trial_v_ti')], 1, mysum)
Pearson$pea_ravlt_tc               <- apply(Pearson[c('pea_ravlt_sd_trial_i_tc', 'pea_ravlt_sd_trial_ii_tc', 'pea_ravlt_sd_trial_iii_tc', 'pea_ravlt_sd_trial_iv_tc', 'pea_ravlt_sd_trial_v_tc', 'pea_ravlt_sd_listb_tc', 'pea_ravlt_sd_trial_vi_tc', 'pea_ravlt_ld_trial_vii_tc')], 1, mysum)
RecMem$overall_dprime              <- apply(RecMem[c('tfmri_rec_all_beh_posf_dpr', 'tfmri_rec_all_beh_neutf_dp', 'tfmri_rec_all_beh_negf_dp', 'tfmri_rec_all_beh_place_dp')], 1, mymean)

####### Merging #########

ABCD.merge <- Reduce(function(x,y) merge(x = x, y = y, by = "subjectkey", all.x = TRUE, all.y = TRUE), list(Demographics, Screener, RAChecklist, ScannerIDs, Siblings, NeuroCog, Pearson, CashChoice, LittleMan, MRINBack, RecMem, MRISST, MRIMID, Incidental))
ABCD.clean <- ABCD.merge[ which(ABCD.merge$scrn_asd==0 & ABCD.merge$scrn_medcond_other==0), ]
CorrMatrix <- subset(ABCD.clean, select = c(nihtbx_picvocab_uncorrected, nihtbx_flanker_uncorrected, nihtbx_list_uncorrected, nihtbx_cardsort_uncorrected, nihtbx_pattern_uncorrected, nihtbx_picture_uncorrected, nihtbx_reading_uncorrected, pea_wiscv_tss, lmt_scr_efficiency, tfmri_nb_all_beh_c0b_rate, tfmri_nb_all_beh_c2b_rate, overall_dprime, tfmri_sst_all_beh_total_meanrt, tfmri_mid_all_beh_t_earnings))
#CorrMatrix<-rename(CorrMatrix, c("nihtbx_picvocab_uncorrected"="PictureSequenceUC, "nihtbx_flanker_uncorrected"="FlankerUC", "nihtbx_list_uncorrected"="ListSortUC", "nihtbx_cardsort_uncorrected"="CardSortUC", "nihtbx_pattern_uncorrected"="PatternSequence", "nihtbx_reading_uncorrected"="ReadingUC", "lmt_scr_efficiency"="LMT", "tfmri_nb_all_beh_c0b_rate"="0BackAccuracy", "tfmri_nb_all_beh_c2b_rate"=2BackAccuracy", "overall_dprime"="DPrime", "tfmri_sst_all_beh_total_meanrt"="SSTRT", "tfmri_mid_all_beh_t_earnings"="MIDEarnings")) )

######### +-3SD ############

sd_thresh <- 2.5
data.excl <- CorrMatrix

for (i in 1:length(data.excl)) {
tmp_mean <- mean(data.excl[,i], na.rm=TRUE)
tmp_sd   <- sd(data.excl[,i], na.rm=TRUE)
data.excl[which((data.excl[,i]<=(tmp_mean - sd_thresh*tmp_sd) | data.excl[,i]>=(tmp_mean + sd_thresh*tmp_sd))), i]<-NA
rm(tmp_mean, tmp_sd)

}

####### Exclusionary Criteria ##########

ExclNAs <- data.frame()

for (i in 1:length(data.excl)) {
n <- sum(is.na(data.excl[,i]))-sum(is.na(CorrMatrix[,i]))
ExclNAs[1,i] <- n
ExclNAs[2,i] <- n/nrow(data.excl)*100

}

ExclNAs <- data.excl[NA,]

for (i in 1:length(data.excl)) {
n <- sum(is.na(data.excl[,i]))-sum(is.na(CorrMatrix[,i]))
ExclNAs[1,i] <- n
ExclNAs[2,i] <- n/nrow(data.excl)*100
ExclNAs <- ExclNAs %>% slice (1:2)

}


ExclNAs$MedCondOther <- sum(ABCD.merge$scrn_medcond_other==1, na.rm=TRUE)
ExclNAs$MedCondOther[2] <- sum(ABCD.merge$scrn_medcond_other==1, na.rm=TRUE)/length(ABCD.merge$scrn_medcond_other)*100

ExclNAs$ASD <- sum(ABCD.merge$scrn_asd==1, na.rm=TRUE)
ExclNAs$ASD[2] <- sum(ABCD.merge$scrn_asd==1, na.rm=TRUE)/length(ABCD.merge$scrn_asd)*100

ExclNAs$Inc3 <- sum(ABCD.merge$mrif_score==3, na.rm=TRUE)
ExclNAs$Inc3[2] <- sum(ABCD.merge$mrif_score==3, na.rm=TRUE)/length(ABCD.merge$mrif_score)*100

ExclNAs$Inc4 <- sum(ABCD.merge$mrif_score==4, na.rm=TRUE)
ExclNAs$Inc4[2] <- sum(ABCD.merge$mrif_score==4, na.rm=TRUE)/length(ABCD.merge$mrif_score)*100

ExclNAs$BehavioralMID <- sum(ABCD.merge$ra_scan_cl_mid_scan_lap %in% 2, na.RM=TRUE)
ExclNAs$BehavioralMID[2] <- sum(ABCD.merge$ra_scan_cl_mid_scan_lap==2, na.rm=TRUE)/length(ABCD.merge$ra_scan_cl_mid_scan_lap)*100

ExclNAs$BehavioralSST <- sum(ABCD.merge$ra_scan_cl_sst_scan_lap %in% 2, na.RM=TRUE)
ExclNAs$BehavioralSST[2] <- sum(ABCD.merge$ra_scan_cl_sst_scan_lap==2, na.rm=TRUE)/length(ABCD.merge$ra_scan_cl_sst_scan_lap)*100

ExclNAs$BehavioralNBack <- sum(ABCD.merge$ra_scan_cl_nbac_scan_lap %in% 2, na.RM=TRUE)
ExclNAs$BehavioralNBack[2] <- sum(ABCD.merge$ra_scan_cl_nbac_scan_lap==2, na.rm=TRUE)/length(ABCD.merge$ra_scan_cl_nbac_scan_lap)*100

ExclNAs$Total <- rowSums(ExclNAs)
ExclNAs$Total[2] <- 0

ExclNAs$Missing <- sum(is.na(data.excl[,i]))
ExclNAs$Missing[2] <- sum(is.na(data.excl[,i]))/nrow(data.excl)*100

out-of-scanner task, people who did it on laptop
familyIDs
No missing data at all, on all those measures
total

######### Exclude family members #########
family_idx <- data.frame()

for (i in Siblings$rel_family_id) {
tmp_ids <- (which(Siblings$rel_family_id %in% i))
family_idx <- rbind(tmp_ids[1],family_idx)
rm(tmp_ids)

}

data.family  <- unique(subset(family_idx, select = c (X1L)))


########## Correlation Matrix ########
data.excl<-cor(data.excl, use="pairwise.complete.obs", method="pearson") 
corrplot(data.excl, method="color") #,order="hclust"



#####################################################################################################
corrplot(CorrMatrix, col=colorRampPalette(c("blue","white","red"))(200))
#Compare these with the original one
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(CorrMatrix, type="full")
corrplot(Matrix, type="upper", order="hclust", tl.col="black", tl.srt=1)  

cormat.neurcog.clean<-cor(data.excl, use="pairwise.complete.obs", method="spearman")
corrplot(cormat.neurcog.clean,method="color",tl.cex=.4,tl.col = "black")  #,order="hclust"

###### Standard Error #######
describe(df$variable, type=2)

########## Histograms ##########
qplot(ABCD_clean$TwoBackMean, geom="histogram", main="2 Back Accuracy", xlab="2BackMean", fill=I("green"), col=I("blue"), alpha=I(.2))
qplot(ABCD_clean$ZeroBackMean, geom="histogram", main="0 Back Accuracy", xlab="0BackMean", fill=I("green"), col=I("blue"), alpha=I(.2))
qplot(ABCD_clean$TotalAccMean, geom="histogram", main="Total Accuracy", xlab="TotalAccMean", fill=I("green"), col=I("blue"), alpha=I(.2)) + stat_cor()
qplot(ABCD_clean$DPrimeMean, geom="histogram", main="DPrime", xlab="DPrimeMean", fill=I("green"), col=I("blue"), alpha=I(.2))
qplot(ABCD_clean$rel_relationship, geom="histogram", main="Siblings", xlab="Siblings", fill=I("green"), col=I("blue"), alpha=I(.2))

######## Scatterplots #######
ggplot(data.excl, aes(x=tfmri_nb_all_beh_c2b_rate, y=overall_dprime)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred") + stat_cor()
ggplot(ABCD_clean, aes(x=ZeroBackMean, y=DPrimeMean)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred") + stat_cor()
ggplot(ABCD_clean, aes(x=TotalAccMean, y=DPrimeMean)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred") + stat_cor()

####### Box Plots ###############
ggplot(data.excl, aes(x=tfmri_nb_all_beh_c2b_rate, y=overall_dprime)) + geom_boxplot(outlier.colour="red", outlier.shape = 8, outlier.size =4)

########## VennDiagrams ###########

NBackVennPlot <- draw.pairwise.venn(663, 68, 68, c("NBackFlag", "NBackSD"), fill=rainbow(2))
MIDVennPlot <- draw.pairwise.venn(451, 41, 20, c("MIDFlag", "MIDSD"), fill=rainbow(2))
SSTVennPlot <- draw.pairwise.venn(568, 46, 30, c("SSTFlag", "SSTSD"), fill=rainbow(2))

#NBack
NBackFlag <- ABCD[which(ABCD$tfmri_nback_beh_performflag==0),]
NBackFlag <- NBackFlag %>% select(subjectkey)
NBackSD <- ABCD[which(ABCD$TotalAccMean<=(mean(ABCD$TotalAccMean, na.rm=TRUE)-3*sd(ABCD$TotalAccMean, na.rm=TRUE)) | ABCD$TotalAccMean>=(mean(ABCD$TotalAccMean, na.rm=TRUE)+3*sd(ABCD$TotalAccMean, na.rm=TRUE))), ]
NBackSD <- NBackSD %>% select(subjectkey)
NBackLower<- ABCD[which(ABCD$TotalAccMean<=(mean(ABCD$TotalAccMean, na.rm=TRUE)-3*sd(ABCD$TotalAccMean, na.rm=TRUE))), ]
NBackLower <- NBackLower %>% select(subjectkey)

#MID
MIDFlag <- ABCD[which(ABCD$tfmri_mid_beh_performflag==0),]
MIDFlag <- MIDFlag %>% select(subjectkey)
MIDSD <- ABCD[which(ABCD$TotalMIDMean<=(mean(ABCD$TotalMIDMean, na.rm=TRUE)-3*sd(ABCD$TotalMIDMean, na.rm=TRUE)) | ABCD$TotalMIDMean>=(mean(ABCD$TotalMIDMean, na.rm=TRUE)+3*sd(ABCD$TotalMIDMean, na.rm=TRUE))), ]
MIDSD <- MIDSD %>% select(subjectkey)
MIDLower <- ABCD[which(ABCD$TotalMIDMean<=(mean(ABCD$TotalMIDMean, na.rm=TRUE)-3*sd(ABCD$TotalMIDMean, na.rm=TRUE))), ]

#SST
SSTFlag <- ABCD[which(ABCD$tfmri_sst_beh_performflag==0),]
SSTFlag <- SSTFlag %>% select(subjectkey)
SSTSD <- ABCD[which(ABCD$TotalSSTMean<=(mean(ABCD$TotalSSTMean, na.rm=TRUE)-3*sd(ABCD$TotalSSTMean, na.rm=TRUE)) | ABCD$TotalSSTMean>=(mean(ABCD$TotalSSTMean, na.rm=TRUE)+3*sd(ABCD$TotalSSTMean, na.rm=TRUE))), ]
SSTSD <- SSTSD %>% select(subjectkey)
SSTLower <- ABCD[which(ABCD$TotalSSTMean<=(mean(ABCD$TotalSSTMean, na.rm=TRUE)-3*sd(ABCD$TotalSSTMean, na.rm=TRUE))), ]
SSTHigher <- ABCD[which(ABCD$TotalSSTMean>=(mean(ABCD$TotalSSTMean, na.rm=TRUE)+3*sd(ABCD$TotalSSTMean, na.rm=TRUE))), ]

inner_join(NBackSD,NBackFlag)
anti_join(NBackFlag,NBackSD)

inner_join(MIDSD,MIDFlag)
anti_join(MIDFlag,MIDSD)

inner_join(SSTSD,SSTFlag)
anti_join(SSTFlag,SSTSD)

unique(c(NBackFlag$subjectkey, NBackSD$subjectkey))

############################################################################################################################
#Principal Component Analysis
ABCD.neurcog.pca <- prcomp(na.omit(ABCD.neurcog.clean), center = TRUE,scale = TRUE)
plot(ABCD.neurcog.pca)
corrplot(ABCD.neurcog.pca[["rotation"]][,1:10],method="color",tl.cex=.4,tl.col = "black")
summary(ABCD.neurcog.pca)

#BayesianPCA

########### As Numeric #########
ABCD$DPrimeMean<-rowMeans(cbind(as.numeric(ABCD$tfmri_rec_all_beh_negf_dp),as.numeric(ABCD$beh_rec_posface_dprime),as.numeric(ABCD$tfmri_rec_all_beh_neutf_dp),as.numeric(ABCD$beh_rec_place_dprime)),na.rm=TRUE, dims=1)
ABCD$DPrimeMean[is.nan(ABCD$DPrimeMean)]<-NA
ABCD$DPrimeMean<-as.numeric(ABCD$DPrimeMean)

ABCD$ZeroBackMean<-rowMeans(cbind(as.numeric(ABCD$tfmri_nb_all_beh_c0b_rate)),na.rm=TRUE, dims=1)
ABCD$ZeroBackMean[is.nan(ABCD$ZeroBackMean)]<-NA
ABCD$ZeroBackMean<-as.numeric(ABCD$ZeroBackMean)

ABCD_clean <- ABCD[which(ABCD$scrn_asd==0 & ABCD$scrn_medcond_other==0 & ABCD$DPrimeMean>=(mean(ABCD$DPrimeMean, na.rm=TRUE) - 3*sd(ABCD$DPrimeMean, na.rm=TRUE)) & ABCD$DPrimeMean<=(mean(ABCD$DPrimeMean, na.rm=TRUE) + 3*sd(ABCD$DPrimeMean, na.rm=TRUE)) & ABCD$ZeroBackMean>=(mean(ABCD$ZeroBackMean, na.rm=TRUE)-3*sd(ABCD$ZeroBackMean, na.rm=TRUE)) & ABCD$ZeroBackMean<=(mean(ABCD$ZeroBackMean, na.rm=TRUE)+3*sd(ABCD$ZeroBackMean, na.rm=TRUE)) & ABCD$TwoBackMean>=(mean(ABCD$TwoBackMean, na.rm=TRUE)-3*sd(ABCD$TwoBackMean, na.rm=TRUE)) & ABCD$TwoBackMean<=(mean(ABCD$TwoBackMean, na.rm=TRUE)+3*sd(ABCD$TwoBackMean, na.rm=TRUE))), ]
View(ABCD_clean)
