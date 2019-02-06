library(dplyr)
library(corrplot)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(effsize)
library(rmatio)
library(RColorBrewer)
library(tidyr)
library(gridExtra)

rm(list=ls())

dataDir   <-"/Users/monica/Documents/Projects/ABCD/WM_paper/Data/ABCDStudyNDA/"
outputDir <-"/Users/monica/Documents/Projects/ABCD/WM_paper/Data/"

mysum  <- function(x)sum(x,na.rm = any(!is.na(x)))
mymean <- function(x)mean(x,na.rm = any(!is.na(x)))

######### Read files into R #########
Demographics <- read.delim(paste(dataDir,"abcddemo01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
Screener     <- read.delim(paste(dataDir,"abcd_screen01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
RAChecklist  <- read.delim(paste(dataDir,"abcd_ra01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
ScannerID    <- read.delim(paste(dataDir,"abcd_mri01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
SiteID       <- read.delim(paste(dataDir,"abcd_lt01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
Family       <- read.delim(paste(dataDir,"acspsw02.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)

NIH_toolbox  <- read.delim(paste(dataDir,"abcd_tbss01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
Pearson      <- read.delim(paste(dataDir,"abcd_ps01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
CashChoice   <- read.delim(paste(dataDir,"cct01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
LittleMan    <- read.delim(paste(dataDir,"lmtp201.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)

Nback        <- read.delim(paste(dataDir,"abcd_mrinback02.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
RecMem       <- read.delim(paste(dataDir,"mribrec02.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
SST          <- read.delim(paste(dataDir,"abcd_sst02.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
MID          <- read.delim(paste(dataDir,"abcd_mid02.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)

######### Subset variables #########
Demographics <- unique(subset(Demographics, select = c(subjectkey, interview_age, gender)))
Screener     <- unique(subset(Screener, select = c(subjectkey, scrn_asd, scrn_medcond_other, scrn_epls, scrn_seizure)))
RAChecklist  <- unique(subset(RAChecklist, select = c(subjectkey, ra_scan_check_list_rcom, ra_scan_cl_mid_scan_lap, ra_scan_check_list_vemorc, ra_scan_cl_nbac_scan_lap, ra_scan_check_list_sstrc, ra_scan_cl_sst_scan_lap)))
ScannerID    <- unique(subset(ScannerID, select = c(subjectkey, mri_info_deviceserialnumber)))
SiteID       <- unique(subset(SiteID, select = c(subjectkey, site_id_l)))
Family       <- unique(subset(Family, select = c(subjectkey, rel_relationship, rel_family_id)))

NIH_toolbox  <- unique(subset(NIH_toolbox, select = c(subjectkey, nihtbx_picvocab_uncorrected, nihtbx_flanker_uncorrected, nihtbx_list_uncorrected, nihtbx_cardsort_uncorrected, nihtbx_pattern_uncorrected, nihtbx_picture_uncorrected, nihtbx_reading_uncorrected, nihtbx_fluidcomp_uncorrected, nihtbx_cryst_uncorrected, nihtbx_totalcomp_uncorrected)))
Pearson      <- unique(subset(Pearson, select = c(subjectkey, pea_wiscv_tss, pea_ravlt_sd_trial_i_tc, pea_ravlt_sd_trial_ii_tc, pea_ravlt_sd_trial_iii_tc, pea_ravlt_sd_trial_iv_tc, pea_ravlt_sd_trial_v_tc, pea_ravlt_sd_trial_i_tr, pea_ravlt_sd_trial_ii_tr, pea_ravlt_sd_trial_iii_tr, pea_ravlt_sd_trial_iv_tr, pea_ravlt_sd_trial_v_tr, pea_ravlt_sd_trial_i_ti, pea_ravlt_sd_trial_ii_ti, pea_ravlt_sd_trial_iii_ti, pea_ravlt_sd_trial_iv_ti, pea_ravlt_sd_trial_v_ti, pea_ravlt_sd_listb_tc, pea_ravlt_sd_listb_tr, pea_ravlt_sd_listb_ti, pea_ravlt_sd_trial_vi_tc, pea_ravlt_sd_trial_vi_tr, pea_ravlt_sd_trial_vi_ti, pea_ravlt_ld_trial_vii_tc, pea_ravlt_ld_trial_vii_tr, pea_ravlt_ld_trial_vii_ti)))
CashChoice   <- unique(subset(CashChoice, select = c(subjectkey, cash_choice_task)))
LittleMan    <- unique(subset(LittleMan, select = c(subjectkey, lmt_scr_efficiency, lmt_scr_perc_correct, lmt_scr_rt_correct)))

Nback        <- unique(subset(Nback, select = c(subjectkey, tfmri_nback_beh_switchflag, tfmri_nback_beh_performflag, tfmri_nb_all_beh_ctotal_mrt, tfmri_nb_all_beh_ctotal_stdrt, tfmri_nb_all_beh_c0b_rate, tfmri_nb_all_beh_c0bnf_rate, tfmri_nb_all_beh_c0bngf_rate, tfmri_nb_all_beh_c0bp_rate, tfmri_nb_all_beh_c0bpf_rate, tfmri_nb_all_beh_c2b_rate, tfmri_nb_all_beh_c2bnf_rate, tfmri_nb_all_beh_c2bngf_rate, tfmri_nb_all_beh_c2bp_rate, tfmri_nb_all_beh_c2bpf_rate, tfmri_nb_all_beh_cnf_rate, tfmri_nb_all_beh_cngf_rate, tfmri_nb_all_beh_cpf_rate, tfmri_nb_all_beh_cplace_rate, tfmri_nb_all_beh_ctotal_rate)))
RecMem       <- unique(subset(RecMem, select = c(subjectkey, tfmri_rec_beh_switchflag, tfmri_rec_all_beh_posface_br, tfmri_rec_all_beh_posf_dpr, tfmri_rec_all_beh_neutface_br, tfmri_rec_all_beh_neutf_dp, tfmri_rec_all_beh_negface_br, tfmri_rec_all_beh_negf_dp, tfmri_rec_all_beh_place_br, tfmri_rec_all_beh_place_dp)))
SST          <- unique(subset(SST, select = c(subjectkey, tfmri_sst_beh_switchflag, tfmri_sst_beh_performflag, tfmri_sst_all_beh_crgo_rt, tfmri_sst_all_beh_crgo_mrt, tfmri_sst_all_beh_crgo_stdrt, tfmri_sst_all_beh_crlg_rt, tfmri_sst_all_beh_incrgo_rt, tfmri_sst_all_beh_incrlg_rt, tfmri_sst_all_beh_nrgo_rt, tfmri_sst_all_beh_crs_rt, tfmri_sst_all_beh_incrs_rt, tfmri_sst_all_beh_ssds_rt, tfmri_sst_all_beh_tot_mssd, tfmri_sst_all_beh_total_meanrt)))
MID          <- unique(subset(MID, select = c(subjectkey, tfmri_mid_beh_switchflag, tfmri_mid_beh_performflag, tfmri_mid_all_beh_srwpfb_rate, tfmri_mid_all_beh_lrwpfb_rate, tfmri_mid_all_beh_slpfb_rate, tfmri_mid_all_beh_llpfb_rate, tfmri_mid_all_beh_ntpfb_rate, tfmri_mid_all_beh_t_earnings)))

######### Convert to numeric #########
Demographics[, 2]                  <- sapply(Demographics[, 2], as.numeric)
Screener[,2:ncol(Screener)]        <- sapply(Screener[, 2:ncol(Screener)], as.numeric)
Family[,2:ncol(Family)]            <- sapply(Family[, 2:ncol(Family)], as.numeric)
NIH_toolbox[, 2:ncol(NIH_toolbox)] <- sapply(NIH_toolbox[, 2:ncol(NIH_toolbox)], as.numeric)
Pearson[, 2:ncol(Pearson)]         <- sapply(Pearson[, 2:ncol(Pearson)], as.numeric)
CashChoice[, 2]                    <- sapply(CashChoice[, 2], as.numeric)
LittleMan[, 2:ncol(LittleMan)]     <- sapply(LittleMan[, 2:ncol(LittleMan)], as.numeric)
Nback[, 4:ncol(Nback)]             <- sapply(Nback[, 4:ncol(Nback)], as.numeric)
RecMem[, 3:ncol(RecMem)]           <- sapply(RecMem[, 3:ncol(RecMem)], as.numeric)
SST[, 4:ncol(SST)]                 <- sapply(SST[, 4:ncol(SST)], as.numeric)
MID[, 4:ncol(MID)]                 <- sapply(MID[, 4:ncol(MID)], as.numeric)

######### Add performance measure columns #########
Pearson$pea_ravlt_sd_trial_itov_tc <- apply(Pearson[c('pea_ravlt_sd_trial_i_tc', 'pea_ravlt_sd_trial_ii_tc', 'pea_ravlt_sd_trial_iii_tc', 'pea_ravlt_sd_trial_iv_tc', 'pea_ravlt_sd_trial_v_tc')], 1, mysum)
Pearson$pea_ravlt_sd_trial_itov_tr <- apply(Pearson[c('pea_ravlt_sd_trial_i_tr', 'pea_ravlt_sd_trial_ii_tr', 'pea_ravlt_sd_trial_iii_tr', 'pea_ravlt_sd_trial_iv_tr', 'pea_ravlt_sd_trial_v_tr')], 1, mysum)
Pearson$pea_ravlt_sd_trial_itov_ti <- apply(Pearson[c('pea_ravlt_sd_trial_i_ti', 'pea_ravlt_sd_trial_ii_ti', 'pea_ravlt_sd_trial_iii_ti', 'pea_ravlt_sd_trial_iv_ti', 'pea_ravlt_sd_trial_v_ti')], 1, mysum)
Pearson$pea_ravlt_tc               <- apply(Pearson[c('pea_ravlt_sd_trial_i_tc', 'pea_ravlt_sd_trial_ii_tc', 'pea_ravlt_sd_trial_iii_tc', 'pea_ravlt_sd_trial_iv_tc', 'pea_ravlt_sd_trial_v_tc', 'pea_ravlt_sd_listb_tc', 'pea_ravlt_sd_trial_vi_tc', 'pea_ravlt_ld_trial_vii_tc')], 1, mysum)
RecMem$overall_dprime              <- apply(RecMem[c('tfmri_rec_all_beh_posf_dpr', 'tfmri_rec_all_beh_neutf_dp', 'tfmri_rec_all_beh_negf_dp', 'tfmri_rec_all_beh_place_dp')], 1, mymean)

######### Invert SSRT #########
SST$tfmri_sst_all_beh_total_meanrt <- SST$tfmri_sst_all_beh_total_meanrt*-1

######### Remove cash choice option 3 ("don't know") #########
CashChoice$cash_choice_task[CashChoice$cash_choice_task == 3] <- NA

######### Merge, clean, crop data #########
data.merge <- Reduce(function(x,y) merge(x = x, y = y, by = "subjectkey", all.x = TRUE, all.y = TRUE), list(Demographics, Screener, RAChecklist, ScannerID, SiteID, Family, NIH_toolbox, Pearson, CashChoice, LittleMan, Nback, RecMem, SST, MID))
data.crop  <- data.merge[ which(data.merge$scrn_asd==0 & (data.merge$scrn_epls!=1 | is.na(data.merge$scrn_epls))), ]
#data.crop  <- subset(data.crop, select = c(rel_family_id, site_id_l, ra_scan_cl_mid_scan_lap, ra_scan_cl_nbac_scan_lap, ra_scan_cl_sst_scan_lap, interview_age, gender, nihtbx_list_uncorrected, nihtbx_picvocab_uncorrected, nihtbx_flanker_uncorrected, nihtbx_cardsort_uncorrected, nihtbx_pattern_uncorrected, nihtbx_picture_uncorrected, nihtbx_reading_uncorrected, pea_wiscv_tss, pea_ravlt_sd_trial_vi_tc, pea_ravlt_ld_trial_vii_tc,  cash_choice_task, lmt_scr_efficiency, tfmri_nb_all_beh_c0b_rate, tfmri_nb_all_beh_c2b_rate, overall_dprime, tfmri_sst_all_beh_total_meanrt, tfmri_mid_all_beh_t_earnings))
data.crop  <- subset(data.crop, select = c(rel_family_id, site_id_l, ra_scan_cl_mid_scan_lap, ra_scan_cl_nbac_scan_lap, ra_scan_cl_sst_scan_lap, interview_age, gender, tfmri_nback_beh_performflag, tfmri_sst_beh_performflag, tfmri_mid_beh_performflag, nihtbx_list_uncorrected, nihtbx_reading_uncorrected, nihtbx_picvocab_uncorrected, tfmri_nb_all_beh_c2b_rate, pea_ravlt_sd_trial_vi_tc, pea_wiscv_tss, pea_ravlt_ld_trial_vii_tc, nihtbx_picture_uncorrected, tfmri_nb_all_beh_c0b_rate, nihtbx_cardsort_uncorrected, nihtbx_flanker_uncorrected, lmt_scr_efficiency, overall_dprime, nihtbx_pattern_uncorrected, tfmri_mid_all_beh_t_earnings, tfmri_sst_all_beh_total_meanrt, cash_choice_task))
#write.csv(data.crop,file=paste(outputDir,"data.crop.csv",sep=""))
start_col <- 11

######### Get complete cases #########
data.comp <- data.crop[which(rowSums(is.na(data.crop[,start_col:ncol(data.crop)]))==0),]

######### Exclude outliers #########
sd_thresh <- 2.5
data.excl <- data.crop

for (i in start_col:ncol(data.excl)) {
  tmp_mean <- mean(data.excl[,i], na.rm=TRUE)
  tmp_sd   <- sd(data.excl[,i], na.rm=TRUE)
  data.excl[which((data.excl[,i]<=(tmp_mean - sd_thresh*tmp_sd) | data.excl[,i]>=(tmp_mean + sd_thresh*tmp_sd))), i]<-NA
  rm(tmp_mean, tmp_sd)
}

######### Count NAs #########
data.tmp1 <- data.crop$overall_dprime
data.tmp2 <- data.excl$overall_dprime
round(sum(is.na(data.tmp1))/length(data.tmp1)*100,digits=3)
round((sum(is.na(data.tmp2))-sum(is.na(data.tmp1)))/(length(data.tmp1)-sum(is.na(data.tmp1)))*100,digits=2)
rm(data.tmp1, data.tmp2)

######### Exclude family members #########
family_idx <- data.frame()
for (i in data.excl$rel_family_id) {
  tmp_ids <- (which(data.excl$rel_family_id %in% i))
  family_idx <- rbind(tmp_ids[1],family_idx)
  rm(tmp_ids)
}
family_idx  <- unique(family_idx)
data.family <- data.excl[family_idx$X1L, ]

######### Exclude participants who completed neuroimaging tasks outside the scanner #########
data.scan <- data.crop[ which(data.crop$ra_scan_cl_mid_scan_lap %in% 1 & data.crop$ra_scan_cl_sst_scan_lap %in% 1 & data.crop$ra_scan_cl_nbac_scan_lap %in% 1), ]

######### Exclude participants with ABCD performance flags #########
data.flag <- data.crop[ which(data.crop$tfmri_nback_beh_performflag %in% 1 & data.crop$tfmri_sst_beh_performflag %in% 1 & data.crop$tfmri_mid_beh_performflag %in% 1), ]

######### Correlation matrices #########
cormat.crop   <- cor(data.crop[,start_col:ncol(data.crop)], use="pairwise.complete.obs", method="spearman")
cormat.crop.flip <- cormat.crop*-1
corrplot(cormat.crop.flip,method="color",tl.cex=.4,tl.col = "black",diag=FALSE,addgrid.col="white")#,order="hclust")
corrplot(cormat.crop,add=TRUE, type="upper", method="number",
         diag=FALSE, tl.pos="n", cl.pos="n",col="black", 
         number.cex=.7,number.digits=2,tl.cex=.4,
         addgrid.col="white")

corrplot(cormat.crop.flip,method="color",tl.cex=.4,
         tl.col = "black",diag=FALSE,addgrid.col="white",
         tl.pos="n", cl.pos="n",addCoef.col="black",
         number.cex=.8,number.digits=2)

cormat.partial <- read.mat("/Users/monica/Documents/Projects/ABCD/WM_paper/Matlab/part.mat")
cormat.partial <- cormat.partial$part
#corrplot(cormat.partial,method="color",tl.cex=.4,tl.col = "black")
cor.test(cormat.crop[lower.tri(cormat.crop, diag = FALSE)],cormat.partial[lower.tri(cormat.partial, diag = FALSE)],method = "spearman")

cormat.excl   <- cor(data.excl[,start_col:ncol(data.excl)], use="pairwise.complete.obs", method="spearman")
#corrplot(cormat.excl,method="color",tl.cex=.4,tl.col = "black")
cor.test(cormat.crop[lower.tri(cormat.crop, diag = FALSE)],cormat.excl[lower.tri(cormat.excl, diag = FALSE)],method = "spearman")

cormat.comp   <- cor(data.comp[,start_col:ncol(data.comp)], use="pairwise.complete.obs", method="spearman")
#corrplot(cormat.comp,method="color",tl.cex=.4,tl.col = "black")
cor.test(cormat.crop[lower.tri(cormat.crop, diag = FALSE)],cormat.comp[lower.tri(cormat.comp, diag = FALSE)],method = "spearman")

cormat.scan   <- cor(data.scan[,start_col:ncol(data.scan)], use="pairwise.complete.obs", method="spearman")
#corrplot(cormat.scan,method="color",tl.cex=.4,tl.col = "black")
cor.test(cormat.crop[lower.tri(cormat.crop, diag = FALSE)],cormat.scan[lower.tri(cormat.scan, diag = FALSE)],method = "spearman")

cormat.family <- cor(data.family[,start_col:ncol(data.family)], use="pairwise.complete.obs", method="spearman")
#corrplot(cormat.family,method="color",tl.cex=.4,tl.col = "black")
cor.test(cormat.crop[lower.tri(cormat.crop, diag = FALSE)],cormat.family[lower.tri(cormat.family, diag = FALSE)],method = "spearman")

cormat.flag <- cor(data.flag[,start_col:ncol(data.family)], use="pairwise.complete.obs", method="spearman")
#corrplot(cormat.flag,method="color",tl.cex=.4,tl.col = "black")
cor.test(cormat.crop[lower.tri(cormat.crop, diag = FALSE)],cormat.flag[lower.tri(cormat.flag, diag = FALSE)],method = "spearman")

######### Site-specific correlation matrices #########
sites     <- unique(data.crop$site_id_l)
sites     <- sites[!is.na(sites)]
site_size <- matrix(0, ncol = 1, nrow = 21)
site_corr <- matrix(0, ncol = 21, nrow = 136)
count     <- 0
for (i in sites) {
  count              <- count+1
  tmp_data           <- data.crop[which(data.crop[,2]==i),start_col:ncol(data.crop)]
  site_corrmat       <- cor(tmp_data, use="pairwise.complete.obs", method="spearman")
  site_corr[,count]  <- site_corrmat[lower.tri(site_corrmat, diag = FALSE)]
  site_size[count,1] <- nrow(tmp_data)
  rm(tmp_data, site_corrmat)
}
site_xcor        <- cor(site_corr,method = "spearman")
site_xcor        <- site_xcor[1:20,1:20] # Exclude 21st site with 31 participants
diag(site_xcor)  <- NA
site_typicality  <- rowMeans(site_xcor,na.rm=TRUE)
cor.test(site_typicality,site_size[1:20],method = "spearman")
min(site_xcor[lower.tri(site_xcor, diag = FALSE)])
max(site_xcor[lower.tri(site_xcor, diag = FALSE)])
mean(site_xcor[lower.tri(site_xcor, diag = FALSE)])
sd(site_xcor[lower.tri(site_xcor, diag = FALSE)])


######### Age & gender effects #########
cor.test(data.crop$nihtbx_list_uncorrected,data.crop$interview_age,method='pearson')
t.test(data.crop$nihtbx_list_uncorrected~data.crop$gender)
cohen.d(data.crop$nihtbx_list_uncorrected~data.crop$gender)

t.test(data.crop$nihtbx_picvocab_uncorrected~data.crop$gender)
t.test(data.crop$nihtbx_flanker_uncorrected~data.crop$gender)
t.test(data.crop$nihtbx_cardsort_uncorrected~data.crop$gender)
t.test(data.crop$nihtbx_pattern_uncorrected~data.crop$gender)
t.test(data.crop$nihtbx_picture_uncorrected~data.crop$gender)
t.test(data.crop$nihtbx_reading_uncorrected~data.crop$gender)
t.test(data.crop$pea_wiscv_tss~data.crop$gender)
t.test(data.crop$pea_ravlt_sd_trial_vi_tc~data.crop$gender)
t.test(data.crop$pea_ravlt_ld_trial_vii_tc~data.crop$gender)
t.test(data.crop$lmt_scr_efficiency~data.crop$gender)
t.test(data.crop$cash_choice_task~data.crop$gender)
t.test(data.crop$tfmri_nb_all_beh_c0b_rate~data.crop$gender)
t.test(data.crop$tfmri_nb_all_beh_c2b_rate~data.crop$gender)
t.test(data.crop$overall_dprime~data.crop$gender)
t.test(data.crop$tfmri_sst_all_beh_total_meanrt~data.crop$gender)
t.test(data.crop$tfmri_mid_all_beh_t_earnings~data.crop$gender)

######### Steiger's z-test #########
library(cocor)
data.steiger1 <- data.crop[which(rowSums(is.na(data.crop[,c(11,14,19)]))==0),c(11,14,19)]
cocor.dep.groups.overlap(cor(data.steiger1$tfmri_nb_all_beh_c0b_rate, data.steiger1$tfmri_nb_all_beh_c2b_rate, use="pairwise.complete.obs", method="spearman"), 
                         cor(data.steiger1$nihtbx_list_uncorrected, data.steiger1$tfmri_nb_all_beh_c0b_rate, use="pairwise.complete.obs", method="spearman"), 
                         cor(data.steiger1$nihtbx_list_uncorrected, data.steiger1$tfmri_nb_all_beh_c2b_rate, use="pairwise.complete.obs", method="spearman"), 
                         nrow(data.steiger1), alternative = "two.sided",
                         test = "steiger1980", alpha = 0.05, conf.level = 0.95, null.value = 0,
                         data.name = NULL, var.labels = NULL, return.htest = FALSE)

data.steiger2 <- data.crop[which(rowSums(is.na(data.crop[,c(11,15,17)]))==0),c(11,15,17)]
cocor.dep.groups.overlap(cor(data.steiger2$pea_ravlt_sd_trial_vi_tc, data.steiger2$pea_ravlt_ld_trial_vii_tc, use="pairwise.complete.obs", method="spearman"), 
                         cor(data.steiger2$nihtbx_list_uncorrected, data.steiger2$pea_ravlt_sd_trial_vi_tc, use="pairwise.complete.obs", method="spearman"), 
                         cor(data.steiger2$nihtbx_list_uncorrected, data.steiger2$pea_ravlt_ld_trial_vii_tc, use="pairwise.complete.obs", method="spearman"), 
                         nrow(data.steiger2), alternative = "two.sided",
                         test = "steiger1980", alpha = 0.05, conf.level = 0.95, null.value = 0,
                         data.name = NULL, var.labels = NULL, return.htest = FALSE)

######### Figure 1: Behavioral distributions #########
fig1_theme1 <- theme_minimal() + theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(size = 14), axis.title.y = element_text(colour = "black", size = 14))
fig1_theme2 <- theme_minimal() + theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(size = 14), axis.title.y = element_text(colour = "white", size = 14))
fig1_theme3 <- theme_minimal() + theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(colour = "white", size = 14), axis.title.y = element_text(colour = "white", size = 14), axis.text.x = element_text(colour="white"), axis.text.y = element_text(colour="white"))
fig1_theme4 <- theme_minimal() + theme(legend.position=c(.1,.5), legend.title = element_blank(), legend.text = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(size = 14), axis.title.y = element_text(colour = "white", size = 14))
fig1_theme5 <- theme_minimal() + theme(legend.position=c(.1,.5), legend.title = element_blank(), legend.text = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(size = 14), axis.title.y = element_text(colour = "black", size = 14))

color1 <- "#77AAAD"
color2 <- "#6E7783"
color3 <- "#D8E6E7"

p1 <- ggplot(data.crop, aes(x=nihtbx_list_uncorrected))     + geom_density(alpha = 1, fill = "white", colour = "white") + xlab("List Sorting Working Memory Test") + fig1_theme3

# NIH Toolbox measures
p2 <- ggplot(data.crop, aes(x=nihtbx_list_uncorrected))     + geom_density(alpha = 1, fill = color1, colour = "black") + xlab("List Sorting Working Memory Test") + fig1_theme1
p3 <- ggplot(data.crop, aes(x=nihtbx_picvocab_uncorrected)) + geom_density(alpha = 1, fill = color1, colour = "black") + xlab("Picture  Vocabulary Test") + fig1_theme2
p4 <- ggplot(data.crop, aes(x=nihtbx_flanker_uncorrected))  + geom_density(alpha = 1, fill = color1, colour = "black") + xlab("Flanker Test") + fig1_theme2
p5 <- ggplot(data.crop, aes(x=nihtbx_cardsort_uncorrected)) + geom_density(alpha = 1, fill = color1, colour = "black") + xlab("Dimensional Change Card Sort Test") + fig1_theme1
p6 <- ggplot(data.crop, aes(x=nihtbx_pattern_uncorrected))  + geom_density(alpha = 1, fill = color1, colour = "black") + xlab("Pattern Comparison Processing Speed Test") + fig1_theme2
p7 <- ggplot(data.crop, aes(x=nihtbx_picture_uncorrected))  + geom_density(alpha = 1, fill = color1, colour = "black") + xlab("Picture Sequence Memory Test") + fig1_theme2
p8 <- ggplot(data.crop, aes(x=nihtbx_reading_uncorrected))  + geom_density(alpha = 1, fill = color1, colour = "black") + xlab("Oral Reading Recognition Test") + fig1_theme2

# Other neurocognitive measures
p9 <- ggplot(data.crop, aes(x=pea_wiscv_tss)) + geom_density(alpha = 1, fill = color2, colour = "black") + xlab("WISC-V: Matrix reasoning scaled score") + fig1_theme1

data.reshape <- gather(data.crop, "ravlt", "score", c(15,17))
p10 <- ggplot(data.reshape, aes(x=score, fill=ravlt, color=ravlt)) + geom_density(alpha = .5) + scale_colour_manual(values=c("black", "black")) + scale_fill_manual(values=c(color2, color2)) + xlab("RAVLT: Total correct") + fig1_theme2

data.reshape <- data.crop
data.reshape$lmt_scr_efficiency <- data.reshape$lmt_scr_efficiency*10000
p11 <- ggplot(data.reshape, aes(x=lmt_scr_efficiency)) + geom_density(alpha = 1, fill = color2, colour = "black") + xlab("Little Man task: Efficiency ratio") + fig1_theme2

data.reshape <- data.crop[!is.na(data.crop$cash_choice_task),]
data.reshape$cash_choice_task[data.reshape$cash_choice_task == 1] <- "smaller-sooner"
data.reshape$cash_choice_task[data.reshape$cash_choice_task == 2] <- "larger-later"
data.reshape$cash_choice_task[data.reshape$cash_choice_task == 3] <- "don't know"
## set the levels in order we want
data.reshape <- within(data.reshape, cash_choice_task <- factor(cash_choice_task, levels=names(sort(table(cash_choice_task)))))
p12 <- ggplot(data.reshape, aes(x=cash_choice_task)) + geom_bar(alpha = 1, fill = color2, colour = "black", label = c("smaller","larger","don't know")) + xlab("Cash choice task") + fig1_theme1

# Neuroimaging task measures
data.reshape <- gather(data.crop, "enback", "score", c(19,14))
data.reshape$score <- data.reshape$score*100
enback_title <- expression(paste("Emotional ", italic("n"), "-back task: ", "Percent accuracy"))
p13 <- ggplot(data.reshape, aes(x=score, fill=enback, color=enback)) + geom_density(alpha = .5) + scale_colour_manual(values=c("black", "black")) + scale_fill_manual(values=c(color3, color3)) + xlab(enback_title) + fig1_theme1

dprime_title <- expression(paste("Emotional ", italic("n"), "-back recognition memory task: ", italic("d'")))
p14 <- ggplot(data.crop, aes(x=overall_dprime)) + geom_density(alpha = 1, fill = color3, colour = "black") + xlab(dprime_title) + fig1_theme2

data.reshape <- data.crop
data.reshape$tfmri_sst_all_beh_total_meanrt <- data.reshape$tfmri_sst_all_beh_total_meanrt*-1
p15 <- ggplot(data.reshape, aes(x=tfmri_sst_all_beh_total_meanrt)) + geom_density(alpha = 1, fill = color3, colour = "black") + xlab("Stop-signal task: Stop-signal reaction time") + fig1_theme2

p16 <- ggplot(data.crop, aes(x=tfmri_mid_all_beh_t_earnings)) + geom_density(alpha = 1, fill = color3, colour = "black") + xlab("Monetary incentive delay task: Earnings") + fig1_theme2

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, nrow = 4)
g<-arrangeGrob(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, nrow = 4)
#ggsave(file="/Users/monica/Desktop/test.pdf", g, width = 16, height = 8, units = c("in"), dpi = 300)


######### Multidimensional scaling #########
library(MASS)
data.transposed <- scale(data.comp[,start_col:ncol(data.comp)])
data.transposed <- data.frame(t(data.transposed))
d <- dist(data.transposed) # euclidean distances between the rows
mds.cmdscale       <- as.data.frame(cmdscale(as.matrix(d)))
mds.cmdscale$names <- rownames(mds.cmdscale)
mds.cmdscale$types <- c("NIH Toolbox","NIH Toolbox","NIH Toolbox",
                        "Neuroimaging","Neurocognitive","Neurocognitive",
                        "Neurocognitive","NIH Toolbox","Neuroimaging",
                        "NIH Toolbox","NIH Toolbox","Neurocognitive",
                        "Neuroimaging","NIH Toolbox","Neuroimaging",
                        "Neuroimaging","Neurocognitive")

ggplot(mds.cmdscale, aes(V1, V2, label=names)) + 
  geom_point(aes(fill=types),colour="black",pch=21, size=10) +
  labs(x="", y="") + theme_minimal() + 
  theme(legend.position="none", panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  scale_colour_manual(values=c("black", "black","black")) + 
  scale_fill_manual(values=c(color2, color3, color1)) +
  geom_text(aes(colour=factor(types)), size=2.2, 
            hjust = "center", vjust = "bottom", nudge_x = 0, nudge_y = 0.005)
