library(dplyr)
library(corrplot)
library(ggplot2)

rm(list=ls())

dataDir   <-"/Users/monica/Documents/Projects/ABCD/WM_paper/Data/ABCDStudyNDA/"
outputDir <-"/Users/monica/Documents/Projects/ABCD/WM_paper/Data/ABCDStudyNDA/"

mysum  <- function(x)sum(x,na.rm = any(!is.na(x)))
mymean <- function(x)mean(x,na.rm = any(!is.na(x)))

######### Read files into R #########
Demographics <- read.delim(paste(dataDir,"abcddemo01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
Screener     <- read.delim(paste(dataDir,"abcd_screen01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
RAChecklist  <- read.delim(paste(dataDir,"abcd_ra01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
ScannerID    <- read.delim(paste(dataDir,"abcd_mri01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
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
Family       <- unique(subset(Family, select = c(subjectkey, rel_relationship)))

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
NIH_toolbox[, 2:ncol(NIH_toolbox)] <- sapply(NIH_toolbox[, 2:ncol(NIH_toolbox)], as.numeric)
Pearson[, 2:ncol(Pearson)]         <- sapply(Pearson[, 2:ncol(Pearson)], as.numeric)
CashChoice[, 2]                    <- sapply(CashChoice[, 2], as.numeric)
LittleMan[, 2:ncol(LittleMan)]     <- sapply(LittleMan[, 2:ncol(LittleMan)], as.numeric)
Nback[, 4:ncol(Nback)]             <- sapply(Nback[, 4:ncol(Nback)], as.numeric)
RecMem[, 3:ncol(RecMem)]           <- sapply(RecMem[, 3:ncol(RecMem)], as.numeric)
SST[, 4:ncol(SST)]                 <- sapply(SST[, 4:ncol(SST)], as.numeric)
MID[, 4:ncol(MID)]                 <- sapply(MID[, 4:ncol(MID)], as.numeric)

######### Add columns #########
Pearson$pea_ravlt_sd_trial_itov_tc <- apply(Pearson[c('pea_ravlt_sd_trial_i_tc', 'pea_ravlt_sd_trial_ii_tc', 'pea_ravlt_sd_trial_iii_tc', 'pea_ravlt_sd_trial_iv_tc', 'pea_ravlt_sd_trial_v_tc')], 1, mysum)
Pearson$pea_ravlt_sd_trial_itov_tr <- apply(Pearson[c('pea_ravlt_sd_trial_i_tr', 'pea_ravlt_sd_trial_ii_tr', 'pea_ravlt_sd_trial_iii_tr', 'pea_ravlt_sd_trial_iv_tr', 'pea_ravlt_sd_trial_v_tr')], 1, mysum)
Pearson$pea_ravlt_sd_trial_itov_ti <- apply(Pearson[c('pea_ravlt_sd_trial_i_ti', 'pea_ravlt_sd_trial_ii_ti', 'pea_ravlt_sd_trial_iii_ti', 'pea_ravlt_sd_trial_iv_ti', 'pea_ravlt_sd_trial_v_ti')], 1, mysum)
Pearson$pea_ravlt_tc               <- apply(Pearson[c('pea_ravlt_sd_trial_i_tc', 'pea_ravlt_sd_trial_ii_tc', 'pea_ravlt_sd_trial_iii_tc', 'pea_ravlt_sd_trial_iv_tc', 'pea_ravlt_sd_trial_v_tc', 'pea_ravlt_sd_listb_tc', 'pea_ravlt_sd_trial_vi_tc', 'pea_ravlt_ld_trial_vii_tc')], 1, mysum)
RecMem$overall_dprime              <- apply(RecMem[c('tfmri_rec_all_beh_posf_dpr', 'tfmri_rec_all_beh_neutf_dp', 'tfmri_rec_all_beh_negf_dp', 'tfmri_rec_all_beh_place_dp')], 1, mymean)

######### Merge, clean, crop data #########
data.merge <- Reduce(function(x,y) merge(x = x, y = y, by = "subjectkey", all.x = TRUE, all.y = TRUE), list(Demographics, Screener, RAChecklist, ScannerID, Family, NIH_toolbox, Pearson, CashChoice, LittleMan, Nback, RecMem, SST, MID))
data.clean <- data.merge[ which(data.merge$scrn_asd==0 & data.merge$scrn_medcond_other==0), ]
data.crop  <- subset(data.clean, select = c(nihtbx_picvocab_uncorrected, nihtbx_flanker_uncorrected, nihtbx_list_uncorrected, nihtbx_cardsort_uncorrected, nihtbx_pattern_uncorrected, nihtbx_picture_uncorrected, nihtbx_reading_uncorrected, pea_wiscv_tss, lmt_scr_efficiency, tfmri_nb_all_beh_c0b_rate, tfmri_nb_all_beh_c2b_rate, overall_dprime, tfmri_sst_all_beh_total_meanrt, tfmri_mid_all_beh_t_earnings))

######### Exclude outliers #########
sd_thresh <- 3
data.excl <- data.crop

for (i in 1:length(data.excl)) {
    tmp_mean <- mean(data.excl[,i], na.rm=TRUE)
    tmp_sd   <- sd(data.excl[,i], na.rm=TRUE)
    data.excl[which((data.excl[,i]<=(tmp_mean - sd_thresh*tmp_sd) | data.excl[,i]>=(tmp_mean + sd_thresh*tmp_sd))), i]<-NA
    rm(tmp_mean, tmp_sd)
}

######### Visualization #########
data.vis <- data.crop
corrplot(cor(data.vis, use="pairwise.complete.obs", method="spearman"),method="color",tl.cex=.4,tl.col = "black")#,order="hclust")
ggplot(data.vis, aes(x=tfmri_nb_all_beh_c2b_rate, y=nihtbx_list_uncorrected)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred")
cor.test(data.vis$tfmri_nb_all_beh_c2b_rate,data.vis$nihtbx_list_uncorrected,use="pairwise.complete.obs")

