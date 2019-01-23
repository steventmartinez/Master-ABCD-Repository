library(dplyr)
library(corrplot)
library(ggplot2)

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
data.crop  <- subset(data.crop, select = c(rel_family_id, site_id_l, ra_scan_cl_mid_scan_lap, ra_scan_cl_nbac_scan_lap, ra_scan_cl_sst_scan_lap, nihtbx_list_uncorrected, nihtbx_reading_uncorrected, nihtbx_picvocab_uncorrected, tfmri_nb_all_beh_c2b_rate, pea_ravlt_sd_trial_vi_tc, pea_wiscv_tss, pea_ravlt_ld_trial_vii_tc, nihtbx_picture_uncorrected, tfmri_nb_all_beh_c0b_rate, nihtbx_cardsort_uncorrected, nihtbx_flanker_uncorrected, lmt_scr_efficiency, overall_dprime, nihtbx_pattern_uncorrected, tfmri_mid_all_beh_t_earnings, tfmri_sst_all_beh_total_meanrt, cash_choice_task))
write.csv(data.crop,file=paste(outputDir,"data.crop.csv",sep=""))

######### Exclude outliers #########
sd_thresh <- 2.5
data.excl <- data.crop

for (i in 6:ncol(data.excl)) {
    tmp_mean <- mean(data.excl[,i], na.rm=TRUE)
    tmp_sd   <- sd(data.excl[,i], na.rm=TRUE)
    data.excl[which((data.excl[,i]<=(tmp_mean - sd_thresh*tmp_sd) | data.excl[,i]>=(tmp_mean + sd_thresh*tmp_sd))), i]<-NA
    rm(tmp_mean, tmp_sd)
}

######### Count NAs #########
data.tmp1 <- data.crop$nihtbx_list_uncorrected
data.tmp2 <- data.excl$nihtbx_list_uncorrected
round(sum(is.na(data.tmp1))/length(data.tmp1)*100,digits=2)
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

######### Correlation matrices #########
cormat.crop   <- cor(data.crop[,6:ncol(data.crop)], use="pairwise.complete.obs", method="spearman")
corrplot(cormat.crop,method="color",tl.cex=.4,tl.col = "black")#,order="hclust")

cormat.excl   <- cor(data.excl[,6:ncol(data.excl)], use="pairwise.complete.obs", method="spearman")
corrplot(cormat.excl,method="color",tl.cex=.4,tl.col = "black")#,order="hclust")
cor.test(cormat.crop[lower.tri(cormat.crop, diag = FALSE)],cormat.excl[lower.tri(cormat.excl, diag = FALSE)],method = "spearman")

cormat.family <- cor(data.family[,6:ncol(data.family)], use="pairwise.complete.obs", method="spearman")
corrplot(cormat.family,method="color",tl.cex=.4,tl.col = "black")#,order="hclust")
cor.test(cormat.crop[lower.tri(cormat.crop, diag = FALSE)],cormat.family[lower.tri(cormat.family, diag = FALSE)],method = "spearman")

cormat.scan   <- cor(data.scan[,6:ncol(data.scan)], use="pairwise.complete.obs", method="spearman")
corrplot(cormat.scan,method="color",tl.cex=.4,tl.col = "black")#,order="hclust")
cor.test(cormat.crop[lower.tri(cormat.crop, diag = FALSE)],cormat.scan[lower.tri(cormat.scan, diag = FALSE)],method = "spearman")

######### Site-specific correlation matrices #########
sites     <- unique(data.crop$site_id_l)
sites     <- sites[!is.na(sites)]
site_size <- matrix(0, ncol = 1, nrow = 21)
site_corr <- matrix(0, ncol = 21, nrow = 136)
count     <- 0

for (i in sites) {
    count              <- count+1
    tmp_data           <- data.crop[which(data.crop[,2]==i),6:ncol(data.crop)]
    site_corrmat       <- cor(tmp_data, use="pairwise.complete.obs", method="spearman")
    site_corr[,count]  <- site_corrmat[lower.tri(site_corrmat, diag = FALSE)]
    site_size[count,1] <- nrow(tmp_data)
    rm(tmp_data, site_corrmat)
}

site_xcor <- cor(site_corr,method = "spearman")
site_xcor <- site_xcor[1:20,1:20] # Exclude 21st site with 31 participants
min(site_xcor[lower.tri(site_xcor, diag = FALSE)])
max(site_xcor[lower.tri(site_xcor, diag = FALSE)])
mean(site_xcor[lower.tri(site_xcor, diag = FALSE)])
sd(site_xcor[lower.tri(site_xcor, diag = FALSE)])

######### Visualization #########
data.vis <- data.family
#t.test(data.vis$nihtbx_list_uncorrected~data.vis$cash_choice_task)

ggplot(data.vis[,2:ncol(data.vis)], aes(x=cash_choice_task, y=nihtbx_picvocab_uncorrected)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred")
ggplot(data.vis[,2:ncol(data.vis)], aes(x=cash_choice_task, y=nihtbx_picture_uncorrected)) + geom_point(color="blue") + geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred")


