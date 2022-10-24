#set working directory
setwd("/home/max/Documents/ABCD_4.0")
#setwd("/home/owensmax/ABCD_4.0")

#install.packages('dplyr') #data cleaning
#install.packages('gamm4') #mixed model
#install.packages('MuMIn') #mixed model R^2
#install.packages('DescTools') #winsorizing
library('dplyr') #data cleaning
library('gamm4') #mixed model
library('MuMIn') #mixed model R^2
library('DescTools') #winsorizing

##############load and filter data##########

#name tables to use
tables <- c('abcd_mhy02.txt', 'abcd_smrip10201.txt', 'abcd_smrip20201.txt',#upps scores, smri p1-2
            'abcd_fsurfqc01.txt', 'mriqcrp10301.txt', 'abcd_dti_p101.txt',#fs qc,mri qc, dti p1
            'abcd_dti_p201.txt', 'abcd_mri01.txt')#dti p2, scanner id

# load multiple abcd tables function
read_abcd_tables <- function(tables){
  i <- 1
  headers <- list()
  files <- list()
  
  for (t in tables){
    headers[[i]] <- read.table(t, nrows = 1, header = FALSE, stringsAsFactors = FALSE)
    files[[i]]    <- read.table(t, skip = 2, header = FALSE)
    colnames( files[[i]] ) <- unlist(headers[[i]])
    files[[i]]$dataset_id <- NULL
    
    if (i == 1){file <- files[[i]]}
    if (i > 1){
      file <- merge(file,files[[i]],by=intersect(names(file), names(files[[i]])), all.x = TRUE)
    }
    i <- i+1
  }
  return(file)
}

#load in data
data <- read_abcd_tables(tables)
famid <- read_abcd_tables('acspsw03.txt')

upps_vars <- c('upps_y_ss_negative_urgency', 'upps_y_ss_lack_of_planning',
               'upps_y_ss_sensation_seeking','upps_y_ss_positive_urgency',
               'upps_y_ss_lack_of_perseverance')

smri_vars <- readLines('smri_orig_names.txt')
smri_names <- readLines('smri_names.txt')

dmri_vars <- readLines('dmri_vars.txt')

qc_vars <- c('imgincl_t1w_include',  'imgincl_t2w_include', 'imgincl_dmri_include' )
qc <- read_abcd_tables('abcd_imgincl01.txt')
data <- merge(data, qc, by = c('src_subject_id', 'eventname', 'interview_age', 'interview_date', 'sex'))

aux_vars <- c('src_subject_id', 'eventname', 'mri_info_deviceserialnumber','interview_date', 'smri_vol_scs_intracranialv')

#data_short <- data[c(smri_vars[160])]
data_short <- data[c(aux_vars, qc_vars, upps_vars, smri_vars, dmri_vars)]
names(data_short) <- c(aux_vars, qc_vars, upps_vars, smri_names, dmri_vars)
rm(data)

#data_short$fsqc_qc[is.na(data_short$fsqc_qc)] <- 1
#data_short$iqc_dmri_good_ser[is.na(data_short$iqc_dmri_good_ser)] <- 1

#data_short <- filter(data_short, complete.cases(data_short[c(upps_vars, smri_names)]))

data_bsl <- filter(data_short, eventname == 'baseline_year_1_arm_1')
data_fu2 <- filter(data_short, eventname == '2_year_follow_up_y_arm_1')
famid <- filter(famid, eventname == 'baseline_year_1_arm_1')
data_bsl <- merge(data_bsl, famid[c('src_subject_id', 'rel_family_id')], by = 'src_subject_id')
data_fu2 <- merge(data_fu2, famid[c('src_subject_id', 'rel_family_id')], by = 'src_subject_id')

data_bsl$rel_family_id <- as.factor(data_bsl$rel_family_id)
data_bsl$mri_info_deviceserialnumber <- as.factor(data_bsl$mri_info_deviceserialnumber)
data_fu2$rel_family_id <- as.factor(data_fu2$rel_family_id)
data_fu2$mri_info_deviceserialnumber <- as.factor(data_fu2$mri_info_deviceserialnumber)

data_bsl[data_bsl == 777] <- NA
data_bsl[data_bsl == 999] <- NA
data_bsl[data_bsl == ''] <- NA
data_fu2[data_fu2 == 777] <- NA
data_fu2[data_fu2 == 999] <- NA
data_fu2[data_fu2 == ''] <- NA

data_bsl <- filter(data_bsl, complete.cases(data_bsl[c(upps_vars)]))
data_fu2 <- filter(data_fu2, complete.cases(data_fu2[c(upps_vars)]))

data_bsl <- filter(data_bsl, complete.cases(data_bsl[c(smri_names)]))
data_fu2 <- filter(data_fu2, complete.cases(data_fu2[c(smri_names)]))

#check fsqc
#filter on sMRI
data_bsl <- filter(data_bsl, imgincl_t1w_include == 1)
data_bsl <- filter(data_bsl, imgincl_t2w_include == 1)
data_fu2 <- filter(data_fu2, imgincl_t1w_include == 1)
data_fu2 <- filter(data_fu2, imgincl_t2w_include == 1)

data_bsl_dmri <- data_bsl
data_fu2_dmri <- data_fu2

data_bsl_dmri[data_bsl_dmri == 777] <- NA
data_bsl_dmri[data_bsl_dmri == 999] <- NA
data_bsl_dmri[data_bsl_dmri == ''] <- NA
data_fu2_dmri[data_fu2_dmri == 777] <- NA
data_fu2_dmri[data_fu2_dmri == 999] <- NA
data_fu2_dmri[data_fu2_dmri == ''] <- NA

data_bsl_dmri <- filter(data_bsl_dmri, complete.cases(data_bsl_dmri[c(dmri_vars)]))
data_fu2_dmri <- filter(data_fu2_dmri, complete.cases(data_fu2_dmri[c(dmri_vars)]))

data_bsl_dmri <- filter(data_bsl_dmri, imgincl_t1w_include == 1)
data_bsl_dmri <- filter(data_bsl_dmri, imgincl_t2w_include == 1)
data_bsl_dmri <- filter(data_bsl_dmri, imgincl_dmri_include == 1)
data_fu2_dmri <- filter(data_fu2_dmri, imgincl_t1w_include == 1)
data_fu2_dmri <- filter(data_fu2_dmri, imgincl_t2w_include == 1)
data_fu2_dmri <- filter(data_fu2_dmri, imgincl_dmri_include == 1)


#saveRDS(data_bsl, file = "/home/owensmax/upps_replication/smri_bsl_data.rds")
#saveRDS(data_fu2, file = "/home/owensmax/upps_replication/smri_fu2_data.rds")
#saveRDS(data_bsl_dmri, file = "/home/owensmax/upps_replication/dmri_bsl_data.rds")
#saveRDS(data_fu2_dmri, file = "/home/owensmax/upps_replication/dmri_fu2_data.rds")
saveRDS(data_bsl, file = "/home/max/Dropbox/ABCD/UPPS_replication/smri_bsl_data.rds")
saveRDS(data_fu2, file = "/home/max/Dropbox/ABCD/UPPS_replication/smri_fu2_data.rds")
saveRDS(data_bsl_dmri, file = "/home/max/Dropbox/ABCD/UPPS_replication/dmri_bsl_data.rds")
saveRDS(data_fu2_dmri, file = "/home/max/Dropbox/ABCD/UPPS_replication/dmri_fu2_data.rds")

######winsorize mri###########

data_bsl[c(smri_names)] <- as.data.frame(sapply(data_bsl[c(smri_names)],
                                               Winsorize,probs = c(0.05, 0.95),type=7))
data_fu2[c(smri_names)]<-as.data.frame(sapply(data_fu2[c(smri_names)],
                                             Winsorize,probs = c(0.05, 0.95),type=7))

data_bsl_dmri[c(dmri_vars)]<-as.data.frame(sapply(data_bsl_dmri[c(dmri_vars)],
                                                  Winsorize,probs = c(0.05, 0.95),type=7))
data_fu2_dmri[c(dmri_vars)]<-as.data.frame(sapply(data_fu2_dmri[c(dmri_vars)],
                                                  Winsorize,probs = c(0.05, 0.95),type=7))

#save winsorized values for ICC
saveRDS(data_bsl, file = "/home/max/Dropbox/ABCD/UPPS_replication/smri_bsl_data_win.rds")
saveRDS(data_fu2, file = "/home/max/Dropbox/ABCD/UPPS_replication/smri_fu2_data_win.rds")
saveRDS(data_bsl_dmri, file = "/home/max/Dropbox/ABCD/UPPS_replication/dmri_bsl_data_win.rds")
saveRDS(data_fu2_dmri, file = "/home/max/Dropbox/ABCD/UPPS_replication/dmri_fu2_data_win.rds")

######standardize mri###########
#ACTUALLY DIDN'T DO THIS IN LAST PAPER
#data_bsl[c(smri_names, 'smri_vol_scs_intracranialv')] <- as.data.frame(sapply(data_bsl[c(smri_names, 'smri_vol_scs_intracranialv')], scale))
#data_fu2[c(smri_names, 'smri_vol_scs_intracranialv')]<-as.data.frame(sapply(data_fu2[c(smri_names, 'smri_vol_scs_intracranialv')],scale))
#data_bsl_dmri[c(dmri_vars, 'smri_vol_scs_intracranialv')]<-as.data.frame(sapply(data_bsl_dmri[c(dmri_vars, 'smri_vol_scs_intracranialv')], scale))
#data_fu2_dmri[c(dmri_vars, 'smri_vol_scs_intracranialv')]<-as.data.frame(sapply(data_fu2_dmri[c(dmri_vars, 'smri_vol_scs_intracranialv')], scale))

##########################ANALYSIS#######################
#####standard abcd nested random effect####
mixed_model=function(x,y,covs,data){
  stat_holder <- data.frame()
  stat_names <- c('B','SE','t','p','R2')
  for (k in stat_names) stat_holder[k] <- as.double()
  
  form_cov_only <- formula(paste(y, "~", paste(covs, collapse="+")))
  form <- formula(paste(y, "~", x, "+", paste(covs, collapse="+")))
  model <- gamm4(form, data=data, random =~(1|mri_info_deviceserialnumber/rel_family_id) )
  model2 <- gamm4(form_cov_only, data=data, random =~(1|mri_info_deviceserialnumber/rel_family_id))
  r2_delta = round(as.numeric(r.squaredLR(model$mer,model2$mer)),5)
  sg<-summary(model$gam)
  
  for (statnum in 1:4){
    stat_holder[1,statnum]<-sg$p.table[2,statnum]
  }
  stat_holder[1,5]<-r2_delta
  return(stat_holder)
}

cleanup=function(stat_list, names){
  stat_matrix <- data.frame()
  stat_names <- c('B','SE','t','p','R2')
  for (k in stat_names) stat_matrix[k] <- as.double()
  for (i in 1:length(stat_list)){
    stat_matrix[i,] <- stat_list[[i]]
  }
  row.names(stat_matrix) <- names
  return(stat_matrix)}

data_fu2$smri_vol_subcort.aseg_cerebral.white.matter.total <- rowMeans(data_fu2[c('smri_vol_subcort.aseg_cerebral.white.matter.lh', 'smri_vol_subcort.aseg_cerebral.white.matter.rh')])
data_fu2[c('smri_vol_subcort.aseg_cerebral.white.matter.lh', 'smri_vol_subcort.aseg_cerebral.white.matter.rh')] <- NULL
smri_names <- setdiff(smri_names, c('smri_vol_subcort.aseg_cerebral.white.matter.lh', 'smri_vol_subcort.aseg_cerebral.white.matter.rh'))
omni_vars <- c("smri_vol_subcort.aseg_cerebral.white.matter.total", "smri_area_cdk_total", 
               "smri_vol_cort.desikan_total", "smri_vol_scs_subcorticalgv",
               "smri_thick_cdk_mean")

#do omnibus analyses
ICV <- 'smri_vol_scs_intracranialv'
i <- 1
stat_list_smri_omni <- list()
for (y in upps_vars){
  stat_list_smri_omni[[i]] <- lapply(omni_vars, mixed_model, y, covs=ICV, data=data_fu2)
  i = i + 1
}

#setwd("/home/owensmax/upps_replication")
saveRDS(stat_list_smri_omni, file = "smri_fu2_omni_out.rds")

write.csv(stat_list_smri_omni, "smri_fu2_omni_out.csv")

############alternate covariate analyses################
#########no icv#########
mixed_model_nocovs=function(x,y,data){
  stat_holder <- data.frame()
  stat_names <- c('B','SE','t','p','R2')
  for (k in stat_names) stat_holder[k] <- as.double()
  form <- formula(paste(y, "~", x))
  for_null <- formula(paste(y, "~", 1))
  model <- gamm4(form, data=data, random =~(1|mri_info_deviceserialnumber/rel_family_id) )
  model_null <- gamm4(for_null, data=data, random =~(1|mri_info_deviceserialnumber/rel_family_id) )
  r2_delta = round(as.numeric(r.squaredLR(model$mer,model_null$mer)),5)
  sg<-summary(model$gam)
  
  for (statnum in 1:4){
    stat_holder[1,statnum]<-sg$p.table[2,statnum]
  }
  stat_holder[1,5]<-r2_delta
  return(stat_holder)
}

stat_list_smrifu2_nocovs <- list()
stat_list_dmrifu2_nocovs <- list()
#do main analysis
i <- 1
for (y in upps_vars){
  stat_list_smrifu2_nocovs[[i]] <- lapply(setdiff(smri_names, omni_vars), mixed_model_nocovs, y, data=data_fu2)
  stat_list_dmrifu2_nocovs[[i]] <- lapply(dmri_vars, mixed_model_nocovs, y, data=data_fu2_dmri)
  i = i + 1
}

mixed_model_nocovs(smri_names[1],upps_vars[1],data_fu2)

#do omnibus analyses
i <- 1
stat_list_smri_omni_nocovs <- list()
for (y in upps_vars){
  stat_list_smri_omni_nocovs[[i]] <- lapply(omni_vars, mixed_model_nocovs, y, data=data_fu2)
  i = i + 1
}

saveRDS(stat_list_smrifu2_nocovs, file = "/home/max/Dropbox/ABCD/UPPS_replication/smri_nocovs.rds")
saveRDS(stat_list_dmrifu2_nocovs, file = "/home/max/Dropbox/ABCD/UPPS_replication/dmri_nocovs.rds")
saveRDS(stat_list_smri_omni_nocovs, file = "/home/max/Dropbox/ABCD/UPPS_replication/omni_nocovs.rds")

#stat_list_smrifu2_nocovs <- readRDS(file = "/home/max/Dropbox/ABCD/UPPS_replication/smri_nocovs.rds")
#stat_list_dmrifu2_nocovs <- readRDS(file = "/home/max/Dropbox/ABCD/UPPS_replication/dmri_nocovs.rds")
#stat_list_smri_omni_nocovs <- readRDS(file = "/home/max/Dropbox/ABCD/UPPS_replication/omni_nocovs.rds")


#cleanup
smri_fu2_out <- list()
dmri_fu2_out <- list()
smri_omni_out <- list()
for (y in 1:length(upps_vars)){
  smri_fu2_out[[y]] <-cleanup(stat_list_smrifu2_nocovs[[y]], setdiff(smri_names, omni_vars))
  dmri_fu2_out[[y]] <-cleanup(stat_list_dmrifu2_nocovs[[y]], dmri_vars)
  smri_omni_out[[y]] <-cleanup(stat_list_smri_omni_nocovs[[y]], omni_vars)
}

setwd("/home/max/Dropbox/ABCD/UPPS_replication")
saveRDS(smri_fu2_out, file = "smri_fu2_out_nocovs.rds")
saveRDS(dmri_fu2_out, file = "dmri_fu2_out_nocovs.rds")
saveRDS(smri_omni_out, file = "smri_fu2_omni_out_nocovs.rds")

write.csv(smri_fu2_out, "smri_fu2_out_nocovs.csv")
write.csv(dmri_fu2_out, "dmri_fu2_out_nocovs.csv")
write.csv(smri_omni_out, "smri_fu2_omni_out_nocovs.csv")


##################do with added covs######################
#name tables to use
demo_table <- '/home/max/Documents/ABCD_4.0/pdem02.txt'
acs_table <- '/home/max/Documents/ABCD_4.0/acspsw03.txt'
smri_table <- '/home/max/Documents/ABCD_4.0/abcd_smrip10201.txt'
nih_table <- '/home/max/Documents/ABCD_4.0/abcd_tbss01.txt'

demo <- read_abcd_tables(demo_table)
acs <- read_abcd_tables(acs_table)
smri <- read_abcd_tables(smri_table)
nihtb <- read_abcd_tables(nih_table)
demo <- filter(demo, eventname == 'baseline_year_1_arm_1')
acs <- filter(acs, eventname == 'baseline_year_1_arm_1')
smri <- filter(smri, eventname == '2_year_follow_up_y_arm_1')
nihtb <- filter(nihtb, eventname == '2_year_follow_up_y_arm_1')
nihtb$total_cog <- rowMeans(nihtb[c('nihtbx_picvocab_uncorrected','nihtbx_flanker_uncorrected',
                                    'nihtbx_pattern_uncorrected', 'nihtbx_picture_uncorrected',
                                    'nihtbx_reading_uncorrected')])

smri_fu2_data_dems <- merge(data_fu2, acs[c('src_subject_id', 'race_ethnicity')], 
                            by = c('src_subject_id'), all.x = TRUE)
smri_fu2_data_dems <- merge(smri_fu2_data_dems, demo[c('src_subject_id', 'sex', 'demo_ethn_v2',
                                                       'demo_comb_income_v2', 'demo_prnt_ed_v2', 
                                                       'demo_prtnr_ed_v2')], by = c('src_subject_id'), 
                                                all.x = TRUE)
smri_fu2_data_dems <- merge(smri_fu2_data_dems, nihtb[c('src_subject_id', 'total_cog')], 
                            by = c('src_subject_id'), all.x = TRUE)
smri_fu2_data_dems <- merge(smri_fu2_data_dems, smri[c('src_subject_id', 'interview_age')], by = c('src_subject_id'), all.x = TRUE)
smri_fu2_data_dems$max_ed <- max(smri_fu2_data_dems$demo_prnt_ed_v2, smri_fu2_data_dems$demo_prtnr_ed_v2, na.rm = TRUE)
smri_fu2_data_dems[smri_fu2_data_dems == 777] <- NA
smri_fu2_data_dems[smri_fu2_data_dems == 999] <- NA
smri_fu2_data_dems[smri_fu2_data_dems == ''] <- NA
smri_fu2_data_dems$demo_prtnr_ed_v2[is.na(smri_fu2_data_dems$demo_prtnr_ed_v2)] <- 0
smri_fu2_data_dems$max_ed <- apply(smri_fu2_data_dems[c('demo_prnt_ed_v2','demo_prtnr_ed_v2')], 1, max)

#dmri
dmri_fu2_data_dems <- merge(data_fu2_dmri, acs[c('src_subject_id', 'race_ethnicity')], 
                            by = c('src_subject_id'), all.x = TRUE)
dmri_fu2_data_dems <- merge(dmri_fu2_data_dems, demo[c('src_subject_id', 'sex', 'demo_ethn_v2',
                                                       'demo_comb_income_v2', 'demo_prnt_ed_v2', 
                                                       'demo_prtnr_ed_v2')], by = c('src_subject_id'), 
                            all.x = TRUE)
dmri_fu2_data_dems <- merge(dmri_fu2_data_dems, nihtb[c('src_subject_id', 'total_cog')], 
                            by = c('src_subject_id'), all.x = TRUE)
dmri_fu2_data_dems <- merge(dmri_fu2_data_dems, smri[c('src_subject_id', 'interview_age')], by = c('src_subject_id'), all.x = TRUE)
dmri_fu2_data_dems$max_ed <- max(dmri_fu2_data_dems$demo_prnt_ed_v2, dmri_fu2_data_dems$demo_prtnr_ed_v2, na.rm = TRUE)
dmri_fu2_data_dems[dmri_fu2_data_dems == 777] <- NA
dmri_fu2_data_dems[dmri_fu2_data_dems == 999] <- NA
dmri_fu2_data_dems[dmri_fu2_data_dems == ''] <- NA
dmri_fu2_data_dems$demo_prtnr_ed_v2[is.na(dmri_fu2_data_dems$demo_prtnr_ed_v2)] <- 0
dmri_fu2_data_dems$max_ed <- apply(dmri_fu2_data_dems[c('demo_prnt_ed_v2','demo_prtnr_ed_v2')], 1, max)

movement1<-read_abcd_tables("/home/max/Documents/ABCD_4.0/mriqcrp10301.txt")
movement2<-read_abcd_tables("/home/max/Documents/ABCD_4.0/mriqcrp20301.txt")
movement<-cbind(movement1,movement2)
movement<-movement[c('src_subject_id', 'eventname', 'iqc_mid_all_mean_motion','iqc_sst_all_mean_motion','iqc_nback_all_mean_motion','iqc_dmri_all_mean_motion','iqc_rsfmri_all_mean_motion')]
movement <- filter(movement, eventname == '2_year_follow_up_y_arm_1')
movement$iqc_mid_all_mean_motion<-as.numeric(movement$iqc_mid_all_mean_motion)
movement$iqc_sst_all_mean_motion<-as.numeric(movement$iqc_sst_all_mean_motion)
movement$iqc_nback_all_mean_motion<-as.numeric(movement$iqc_nback_all_mean_motion)
movement$iqc_dmri_all_mean_motion<-as.numeric(movement$iqc_dmri_all_mean_motion)
movement$iqc_rsfmri_all_mean_motion<-as.numeric(movement$iqc_rsfmri_all_mean_motion)
movement<-movement[complete.cases(movement),]
movement['average_motion']<-rowMeans(movement[c(3:7)])
smri_fu2_data_dems <- merge(smri_fu2_data_dems, movement[c('src_subject_id', 'average_motion')],
                            by = 'src_subject_id')
dmri_fu2_data_dems <- merge(dmri_fu2_data_dems, movement[c('src_subject_id', 'average_motion')],
                            by = 'src_subject_id')

all_covs <- c('sex','interview_age','demo_comb_income_v2','demo_prnt_ed_v2','total_cog','average_motion')

stat_list_smrifu2_allcovs <- list()
stat_list_dmrifu2_allcovs <- list()
#do main analysis
i <- 1
for (y in upps_vars){
  stat_list_smrifu2_allcovs[[i]] <- lapply(setdiff(smri_names, omni_vars), mixed_model, covs = all_covs, y, data=smri_fu2_data_dems)
  stat_list_dmrifu2_allcovs[[i]] <- lapply(dmri_vars, mixed_model, covs = all_covs, y, data=dmri_fu2_data_dems)
  i = i + 1
}

#do omnibus analyses
i <- 1
stat_list_smri_omni_allcovs <- list()
for (y in upps_vars){
  stat_list_smri_omni_allcovs[[i]] <- lapply(omni_vars, mixed_model, covs = all_covs, y, data=smri_fu2_data_dems)
  i <- i + 1
}

saveRDS(stat_list_smrifu2_allcovs, file = "/home/max/Dropbox/ABCD/UPPS_replication/smri_allcovs.rds")
saveRDS(stat_list_dmrifu2_allcovs, file = "/home/max/Dropbox/ABCD/UPPS_replication/dmri_allcovs.rds")
saveRDS(stat_list_smri_omni_allcovs, file = "/home/max/Dropbox/ABCD/UPPS_replication/omni_allcovs.rds")

stat_list_smrifu2_allcovs <- readRDS(file = "/home/max/Dropbox/ABCD/UPPS_replication/smri_allcovs.rds")
stat_list_dmrifu2_allcovs <- readRDS(file = "/home/max/Dropbox/ABCD/UPPS_replication/dmri_allcovs.rds")
stat_list_smri_omni_allcovs <- readRDS(file = "/home/max/Dropbox/ABCD/UPPS_replication/omni_allcovs.rds")

#cleanup
smri_fu2_out <- list()
dmri_fu2_out <- list()
smri_omni_out <- list()
for (y in 1:length(upps_vars)){
  smri_fu2_out[[y]] <-cleanup(stat_list_smrifu2_allcovs[[y]], setdiff(smri_names, omni_vars))
  dmri_fu2_out[[y]] <-cleanup(stat_list_dmrifu2_allcovs[[y]], dmri_vars)
  smri_omni_out[[y]] <-cleanup(stat_list_smri_omni_allcovs[[y]], omni_vars)
}

setwd("/home/max/Dropbox/ABCD/UPPS_replication")
saveRDS(smri_fu2_out, file = "smri_fu2_out_allcovs.rds")
saveRDS(dmri_fu2_out, file = "dmri_fu2_out_allcovs.rds")
saveRDS(smri_omni_out, file = "smri_fu2_omni_out_allcovs.rds")

write.csv(smri_fu2_out, "smri_fu2_out_allcovs.csv")
write.csv(dmri_fu2_out, "dmri_fu2_out_allcovs.csv")
write.csv(smri_omni_out, "smri_fu2_omni_out_allcovs.csv")
