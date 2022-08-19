library(readxl)
library(dplyr)
setwd("/home/max/Dropbox/ABCD/UPPS_replication")
smri_fu2_out <- readRDS(file = "smri_fu2_out.rds")
dmri_fu2_out <- readRDS(file = "dmri_fu2_out.rds")
omni_fu2_out <- readRDS(file = "smri_fu2_omni_out_nocovs.rds")

smri_fig_names <- read.csv('smri_names_figure.csv', header =  TRUE)
dmri_fig_names <- read.csv('dmri_names_figure.csv', header =  TRUE)
omni_fig_names <- read.csv('omni_names_figure.csv', header =  TRUE)

smri_names <- readLines('smri_names.txt')
omni_vars <- c("smri_vol_subcort.aseg_cerebral.white.matter.total", "smri_area_cdk_total", 
               "smri_vol_cort.desikan_total", "smri_vol_scs_subcorticalgv",
               "smri_thick_cdk_mean")
dmri_vars <- readLines('dmri_vars.txt')
dmri_omni <- grep("allfibers", dmri_vars, value = TRUE)
omni_vars <- c(omni_vars, dmri_omni)

for (i in 1:5){
  for (v in dmri_omni){
    omni_fu2_out[[i]] <- rbind(omni_fu2_out[[i]],dmri_fu2_out[[i]][v,])
  }}

smri_old_results <- list()
smri_fu2_out_thresh_old <- list()
smri_fu2_out_thresh_new <- list()
dmri_fu2_out_thresh_old <- list()
dmri_fu2_out_thresh_new <- list()

for (s in 1:5){
  smri_fu2_out[[s]] <- smri_fu2_out[[s]][order(row.names(smri_fu2_out[[s]])),] #sort new results
  smri_fu2_out[[s]] <- cbind(smri_fig_names, smri_fu2_out[[s]]) #add new names to new results
  smri_old_results[[(s)]] <- read_xlsx('Owens_MM_Supplementary_Tables_RR_mod.xlsx', sheet = (s+1), 
                                       col_names = TRUE, skip = 1)#read in old results
  smri_fu2_out[[s]] <- merge(smri_fu2_out[[s]], smri_old_results[[s]], 
                             by =c("Hemi", "Region", "Metric") ) #merge old and new reuslts
  smri_fu2_out[[s]] <- smri_fu2_out[[s]][order(smri_fu2_out[[s]]$p.y),] #sort new results
  smri_fu2_out[[s]]$direction <- (smri_fu2_out[[s]]$B.x * smri_fu2_out[[s]]$B.y > 0)
  smri_fu2_out[[s]]$signew <- (smri_fu2_out[[s]]$p.x < .05)
  smri_fu2_out[[s]]$CI_upper <- (smri_fu2_out[[s]]$B.y + (1.96 * smri_fu2_out[[s]]$SE.y) )
  smri_fu2_out[[s]]$CI_lower <- (smri_fu2_out[[s]]$B.y - (1.96 * smri_fu2_out[[s]]$SE.y))
  smri_fu2_out[[s]]$inCI <- (smri_fu2_out[[s]]$B.x > smri_fu2_out[[s]]$CI_lower & smri_fu2_out[[s]]$B.x < smri_fu2_out[[s]]$CI_upper)
  smri_fu2_out[[s]]$BH_old <- p.adjust(smri_fu2_out[[s]]$p.y, method = 'BH', n = 154)
  smri_fu2_out[[s]]$BH_new <- p.adjust(smri_fu2_out[[s]]$p.x, method = 'BH', n = 154)
  smri_fu2_out_thresh_old[[s]] <- filter(smri_fu2_out[[s]], BH_old < .05)
  smri_fu2_out_thresh_new[[s]] <- filter(smri_fu2_out[[s]], BH_new < .05)
  #write.csv(smri_fu2_out_thresh_old[[s]], paste(s,"smri_results_thresholded_old.csv"))
  #write.csv(smri_fu2_out_thresh_new[[s]], paste(s,"smri_results_thresholded_new.csv"))
  #write.csv(smri_fu2_out[[s]], paste(s,"smri_results.csv"))
  
  smri_BH_old <- list()
  smri_BH_new <- list()
  smri_BH_old[[s]] <- smri_fu2_out[[s]]$BH_old
  smri_BH_new[[s]] <- smri_fu2_out[[s]]$BH_new
}


dmri_old_results <- list()
for (s in 1:5){
  dmri_fu2_out[[s]] <- dmri_fu2_out[[s]][order(row.names(dmri_fu2_out[[s]])),] #sort new results
  dmri_fu2_out[[s]] <- cbind(dmri_fig_names, dmri_fu2_out[[s]]) #add new names to new results
  dmri_old_results[[(s)]] <- read_xlsx('Owens_MM_Supplementary_Tables_RR_mod.xlsx', sheet = (s+6), 
                                       col_names = TRUE, skip = 1)#read in old results
  dmri_old_results[[(s)]]$Hemi[is.na(dmri_old_results[[(s)]]$Hemi)]<- 'Bilateral'
  #dmri_fu2_out[[(s)]]$Hemi[is.na(dmri_fu2_out[[(s)]]$Hemi)]<- 'Bilateral'
  dmri_fu2_out[[s]] <- merge(dmri_fu2_out[[s]], dmri_old_results[[s]], 
                             by =c("Hemi", "Region", "Metric") ) #merge old and new reuslts
  dmri_fu2_out[[s]] <- dmri_fu2_out[[s]][order(dmri_fu2_out[[s]]$p.y),] #sort new results
  dmri_fu2_out[[s]]$direction <- (dmri_fu2_out[[s]]$B.x * dmri_fu2_out[[s]]$B.y > 0)
  dmri_fu2_out[[s]]$signew <- (dmri_fu2_out[[s]]$p.x < .05)
  dmri_fu2_out[[s]]$CI_upper <- (dmri_fu2_out[[s]]$B.y + (1.96 * dmri_fu2_out[[s]]$SE.y) )
  dmri_fu2_out[[s]]$CI_lower <- (dmri_fu2_out[[s]]$B.y - (1.96 * dmri_fu2_out[[s]]$SE.y))
  dmri_fu2_out[[s]]$inCI <- (dmri_fu2_out[[s]]$B.x > dmri_fu2_out[[s]]$CI_lower & dmri_fu2_out[[s]]$B.x < dmri_fu2_out[[s]]$CI_upper)
  dmri_fu2_out[[s]]$BH_old <- p.adjust(dmri_fu2_out[[s]]$p.y, method = 'BH', n = 54)
  dmri_fu2_out[[s]]$BH_new <- p.adjust(dmri_fu2_out[[s]]$p.x, method = 'BH', n = 54)
  dmri_fu2_out_thresh_old[[s]] <- filter(dmri_fu2_out[[s]], BH_old < .05)
  dmri_fu2_out_thresh_new[[s]] <- filter(dmri_fu2_out[[s]], BH_new < .05)
  #write.csv(dmri_fu2_out_thresh_old[[s]], paste(s,"dmri_results_thresholded_old.csv"))
  #write.csv(dmri_fu2_out_thresh_new[[s]], paste(s,"dmri_results_thresholded_new.csv"))
  #write.csv(dmri_fu2_out[[s]], paste(s,"dmri_results.csv"))
  
  dmri_BH_old <- list()
  dmri_BH_new <- list()
  dmri_BH_old[[s]] <- dmri_fu2_out[[s]]$BH_old
  dmri_BH_new[[s]] <- dmri_fu2_out[[s]]$BH_new
}



for (s in 1:5){
  omni_fu2_out[[s]] <- cbind(omni_fig_names, omni_fu2_out[[s]]) #add new names to new results
  omni_fu2_out[[s]]$BH_new <- p.adjust(omni_fu2_out[[s]]$p.x, method = 'BH', n = 7)
  omni_fu2_out[[s]][c('B','SE','t','p','R2')] <- round(omni_fu2_out[[s]][c('B','SE','t','p','R2')], digits = 4)
  write.csv(omni_fu2_out[[s]], paste(s,"omni_results.csv"))
}

####################no covariates (ie no icv)########################
setwd("/home/max/Dropbox/ABCD/UPPS_replication")
smri_fu2_out <- readRDS(file = "smri_fu2_out_nocovs.rds")
dmri_fu2_out <- readRDS(file = "dmri_fu2_out_nocovs.rds")
omni_fu2_out <- readRDS(file = "smri_fu2_omni_out_nocovs.rds")

smri_fig_names <- read.csv('smri_names_figure_supplemental.csv', header =  TRUE)
dmri_fig_names <- read.csv('dmri_names_figure.csv', header =  TRUE)
omni_fig_names <- read.csv('omni_names_figure.csv', header =  TRUE)

smri_names <- readLines('smri_names.txt')
omni_vars <- c("smri_vol_subcort.aseg_cerebral.white.matter.total", "smri_area_cdk_total", 
               "smri_vol_cort.desikan_total", "smri_vol_scs_subcorticalgv",
               "smri_thick_cdk_mean")
dmri_vars <- readLines('dmri_vars.txt')
dmri_omni <- grep("allfibers", dmri_vars, value = TRUE)
omni_vars <- c(omni_vars, dmri_omni)

setwd("/home/max/Dropbox/ABCD/UPPS_replication/no_icv")

for (i in 1:5){
  for (v in dmri_omni){
    omni_fu2_out[[i]] <- rbind(omni_fu2_out[[i]],dmri_fu2_out[[i]][v,])
  }}

smri_old_results <- list()
smri_fu2_out_thresh_old <- list()
smri_fu2_out_thresh_new <- list()
dmri_fu2_out_thresh_old <- list()
dmri_fu2_out_thresh_new <- list()

for (s in 1:5){
  smri_fu2_out[[s]] <- smri_fu2_out[[s]][order(row.names(smri_fu2_out[[s]])),] #sort new results
  smri_fu2_out[[s]] <- cbind(smri_fig_names, smri_fu2_out[[s]]) #add new names to new results
  smri_old_results[[(s)]] <- read_xlsx('../Owens_MM_Supplementary_Tables_RR_mod.xlsx', sheet = (s+11), 
                                       col_names = TRUE, skip = 1)#read in old results
  smri_fu2_out[[s]] <- merge(smri_fu2_out[[s]], smri_old_results[[s]], 
                             by =c("Hemi", "Region", "Metric") ) #merge old and new reuslts
  smri_fu2_out[[s]] <- smri_fu2_out[[s]][order(smri_fu2_out[[s]]$p.y),] #sort new results
  smri_fu2_out[[s]]$direction <- (smri_fu2_out[[s]]$B.x * smri_fu2_out[[s]]$B.y > 0)
  smri_fu2_out[[s]]$signew <- (smri_fu2_out[[s]]$p.x < .05)
  smri_fu2_out[[s]]$CI_upper <- (smri_fu2_out[[s]]$B.y + (1.96 * smri_fu2_out[[s]]$SE.y) )
  smri_fu2_out[[s]]$CI_lower <- (smri_fu2_out[[s]]$B.y - (1.96 * smri_fu2_out[[s]]$SE.y))
  smri_fu2_out[[s]]$inCI <- (smri_fu2_out[[s]]$B.x > smri_fu2_out[[s]]$CI_lower & smri_fu2_out[[s]]$B.x < smri_fu2_out[[s]]$CI_upper)
  smri_fu2_out[[s]]$BH_old <- p.adjust(smri_fu2_out[[s]]$p.y, method = 'BH', n = 154)
  smri_fu2_out[[s]]$BH_new <- smri_BH_new
  smri_fu2_out_thresh_old[[s]] <- filter(smri_fu2_out[[s]], BH_old < .05)
  smri_fu2_out_thresh_new[[s]] <- filter(smri_fu2_out[[s]], BH_new < .05)
  write.csv(smri_fu2_out_thresh_old[[s]], paste(s,"nocovs_smri_results_thresholded_old.csv"))
  write.csv(smri_fu2_out_thresh_new[[s]], paste(s,"nocovs_smri_results_thresholded_new.csv"))
  write.csv(smri_fu2_out[[s]], paste(s,"nocovs_smri_results.csv"))
}

dmri_old_results <- list()
for (s in 1:5){
  dmri_fu2_out[[s]] <- dmri_fu2_out[[s]][order(row.names(dmri_fu2_out[[s]])),] #sort new results
  dmri_fu2_out[[s]] <- cbind(dmri_fig_names, dmri_fu2_out[[s]]) #add new names to new results
  dmri_old_results[[(s)]] <- read_xlsx('../Owens_MM_Supplementary_Tables_RR_mod.xlsx', sheet = (s+16), 
                                       col_names = TRUE, skip = 1)#read in old results
  dmri_old_results[[(s)]]$Hemi[is.na(dmri_old_results[[(s)]]$Hemi)]<- 'Bilateral'
  #dmri_fu2_out[[(s)]]$Hemi[is.na(dmri_fu2_out[[(s)]]$Hemi)]<- 'Bilateral'
  dmri_fu2_out[[s]] <- merge(dmri_fu2_out[[s]], dmri_old_results[[s]], 
                             by =c("Hemi", "Region", "Metric") ) #merge old and new reuslts
  dmri_fu2_out[[s]] <- dmri_fu2_out[[s]][order(dmri_fu2_out[[s]]$p.y),] #sort new results
  dmri_fu2_out[[s]]$direction <- (dmri_fu2_out[[s]]$B.x * dmri_fu2_out[[s]]$B.y > 0)
  dmri_fu2_out[[s]]$signew <- (dmri_fu2_out[[s]]$p.x < .05)
  dmri_fu2_out[[s]]$CI_upper <- (dmri_fu2_out[[s]]$B.y + (1.96 * dmri_fu2_out[[s]]$SE.y) )
  dmri_fu2_out[[s]]$CI_lower <- (dmri_fu2_out[[s]]$B.y - (1.96 * dmri_fu2_out[[s]]$SE.y))
  dmri_fu2_out[[s]]$inCI <- (dmri_fu2_out[[s]]$B.x > dmri_fu2_out[[s]]$CI_lower & dmri_fu2_out[[s]]$B.x < dmri_fu2_out[[s]]$CI_upper)
  dmri_fu2_out[[s]]$BH_old <- p.adjust(dmri_fu2_out[[s]]$p.y, method = 'BH', n = 54)
  dmri_fu2_out[[s]]$BH_new <- p.adjust(dmri_fu2_out[[s]]$p.x, method = 'BH', n = 54)
  dmri_fu2_out_thresh_old[[s]] <- filter(dmri_fu2_out[[s]], BH_old < .05)
  dmri_fu2_out_thresh_new[[s]] <- filter(dmri_fu2_out[[s]], BH_new < .05)
  write.csv(dmri_fu2_out_thresh_old[[s]], paste(s,"nocovs_dmri_results_thresholded_old.csv"))
  write.csv(dmri_fu2_out_thresh_new[[s]], paste(s,"nocovs_dmri_results_thresholded_new.csv"))
  write.csv(dmri_fu2_out[[s]], paste(s,"nocovs_dmri_results.csv"))
}

for (s in 1:5){
  omni_fu2_out[[s]] <- cbind(omni_fig_names, omni_fu2_out[[s]]) #add new names to new results
  omni_fu2_out[[s]]$BH_new <- p.adjust(omni_fu2_out[[s]]$p.x, method = 'BH', n = 7)
  omni_fu2_out[[s]][c('B','SE','t','p','R2')] <- round(omni_fu2_out[[s]][c('B','SE','t','p','R2')], digits = 4)
  write.csv(omni_fu2_out[[s]], paste(s,"nocovs_omni_results.csv"))
}

############################all covariates#####################################
setwd("/home/max/Dropbox/ABCD/UPPS_replication")
smri_fu2_out <- readRDS(file = "smri_fu2_out_allcovs.rds")
dmri_fu2_out <- readRDS(file = "dmri_fu2_out_allcovs.rds")
omni_fu2_out <- readRDS(file = "smri_fu2_omni_out_allcovs.rds")

smri_fig_names <- read.csv('smri_names_figure_supplemental.csv', header =  TRUE)
dmri_fig_names <- read.csv('dmri_names_figure.csv', header =  TRUE)
omni_fig_names <- read.csv('omni_names_figure.csv', header =  TRUE)

dmri_vars <- readLines('dmri_vars.txt')
dmri_omni <- grep("allfibers", dmri_vars, value = TRUE)
omni_vars <- c(omni_vars, dmri_omni)

setwd("/home/max/Dropbox/ABCD/UPPS_replication/all_covs")

for (i in 1:5){
  for (v in dmri_omni){
    omni_fu2_out[[i]] <- rbind(omni_fu2_out[[i]],dmri_fu2_out[[i]][v,])
  }}

smri_old_results <- list()
smri_fu2_out_thresh_old <- list()
smri_fu2_out_thresh_new <- list()
dmri_fu2_out_thresh_old <- list()
dmri_fu2_out_thresh_new <- list()

for (s in 1:5){
  smri_fu2_out[[s]] <- smri_fu2_out[[s]][order(row.names(smri_fu2_out[[s]])),] #sort new results
  smri_fu2_out[[s]] <- cbind(smri_fig_names, smri_fu2_out[[s]]) #add new names to new results
  smri_old_results[[(s)]] <- read_xlsx('../Owens_MM_Supplementary_Tables_RR_mod.xlsx', sheet = (s+21), 
                                       col_names = TRUE, skip = 1)#read in old results
  smri_fu2_out[[s]] <- merge(smri_fu2_out[[s]], smri_old_results[[s]], 
                             by =c("Hemi", "Region", "Metric") ) #merge old and new reuslts
  smri_fu2_out[[s]] <- smri_fu2_out[[s]][order(smri_fu2_out[[s]]$p.y),] #sort new results
  smri_fu2_out[[s]]$direction <- (smri_fu2_out[[s]]$B.x * smri_fu2_out[[s]]$B.y > 0)
  smri_fu2_out[[s]]$signew <- (smri_fu2_out[[s]]$p.x < .05)
  smri_fu2_out[[s]]$CI_upper <- (smri_fu2_out[[s]]$B.y + (1.96 * smri_fu2_out[[s]]$SE.y) )
  smri_fu2_out[[s]]$CI_lower <- (smri_fu2_out[[s]]$B.y - (1.96 * smri_fu2_out[[s]]$SE.y))
  smri_fu2_out[[s]]$inCI <- (smri_fu2_out[[s]]$B.x > smri_fu2_out[[s]]$CI_lower & smri_fu2_out[[s]]$B.x < smri_fu2_out[[s]]$CI_upper)
  smri_fu2_out[[s]]$BH_old <- p.adjust(smri_fu2_out[[s]]$p.y, method = 'BH', n = 154)
  smri_fu2_out[[s]]$BH_new <- p.adjust(smri_fu2_out[[s]]$p.x, method = 'BH', n = 154)
  smri_fu2_out_thresh_old[[s]] <- filter(smri_fu2_out[[s]], BH_old < .05)
  smri_fu2_out_thresh_new[[s]] <- filter(smri_fu2_out[[s]], BH_new < .05)
  write.csv(smri_fu2_out_thresh_old[[s]], paste(s,"allcovs_smri_results_thresholded_old.csv"))
  write.csv(smri_fu2_out_thresh_new[[s]], paste(s,"allcovs_smri_results_thresholded_new.csv"))
  write.csv(smri_fu2_out[[s]], paste(s,"allcovs_smri_results.csv"))
}

dmri_old_results <- list()
for (s in 1:5){
  dmri_fu2_out[[s]] <- dmri_fu2_out[[s]][order(row.names(dmri_fu2_out[[s]])),] #sort new results
  dmri_fu2_out[[s]] <- cbind(dmri_fig_names, dmri_fu2_out[[s]]) #add new names to new results
  dmri_old_results[[(s)]] <- read_xlsx('../Owens_MM_Supplementary_Tables_RR_mod.xlsx', sheet = (s+26), 
                                       col_names = TRUE, skip = 1)#read in old results
  dmri_old_results[[(s)]]$Hemi[is.na(dmri_old_results[[(s)]]$Hemi)]<- 'Bilateral'
  #dmri_fu2_out[[(s)]]$Hemi[is.na(dmri_fu2_out[[(s)]]$Hemi)]<- 'Bilateral'
  dmri_fu2_out[[s]] <- merge(dmri_fu2_out[[s]], dmri_old_results[[s]], 
                             by =c("Hemi", "Region", "Metric") ) #merge old and new reuslts
  dmri_fu2_out[[s]] <- dmri_fu2_out[[s]][order(dmri_fu2_out[[s]]$p.y),] #sort new results
  dmri_fu2_out[[s]]$direction <- (dmri_fu2_out[[s]]$B.x * dmri_fu2_out[[s]]$B.y > 0)
  dmri_fu2_out[[s]]$signew <- (dmri_fu2_out[[s]]$p.x < .05)
  dmri_fu2_out[[s]]$CI_upper <- (dmri_fu2_out[[s]]$B.y + (1.96 * dmri_fu2_out[[s]]$SE.y) )
  dmri_fu2_out[[s]]$CI_lower <- (dmri_fu2_out[[s]]$B.y - (1.96 * dmri_fu2_out[[s]]$SE.y))
  dmri_fu2_out[[s]]$inCI <- (dmri_fu2_out[[s]]$B.x > dmri_fu2_out[[s]]$CI_lower & dmri_fu2_out[[s]]$B.x < dmri_fu2_out[[s]]$CI_upper)
  dmri_fu2_out[[s]]$BH_old <- p.adjust(dmri_fu2_out[[s]]$p.y, method = 'BH', n = 54)
  dmri_fu2_out[[s]]$BH_new <- p.adjust(dmri_fu2_out[[s]]$p.x, method = 'BH', n = 54)
  dmri_fu2_out_thresh_old[[s]] <- filter(dmri_fu2_out[[s]], BH_old < .05)
  dmri_fu2_out_thresh_new[[s]] <- filter(dmri_fu2_out[[s]], BH_new < .05)
  write.csv(dmri_fu2_out_thresh_old[[s]], paste(s,"allcovs_dmri_results_thresholded_old.csv"))
  write.csv(dmri_fu2_out_thresh_new[[s]], paste(s,"allcovs_dmri_results_thresholded_new.csv"))
  write.csv(dmri_fu2_out[[s]], paste(s,"allcovs_dmri_results.csv"))
}

for (s in 1:5){
  omni_fu2_out[[s]] <- cbind(omni_fig_names, omni_fu2_out[[s]]) #add new names to new results
  omni_fu2_out[[s]]$BH_new <- p.adjust(omni_fu2_out[[s]]$p.x, method = 'BH', n = 7)
  omni_fu2_out[[s]][c('B','SE','t','p','R2')] <- round(omni_fu2_out[[s]][c('B','SE','t','p','R2')], digits = 4)
  write.csv(omni_fu2_out[[s]], paste(s,"allcovs_omni_results.csv"))
}

#########################ICCs##################################
library('irr')
#Build Final Table
icc_table_row_names <- as.character(read.delim("icc_table_row_names.txt", header = FALSE)$V1)
final_table <- data.frame("ICC"=1:35, "lower"=1:35, "upper"=1:35, row.names = icc_table_row_names)

#######ICC of associations#######
#make results datasets for each MRI metric
area_fu2_out <- list()
thick_fu2_out <- list()
vol_fu2_out <- list()

for (i in 1:5){
area_fu2_out[[i]] <- filter(smri_fu2_out[[i]], Metric == 'Surface Area')
thick_fu2_out[[i]] <- filter(smri_fu2_out[[i]], Metric == 'Thickness')
vol_fu2_out[[i]] <- filter(smri_fu2_out[[i]], Metric == 'Gray Matter Volume')
}

fa_fu2_out <- list()
md_fu2_out <- list()
for (i in 1:5){
  fa_fu2_out[[i]] <- filter(dmri_fu2_out[[i]], Metric == 'FA')
  md_fu2_out[[i]] <- filter(dmri_fu2_out[[i]], Metric == 'MD')
  }

for (uvn in 1:5){
icc <- icc(data.frame(area_fu2_out[[uvn]]$B.y, area_fu2_out[[uvn]]$B.x), 
             model = 'twoway', type = 'agreement', unit = 'single')
final_table[[(10+uvn),1]] <- icc$value
final_table[[(10+uvn),2]] <- icc$lbound
final_table[[(10+uvn),3]] <- icc$ubound
}

for (uvn in 1:5){
  icc <- icc(data.frame(thick_fu2_out[[uvn]]$B.y, thick_fu2_out[[uvn]]$B.x), 
             model = 'twoway', type = 'agreement', unit = 'single')
  final_table[[(15+uvn),1]] <- icc$value
  final_table[[(15+uvn),2]] <- icc$lbound
  final_table[[(15+uvn),3]] <- icc$ubound
}

for (uvn in 1:5){
  icc <- icc(data.frame(vol_fu2_out[[uvn]]$B.y, vol_fu2_out[[uvn]]$B.x), 
             model = 'twoway', type = 'agreement', unit = 'single')
  final_table[[(20+uvn),1]] <- icc$value
  final_table[[(20+uvn),2]] <- icc$lbound
  final_table[[(20+uvn),3]] <- icc$ubound
}

for (uvn in 1:5){
  icc <- icc(data.frame(fa_fu2_out[[uvn]]$B.y, fa_fu2_out[[uvn]]$B.x), 
             model = 'twoway', type = 'agreement', unit = 'single')
  final_table[[(25+uvn),1]] <- icc$value
  final_table[[(25+uvn),2]] <- icc$lbound
  final_table[[(25+uvn),3]] <- icc$ubound
}

for (uvn in 1:5){
  icc <- icc(data.frame(md_fu2_out[[uvn]]$B.y, md_fu2_out[[uvn]]$B.x), 
             model = 'twoway', type = 'agreement', unit = 'single')
  final_table[[(30+uvn),1]] <- icc$value
  final_table[[(30+uvn),2]] <- icc$lbound
  final_table[[(30+uvn),3]] <- icc$ubound
}

smri_bsl_data <- readRDS(file = "smri_bsl_data_win.rds")
smri_fu2_data <- readRDS(file = "smri_fu2_data_win.rds")
dmri_bsl_data <- readRDS(file = "dmri_bsl_data_win.rds")
dmri_fu2_data <- readRDS(file = "dmri_fu2_data_win.rds")

#######ICC of UPPS Traits#######
upps_vars <- c('upps_y_ss_negative_urgency', 'upps_y_ss_lack_of_planning',
               'upps_y_ss_sensation_seeking','upps_y_ss_positive_urgency',
               'upps_y_ss_lack_of_perseverance')

smri_vars <- readLines('smri_orig_names.txt')
smri_names <- readLines('smri_names.txt')

dmri_vars <- readLines('dmri_vars.txt')

smri_both_data <- merge(smri_bsl_data, smri_fu2_data, by='src_subject_id')
dmri_both_data <- merge(smri_bsl_data, smri_fu2_data, by='src_subject_id')

upps_iccs <- data.frame("ICC"=c(0,0,0,0,0), "lower"=c(0,0,0,0,0), "upper"=c(0,0,0,0,0), row.names = upps_vars)
counter <- 1
for (v in upps_vars){
  fu2_st <- paste(v, ".y", sep = "")
  bsl_st <- paste(v, ".x", sep = "")
  res <- icc(data.frame(smri_both_data[c(fu2_st)],smri_both_data[c(bsl_st)]), 
      model = 'twoway', type = 'agreement', unit = 'single')
  final_table[counter, 1] <- res$value
  final_table[counter, 2] <- res$lbound
  final_table[counter, 3] <- res$ubound
  counter <- counter+1
}

#######ICC of brain vars#######
thick_names <- grep("_thick_", smri_names, value = TRUE)  
area_names <- grep("_area_", smri_names, value = TRUE)  
vol_names <- grep("_vol_", smri_names, value = TRUE)
fa_names <- grep("_dtifa_", dmri_vars, value = TRUE)  
md_names <- grep("_dtimd_", dmri_vars, value = TRUE)

thick_iccs <- data.frame("ICC"=c(0,0,0,0,0), "lower"=c(0,0,0,0,0), "upper"=c(0,0,0,0,0))
area_iccs <- data.frame("ICC"=c(0,0,0,0,0), "lower"=c(0,0,0,0,0), "upper"=c(0,0,0,0,0))
vol_iccs <- data.frame("ICC"=c(0,0,0,0,0), "lower"=c(0,0,0,0,0), "upper"=c(0,0,0,0,0))
fa_iccs <- data.frame("ICC"=c(0,0,0,0,0), "lower"=c(0,0,0,0,0), "upper"=c(0,0,0,0,0))
md_iccs <- data.frame("ICC"=c(0,0,0,0,0), "lower"=c(0,0,0,0,0), "upper"=c(0,0,0,0,0))

counter <- 1
for (v in thick_names){
  fu2_st <- paste(v, ".y", sep = "")
  bsl_st <- paste(v, ".x", sep = "")
  res <- icc(data.frame(smri_both_data[c(fu2_st)],smri_both_data[c(bsl_st)]), 
             model = 'twoway', type = 'agreement', unit = 'single')
  thick_iccs[counter, 1] <- res$value
  thick_iccs[counter, 2] <- res$lbound
  thick_iccs[counter, 3] <- res$ubound
  counter <- counter+1
}

counter <- 1
for (v in area_names){
  fu2_st <- paste(v, ".y", sep = "")
  bsl_st <- paste(v, ".x", sep = "")
  res <- icc(data.frame(smri_both_data[c(fu2_st)],smri_both_data[c(bsl_st)]), 
             model = 'twoway', type = 'agreement', unit = 'single')
  area_iccs[counter, 1] <- res$value
  area_iccs[counter, 2] <- res$lbound
  area_iccs[counter, 3] <- res$ubound
  counter <- counter+1
}

counter <- 1
for (v in vol_names){
  fu2_st <- paste(v, ".y", sep = "")
  bsl_st <- paste(v, ".x", sep = "")
  res <- icc(data.frame(smri_both_data[c(fu2_st)],smri_both_data[c(bsl_st)]), 
             model = 'twoway', type = 'agreement', unit = 'single')
  vol_iccs[counter, 1] <- res$value
  vol_iccs[counter, 2] <- res$lbound
  vol_iccs[counter, 3] <- res$ubound
  counter <- counter+1
}

counter <- 1
for (v in fa_names){
  fu2_st <- paste(v, ".y", sep = "")
  bsl_st <- paste(v, ".x", sep = "")
  res <- icc(data.frame(dmri_both_data[c(fu2_st)],dmri_both_data[c(bsl_st)]), 
             model = 'twoway', type = 'agreement', unit = 'single')
  fa_iccs[counter, 1] <- res$value
  fa_iccs[counter, 2] <- res$lbound
  fa_iccs[counter, 3] <- res$ubound
  counter <- counter+1
}

counter <- 1
for (v in md_names){
  fu2_st <- paste(v, ".y", sep = "")
  bsl_st <- paste(v, ".x", sep = "")
  res <- icc(data.frame(dmri_both_data[c(fu2_st)],dmri_both_data[c(bsl_st)]), 
             model = 'twoway', type = 'agreement', unit = 'single')
  md_iccs[counter, 1] <- res$value
  md_iccs[counter, 2] <- res$lbound
  md_iccs[counter, 3] <- res$ubound
  counter <- counter+1
}

row.names(thick_iccs) <- thick_names
row.names(area_iccs) <- area_names
row.names(vol_iccs) <- vol_names
row.names(fa_iccs) <- fa_names
row.names(md_iccs) <- md_names

#get results from smri ICCs
final_table[6,1] <- mean(thick_iccs$ICC)
final_table[7,1] <- mean(area_iccs$ICC)
final_table[8,1] <- mean(vol_iccs$ICC)
final_table[9,1] <- mean(fa_iccs$ICC)
final_table[10,1] <- mean(md_iccs$ICC)

final_table[6,2] <- mean(thick_iccs$lower)
final_table[7,2] <- mean(area_iccs$lower)
final_table[8,2] <- mean(vol_iccs$lower)
final_table[9,2] <- mean(fa_iccs$lower)
final_table[10,2] <- mean(md_iccs$lower)

final_table[6,3] <- mean(thick_iccs$upper)
final_table[7,3] <- mean(area_iccs$upper)
final_table[8,3] <- mean(vol_iccs$upper)
final_table[9,3] <- mean(fa_iccs$upper)
final_table[10,3] <- mean(md_iccs$upper)

write.csv(final_table, "ICC_table.csv")


#########################ICCs##################################
library('irr')
#Build Final Table
setwd("/home/max/Dropbox/ABCD/UPPS_replication")
icc_table_row_names <- as.character(read.delim("icc_table_row_names.txt", header = FALSE)$V1)
final_table <- data.frame("ICC"=1:35, "lower"=1:35, "upper"=1:35, row.names = icc_table_row_names)

#######ICC of associations#######
#make results datasets for each MRI metric
area_fu2_out <- list()
thick_fu2_out <- list()
vol_fu2_out <- list()

for (i in 1:5){
  area_fu2_out[[i]] <- filter(smri_fu2_out[[i]], Metric == 'Surface Area')
  thick_fu2_out[[i]] <- filter(smri_fu2_out[[i]], Metric == 'Thickness')
  vol_fu2_out[[i]] <- filter(smri_fu2_out[[i]], Metric == 'Gray Matter Volume')
}

fa_fu2_out <- list()
md_fu2_out <- list()
for (i in 1:5){
  fa_fu2_out[[i]] <- filter(dmri_fu2_out[[i]], Metric == 'FA')
  md_fu2_out[[i]] <- filter(dmri_fu2_out[[i]], Metric == 'MD')
}

for (uvn in 1:5){
  icc <- icc(data.frame(area_fu2_out[[uvn]]$B.y, area_fu2_out[[uvn]]$B.x), 
             model = 'twoway', type = 'agreement', unit = 'single')
  final_table[[(10+uvn),1]] <- icc$value
  final_table[[(10+uvn),2]] <- icc$lbound
  final_table[[(10+uvn),3]] <- icc$ubound
}

for (uvn in 1:5){
  icc <- icc(data.frame(thick_fu2_out[[uvn]]$B.y, thick_fu2_out[[uvn]]$B.x), 
             model = 'twoway', type = 'agreement', unit = 'single')
  final_table[[(15+uvn),1]] <- icc$value
  final_table[[(15+uvn),2]] <- icc$lbound
  final_table[[(15+uvn),3]] <- icc$ubound
}

for (uvn in 1:5){
  icc <- icc(data.frame(vol_fu2_out[[uvn]]$B.y, vol_fu2_out[[uvn]]$B.x), 
             model = 'twoway', type = 'agreement', unit = 'single')
  final_table[[(20+uvn),1]] <- icc$value
  final_table[[(20+uvn),2]] <- icc$lbound
  final_table[[(20+uvn),3]] <- icc$ubound
}

for (uvn in 1:5){
  icc <- icc(data.frame(fa_fu2_out[[uvn]]$B.y, fa_fu2_out[[uvn]]$B.x), 
             model = 'twoway', type = 'agreement', unit = 'single')
  final_table[[(25+uvn),1]] <- icc$value
  final_table[[(25+uvn),2]] <- icc$lbound
  final_table[[(25+uvn),3]] <- icc$ubound
}

for (uvn in 1:5){
  icc <- icc(data.frame(md_fu2_out[[uvn]]$B.y, md_fu2_out[[uvn]]$B.x), 
             model = 'twoway', type = 'agreement', unit = 'single')
  final_table[[(30+uvn),1]] <- icc$value
  final_table[[(30+uvn),2]] <- icc$lbound
  final_table[[(30+uvn),3]] <- icc$ubound
}

smri_bsl_data <- readRDS(file = "smri_bsl_data_win.rds")
smri_fu2_data <- readRDS(file = "smri_fu2_data_win.rds")
dmri_bsl_data <- readRDS(file = "dmri_bsl_data_win.rds")
dmri_fu2_data <- readRDS(file = "dmri_fu2_data_win.rds")

#######ICC of UPPS Traits#######
upps_vars <- c('upps_y_ss_negative_urgency', 'upps_y_ss_lack_of_planning',
               'upps_y_ss_sensation_seeking','upps_y_ss_positive_urgency',
               'upps_y_ss_lack_of_perseverance')

smri_vars <- readLines('smri_orig_names.txt')
smri_names <- readLines('smri_names.txt')

dmri_vars <- readLines('dmri_vars.txt')

smri_both_data <- merge(smri_bsl_data, smri_fu2_data, by='src_subject_id')
dmri_both_data <- merge(smri_bsl_data, smri_fu2_data, by='src_subject_id')

upps_iccs <- data.frame("ICC"=c(0,0,0,0,0), "lower"=c(0,0,0,0,0), "upper"=c(0,0,0,0,0), row.names = upps_vars)
counter <- 1
for (v in upps_vars){
  fu2_st <- paste(v, ".y", sep = "")
  bsl_st <- paste(v, ".x", sep = "")
  res <- icc(data.frame(smri_both_data[c(fu2_st)],smri_both_data[c(bsl_st)]), 
             model = 'twoway', type = 'agreement', unit = 'single')
  final_table[counter, 1] <- res$value
  final_table[counter, 2] <- res$lbound
  final_table[counter, 3] <- res$ubound
  counter <- counter+1
}

#######ICC of brain vars#######
thick_names <- grep("_thick_", smri_names, value = TRUE)  
area_names <- grep("_area_", smri_names, value = TRUE)  
vol_names <- grep("_vol_", smri_names, value = TRUE)
fa_names <- grep("_dtifa_", dmri_vars, value = TRUE)  
md_names <- grep("_dtimd_", dmri_vars, value = TRUE)

thick_iccs <- data.frame("ICC"=c(0,0,0,0,0), "lower"=c(0,0,0,0,0), "upper"=c(0,0,0,0,0))
area_iccs <- data.frame("ICC"=c(0,0,0,0,0), "lower"=c(0,0,0,0,0), "upper"=c(0,0,0,0,0))
vol_iccs <- data.frame("ICC"=c(0,0,0,0,0), "lower"=c(0,0,0,0,0), "upper"=c(0,0,0,0,0))
fa_iccs <- data.frame("ICC"=c(0,0,0,0,0), "lower"=c(0,0,0,0,0), "upper"=c(0,0,0,0,0))
md_iccs <- data.frame("ICC"=c(0,0,0,0,0), "lower"=c(0,0,0,0,0), "upper"=c(0,0,0,0,0))

counter <- 1
for (v in thick_names){
  fu2_st <- paste(v, ".y", sep = "")
  bsl_st <- paste(v, ".x", sep = "")
  res <- icc(data.frame(smri_both_data[c(fu2_st)],smri_both_data[c(bsl_st)]), 
             model = 'twoway', type = 'agreement', unit = 'single')
  thick_iccs[counter, 1] <- res$value
  thick_iccs[counter, 2] <- res$lbound
  thick_iccs[counter, 3] <- res$ubound
  counter <- counter+1
}

counter <- 1
for (v in area_names){
  fu2_st <- paste(v, ".y", sep = "")
  bsl_st <- paste(v, ".x", sep = "")
  res <- icc(data.frame(smri_both_data[c(fu2_st)],smri_both_data[c(bsl_st)]), 
             model = 'twoway', type = 'agreement', unit = 'single')
  area_iccs[counter, 1] <- res$value
  area_iccs[counter, 2] <- res$lbound
  area_iccs[counter, 3] <- res$ubound
  counter <- counter+1
}

counter <- 1
for (v in vol_names){
  fu2_st <- paste(v, ".y", sep = "")
  bsl_st <- paste(v, ".x", sep = "")
  res <- icc(data.frame(smri_both_data[c(fu2_st)],smri_both_data[c(bsl_st)]), 
             model = 'twoway', type = 'agreement', unit = 'single')
  vol_iccs[counter, 1] <- res$value
  vol_iccs[counter, 2] <- res$lbound
  vol_iccs[counter, 3] <- res$ubound
  counter <- counter+1
}

counter <- 1
for (v in fa_names){
  fu2_st <- paste(v, ".y", sep = "")
  bsl_st <- paste(v, ".x", sep = "")
  res <- icc(data.frame(dmri_both_data[c(fu2_st)],dmri_both_data[c(bsl_st)]), 
             model = 'twoway', type = 'agreement', unit = 'single')
  fa_iccs[counter, 1] <- res$value
  fa_iccs[counter, 2] <- res$lbound
  fa_iccs[counter, 3] <- res$ubound
  counter <- counter+1
}

counter <- 1
for (v in md_names){
  fu2_st <- paste(v, ".y", sep = "")
  bsl_st <- paste(v, ".x", sep = "")
  res <- icc(data.frame(dmri_both_data[c(fu2_st)],dmri_both_data[c(bsl_st)]), 
             model = 'twoway', type = 'agreement', unit = 'single')
  md_iccs[counter, 1] <- res$value
  md_iccs[counter, 2] <- res$lbound
  md_iccs[counter, 3] <- res$ubound
  counter <- counter+1
}

row.names(thick_iccs) <- thick_names
row.names(area_iccs) <- area_names
row.names(vol_iccs) <- vol_names
row.names(fa_iccs) <- fa_names
row.names(md_iccs) <- md_names

#get results from smri ICCs
final_table[6,1] <- mean(thick_iccs$ICC)
final_table[7,1] <- mean(area_iccs$ICC)
final_table[8,1] <- mean(vol_iccs$ICC)
final_table[9,1] <- mean(fa_iccs$ICC)
final_table[10,1] <- mean(md_iccs$ICC)

final_table[6,2] <- mean(thick_iccs$lower)
final_table[7,2] <- mean(area_iccs$lower)
final_table[8,2] <- mean(vol_iccs$lower)
final_table[9,2] <- mean(fa_iccs$lower)
final_table[10,2] <- mean(md_iccs$lower)

final_table[6,3] <- mean(thick_iccs$upper)
final_table[7,3] <- mean(area_iccs$upper)
final_table[8,3] <- mean(vol_iccs$upper)
final_table[9,3] <- mean(fa_iccs$upper)
final_table[10,3] <- mean(md_iccs$upper)

write.csv(final_table, "ICC_table.csv")

###########demographics###############
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

smri_fu2_data <- readRDS(file = "smri_fu2_data_win.rds")
dmri_fu2_data <- readRDS(file = "dmri_fu2_data_win.rds")

#name tables to use
demo_table <- '/home/max/Documents/ABCD_4.0/pdem02.txt'
acs_table <- '/home/max/Documents/ABCD_4.0/acspsw03.txt'
smri_table <- '/home/max/Documents/ABCD_4.0/abcd_smrip10201.txt'

demo <- read_abcd_tables(demo_table)
acs <- read_abcd_tables(acs_table)
smri <- read_abcd_tables(smri_table)
demo <- filter(demo, eventname == 'baseline_year_1_arm_1')
acs <- filter(acs, eventname == 'baseline_year_1_arm_1')
smri <- filter(smri, eventname == '2_year_follow_up_y_arm_1')

smri_fu2_data_dems <- merge(smri_fu2_data, acs[c('src_subject_id', 'race_ethnicity')], 
               by = c('src_subject_id'), all.x = TRUE)
smri_fu2_data_dems <- merge(smri_fu2_data_dems, demo[c('src_subject_id', 'sex', 'demo_ethn_v2',
                                            'demo_comb_income_v2', 'demo_prnt_ed_v2', 'demo_prtnr_ed_v2')], by = c('src_subject_id'), all.x = TRUE)
smri_fu2_data_dems <- merge(smri_fu2_data_dems, smri[c('src_subject_id', 'interview_age')], by = c('src_subject_id'), all.x = TRUE)
smri_fu2_data_dems$max_ed <- max(smri_fu2_data_dems$demo_prnt_ed_v2, smri_fu2_data_dems$demo_prtnr_ed_v2, na.rm = TRUE)
smri_fu2_data_dems[smri_fu2_data_dems == 777] <- NA
smri_fu2_data_dems[smri_fu2_data_dems == 999] <- NA
smri_fu2_data_dems[smri_fu2_data_dems == ''] <- NA
smri_fu2_data_dems$demo_prtnr_ed_v2[is.na(smri_fu2_data_dems$demo_prtnr_ed_v2)] <- 0
smri_fu2_data_dems$max_ed <- apply(smri_fu2_data_dems[c('demo_prnt_ed_v2','demo_prtnr_ed_v2')], 1, max)

smri_fu2_data_dems <- filter(smri_fu2_data_dems, complete.cases(smri_fu2_data_dems[c('race_ethnicity', 'sex', 'interview_age','demo_ethn_v2', 'demo_comb_income_v2', 'demo_prnt_ed_v2', 'demo_prtnr_ed_v2')]))

dmri_fu2_data_dems <- merge(dmri_fu2_data, smri_fu2_data_dems, by = c('src_subject_id','eventname'))

demographics<-data.frame(Age=rep(NA,2),Sex1=rep(NA,2),Race1=rep(NA,2),Race2=rep(NA,2),Race3=rep(NA,2),Race4=rep(NA,2),Race5=rep(NA,2),Income1=rep(NA,2),Income2=rep(NA,2),Income3=rep(NA,2),Income4=rep(NA,2),
                         Income5=rep(NA,2),Income6=rep(NA,2),Income7=rep(NA,2),Income8=rep(NA,2),
                         Income9=rep(NA,2),Income10=rep(NA,2),Education=rep(NA,2))

data=smri_fu2_data_dems
r = 1
demographics[r,1]<-mean(data$interview_age)
demographics[(r+1),1]<-sd(data$interview_age)
demographics[r,2]<-table(data['sex'])[1]
for (i in 1:5) {demographics[r,(2+i)]<-table(data['race_ethnicity'])[i]}
demographics[1,18]<-mean(data$max_ed)
demographics[2,18]<-sd(data$max_ed)
for (i in 1:10) {demographics[1,(7+i)]<-table(data['demo_comb_income_v2'])[i]}
demographics[(1+1),2]<-table(data['sex'])[1]/length(data$sex)
demographics[(1+1),(3+i)]<-table(data['race_ethnicity'])[i]/length(data$race_ethnicity)

is.num <- sapply(demographics, is.numeric)
demographics[is.num] <- lapply(demographics[is.num], round, 2)

percent <- function(x, digits = 0, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
demographics[2,3]<-percent(demographics$Race1[1]/6467)
demographics[2,4]<-percent(demographics$Race2[1]/6467)
demographics[2,5]<-percent(demographics$Race3[1]/6467)
demographics[2,6]<-percent(demographics$Race4[1]/6467)
demographics[2,7]<-percent(demographics$Race5[1]/6467)

demographics[2,8]<-percent(demographics$Income1[1]/6467)
demographics[2,9]<-percent(demographics$Income2[1]/6467)
demographics[2,10]<-percent(demographics$Income3[1]/6467)
demographics[2,11]<-percent(demographics$Income4[1]/6467)
demographics[2,12]<-percent(demographics$Income5[1]/6467)
demographics[2,13]<-percent(demographics$Income6[1]/6467)
demographics[2,14]<-percent(demographics$Income7[1]/6467)
demographics[2,15]<-percent(demographics$Income8[1]/6467)
demographics[2,16]<-percent(demographics$Income9[1]/6467)
demographics[2,17]<-percent(demographics$Income10[1]/6467)

library(data.table)
demographics_t <- transpose(demographics)
row.names(demographics_t)<-names(demographics)

write.csv(demographics_t,"/home/max/Dropbox/ABCD/UPPS_replication/demographics_upps_replication.csv")

