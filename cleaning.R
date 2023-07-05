library(tidyverse)

df = read_tsv('standard_data.tsv')
available_data = read_tsv('homeinterview_data.tsv')
rt_choice <- read_tsv('RTchoice_summary.tsv') %>% select(CCID, Include_participant, RTmean_all) %>%
  rename(RTmean_choice = RTmean_all) %>%
  filter(Include_participant == 1) %>%
  select(-Include_participant)
rt_simple <- read_tsv('RTsimple_summary.tsv') %>%
  select(CCID, Include_participant, RTmean) %>%
  rename(RTmean_simple = RTmean) %>%
  filter(Include_participant == 1) %>%
  select(-Include_participant)
emotional_mem <- read_tsv('EmotionalMemory_summary.tsv', col_names = T) %>%
  select(CCID, ObjPrPos, ObjPrNeu, ObjPrNeg, ValPrPos, ValPrNeu, ValPrNeg)
df = full_join(df, available_data)
df = left_join(df, rt_choice)
df = left_join(df, rt_simple)
df = left_join(df, emotional_mem)

df$SCD <- df$homeint_v230
df$SCD[df$SCD == 1] <- FALSE
df$SCD[df$SCD == 2] <- TRUE
df$SCD[is.na(df$SCD)] <- FALSE

df_old <- df %>% filter(Age > 55)

df_old <- df_old %>% 
  rename('participant_id'='CCID', 'gender_text'='Sex') %>% rename_with(~ tolower(.x))

#import dwi participant list and possible data issues, then join with df_old
#dwi_participants.tsv is from source_materials/imaging/dwi/participants.tsv
dwi <- read_tsv('dwi_participants.tsv') %>% select('participant_id', 'gender_code', 'tiv_cubicmm')
dwi$participant_id <- gsub('sub-', '', dwi$participant_id)
#data issues from https://camcan-archive.mrc-cbu.cam.ac.uk/dataaccess/issues.php
dwi_data_issues <- read_csv('dwi_data_issues.csv')
dwi <- left_join(dwi, dwi_data_issues)
df_old_dwi <- inner_join(df_old, dwi)
sum(df_old_dwi$scd)

df_old_dwi_bad <- df_old_dwi %>% filter(!is.na(data_qual_notes))
sum(df_old_dwi_bad$scd, na.rm = T)

#there are 325 participants over age 55 with DWI available. 127 are SCD. 8 of them have data quality issues
#with DWI, and 1 of the participants with DWI data qual issues is SCD.

df_old_dwi <- df_old_dwi %>% arrange(scd, participant_id)
df_old_dwi <- df_old_dwi %>%
  mutate(additional_hads_depression = na_if(additional_hads_depression, 9999))

#mean center all non-imaging and non-percentage numeric variables
df_old_dwi_centered_num <- df_old_dwi %>% 
  select(where(is.numeric)) %>% select(-scd, -mt_tr, -gender_code, -tiv_cubicmm,
                                       -objprpos, -objprneu, -objprneg, -valprpos, -valprneu, -valprneg) %>%
  scale(.,scale = F) 
df_old_dwi_centered <- df_old_dwi 
df_old_dwi_centered[,colnames(df_old_dwi_centered_num)] <- df_old_dwi_centered_num

# using tables generated from MatLab tbss_means
matlab_folder <- "/Volumes/Research/lazarm03lab/labspace/AD/camcan995/derivatives/MatLab/tbss_means"
csv_files <- list.files(matlab_folder, ".csv$")
file.copy(file.path(matlab_folder, csv_files), getwd())

scd_table <- read_csv('scd_table.csv')
scd_table$cohort <- 'scd'
names(scd_table) <- sub('scd_', '', names(scd_table))
scd_table_l_lower_cingulum <- read_csv('scd_table_l_lower_cingulm.csv')
scd_table_l_lower_cingulum$cohort <- 'scd'
names(scd_table_l_lower_cingulum) <- sub('scd_', '', names(scd_table_l_lower_cingulum))
ctl_table <- read_csv('ctl_table.csv')
ctl_table$cohort <- 'ctl'
names(ctl_table) <- sub('ctl_', '', names(ctl_table))
df <- rbind(ctl_table, scd_table)
ctl_table_l_lower_cingulum <- read_csv('ctl_table_l_lower_cingulum.csv')
ctl_table_l_lower_cingulum$cohort <- 'ctl'
names(ctl_table_l_lower_cingulum) <- sub('ctl_', '', names(ctl_table_l_lower_cingulum))
df_l_lower_cingulum <- rbind(ctl_table_l_lower_cingulum, scd_table_l_lower_cingulum)
df <- cbind(df, df_l_lower_cingulum)
duplicated_columns <- duplicated(colnames(df))
df <- df[!duplicated_columns]
# df <- df %>% relocate(cohort, .after = last_col())
df <- df %>% select(cohort, everything())
df <- cbind(df_old_dwi_centered, df) %>% rename(gender = gender_text, story_d = homeint_storyrecall_d)
df$gender <- tolower(df$gender)

remove_outliers <- function(x, na.rm = TRUE) 
{
  ## Find 25% and 75% Quantiles using inbuild function
  quant <- quantile(x, probs=c(.25, .75), na.rm = na.rm)
  
  ## Find Interquantile range and multiply it by 1.5 
  ## to derive factor for range calculation
  H <- 1.5 * IQR(x, na.rm = na.rm)
  
  y <- x
  
  ## fill the outlier elements with NA
  y[x < (quant[1] - H)] <- NA
  y[x > (quant[2] + H)] <- NA
  
  y
}