source('imaging_available.R')
source('cleaning.R')
library(tidyverse)
library(arsenal)


df_old_dwi <- df_old_dwi %>% 
  select(participant_id, scd, age, gender_text, 
         homeint_storyrecall_i, homeint_storyrecall_d, 
         additional_hads_anxiety, additional_hads_depression
         ) %>%
  
# df_old_dwi <- read_csv('old_dwi.csv') %>% 
#   select(participant_id, age, gender_text, scd) %>%
#   left_join(.,df_old_cog) %>%
  rename('Age' = 'age', 'Sex' = 'gender_text')
df_old_dwi$scd <- factor(df_old_dwi$scd,
                         levels = c(1, 0),
                         labels = c('SCD', 'Control'))
attr(df_old_dwi$scd, 'label') <- 'Subjective Cognitive Decline'
attr(df_old_dwi$homeint_storyrecall_i, 'label') <- 'Story Recall Immediate'
attr(df_old_dwi$homeint_storyrecall_d, 'label') <- 'Story Recall Delayed'
attr(df_old_dwi$additional_hads_anxiety, 'label') <- 'Anxiety (HADS)'
attr(df_old_dwi$additional_hads_depression, 'label') <- 'Depression (HADS)'
results <- "asis"
# demos <- tableby(scd ~ Sex + Age + homeint_storyrecall_i + homeint_storyrecall_d, data = df_old_dwi)
demos <- tableby(scd ~ Sex + Age + additional_hads_anxiety + additional_hads_depression, data = df_old_dwi)
summary(demos, text = TRUE)
write2word(demos, "demo_table.docx")
