source('cleaning.R')

library(tidyverse)
library(jtools)
library(ggpmisc)
library(interactions)
library(mediation)

#select variables and remove outliers
df_FA <- df 
df_FA$mean_FA_lower_cingulum_mask <- remove_outliers(df$mean_FA_lower_cingulum_mask)
df_FA <- df_FA %>% drop_na(mean_FA_lower_cingulum_mask)
FA_cohort_age_gender <- lm(mean_FA_lower_cingulum_mask ~ cohort + age + gender, df_FA)
summary(FA_cohort_age_gender)
#depression as mediator
cohort_depression <- lm(additional_hads_depression ~ cohort + age + gender, df_FA)
summary(cohort_depression)
FA_cohort_depression_gender_age <- lm(mean_FA_lower_cingulum_mask ~ cohort + additional_hads_depression + gender + age, df_FA)
summary(FA_cohort_depression_gender_age) #no
#anxiety as mediator
cohort_anxiety <- lm(additional_hads_anxiety ~ cohort + age + gender, df_FA)
summary(cohort_anxiety)
FA_cohort_anxiety <- lm(mean_FA_lower_cingulum_mask ~ cohort + additional_hads_anxiety + gender + age, df_FA)
summary(FA_cohort_anxiety) #no
#story_d as mediator
cohort_story <- lm(story_d ~ cohort + age + gender, df_FA)
summary(cohort_story)
FA_cohort_story <- lm(mean_FA_lower_cingulum_mask ~ cohort + story_d + gender + age, df_FA)
summary(FA_cohort_story) #no
#mmse as mediator
cohort_mmse <- lm(homeint_mmse_cal ~ cohort + age + gender, df_FA)
summary(cohort_mmse) #no

df_MD <- df 
df_MD$mean_MD_lower_cingulum_mask <- remove_outliers(df$mean_MD_lower_cingulum_mask)
df_MD <- df_MD %>% drop_na(mean_MD_lower_cingulum_mask)
MD_cohort_age_gender <- lm(mean_MD_lower_cingulum_mask ~ cohort + age + gender, df_MD)
summary(MD_cohort_age_gender)
#depression as mediator
cohort_depression <- lm(additional_hads_depression ~ cohort + age + gender, df_MD)
summary(cohort_depression)
#effect of depression on total effect
MD_cohort_depression <- lm(mean_MD_lower_cingulum_mask ~ cohort + additional_hads_depression + gender + age, df_MD)
summary(MD_cohort_depression) #not mediated by depression
#anxiety has an interaction effect
#story as mediator
cohort_story <- lm(story_d ~ cohort + age + gender, df_MD)
summary(cohort_story)
#effect of story on total effect
MD_cohort_story <- lm(mean_MD_lower_cingulum_mask ~ cohort + story_d + gender + age, df_MD)
summary(MD_cohort_story) #not mediated by story

#select variables and remove outliers
df_L1 <- df 
df_L1$mean_L1_lower_cingulum_mask <- remove_outliers(df$mean_L1_lower_cingulum_mask)
df_L1 <- df_L1 %>% drop_na(mean_L1_lower_cingulum_mask)
L1_cohort_age_gender <- lm(mean_L1_lower_cingulum_mask ~ cohort + age + gender, df_L1)
summary(L1_cohort_age_gender)
#depression as mediator
cohort_depression <- lm(additional_hads_depression ~ cohort + age + gender, df_L1)
summary(cohort_depression)
L1_cohort_depression_gender_age <- lm(mean_L1_lower_cingulum_mask ~ cohort + additional_hads_depression + gender + age, df_L1)
summary(L1_cohort_depression_gender_age) #no
#anxiety as mediator
cohort_anxiety <- lm(additional_hads_anxiety ~ cohort + age + gender, df_L1)
summary(cohort_anxiety)
L1_cohort_anxiety <- lm(mean_L1_lower_cingulum_mask ~ cohort + additional_hads_anxiety + gender + age, df_L1)
summary(L1_cohort_anxiety) #no
#story_d has interaction effect
#mmse as mediator
cohort_mmse <- lm(homeint_mmse_cal ~ cohort + age + gender, df_L1)
summary(cohort_mmse) #no

#select variables and remove outliers
df_RD <- df 
df_RD$mean_RD_lower_cingulum_mask <- remove_outliers(df$mean_RD_lower_cingulum_mask)
df_RD <- df_RD %>% drop_na(mean_RD_lower_cingulum_mask)
RD_cohort_age_gender <- lm(mean_RD_lower_cingulum_mask ~ cohort + age + gender, df_RD)
summary(RD_cohort_age_gender)
#depression as mediator
cohort_depression <- lm(additional_hads_depression ~ cohort + age + gender, df_RD)
summary(cohort_depression)
RD_cohort_depression <- lm(mean_RD_lower_cingulum_mask ~ cohort + additional_hads_depression + gender + age, df_RD)
summary(RD_cohort_depression) #no
#anxiety as mediator
cohort_anxiety <- lm(additional_hads_anxiety ~ cohort + age + gender, df_RD)
summary(cohort_anxiety)
RD_cohort_anxiety <- lm(mean_RD_lower_cingulum_mask ~ cohort + additional_hads_anxiety + gender + age, df_RD)
summary(RD_cohort_anxiety) #no
#story_d as mediator
cohort_story <- lm(story_d ~ cohort + age + gender, df_RD)
summary(cohort_story)
RD_cohort_story <- lm(mean_RD_lower_cingulum_mask ~ cohort + story_d + gender + age, df_RD)
summary(RD_cohort_story) #no
#mmse as mediator
cohort_mmse <- lm(homeint_mmse_cal ~ cohort + age + gender, df_RD)
summary(cohort_mmse) #no

df_OD <- df 
df_OD$mean_OD_lower_cingulum_mask <- remove_outliers(df$mean_OD_lower_cingulum_mask)
df_OD <- df_OD %>% drop_na(mean_OD_lower_cingulum_mask)
OD_cohort_age_gender <- lm(mean_OD_lower_cingulum_mask ~ cohort + age + gender, df_OD)
summary(OD_cohort_age_gender)
#depression as mediator
cohort_depression <- lm(additional_hads_depression ~ cohort + age + gender, df)
summary(cohort_depression)
#effect of depression on total effect
OD_cohort_depression <- lm(mean_OD_lower_cingulum_mask ~ cohort + additional_hads_depression + gender + age, df_OD)
summary(OD_cohort_depression) #not mediated by depression
#anxiety as mediator
cohort_anxiety <- lm(additional_hads_anxiety ~ cohort + age + gender, df)
summary(cohort_anxiety)
#effect of anxiety on total effect
OD_cohort_anxiety <- lm(mean_OD_lower_cingulum_mask ~ cohort + additional_hads_anxiety + gender + age, df_OD)
summary(OD_cohort_anxiety) #not mediated by anxiety
#story as mediator
cohort_story <- lm(story_d ~ cohort + age + gender, df_OD)
summary(cohort_story)
#effect of story on total effect
OD_cohort_story <- lm(mean_OD_lower_cingulum_mask ~ cohort + story_d + gender + age, df_OD)
summary(OD_cohort_story) #story mediates relationship between cohort and OD, OD mediates relationship between cohort and story
cohort_mmse <- lm(homeint_mmse_cal ~ cohort + age + gender, df_OD)
summary(cohort_mmse) #no

story_OD_mediation <- mediate(cohort_story, OD_cohort_story, treat = 'cohort',
                              mediator = 'story_d', boot = T)
summary(story_OD_mediation)

df_ISOVF <- df 
df_ISOVF$mean_ISOVF_lower_cingulum_mask <- remove_outliers(df$mean_ISOVF_lower_cingulum_mask)
df_ISOVF <- df_ISOVF %>% drop_na(mean_ISOVF_lower_cingulum_mask)
ISOVF_cohort_age_gender <- lm(mean_ISOVF_lower_cingulum_mask ~ cohort + age + gender, df_ISOVF)
summary(ISOVF_cohort_age_gender)
#depression as mediator
cohort_depression <- lm(additional_hads_depression ~ cohort + age + gender, df)
summary(cohort_depression)
#effect of depression on total effect
ISOVF_cohort_depression <- lm(mean_ISOVF_lower_cingulum_mask ~ cohort + additional_hads_depression + gender + age, df_ISOVF)
summary(ISOVF_cohort_depression) #not mediated by depression
#anxiety as mediator
cohort_anxiety <- lm(additional_hads_anxiety ~ cohort + age + gender, df)
summary(cohort_anxiety)
#effect of anxiety on total effect
ISOVF_cohort_anxiety <- lm(mean_ISOVF_lower_cingulum_mask ~ cohort + additional_hads_anxiety + gender + age, df_ISOVF)
summary(ISOVF_cohort_anxiety) #not mediated by anxiety
#story has interaction effect
cohort_mmse <- lm(homeint_mmse_cal ~ cohort + age + gender, df_ISOVF)
summary(cohort_mmse) #no

df_ICVF <- df 
df_ICVF$mean_ICVF_lower_cingulum_mask <- remove_outliers(df$mean_ICVF_lower_cingulum_mask)
df_ICVF <- df_ICVF %>% drop_na(mean_ICVF_lower_cingulum_mask)
ICVF_cohort_age_gender <- lm(mean_ICVF_lower_cingulum_mask ~ cohort + age + gender, df_ICVF)
summary(ICVF_cohort_age_gender)
#effect of depression on total effect
ICVF_cohort_depression <- lm(mean_ICVF_lower_cingulum_mask ~ cohort + additional_hads_depression + gender + age, df_ICVF)
summary(ICVF_cohort_depression) #not mediated by depression
#anxiety has interaction effect
#story has interaction effect

