source('cleaning.R')

library(tidyverse)
library(jtools)
library(ggpmisc)
library(interactions)
library(mediation)

df_MD <- df 
df_MD$mean_MD_fornix_mask <- remove_outliers(df$mean_MD_fornix_mask)
df_MD <- df_MD %>% drop_na(mean_MD_fornix_mask)

#total effect, with age and gender covariates
MD_cohort_age_gender <- lm(mean_MD_fornix_mask ~ cohort + age + gender, df_MD)
summary(MD_cohort_age_gender)
#depression as mediator
cohort_depression <- lm(additional_hads_depression ~ cohort, df_MD)
summary(cohort_depression)
#effect of depression on total effect
MD_cohort_depression <- lm(mean_MD_fornix_mask ~ cohort + additional_hads_depression + gender + age, df_MD)
summary(MD_cohort_depression) #not mediated by depression
#anxiety as mediator
cohort_anxiety_age_gender <- lm(additional_hads_anxiety ~ cohort + age + gender, df_MD)
summary(cohort_anxiety_age_gender)
#effect of anxiety on total effect
MD_cohort_anxiety <- lm(mean_MD_fornix_mask ~ cohort + additional_hads_anxiety + gender + age, df_MD)
summary(MD_cohort_anxiety) #not mediated by anxiety
#story as mediator
cohort_story_age <- lm(story_d ~ cohort + age, df_MD)
summary(cohort_story_age)
#effect of story on total effect
MD_cohort_story <- lm(mean_MD_fornix_mask ~ cohort + story_d + gender + age, df_MD)
summary(MD_cohort_story) #not mediated by story

df_OD <- df 
df_OD$mean_OD_fornix_mask <- remove_outliers(df$mean_OD_fornix_mask)
df_OD <- df_OD %>% drop_na(mean_OD_fornix_mask)

#total effect, with age and gender covariates
OD_cohort_age_gender <- lm(mean_OD_fornix_mask ~ cohort + age + gender, df_OD)
summary(OD_cohort_age_gender)
#depression as mediator
cohort_depression <- lm(additional_hads_depression ~ cohort, df)
summary(cohort_depression)
#effect of depression on total effect
OD_cohort_depression <- lm(mean_OD_fornix_mask ~ cohort + additional_hads_depression + gender + age, df_OD)
summary(OD_cohort_depression) #not mediated by depression
#anxiety as mediator
cohort_anxiety_age_gender <- lm(additional_hads_anxiety ~ cohort + age + gender, df)
summary(cohort_anxiety_age_gender)
#effect of anxiety on total effect
OD_cohort_anxiety <- lm(mean_OD_fornix_mask ~ cohort + additional_hads_anxiety + gender + age, df_OD)
summary(OD_cohort_anxiety) #not mediated by anxiety
#story as mediator
cohort_story_age <- lm(story_d ~ cohort + age, df_OD)
summary(cohort_story_age)
#effect of story on total effect
OD_cohort_story <- lm(mean_OD_fornix_mask ~ cohort + story_d + gender + age, df_OD)
summary(OD_cohort_story) #not mediated by story


df_ICVF <- df 
df_ICVF$mean_ICVF_fornix_mask <- remove_outliers(df$mean_ICVF_fornix_mask)
df_ICVF <- df_ICVF %>% drop_na(mean_ICVF_fornix_mask)

#total effect, with age and gender covariates
ICVF_cohort_age_gender <- lm(mean_ICVF_fornix_mask ~ cohort + age + gender, df_ICVF)
summary(ICVF_cohort_age_gender)
#effect of depression on total effect
ICVF_cohort_depression <- lm(mean_ICVF_fornix_mask ~ cohort + additional_hads_depression + gender + age, df_ICVF)
summary(ICVF_cohort_depression) #not mediated by depression
#effect of anxiety on total effect
ICVF_cohort_anxiety <- lm(mean_ICVF_fornix_mask ~ cohort + additional_hads_anxiety + gender + age, df_ICVF)
summary(ICVF_cohort_anxiety) #not mediated by anxiety
#effect of story on total effect
ICVF_cohort_story <- lm(mean_ICVF_fornix_mask ~ cohort + story_d + gender + age, df_ICVF)
summary(ICVF_cohort_story) #not mediated by story
