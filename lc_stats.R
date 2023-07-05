source('cleaning.R')

library(tidyverse)
library(jtools)
library(ggpmisc)
library(officer)

lc_stats_pptx <- read_pptx()
layout_summary(lc_stats_pptx)

#select variables and remove outliers
df_FA <- df 
df_FA$mean_FA_lower_cingulum_mask <- remove_outliers(df$mean_FA_lower_cingulum_mask)
df_FA <- df_FA %>% drop_na(mean_FA_lower_cingulum_mask)

#does age improve cohort?
FA_cohort <- lm(mean_FA_lower_cingulum_mask ~ cohort, df_FA)
FA_cohort_age <- lm(mean_FA_lower_cingulum_mask ~ cohort + age, df_FA)
anova(FA_cohort, FA_cohort_age) #age improves cohort
#does gender improve best model above?
FA_cohort_age_gender <- lm(mean_FA_lower_cingulum_mask ~ cohort + gender + age, df_FA)
anova(FA_cohort_age, FA_cohort_age_gender) #gender improves cohort-age
#does removing age from best model above improve the model?
FA_cohort_gender <- lm(mean_FA_lower_cingulum_mask ~ cohort + gender, df_FA)
anova(FA_cohort_gender, FA_cohort_age_gender) #age improves cohort-gender
#cohort_age_gender is best model

summary(FA_cohort_age_gender) #differences between cohort not significant

#FA correlates with anxiety and depression?
FA_anxiety <- lm(mean_FA_lower_cingulum_mask ~ additional_hads_anxiety, df_FA)
FA_anxiety_age <- lm(mean_FA_lower_cingulum_mask ~ additional_hads_anxiety + age, df_FA)
anova(FA_anxiety, FA_anxiety_age)
FA_anxiety_age_gender <- lm(mean_FA_lower_cingulum_mask ~ additional_hads_anxiety + age + gender, df_FA)
anova(FA_anxiety_age, FA_anxiety_age_gender)
summary(FA_anxiety_age_gender) #no
FA_depression <- lm(mean_FA_lower_cingulum_mask ~ additional_hads_depression, df_FA)
FA_depression_age <- lm(mean_FA_lower_cingulum_mask ~ additional_hads_depression + age, df_FA)
anova(FA_depression, FA_depression_age)
FA_depression_age_gender <- lm(mean_FA_lower_cingulum_mask ~ additional_hads_depression + age + gender, df_FA)
anova(FA_depression_age, FA_depression_age_gender)
summary(FA_depression_age_gender) #yes

FA_depression_age_gender_plot <- effect_plot(FA_depression_age_gender, pred = additional_hads_depression, colors = 'CUD Bright', partial.residuals = TRUE, point.alpha = 1, interval = T) + 
  labs(x = "Depression (Mean Corrected HADS)", y = "Mean FA", title = "Bilateral Mean FA",
       subtitle = paste0("Corrected for Age and Gender \n",
                         "p = ",
                         signif(summary(FA_depression_age_gender)$coefficients[2,4], 2))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
FA_depression_age_gender_plot

lc_stats_pptx <- add_slide(lc_stats_pptx)
lc_stats_pptx <- ph_with(x = lc_stats_pptx, FA_depression_age_gender_plot, location = ph_location_type(type = "body"))

#FA correlates with memory?
FA_story_d <- lm(mean_FA_lower_cingulum_mask ~ story_d, df_FA)
FA_story_d_age <- lm(mean_FA_lower_cingulum_mask ~ story_d + age, df_FA)
anova(FA_story_d, FA_story_d_age)
FA_story_d_age_gender <- lm(mean_FA_lower_cingulum_mask ~ story_d + age + gender, df_FA)
anova(FA_story_d_age, FA_story_d_age_gender)
summary(FA_story_d_age_gender) #no
FA_mmse <- lm(mean_FA_lower_cingulum_mask ~ homeint_mmse_cal + age + gender, df_FA)
FA_mmse_age <- lm(mean_FA_lower_cingulum_mask ~ homeint_mmse_cal + age, df_FA)
anova(FA_mmse, FA_mmse_age)
FA_mmse_age_gender <- lm(mean_FA_lower_cingulum_mask ~ homeint_mmse_cal + age + gender, df_FA)
anova(FA_mmse_age, FA_mmse_age_gender)
summary(FA_mmse_age_gender) #no

#select variables and remove outliers
df_MD <- df 
df_MD$mean_MD_lower_cingulum_mask <- remove_outliers(df$mean_MD_lower_cingulum_mask)
df_MD <- df_MD %>% drop_na(mean_MD_lower_cingulum_mask)

#does age improve cohort?
MD_cohort <- lm(mean_MD_lower_cingulum_mask ~ cohort, df_MD)
summary(MD_cohort)
MD_cohort_age <- lm(mean_MD_lower_cingulum_mask ~ cohort + age, df_MD)
anova(MD_cohort, MD_cohort_age) #age improves cohort
#does gender improve best model above?
MD_cohort_age_gender <- lm(mean_MD_lower_cingulum_mask ~ cohort + age + gender, df_MD)
anova(MD_cohort_age, MD_cohort_age_gender) #gender improves cohort-age
#does removing age from best model above improve model?
MD_cohort_gender <- lm(mean_MD_lower_cingulum_mask ~ cohort + gender, df_MD)
anova(MD_cohort_gender, MD_cohort_age_gender) #age improves cohort_gender
summary(MD_cohort_age_gender) #no difference between cohorts

#MD correlates with anxiety and depression?
MD_anxiety <- lm(mean_MD_lower_cingulum_mask ~ additional_hads_anxiety, df_MD)
MD_anxiety_age <- lm(mean_MD_lower_cingulum_mask ~ additional_hads_anxiety + age, df_MD)
anova(MD_anxiety, MD_anxiety_age)
MD_anxiety_age_gender <- lm(mean_MD_lower_cingulum_mask ~ additional_hads_anxiety + age + gender, df_MD)
anova(MD_anxiety_age, MD_anxiety_age_gender)
summary(MD_anxiety_age) #no
MD_depression <- lm(mean_MD_lower_cingulum_mask ~ additional_hads_depression, df_MD)
MD_depression_age <- lm(mean_MD_lower_cingulum_mask ~ additional_hads_depression + age, df_MD)
anova(MD_depression, MD_depression_age)
MD_depression_age_gender <- lm(mean_MD_lower_cingulum_mask ~ additional_hads_depression + age + gender, df_MD)
anova(MD_depression_age, MD_depression_age_gender)
summary(MD_depression_age_gender) #no, after correcting for age and gender

#MD correlates with memory?
MD_story_d <- lm(mean_MD_lower_cingulum_mask ~ story_d, df_MD)
MD_story_d_age <- lm(mean_MD_lower_cingulum_mask ~ story_d + age, df_MD)
anova(MD_story_d, MD_story_d_age)
MD_story_d_age_gender <- lm(mean_MD_lower_cingulum_mask ~ story_d + age + gender, df_MD)
anova(MD_story_d_age, MD_story_d_age_gender)
summary(MD_story_d_age_gender) #no
MD_mmse <- lm(mean_MD_lower_cingulum_mask ~ homeint_mmse_cal, df_MD)
MD_mmse_age <- lm(mean_MD_lower_cingulum_mask ~ homeint_mmse_cal + age, df_MD)
anova(MD_mmse, MD_mmse_age)
MD_mmse_age_gender <- lm(mean_MD_lower_cingulum_mask ~ homeint_mmse_cal + age + gender, df_MD)
anova(MD_mmse_age, MD_mmse_age_gender)
summary(MD_mmse_age_gender) #no

#select variables and remove outliers
df_L1 <- df 
df_L1$mean_L1_lower_cingulum_mask <- remove_outliers(df$mean_L1_lower_cingulum_mask)
df_L1 <- df_L1 %>% drop_na(mean_L1_lower_cingulum_mask)

#does age improve cohort?
L1_cohort <- lm(mean_L1_lower_cingulum_mask ~ cohort, df_L1)
L1_cohort_age <- lm(mean_L1_lower_cingulum_mask ~ cohort + age, df_L1) 
anova(L1_cohort, L1_cohort_age) #age improves cohort
#does gender improve best model above?
L1_cohort_age_gender <- lm(mean_L1_lower_cingulum_mask ~ cohort + age + gender, df_L1)
anova(L1_cohort_age, L1_cohort_age_gender) #gender improves model
summary(L1_cohort_age_gender) #no difference in cohort

#L1 correlates with anxiety and depression?
L1_anxiety <- lm(mean_L1_lower_cingulum_mask ~ additional_hads_anxiety, df_L1)
L1_anxiety_age <- lm(mean_L1_lower_cingulum_mask ~ additional_hads_anxiety + age, df_L1)
anova(L1_anxiety, L1_anxiety_age)
L1_anxiety_age_gender <- lm(mean_L1_lower_cingulum_mask ~ additional_hads_anxiety + age + gender, df_L1)
anova(L1_anxiety_age, L1_anxiety_age_gender)
summary(L1_anxiety_age_gender) #no
L1_depression <- lm(mean_L1_lower_cingulum_mask ~ additional_hads_depression, df_L1)
L1_depression_age <- lm(mean_L1_lower_cingulum_mask ~ additional_hads_depression + age, df_L1)
anova(L1_depression, L1_depression_age)
L1_depression_age_gender <- lm(mean_L1_lower_cingulum_mask ~ additional_hads_depression + age + gender, df_L1)
anova(L1_depression_age, L1_depression_age_gender)
summary(L1_depression_age_gender) #no

#L1 correlates with memory?
L1_story_d <- lm(mean_L1_lower_cingulum_mask ~ story_d, df_L1)
L1_story_d_age <- lm(mean_L1_lower_cingulum_mask ~ story_d + age, df_L1)
anova(L1_story_d, L1_story_d_age)
L1_story_d_age_gender <- lm(mean_L1_lower_cingulum_mask ~ story_d + age + gender, df_L1)
anova(L1_story_d_age, L1_story_d_age_gender)
summary(L1_story_d_age_gender) #yes
L1_mmse <- lm(mean_L1_lower_cingulum_mask ~ homeint_mmse_cal, df_L1)
L1_mmse_age <- lm(mean_L1_lower_cingulum_mask ~ homeint_mmse_cal + age, df_L1)
anova(L1_mmse, L1_mmse_age)
L1_mmse_age_gender <- lm(mean_L1_lower_cingulum_mask ~ homeint_mmse_cal + age + gender, df_L1)
anova(L1_mmse_age, L1_mmse_age_gender)
summary(L1_mmse_age_gender) #no

L1_story_d_age_gender_plot <- effect_plot(L1_story_d_age_gender, pred = story_d, colors = 'CUD Bright', partial.residuals = TRUE, point.alpha = 1, interval = T) + 
  labs(x = "Mean Corrected Delayed Story Recall Score", y = "Mean AD", title = "Bilateral Mean AD",
       subtitle = paste0("Corrected for Age and Gender \n",
                         "p = ",
                         signif(summary(L1_story_d_age_gender)$coefficients[2,4], 2))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
L1_story_d_age_gender_plot

lc_stats_pptx <- add_slide(lc_stats_pptx)
lc_stats_pptx <- ph_with(x = lc_stats_pptx, L1_story_d_age_gender_plot, location = ph_location_type(type = "body"))

#select variables and remove outliers
df_RD <- df 
df_RD$mean_RD_lower_cingulum_mask <- remove_outliers(df$mean_RD_lower_cingulum_mask)
df_RD <- df_RD %>% drop_na(mean_RD_lower_cingulum_mask)

#does age improve cohort?
RD_cohort <- lm(mean_RD_lower_cingulum_mask ~ cohort, df_RD)
RD_cohort_age <- lm(mean_RD_lower_cingulum_mask ~ cohort + age, df_RD) 
anova(RD_cohort, RD_cohort_age) #age improves cohort
#does gender improve best model above?
RD_cohort_age_gender <- lm(mean_RD_lower_cingulum_mask ~ cohort + age + gender, df_RD)
anova(RD_cohort_age, RD_cohort_age_gender) #no improvement
summary(RD_cohort_age) #no difference in cohort

#RD correlates with anxiety and depression?
RD_anxiety <- lm(mean_RD_lower_cingulum_mask ~ additional_hads_anxiety, df_RD)
RD_anxiety_age <- lm(mean_RD_lower_cingulum_mask ~ additional_hads_anxiety + age, df_RD)
anova(RD_anxiety, RD_anxiety_age)
RD_anxiety_age_gender <- lm(mean_RD_lower_cingulum_mask ~ additional_hads_anxiety + age + gender, df_RD)
anova(RD_anxiety_age, RD_anxiety_age_gender)
summary(RD_anxiety_age) #no
RD_depression <- lm(mean_RD_lower_cingulum_mask ~ additional_hads_depression, df_RD)
RD_depression_age <- lm(mean_RD_lower_cingulum_mask ~ additional_hads_depression + age, df_RD)
anova(RD_depression, RD_depression_age)
RD_depression_age_gender <- lm(mean_RD_lower_cingulum_mask ~ additional_hads_depression + age + gender, df_RD)
anova(RD_depression_age, RD_depression_age_gender)
summary(RD_depression_age) #yes
summary(RD_depression_age_gender) #yes

RD_depression_age_plot <- effect_plot(RD_depression_age, pred = additional_hads_depression, colors = 'CUD Bright', partial.residuals = TRUE, point.alpha = 1, interval = T) + 
  labs(x = "Depression (Mean Corrected HADS)", y = "Mean RD", title = "Bilateral Mean RD",
       subtitle = paste0("Corrected for Age \n",
                         "p = ",
                         signif(summary(RD_depression_age)$coefficients[2,4], 2))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
RD_depression_age_plot

lc_stats_pptx <- add_slide(lc_stats_pptx)
lc_stats_pptx <- ph_with(x = lc_stats_pptx, RD_depression_age_plot, location = ph_location_type(type = "body"))

#RD correlates with memory?
RD_story_d <- lm(mean_RD_lower_cingulum_mask ~ story_d, df_RD)
RD_story_d_age <- lm(mean_RD_lower_cingulum_mask ~ story_d + age, df_RD)
anova(RD_story_d, RD_story_d_age)
RD_story_d_age_gender <- lm(mean_RD_lower_cingulum_mask ~ story_d + age + gender, df_RD)
anova(RD_story_d_age, RD_story_d_age_gender)
summary(RD_story_d_age) #no
RD_mmse <- lm(mean_RD_lower_cingulum_mask ~ homeint_mmse_cal, df_RD)
RD_mmse_age <- lm(mean_RD_lower_cingulum_mask ~ homeint_mmse_cal + age, df_RD)
anova(RD_mmse, RD_mmse_age)
RD_mmse_age_gender <- lm(mean_RD_lower_cingulum_mask ~ homeint_mmse_cal + age + gender, df_RD)
anova(RD_mmse_age, RD_mmse_age_gender)
summary(RD_mmse_age) #no

#select variables and remove outliers
df_OD <- df 
df_OD$mean_OD_lower_cingulum_mask <- remove_outliers(df$mean_OD_lower_cingulum_mask)
df_OD <- df_OD %>% drop_na(mean_OD_lower_cingulum_mask)

#does age improve cohort?
OD_cohort <- lm(mean_OD_lower_cingulum_mask ~ cohort, df_OD)
OD_cohort_age <- lm(mean_OD_lower_cingulum_mask ~ cohort + age, df_OD)
anova(OD_cohort, OD_cohort_age) #age improves cohort
OD_cohort_age_gender <- lm(mean_OD_lower_cingulum_mask ~ cohort + age + gender, df_OD)
anova(OD_cohort_age, OD_cohort_age_gender) #gender improves age-cohort

summary(OD_cohort_age_gender) #no difference in cohort

#OD correlates with anxiety and depression?
OD_anxiety <- lm(mean_OD_lower_cingulum_mask ~ additional_hads_anxiety, df_OD)
OD_anxiety_age <- lm(mean_OD_lower_cingulum_mask ~ additional_hads_anxiety + age, df_OD)
anova(OD_anxiety, OD_anxiety_age)
OD_anxiety_age_gender <- lm(mean_OD_lower_cingulum_mask ~ additional_hads_anxiety + age + gender, df_OD)
anova(OD_anxiety_age, OD_anxiety_age_gender)
summary(OD_anxiety_age_gender) #no
OD_depression <- lm(mean_OD_lower_cingulum_mask ~ additional_hads_depression, df_OD)
OD_depression_age <- lm(mean_OD_lower_cingulum_mask ~ additional_hads_depression + age, df_OD)
anova(OD_depression, OD_depression_age)
OD_depression_age_gender <- lm(mean_OD_lower_cingulum_mask ~ additional_hads_depression + age + gender, df_OD)
anova(OD_depression_age, OD_depression_age_gender)
summary(OD_depression_age_gender) #no

#OD correlates with memory?
OD_story_d <- lm(mean_OD_lower_cingulum_mask ~ story_d, df_OD)
OD_story_d_age <- lm(mean_OD_lower_cingulum_mask ~ story_d + age, df_OD)
anova(OD_story_d, OD_story_d_age)
OD_story_d_age_gender <- lm(mean_OD_lower_cingulum_mask ~ story_d + age + gender, df_OD)
anova(OD_story_d_age, OD_story_d_age_gender)
summary(OD_story_d_age_gender) #yes
OD_mmse <- lm(mean_OD_lower_cingulum_mask ~ homeint_mmse_cal, df_OD)
OD_mmse_age <- lm(mean_OD_lower_cingulum_mask ~ homeint_mmse_cal + age, df_OD)
anova(OD_mmse, OD_mmse_age)
OD_mmse_age_gender <- lm(mean_OD_lower_cingulum_mask ~ homeint_mmse_cal + age + gender, df_OD)
anova(OD_mmse_age, OD_mmse_age_gender)
summary(OD_mmse_age_gender) #no

OD_story_d_age_gender_plot <- effect_plot(OD_story_d_age_gender, pred = story_d, colors = 'CUD Bright', partial.residuals = TRUE, point.alpha = 1, interval = T) + 
  labs(x = "Mean Corrected Delayed Story Recall Score", y = "Mean OD", title = "Bilateral Mean OD",
       subtitle = paste0("Corrected for Age and Gender \n",
                         "p = ",
                         signif(summary(OD_story_d_age_gender)$coefficients[2,4], 2))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
OD_story_d_age_gender_plot

lc_stats_pptx <- add_slide(lc_stats_pptx)
lc_stats_pptx <- ph_with(x = lc_stats_pptx, OD_story_d_age_gender_plot, location = ph_location_type(type = "body"))

#select variables and remove outliers
df_ISOVF <- df 
df_ISOVF$mean_ISOVF_lower_cingulum_mask <- remove_outliers(df$mean_ISOVF_lower_cingulum_mask)
df_ISOVF <- df_ISOVF %>% drop_na(mean_ISOVF_lower_cingulum_mask)

#does age improve cohort?
ISOVF_cohort <- lm(mean_ISOVF_lower_cingulum_mask ~ cohort, df_ISOVF)
ISOVF_cohort_age <- lm(mean_ISOVF_lower_cingulum_mask ~ cohort + age, df_ISOVF)
anova(ISOVF_cohort, ISOVF_cohort_age) #age improves cohort
ISOVF_cohort_age_gender <- lm(mean_ISOVF_lower_cingulum_mask ~ cohort + age + gender, df_ISOVF)
anova(ISOVF_cohort_age, ISOVF_cohort_age_gender) #gender improves age-cohort

summary(ISOVF_cohort_age_gender) #no difference in cohort

#ISOVF correlates with anxiety and depression?
ISOVF_anxiety <- lm(mean_ISOVF_lower_cingulum_mask ~ additional_hads_anxiety, df_ISOVF)
ISOVF_anxiety_age <- lm(mean_ISOVF_lower_cingulum_mask ~ additional_hads_anxiety + age, df_ISOVF)
anova(ISOVF_anxiety, ISOVF_anxiety_age)
ISOVF_anxiety_age_gender <- lm(mean_ISOVF_lower_cingulum_mask ~ additional_hads_anxiety + age + gender, df_ISOVF)
anova(ISOVF_anxiety_age, ISOVF_anxiety_age_gender)
summary(ISOVF_anxiety_age_gender) #no
ISOVF_depression <- lm(mean_ISOVF_lower_cingulum_mask ~ additional_hads_depression, df_ISOVF)
ISOVF_depression_age <- lm(mean_ISOVF_lower_cingulum_mask ~ additional_hads_depression + age, df_ISOVF)
anova(ISOVF_depression, ISOVF_depression_age)
ISOVF_depression_age_gender <- lm(mean_ISOVF_lower_cingulum_mask ~ additional_hads_depression + age + gender, df_ISOVF)
anova(ISOVF_depression_age, ISOVF_depression_age_gender)
summary(ISOVF_depression_age_gender) #no

#ISOVF correlates with memory?
ISOVF_story_d <- lm(mean_ISOVF_lower_cingulum_mask ~ story_d, df_ISOVF)
ISOVF_story_d_age <- lm(mean_ISOVF_lower_cingulum_mask ~ story_d + age, df_ISOVF)
anova(ISOVF_story_d, ISOVF_story_d_age)
ISOVF_story_d_age_gender <- lm(mean_ISOVF_lower_cingulum_mask ~ story_d + age + gender, df_ISOVF)
anova(ISOVF_story_d_age, ISOVF_story_d_age_gender)
summary(ISOVF_story_d_age_gender) #no
ISOVF_mmse <- lm(mean_ISOVF_lower_cingulum_mask ~ homeint_mmse_cal, df_ISOVF)
ISOVF_mmse_age <- lm(mean_ISOVF_lower_cingulum_mask ~ homeint_mmse_cal + age, df_ISOVF)
anova(ISOVF_mmse, ISOVF_mmse_age)
ISOVF_mmse_age_gender <- lm(mean_ISOVF_lower_cingulum_mask ~ homeint_mmse_cal + age + gender, df_ISOVF)
anova(ISOVF_mmse_age, ISOVF_mmse_age_gender)
summary(ISOVF_mmse_age_gender) #no

#select variables and remove outliers
df_ICVF <- df 
df_ICVF$mean_ICVF_lower_cingulum_mask <- remove_outliers(df$mean_ICVF_lower_cingulum_mask)
df_ICVF <- df_ICVF %>% drop_na(mean_ICVF_lower_cingulum_mask)

#does age improve cohort?
ICVF_cohort <- lm(mean_ICVF_lower_cingulum_mask ~ cohort, df_ICVF)
ICVF_cohort_age <- lm(mean_ICVF_lower_cingulum_mask ~ cohort + age, df_ICVF)
anova(ICVF_cohort, ICVF_cohort_age) #age improves cohort
ICVF_cohort_age_gender <- lm(mean_ICVF_lower_cingulum_mask ~ cohort + age + gender, df_ICVF)
anova(ICVF_cohort_age, ICVF_cohort_age_gender) #gender improves age-cohort

summary(ICVF_cohort_age_gender) #no difference in cohort

#ICVF correlates with anxiety and depression?
ICVF_anxiety <- lm(mean_ICVF_lower_cingulum_mask ~ additional_hads_anxiety, df_ICVF)
ICVF_anxiety_age <- lm(mean_ICVF_lower_cingulum_mask ~ additional_hads_anxiety + age, df_ICVF)
anova(ICVF_anxiety, ICVF_anxiety_age)
ICVF_anxiety_age_gender <- lm(mean_ICVF_lower_cingulum_mask ~ additional_hads_anxiety + age + gender, df_ICVF)
anova(ICVF_anxiety_age, ICVF_anxiety_age_gender)
summary(ICVF_anxiety_age_gender) #no
ICVF_depression <- lm(mean_ICVF_lower_cingulum_mask ~ additional_hads_depression, df_ICVF)
ICVF_depression_age <- lm(mean_ICVF_lower_cingulum_mask ~ additional_hads_depression + age, df_ICVF)
anova(ICVF_depression, ICVF_depression_age)
ICVF_depression_age_gender <- lm(mean_ICVF_lower_cingulum_mask ~ additional_hads_depression + age + gender, df_ICVF)
anova(ICVF_depression_age, ICVF_depression_age_gender)
summary(ICVF_depression_age_gender) #no

#ICVF correlates with memory?
ICVF_story_d <- lm(mean_ICVF_lower_cingulum_mask ~ story_d, df_ICVF)
ICVF_story_d_age <- lm(mean_ICVF_lower_cingulum_mask ~ story_d + age, df_ICVF)
anova(ICVF_story_d, ICVF_story_d_age)
ICVF_story_d_age_gender <- lm(mean_ICVF_lower_cingulum_mask ~ story_d + age + gender, df_ICVF)
anova(ICVF_story_d_age, ICVF_story_d_age_gender)
summary(ICVF_story_d_age_gender) #yes
ICVF_mmse <- lm(mean_ICVF_lower_cingulum_mask ~ homeint_mmse_cal, df_ICVF)
ICVF_mmse_age <- lm(mean_ICVF_lower_cingulum_mask ~ homeint_mmse_cal + age, df_ICVF)
anova(ICVF_mmse, ICVF_mmse_age)
ICVF_mmse_age_gender <- lm(mean_ICVF_lower_cingulum_mask ~ homeint_mmse_cal + age + gender, df_ICVF)
anova(ICVF_mmse_age, ICVF_mmse_age_gender)
summary(ICVF_mmse_age_gender) #no

ICVF_story_d_age_gender_plot <- effect_plot(ICVF_story_d_age_gender, pred = story_d, colors = 'CUD Bright', partial.residuals = TRUE, point.alpha = 1, interval = T) + 
  labs(x = "Mean Corrected Delayed Story Recall Score", y = "Mean ICVF", title = "Bilateral Mean ICVF",
       subtitle = paste0("Corrected for Age and Gender \n",
                         "p = ",
                         signif(summary(ICVF_story_d_age_gender)$coefficients[2,4], 2))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ICVF_story_d_age_gender_plot

lc_stats_pptx <- add_slide(lc_stats_pptx)
lc_stats_pptx <- ph_with(x = lc_stats_pptx, ICVF_story_d_age_gender_plot, location = ph_location_type(type = "body"))

print(lc_stats_pptx, target = "lc_stats.pptx")

#Gender effect size (per Henry's question)
library(lsr)
FA_male <- df_FA %>% filter(gender == "male")
FA_female <- df_FA %>% filter(gender == "female")
cohensD(FA_male$mean_FA_lower_cingulum_mask, FA_female$mean_FA_lower_cingulum_mask)
MD_male <- df_MD %>% filter(gender == "male")
MD_female <- df_MD %>% filter(gender == "female")
cohensD(MD_male$mean_MD_lower_cingulum_mask, MD_female$mean_MD_lower_cingulum_mask)
L1_male <- df_L1 %>% filter(gender == "male")
L1_female <- df_L1 %>% filter(gender == "female")
cohensD(L1_male$mean_L1_lower_cingulum_mask, L1_female$mean_L1_lower_cingulum_mask)
RD_male <- df_RD %>% filter(gender == "male")
RD_female <- df_RD %>% filter(gender == "female")
cohensD(RD_male$mean_RD_lower_cingulum_mask, RD_female$mean_RD_lower_cingulum_mask)
