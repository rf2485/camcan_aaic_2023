source('cleaning.R')

library(tidyverse)
library(jtools)
library(ggpmisc)
library(interactions)

#select variables and remove outliers
df_FA <- df 
df_FA$mean_FA_fornix_mask <- remove_outliers(df$mean_FA_fornix_mask)
df_FA <- df_FA %>% drop_na(mean_FA_fornix_mask)

#cohort interaction effects
FA_cohort_int_age_gender <- lm(mean_FA_fornix_mask ~ cohort * age + gender, df_FA)
summary(FA_cohort_int_age_gender) #no
FA_cohort_int_gender_age <- lm(mean_FA_fornix_mask ~ cohort * gender + age, df_FA)
summary(FA_cohort_int_gender_age) #no
FA_cohort_int_anxiety_age_gender <- lm(mean_FA_fornix_mask ~ cohort * additional_hads_anxiety + gender + age, df_FA)
summary(FA_cohort_int_anxiety_age_gender) #no
FA_cohort_int_depression_age_gender <- lm(mean_FA_fornix_mask ~ cohort * additional_hads_depression + gender + age, df_FA)
summary(FA_cohort_int_depression_age_gender) #no
FA_cohort_int_story_d_age_gender <- lm(mean_FA_fornix_mask ~ cohort * story_d + gender + age, df_FA)
summary(FA_cohort_int_story_d_age_gender) #yes
FA_cohort_int_mmse_age_gender <- lm(mean_FA_fornix_mask ~ cohort * homeint_mmse_cal + age + gender, df_FA)
summary(FA_cohort_int_mmse_age_gender) #no

ctl_FA <- df_FA %>% filter(cohort == 'ctl')
scd_FA <- df_FA %>% filter(cohort == 'scd')
ctl_FA_story_age_gender <- lm(mean_FA_fornix_mask ~ story_d + age + gender, ctl_FA)
scd_FA_story_age_gender <- lm(mean_FA_fornix_mask ~ story_d + age + gender, scd_FA)

interact_plot(FA_cohort_int_story_d_age_gender, pred = story_d, modx = cohort, 
              partial.residuals = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
              modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Delayed Story Recall Score", y = "Mean FA", title = "Bilateral Mean FA",
       subtitle = paste0("Corrected for Age and Gender \n",
                         "interaction p = ",
                         signif(summary(FA_cohort_int_story_d_age_gender)$coefficients[5,4], 2)
       )
  ) +
  geom_text(aes(x = -10, y = 0.42, vjust = -1, 
                label = paste0("p = ", signif(summary(ctl_FA_story_age_gender)$coefficients[2,4], 2)), 
                color = "Control"), show.legend = F) +
  geom_text(aes(x = -10, y = 0.42, vjust = 1, 
                label = paste0("p = ", signif(summary(scd_FA_story_age_gender)$coefficients[2,4], 2)), 
                color = "SCD"), show.legend = F) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#anxiety interaction effects
FA_anxiety_int_age_gender <- lm(mean_FA_fornix_mask ~ additional_hads_anxiety * age + gender, df_FA)
summary(FA_anxiety_int_age_gender) #no
FA_anxiety_int_gender_age <- lm(mean_FA_fornix_mask ~ additional_hads_anxiety * gender + age, df_FA)
summary(FA_anxiety_int_gender_age) #no
FA_anxiety_int_depression_age_gender <- lm(mean_FA_fornix_mask ~ additional_hads_anxiety * additional_hads_depression + age + gender, df_FA)
summary(FA_anxiety_int_depression_age_gender) #no
FA_anxiety_int_story_age_gender <- lm(mean_FA_fornix_mask ~ additional_hads_anxiety * story_d + age + gender, df_FA)
summary(FA_anxiety_int_story_age_gender) #no
FA_anxiety_int_mmse_age_gender <- lm(mean_FA_fornix_mask ~ additional_hads_anxiety * homeint_mmse_cal + age + gender, df_FA)
summary(FA_anxiety_int_mmse_age_gender) #no

#depression interaction effects
FA_depression_int_age_gender <- lm(mean_FA_fornix_mask ~ additional_hads_depression * age + gender, df_FA)
summary(FA_depression_int_age_gender) #no
FA_depression_int_gender_age <- lm(mean_FA_fornix_mask ~ additional_hads_depression * gender + age, df_FA)
summary(FA_depression_int_gender_age) #no
FA_depression_int_story_age_gender <- lm(mean_FA_fornix_mask ~ additional_hads_depression * story_d + age + gender, df_FA)
summary(FA_depression_int_story_age_gender) #no
FA_depression_int_mmse_age_gender <- lm(mean_FA_fornix_mask ~ additional_hads_depression * homeint_mmse_cal + age + gender, df_FA)
summary(FA_depression_int_mmse_age_gender) #no

#story interaction effects
FA_story_d_int_age_gender <- lm(mean_FA_fornix_mask ~ story_d * age + gender, df_FA)
summary(FA_story_d_int_age_gender) #no
FA_story_d_int_gender_age <- lm(mean_FA_fornix_mask ~ story_d * gender + age, df_FA)
summary(FA_story_d_int_gender_age) #no
FA_story_d_int_mmse_age_gender <- lm(mean_FA_fornix_mask ~ story_d * homeint_mmse_cal + age + gender, df_FA)
summary(FA_story_d_int_mmse_age_gender) #no

#mmse interaction effects
FA_mmse_int_age_gender <- lm(mean_FA_fornix_mask ~ homeint_mmse_cal * age + gender, df_FA)
summary(FA_mmse_int_age_gender) #no
FA_mmse_int_gender_age <- lm(mean_FA_fornix_mask ~ homeint_mmse_cal * gender + age, df_FA)
summary(FA_mmse_int_gender_age) #no

#select variables and remove outliers
df_MD <- df 
df_MD$mean_MD_fornix_mask <- remove_outliers(df$mean_MD_fornix_mask)
df_MD <- df_MD %>% drop_na(mean_MD_fornix_mask)

#cohort interaction effects
MD_cohort_int_age_gender <- lm(mean_MD_fornix_mask ~ cohort * age + gender, df_MD)
summary(MD_cohort_int_age_gender) #no
MD_cohort_int_gender_age <- lm(mean_MD_fornix_mask ~ cohort * gender + age, df_MD)
summary(MD_cohort_int_gender_age) #no
MD_cohort_int_anxiety_age_gender <- lm(mean_MD_fornix_mask ~ cohort * additional_hads_anxiety + gender + age, df_MD)
summary(MD_cohort_int_anxiety_age_gender) #no
MD_cohort_int_depression_age_gender <- lm(mean_MD_fornix_mask ~ cohort * additional_hads_depression + gender + age, df_MD)
summary(MD_cohort_int_depression_age_gender) #no
MD_cohort_int_story_d_age_gender <- lm(mean_MD_fornix_mask ~ cohort * story_d + gender + age, df_MD)
summary(MD_cohort_int_story_d_age_gender) #no
MD_cohort_int_mmse_age_gender <- lm(mean_MD_fornix_mask ~ cohort * homeint_mmse_cal + age + gender, df_MD)
summary(MD_cohort_int_mmse_age_gender) #no

#anxiety interaction effects
MD_anxiety_int_age_gender <- lm(mean_MD_fornix_mask ~ additional_hads_anxiety * age + gender, df_MD)
summary(MD_anxiety_int_age_gender) #no
MD_anxiety_int_gender_age <- lm(mean_MD_fornix_mask ~ additional_hads_anxiety * gender + age, df_MD)
summary(MD_anxiety_int_gender_age) #no
MD_anxiety_int_depression_age_gender <- lm(mean_MD_fornix_mask ~ additional_hads_anxiety * additional_hads_depression + age + gender, df_MD)
summary(MD_anxiety_int_depression_age_gender) #no
MD_anxiety_int_story_age_gender <- lm(mean_MD_fornix_mask ~ additional_hads_anxiety * story_d + age + gender, df_MD)
summary(MD_anxiety_int_story_age_gender) #no
MD_anxiety_int_mmse_age_gender <- lm(mean_MD_fornix_mask ~ additional_hads_anxiety * homeint_mmse_cal + age + gender, df_MD)
summary(MD_anxiety_int_mmse_age_gender) #no

#depression interaction effects
MD_depression_int_age_gender <- lm(mean_MD_fornix_mask ~ additional_hads_depression * age + gender, df_MD)
summary(MD_depression_int_age_gender) #no
MD_depression_int_gender_age <- lm(mean_MD_fornix_mask ~ additional_hads_depression * gender + age, df_MD)
summary(MD_depression_int_gender_age) #no
MD_depression_int_story_age_gender <- lm(mean_MD_fornix_mask ~ additional_hads_depression * story_d + age + gender, df_MD)
summary(MD_depression_int_story_age_gender) #no
MD_depression_int_mmse_age_gender <- lm(mean_MD_fornix_mask ~ additional_hads_depression * homeint_mmse_cal + age + gender, df_MD)
summary(MD_depression_int_mmse_age_gender) #no

#story interaction effects
MD_story_d_int_age_gender <- lm(mean_MD_fornix_mask ~ story_d * age + gender, df_MD)
summary(MD_story_d_int_age_gender) #no
MD_story_d_int_gender_age <- lm(mean_MD_fornix_mask ~ story_d * gender + age, df_MD)
summary(MD_story_d_int_gender_age) #no
MD_story_d_int_mmse_age_gender <- lm(mean_MD_fornix_mask ~ story_d * homeint_mmse_cal + age + gender, df_MD)
summary(MD_story_d_int_mmse_age_gender) #yes

interact_plot(MD_story_d_int_mmse_age_gender, pred = story_d, modx = homeint_mmse_cal, 
              partial.residuals = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
              legend.main = 'MMSE'
) +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Delayed Story Recall Score", y = "Mean MD", title = "Bilateral Mean MD",
       subtitle = paste0("Corrected for Age and Gender \n",
                         "interaction p = ",
                         signif(summary(MD_story_d_int_mmse_age_gender)$coefficients[5,4], 2)
       )
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#mmse interaction effects
MD_mmse_int_age_gender <- lm(mean_MD_fornix_mask ~ homeint_mmse_cal * age + gender, df_MD)
summary(MD_mmse_int_age_gender) #no
MD_mmse_int_gender_age <- lm(mean_MD_fornix_mask ~ homeint_mmse_cal * gender + age, df_MD)
summary(FA_mmse_int_gender_age) #no

df_L1 <- df 
df_L1$mean_L1_fornix_mask <- remove_outliers(df$mean_L1_fornix_mask)
df_L1 <- df_L1 %>% drop_na(mean_L1_fornix_mask)

#cohort interaction effects
L1_cohort_int_age_gender <- lm(mean_L1_fornix_mask ~ cohort * age + gender, df_L1)
summary(L1_cohort_int_age_gender) #no
L1_cohort_int_gender_age <- lm(mean_L1_fornix_mask ~ cohort * gender + age, df_L1)
summary(L1_cohort_int_gender_age) #no
L1_cohort_int_anxiety_age_gender <- lm(mean_L1_fornix_mask ~ cohort * additional_hads_anxiety + gender + age, df_L1)
summary(L1_cohort_int_anxiety_age_gender) #no
L1_cohort_int_depression_age_gender <- lm(mean_L1_fornix_mask ~ cohort * additional_hads_depression + gender + age, df_L1)
summary(L1_cohort_int_depression_age_gender) #no
L1_cohort_int_story_d_age_gender <- lm(mean_L1_fornix_mask ~ cohort * story_d + gender + age, df_L1)
summary(L1_cohort_int_story_d_age_gender) #no
L1_cohort_int_mmse_age_gender <- lm(mean_L1_fornix_mask ~ cohort * homeint_mmse_cal + age + gender, df_L1)
summary(L1_cohort_int_mmse_age_gender) #no

#anxiety interaction effects
L1_anxiety_int_age_gender <- lm(mean_L1_fornix_mask ~ additional_hads_anxiety * age + gender, df_L1)
summary(L1_anxiety_int_age_gender) #no
L1_anxiety_int_gender_age <- lm(mean_L1_fornix_mask ~ additional_hads_anxiety * gender + age, df_L1)
summary(L1_anxiety_int_gender_age) #no
L1_anxiety_int_depression_age_gender <- lm(mean_L1_fornix_mask ~ additional_hads_anxiety * additional_hads_depression + age + gender, df_L1)
summary(L1_anxiety_int_depression_age_gender) #yes
L1_anxiety_int_story_age_gender <- lm(mean_L1_fornix_mask ~ additional_hads_anxiety * story_d + age + gender, df_L1)
summary(L1_anxiety_int_story_age_gender) #no
L1_anxiety_int_mmse_age_gender <- lm(mean_L1_fornix_mask ~ additional_hads_anxiety * homeint_mmse_cal + age + gender, df_L1)
summary(L1_anxiety_int_mmse_age_gender) #no

interact_plot(L1_anxiety_int_depression_age_gender, pred = additional_hads_anxiety, modx = additional_hads_depression, 
              partial.residuals = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
              legend.main = 'Depression (HADS)'
) +
  theme(legend.position = 'none') +
  labs(x = "Anxiety (Mean Centered HADS)", y = "Mean AD", title = "Bilateral Mean AD",
       subtitle = paste0("Corrected for Age and Gender \n",
                         "interaction p = ",
                         signif(summary(L1_anxiety_int_depression_age_gender)$coefficients[5,4], 2)
       )
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#depression interaction effects
L1_depression_int_age_gender <- lm(mean_L1_fornix_mask ~ additional_hads_depression * age + gender, df_L1)
summary(L1_depression_int_age_gender) #no
L1_depression_int_gender_age <- lm(mean_L1_fornix_mask ~ additional_hads_depression * gender + age, df_L1)
summary(L1_depression_int_gender_age) #no
L1_depression_int_story_age_gender <- lm(mean_L1_fornix_mask ~ additional_hads_depression * story_d + age + gender, df_L1)
summary(L1_depression_int_story_age_gender) #no
L1_depression_int_mmse_age_gender <- lm(mean_L1_fornix_mask ~ additional_hads_depression * homeint_mmse_cal + age + gender, df_L1)
summary(L1_depression_int_mmse_age_gender) #no

#story interaction effects
L1_story_d_int_age_gender <- lm(mean_L1_fornix_mask ~ story_d * age + gender, df_L1)
summary(L1_story_d_int_age_gender) #no
L1_story_d_int_gender_age <- lm(mean_L1_fornix_mask ~ story_d * gender + age, df_L1)
summary(L1_story_d_int_gender_age) #no
L1_story_d_int_mmse_age_gender <- lm(mean_L1_fornix_mask ~ story_d * homeint_mmse_cal + age + gender, df_L1)
summary(L1_story_d_int_mmse_age_gender) #no

#mmse interaction effects
L1_mmse_int_age_gender <- lm(mean_L1_fornix_mask ~ homeint_mmse_cal * age + gender, df_L1)
summary(L1_mmse_int_age_gender) #no
L1_mmse_int_gender_age <- lm(mean_L1_fornix_mask ~ homeint_mmse_cal * gender + age, df_L1)
summary(L1_mmse_int_gender_age) #no

#select variables and remove outliers
df_RD <- df 
df_RD$mean_RD_fornix_mask <- remove_outliers(df$mean_RD_fornix_mask)
df_RD <- df_RD %>% drop_na(mean_RD_fornix_mask)

#cohort interaction effects
RD_cohort_int_age_gender <- lm(mean_RD_fornix_mask ~ cohort * age + gender, df_RD)
summary(RD_cohort_int_age_gender) #no
RD_cohort_int_gender_age <- lm(mean_RD_fornix_mask ~ cohort * gender + age, df_RD)
summary(RD_cohort_int_gender_age) #no
RD_cohort_int_anxiety_age_gender <- lm(mean_RD_fornix_mask ~ cohort * additional_hads_anxiety + gender + age, df_RD)
summary(RD_cohort_int_anxiety_age_gender) #no
RD_cohort_int_depression_age_gender <- lm(mean_RD_fornix_mask ~ cohort * additional_hads_depression + gender + age, df_RD)
summary(RD_cohort_int_depression_age_gender) #no
RD_cohort_int_story_d_age_gender <- lm(mean_RD_fornix_mask ~ cohort * story_d + gender + age, df_RD)
summary(RD_cohort_int_story_d_age_gender) #no
RD_cohort_int_mmse_age_gender <- lm(mean_RD_fornix_mask ~ cohort * homeint_mmse_cal + age + gender, df_RD)
summary(RD_cohort_int_mmse_age_gender) #no

#anxiety interaction effects
RD_anxiety_int_age_gender <- lm(mean_RD_fornix_mask ~ additional_hads_anxiety * age + gender, df_RD)
summary(RD_anxiety_int_age_gender) #no
RD_anxiety_int_gender_age <- lm(mean_RD_fornix_mask ~ additional_hads_anxiety * gender + age, df_RD)
summary(RD_anxiety_int_gender_age) #no
RD_anxiety_int_depression_age_gender <- lm(mean_RD_fornix_mask ~ additional_hads_anxiety * additional_hads_depression + age + gender, df_RD)
summary(RD_anxiety_int_depression_age_gender) #no
RD_anxiety_int_story_age_gender <- lm(mean_RD_fornix_mask ~ additional_hads_anxiety * story_d + age + gender, df_RD)
summary(RD_anxiety_int_story_age_gender) #no
RD_anxiety_int_mmse_age_gender <- lm(mean_RD_fornix_mask ~ additional_hads_anxiety * homeint_mmse_cal + age + gender, df_RD)
summary(RD_anxiety_int_mmse_age_gender) #no

#depression interaction effects
RD_depression_int_age_gender <- lm(mean_RD_fornix_mask ~ additional_hads_depression * age + gender, df_RD)
summary(RD_depression_int_age_gender) #no
RD_depression_int_gender_age <- lm(mean_RD_fornix_mask ~ additional_hads_depression * gender + age, df_RD)
summary(RD_depression_int_gender_age) #no
RD_depression_int_story_age_gender <- lm(mean_RD_fornix_mask ~ additional_hads_depression * story_d + age + gender, df_RD)
summary(RD_depression_int_story_age_gender) #no
RD_depression_int_mmse_age_gender <- lm(mean_RD_fornix_mask ~ additional_hads_depression * homeint_mmse_cal + age + gender, df_RD)
summary(RD_depression_int_mmse_age_gender) #no

#story interaction effects
RD_story_d_int_age_gender <- lm(mean_RD_fornix_mask ~ story_d * age + gender, df_RD)
summary(RD_story_d_int_age_gender) #no
RD_story_d_int_gender_age <- lm(mean_RD_fornix_mask ~ story_d * gender + age, df_RD)
summary(RD_story_d_int_gender_age) #no
RD_story_d_int_mmse_age_gender <- lm(mean_RD_fornix_mask ~ story_d * homeint_mmse_cal + age + gender, df_RD)
summary(RD_story_d_int_mmse_age_gender) #yes

interact_plot(RD_story_d_int_mmse_age_gender, pred = story_d, modx = homeint_mmse_cal, 
              partial.residuals = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
              legend.main = 'MMSE'
) +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Delayed Story Recall Score", y = "Mean RD", title = "Bilateral Mean RD",
       subtitle = paste0("Corrected for Age and Gender \n",
                         "interaction p = ",
                         signif(summary(RD_story_d_int_mmse_age_gender)$coefficients[5,4], 2)
       )
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#mmse interaction effects
RD_mmse_int_age_gender <- lm(mean_RD_fornix_mask ~ homeint_mmse_cal * age + gender, df_RD)
summary(RD_mmse_int_age_gender) #no
RD_mmse_int_gender_age <- lm(mean_RD_fornix_mask ~ homeint_mmse_cal * gender + age, df_RD)
summary(RD_mmse_int_gender_age) #no

#select variables and remove outliers
df_OD <- df 
df_OD$mean_OD_fornix_mask <- remove_outliers(df$mean_OD_fornix_mask)
df_OD <- df_OD %>% drop_na(mean_OD_fornix_mask)

#cohort interaction effects
OD_cohort_int_age_gender <- lm(mean_OD_fornix_mask ~ cohort * age + gender, df_OD)
summary(OD_cohort_int_age_gender) #no
OD_cohort_int_gender_age <- lm(mean_OD_fornix_mask ~ cohort * gender + age, df_OD)
summary(OD_cohort_int_gender_age) #no
OD_cohort_int_anxiety_age_gender <- lm(mean_OD_fornix_mask ~ cohort * additional_hads_anxiety + gender + age, df_OD)
summary(OD_cohort_int_anxiety_age_gender) #no
OD_cohort_int_depression_age_gender <- lm(mean_OD_fornix_mask ~ cohort * additional_hads_depression + gender + age, df_OD)
summary(OD_cohort_int_depression_age_gender) #no
OD_cohort_int_story_d_age_gender <- lm(mean_OD_fornix_mask ~ cohort * story_d + gender + age, df_OD)
summary(OD_cohort_int_story_d_age_gender) #no
OD_cohort_int_mmse_age_gender <- lm(mean_OD_fornix_mask ~ cohort * homeint_mmse_cal + age + gender, df_OD)
summary(OD_cohort_int_mmse_age_gender) #no

#anxiety interaction effects
OD_anxiety_int_age_gender <- lm(mean_OD_fornix_mask ~ additional_hads_anxiety * age + gender, df_OD)
summary(OD_anxiety_int_age_gender) #no
OD_anxiety_int_gender_age <- lm(mean_OD_fornix_mask ~ additional_hads_anxiety * gender + age, df_OD)
summary(OD_anxiety_int_gender_age) #no
OD_anxiety_int_depression_age_gender <- lm(mean_OD_fornix_mask ~ additional_hads_anxiety * additional_hads_depression + age + gender, df_OD)
summary(OD_anxiety_int_depression_age_gender) #no
OD_anxiety_int_story_age_gender <- lm(mean_OD_fornix_mask ~ additional_hads_anxiety * story_d + age + gender, df_OD)
summary(OD_anxiety_int_story_age_gender) #no
OD_anxiety_int_mmse_age_gender <- lm(mean_OD_fornix_mask ~ additional_hads_anxiety * homeint_mmse_cal + age + gender, df_OD)
summary(OD_anxiety_int_mmse_age_gender) #no

#depression interaction effects
OD_depression_int_age_gender <- lm(mean_OD_fornix_mask ~ additional_hads_depression * age + gender, df_OD)
summary(OD_depression_int_age_gender) #no
OD_depression_int_gender_age <- lm(mean_OD_fornix_mask ~ additional_hads_depression * gender + age, df_OD)
summary(OD_depression_int_gender_age) #no
OD_depression_int_story_age_gender <- lm(mean_OD_fornix_mask ~ additional_hads_depression * story_d + age + gender, df_OD)
summary(OD_depression_int_story_age_gender) #no
OD_depression_int_mmse_age_gender <- lm(mean_OD_fornix_mask ~ additional_hads_depression * homeint_mmse_cal + age + gender, df_OD)
summary(OD_depression_int_mmse_age_gender) #no

#story interaction effects
OD_story_d_int_age_gender <- lm(mean_OD_fornix_mask ~ story_d * age + gender, df_OD)
summary(OD_story_d_int_age_gender) #no
OD_story_d_int_gender_age <- lm(mean_OD_fornix_mask ~ story_d * gender + age, df_OD)
summary(OD_story_d_int_gender_age) #no
OD_story_d_int_mmse_age_gender <- lm(mean_OD_fornix_mask ~ story_d * homeint_mmse_cal + age + gender, df_OD)
summary(OD_story_d_int_mmse_age_gender) #no

#mmse interaction effects
OD_mmse_int_age_gender <- lm(mean_OD_fornix_mask ~ homeint_mmse_cal * age + gender, df_OD)
summary(OD_mmse_int_age_gender) #no
OD_mmse_int_gender_age <- lm(mean_OD_fornix_mask ~ homeint_mmse_cal * gender + age, df_OD)
summary(OD_mmse_int_gender_age) #no

#select variables and remove outliers
df_ISOVF <- df 
df_ISOVF$mean_ISOVF_fornix_mask <- remove_outliers(df$mean_ISOVF_fornix_mask)
df_ISOVF <- df_ISOVF %>% drop_na(mean_ISOVF_fornix_mask)

#cohort interaction effects
ISOVF_cohort_int_age_gender <- lm(mean_ISOVF_fornix_mask ~ cohort * age + gender, df_ISOVF)
summary(ISOVF_cohort_int_age_gender) #no
ISOVF_cohort_int_gender_age <- lm(mean_ISOVF_fornix_mask ~ cohort * gender + age, df_ISOVF)
summary(ISOVF_cohort_int_gender_age) #no
ISOVF_cohort_int_anxiety_age_gender <- lm(mean_ISOVF_fornix_mask ~ cohort * additional_hads_anxiety + gender + age, df_ISOVF)
summary(ISOVF_cohort_int_anxiety_age_gender) #no
ISOVF_cohort_int_depression_age_gender <- lm(mean_ISOVF_fornix_mask ~ cohort * additional_hads_depression + gender + age, df_ISOVF)
summary(ISOVF_cohort_int_depression_age_gender) #no
ISOVF_cohort_int_story_d_age_gender <- lm(mean_ISOVF_fornix_mask ~ cohort * story_d + gender + age, df_ISOVF)
summary(ISOVF_cohort_int_story_d_age_gender) #no
ISOVF_cohort_int_mmse_age_gender <- lm(mean_ISOVF_fornix_mask ~ cohort * homeint_mmse_cal + age + gender, df_ISOVF)
summary(ISOVF_cohort_int_mmse_age_gender) #no

#anxiety interaction effects
ISOVF_anxiety_int_age_gender <- lm(mean_ISOVF_fornix_mask ~ additional_hads_anxiety * age + gender, df_ISOVF)
summary(ISOVF_anxiety_int_age_gender) #no
ISOVF_anxiety_int_gender_age <- lm(mean_ISOVF_fornix_mask ~ additional_hads_anxiety * gender + age, df_ISOVF)
summary(ISOVF_anxiety_int_gender_age) #no
ISOVF_anxiety_int_depression_age_gender <- lm(mean_ISOVF_fornix_mask ~ additional_hads_anxiety * additional_hads_depression + age + gender, df_ISOVF)
summary(ISOVF_anxiety_int_depression_age_gender) #no
ISOVF_anxiety_int_story_age_gender <- lm(mean_ISOVF_fornix_mask ~ additional_hads_anxiety * story_d + age + gender, df_ISOVF)
summary(ISOVF_anxiety_int_story_age_gender) #no
ISOVF_anxiety_int_mmse_age_gender <- lm(mean_ISOVF_fornix_mask ~ additional_hads_anxiety * homeint_mmse_cal + age + gender, df_ISOVF)
summary(ISOVF_anxiety_int_mmse_age_gender) #no

#depression interaction effects
ISOVF_depression_int_age_gender <- lm(mean_ISOVF_fornix_mask ~ additional_hads_depression * age + gender, df_ISOVF)
summary(ISOVF_depression_int_age_gender) #no
ISOVF_depression_int_gender_age <- lm(mean_ISOVF_fornix_mask ~ additional_hads_depression * gender + age, df_ISOVF)
summary(ISOVF_depression_int_gender_age) #no
ISOVF_depression_int_story_age_gender <- lm(mean_ISOVF_fornix_mask ~ additional_hads_depression * story_d + age + gender, df_ISOVF)
summary(ISOVF_depression_int_story_age_gender) #no
ISOVF_depression_int_mmse_age_gender <- lm(mean_ISOVF_fornix_mask ~ additional_hads_depression * homeint_mmse_cal + age + gender, df_ISOVF)
summary(ISOVF_depression_int_mmse_age_gender) #no

#story interaction effects
ISOVF_story_d_int_age_gender <- lm(mean_ISOVF_fornix_mask ~ story_d * age + gender, df_ISOVF)
summary(ISOVF_story_d_int_age_gender) #no
ISOVF_story_d_int_gender_age <- lm(mean_ISOVF_fornix_mask ~ story_d * gender + age, df_ISOVF)
summary(ISOVF_story_d_int_gender_age) #no
ISOVF_story_d_int_mmse_age_gender <- lm(mean_ISOVF_fornix_mask ~ story_d * homeint_mmse_cal + age + gender, df_ISOVF)
summary(ISOVF_story_d_int_mmse_age_gender) #no

#mmse interaction effects
ISOVF_mmse_int_age_gender <- lm(mean_ISOVF_fornix_mask ~ homeint_mmse_cal * age + gender, df_ISOVF)
summary(ISOVF_mmse_int_age_gender) #no
ISOVF_mmse_int_gender_age <- lm(mean_ISOVF_fornix_mask ~ homeint_mmse_cal * gender + age, df_ISOVF)
summary(ISOVF_mmse_int_gender_age) #no

#select variables and remove outliers
df_ICVF <- df 
df_ICVF$mean_ICVF_fornix_mask <- remove_outliers(df$mean_ICVF_fornix_mask)
df_ICVF <- df_ICVF %>% drop_na(mean_ICVF_fornix_mask)

#cohort interaction effects
df_ICVF <- df_ICVF %>% drop_na(additional_hads_depression)

ICVF_cohort_int_age_gender <- lm(mean_ICVF_fornix_mask ~ cohort * age + gender, df_ICVF)
summary(ICVF_cohort_int_age_gender) #no
ICVF_cohort_int_gender_age <- lm(mean_ICVF_fornix_mask ~ cohort * gender + age, df_ICVF)
summary(ICVF_cohort_int_gender_age) #no
ICVF_cohort_int_anxiety_age_gender <- lm(mean_ICVF_fornix_mask ~ cohort * additional_hads_anxiety + gender + age, df_ICVF)
summary(ICVF_cohort_int_anxiety_age_gender) #no
ICVF_cohort_int_depression_age_gender <- lm(mean_ICVF_fornix_mask ~ cohort * additional_hads_depression + gender + age, df_ICVF)
summary(ICVF_cohort_int_depression_age_gender) #yes
ICVF_cohort_int_story_d_age_gender <- lm(mean_ICVF_fornix_mask ~ cohort * story_d + gender + age, df_ICVF)
summary(ICVF_cohort_int_story_d_age_gender) #no
ICVF_cohort_int_mmse_age_gender <- lm(mean_ICVF_fornix_mask ~ cohort * homeint_mmse_cal + age + gender, df_ICVF)
summary(ICVF_cohort_int_mmse_age_gender) #no

ctl_ICVF <- df_ICVF %>% filter(cohort == 'ctl')
scd_ICVF <- df_ICVF %>% filter(cohort == 'scd')
ctl_ICVF_depression_age_gender <- lm(mean_ICVF_fornix_mask ~ additional_hads_depression + age + gender, ctl_ICVF)
scd_ICVF_depression_age_gender <- lm(mean_ICVF_fornix_mask ~ additional_hads_depression + age + gender, scd_ICVF)

interact_plot(ICVF_cohort_int_depression_age_gender, pred = additional_hads_depression, modx = cohort, 
              partial.residuals = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
              modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Depression (Mean Centered HADS)", y = "Mean ICVF", title = "Mean ICVF in the Fornix",
       subtitle = paste0("Corrected for Age and Gender \n",
                         "interaction p < 0.001"
                         # signif(summary(ICVF_cohort_int_depression_age_gender)$coefficients[5,4], 2)
       )
  ) +
  geom_text(aes(x = 7, y = 0.8, vjust = -1, 
                label = paste0("p = ", signif(summary(ctl_ICVF_depression_age_gender)$coefficients[2,4], 2)), 
                color = "Control"), show.legend = F) +
  geom_text(aes(x = 7, y = 0.8, vjust = 1, 
                label = paste0("p = ", signif(summary(scd_ICVF_depression_age_gender)$coefficients[2,4], 2)), 
                color = "SCD"), show.legend = F) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#anxiety interaction effects
ICVF_anxiety_int_age_gender <- lm(mean_ICVF_fornix_mask ~ additional_hads_anxiety * age + gender, df_ICVF)
summary(ICVF_anxiety_int_age_gender) #no
ICVF_anxiety_int_gender_age <- lm(mean_ICVF_fornix_mask ~ additional_hads_anxiety * gender + age, df_ICVF)
summary(ICVF_anxiety_int_gender_age) #no
ICVF_anxiety_int_depression_age_gender <- lm(mean_ICVF_fornix_mask ~ additional_hads_anxiety * additional_hads_depression + age + gender, df_ICVF)
summary(ICVF_anxiety_int_depression_age_gender) #no
ICVF_anxiety_int_story_age_gender <- lm(mean_ICVF_fornix_mask ~ additional_hads_anxiety * story_d + age + gender, df_ICVF)
summary(ICVF_anxiety_int_story_age_gender) #no
ICVF_anxiety_int_mmse_age_gender <- lm(mean_ICVF_fornix_mask ~ additional_hads_anxiety * homeint_mmse_cal + age + gender, df_ICVF)
summary(ICVF_anxiety_int_mmse_age_gender) #no

#depression interaction effects
ICVF_depression_int_age_gender <- lm(mean_ICVF_fornix_mask ~ additional_hads_depression * age + gender, df_ICVF)
summary(ICVF_depression_int_age_gender) #no
ICVF_depression_int_gender_age <- lm(mean_ICVF_fornix_mask ~ additional_hads_depression * gender + age, df_ICVF)
summary(ICVF_depression_int_gender_age) #no
ICVF_depression_int_story_age_gender <- lm(mean_ICVF_fornix_mask ~ additional_hads_depression * story_d + age + gender, df_ICVF)
summary(ICVF_depression_int_story_age_gender) #no
ICVF_depression_int_mmse_age_gender <- lm(mean_ICVF_fornix_mask ~ additional_hads_depression * homeint_mmse_cal + age + gender, df_ICVF)
summary(ICVF_depression_int_mmse_age_gender) #no

#story interaction effects
ICVF_story_d_int_age_gender <- lm(mean_ICVF_fornix_mask ~ story_d * age + gender, df_ICVF)
summary(ICVF_story_d_int_age_gender) #no
ICVF_story_d_int_gender_age <- lm(mean_ICVF_fornix_mask ~ story_d * gender + age, df_ICVF)
summary(ICVF_story_d_int_gender_age) #no
ICVF_story_d_int_mmse_age_gender <- lm(mean_ICVF_fornix_mask ~ story_d * homeint_mmse_cal + age + gender, df_ICVF)
summary(ICVF_story_d_int_mmse_age_gender) #yes

interact_plot(ICVF_story_d_int_mmse_age_gender, pred = story_d, modx = homeint_mmse_cal, 
              partial.residuals = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
              legend.main = 'MMSE'
) +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Story Delayed Recall Score", y = "Mean ICVF", title = "Bilateral Mean ICVF",
       subtitle = paste0("Corrected for Gender \n",
                         "interaction p = ",
                         signif(summary(ICVF_story_d_int_mmse_age_gender)$coefficients[5,4], 2)
       )
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#mmse interaction effects
ICVF_mmse_int_age_gender <- lm(mean_ICVF_fornix_mask ~ homeint_mmse_cal * age + gender, df_ICVF)
summary(ICVF_mmse_int_age_gender) #no
ICVF_mmse_int_gender_age <- lm(mean_ICVF_fornix_mask ~ homeint_mmse_cal * gender + age, df_ICVF)
summary(ICVF_mmse_int_gender_age) #no

