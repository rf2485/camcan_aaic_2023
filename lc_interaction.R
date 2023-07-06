source('cleaning.R')

library(tidyverse)
library(jtools)
library(ggpmisc)
library(interactions)
library(officer)
library(ggtext)

lc_interaction_pptx <- read_pptx()
layout_summary(lc_interaction_pptx)

#select variables and remove outliers
df_FA <- df 
df_FA$mean_FA_lower_cingulum_mask <- remove_outliers(df$mean_FA_lower_cingulum_mask)
df_FA <- df_FA %>% drop_na(mean_FA_lower_cingulum_mask)

#cohort interaction effects
FA_cohort_int_age_gender <- lm(mean_FA_lower_cingulum_mask ~ cohort * age + gender, df_FA)
summary(FA_cohort_int_age_gender) #no
FA_cohort_int_gender_age <- lm(mean_FA_lower_cingulum_mask ~ cohort * gender + age, df_FA)
summary(FA_cohort_int_gender_age) #no
FA_cohort_int_anxiety_age_gender <- lm(mean_FA_lower_cingulum_mask ~ cohort * additional_hads_anxiety + gender + age, df_FA)
summary(FA_cohort_int_anxiety_age_gender) #no
FA_cohort_int_depression_age_gender <- lm(mean_FA_lower_cingulum_mask ~ cohort * additional_hads_depression + gender + age, df_FA)
summary(FA_cohort_int_depression_age_gender) #no
FA_cohort_int_story_d_age_gender <- lm(mean_FA_lower_cingulum_mask ~ cohort * story_d + gender + age, df_FA)
summary(FA_cohort_int_story_d_age_gender) #no
FA_cohort_int_mmse_age_gender <- lm(mean_FA_lower_cingulum_mask ~ cohort * homeint_mmse_cal + age + gender, df_FA)
summary(FA_cohort_int_mmse_age_gender) #no

#anxiety interaction effects
FA_anxiety_int_age_gender <- lm(mean_FA_lower_cingulum_mask ~ additional_hads_anxiety * age + gender, df_FA)
summary(FA_anxiety_int_age_gender) #no
FA_anxiety_int_gender_age <- lm(mean_FA_lower_cingulum_mask ~ additional_hads_anxiety * gender + age, df_FA)
summary(FA_anxiety_int_gender_age) #no
FA_anxiety_int_depression_age_gender <- lm(mean_FA_lower_cingulum_mask ~ additional_hads_anxiety * additional_hads_depression + age + gender, df_FA)
summary(FA_anxiety_int_depression_age_gender) #no
FA_anxiety_int_story_age_gender <- lm(mean_FA_lower_cingulum_mask ~ additional_hads_anxiety * story_d + age + gender, df_FA)
summary(FA_anxiety_int_story_age_gender) #no
FA_anxiety_int_mmse_age_gender <- lm(mean_FA_lower_cingulum_mask ~ additional_hads_anxiety * homeint_mmse_cal + age + gender, df_FA)
summary(FA_anxiety_int_mmse_age_gender) #no

#depression interaction effects
FA_depression_int_age_gender <- lm(mean_FA_lower_cingulum_mask ~ additional_hads_depression * age + gender, df_FA)
summary(FA_depression_int_age_gender) #no
FA_depression_int_gender_age <- lm(mean_FA_lower_cingulum_mask ~ additional_hads_depression * gender + age, df_FA)
summary(FA_depression_int_gender_age) #no
FA_depression_int_story_age_gender <- lm(mean_FA_lower_cingulum_mask ~ additional_hads_depression * story_d + age + gender, df_FA)
summary(FA_depression_int_story_age_gender) #no
FA_depression_int_mmse_age_gender <- lm(mean_FA_lower_cingulum_mask ~ additional_hads_depression * homeint_mmse_cal + age + gender, df_FA)
summary(FA_depression_int_mmse_age_gender) #no

#story interaction effects
FA_story_d_int_age_gender <- lm(mean_FA_lower_cingulum_mask ~ story_d * age + gender, df_FA)
summary(FA_story_d_int_age_gender) #no
FA_story_d_int_gender_age <- lm(mean_FA_lower_cingulum_mask ~ story_d * gender + age, df_FA)
summary(FA_story_d_int_gender_age) #no
FA_story_d_int_mmse_age_gender <- lm(mean_FA_lower_cingulum_mask ~ story_d * homeint_mmse_cal + age + gender, df_FA)
summary(FA_story_d_int_mmse_age_gender) #no

#mmse interaction effects
FA_mmse_int_age_gender <- lm(mean_FA_lower_cingulum_mask ~ homeint_mmse_cal * age + gender, df_FA)
summary(FA_mmse_int_age_gender) #no
FA_mmse_int_gender_age <- lm(mean_FA_lower_cingulum_mask ~ homeint_mmse_cal * gender + age, df_FA)
summary(FA_mmse_int_gender_age) #no

#select variables and remove outliers
df_MD <- df 
df_MD$mean_MD_lower_cingulum_mask <- remove_outliers(df$mean_MD_lower_cingulum_mask)
df_MD <- df_MD %>% drop_na(mean_MD_lower_cingulum_mask)

#cohort interaction effects
MD_cohort_int_age_gender <- lm(mean_MD_lower_cingulum_mask ~ cohort * age + gender, df_MD)
summary(MD_cohort_int_age_gender) #yes
MD_cohort_int_gender_age <- lm(mean_MD_lower_cingulum_mask ~ cohort * gender + age, df_MD)
summary(MD_cohort_int_gender_age) #no
MD_cohort_int_anxiety_age_gender <- lm(mean_MD_lower_cingulum_mask ~ cohort * additional_hads_anxiety + gender + age, df_MD)
summary(MD_cohort_int_anxiety_age_gender) #no
MD_cohort_int_depression_age_gender <- lm(mean_MD_lower_cingulum_mask ~ cohort * additional_hads_depression + gender + age, df_MD)
summary(MD_cohort_int_depression_age_gender) #no
MD_cohort_int_story_d_age_gender <- lm(mean_MD_lower_cingulum_mask ~ cohort * story_d + gender + age, df_MD)
summary(MD_cohort_int_story_d_age_gender) #no
MD_cohort_int_mmse_age_gender <- lm(mean_MD_lower_cingulum_mask ~ cohort * homeint_mmse_cal + age + gender, df_MD)
summary(MD_cohort_int_mmse_age_gender) #no

ctl_MD <- df_MD %>% filter(cohort == 'ctl')
scd_MD <- df_MD %>% filter(cohort == 'scd')
ctl_MD_age_gender <- lm(mean_MD_lower_cingulum_mask ~ age + gender, ctl_MD)
scd_MD_age_gender <- lm(mean_MD_lower_cingulum_mask ~ age + gender, scd_MD)

MD_cohort_int_age_gender_plot <- interact_plot(MD_cohort_int_age_gender, pred = age, modx = cohort, 
              partial.residuals = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
              modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean MD", title = "Bilateral Mean MD",
       subtitle = paste0("Corrected for Gender \n",
                         "interaction p = ",
                         signif(summary(MD_cohort_int_age_gender)$coefficients[5,4], 2)
       )
  ) +
  geom_richtext(aes(x = -10, y = 7e-04, vjust = -1, 
                # label = paste0("p = ", signif(summary(ctl_MD_age_gender)$coefficients[2,4], 2)), 
                label = paste0("p < 0.001, adj-R<sup>2</sup> = ",
                               signif(summary(ctl_MD_age_gender)$adj.r.squared, 2)),
                color = "Control"), show.legend = F, 
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt")) +
  geom_richtext(aes(x = -10, y = 7e-04, vjust = 1, 
                # label = paste0("p = ", signif(summary(scd_MD_age_gender)$coefficients[2,4], 2)), 
                label = paste0("p < 0.001, adj-R<sup>2</sup> = ",
                               signif(summary(scd_MD_age_gender)$adj.r.squared, 2)),
                color = "SCD"), show.legend = F, 
                fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
MD_cohort_int_age_gender_plot

lc_interaction_pptx <- add_slide(lc_interaction_pptx)
lc_interaction_pptx <- ph_with(x = lc_interaction_pptx, MD_cohort_int_age_gender_plot, location = ph_location_type(type = "body"))

#anxiety interaction effects
df_MD <- df_MD %>% drop_na(additional_hads_anxiety)

MD_anxiety_int_age_gender <- lm(mean_MD_lower_cingulum_mask ~ additional_hads_anxiety * age + gender, df_MD)
summary(MD_anxiety_int_age_gender) #no
MD_anxiety_int_gender_age <- lm(mean_MD_lower_cingulum_mask ~ additional_hads_anxiety * gender + age, df_MD)
summary(MD_anxiety_int_gender_age) #no
MD_anxiety_int_depression_age_gender <- lm(mean_MD_lower_cingulum_mask ~ additional_hads_anxiety * additional_hads_depression + age + gender, df_MD)
summary(MD_anxiety_int_depression_age_gender) #no
MD_anxiety_int_story_age_gender <- lm(mean_MD_lower_cingulum_mask ~ additional_hads_anxiety * story_d + age + gender, df_MD)
summary(MD_anxiety_int_story_age_gender) #no
MD_anxiety_int_mmse_age_gender <- lm(mean_MD_lower_cingulum_mask ~ additional_hads_anxiety * homeint_mmse_cal + age + gender, df_MD)
summary(MD_anxiety_int_mmse_age_gender) #no

male_MD <- df_MD %>% filter(gender == 'male')
female_MD <- df_MD %>% filter(gender == 'female')
male_MD_anxiety_age <- lm(mean_MD_lower_cingulum_mask ~ additional_hads_anxiety + age, male_MD)
female_MD_anxiety_age <- lm(mean_MD_lower_cingulum_mask ~ additional_hads_anxiety + age, female_MD)

#depression interaction effects
MD_depression_int_age_gender <- lm(mean_MD_lower_cingulum_mask ~ additional_hads_depression * age + gender, df_MD)
summary(MD_depression_int_age_gender) #no
MD_depression_int_gender_age <- lm(mean_MD_lower_cingulum_mask ~ additional_hads_depression * gender + age, df_MD)
summary(MD_depression_int_gender_age) #no
MD_depression_int_story_age_gender <- lm(mean_MD_lower_cingulum_mask ~ additional_hads_depression * story_d + age + gender, df_MD)
summary(MD_depression_int_story_age_gender) #no
MD_depression_int_mmse_age_gender <- lm(mean_MD_lower_cingulum_mask ~ additional_hads_depression * homeint_mmse_cal + age + gender, df_MD)
summary(MD_depression_int_mmse_age_gender) #no

#story interaction effects
MD_story_d_int_age_gender <- lm(mean_MD_lower_cingulum_mask ~ story_d * age + gender, df_MD)
summary(MD_story_d_int_age_gender) #no
MD_story_d_int_gender_age <- lm(mean_MD_lower_cingulum_mask ~ story_d * gender + age, df_MD)
summary(MD_story_d_int_gender_age) #no
MD_story_d_int_mmse_age_gender <- lm(mean_MD_lower_cingulum_mask ~ story_d * homeint_mmse_cal + age + gender, df_MD)
summary(MD_story_d_int_mmse_age_gender) #no

#mmse interaction effects
MD_mmse_int_age_gender <- lm(mean_MD_lower_cingulum_mask ~ homeint_mmse_cal * age + gender, df_MD)
summary(MD_mmse_int_age_gender) #no
MD_mmse_int_gender_age <- lm(mean_MD_lower_cingulum_mask ~ homeint_mmse_cal * gender + age, df_MD)
summary(FA_mmse_int_gender_age) #no

df_L1 <- df 
df_L1$mean_L1_lower_cingulum_mask <- remove_outliers(df$mean_L1_lower_cingulum_mask)
df_L1 <- df_L1 %>% drop_na(mean_L1_lower_cingulum_mask)

#cohort interaction effects
L1_cohort_int_age_gender <- lm(mean_L1_lower_cingulum_mask ~ cohort * age + gender, df_L1)
summary(L1_cohort_int_age_gender) #yes
L1_cohort_int_gender_age <- lm(mean_L1_lower_cingulum_mask ~ cohort * gender + age, df_L1)
summary(L1_cohort_int_gender_age) #no
L1_cohort_int_anxiety_age_gender <- lm(mean_L1_lower_cingulum_mask ~ cohort * additional_hads_anxiety + gender + age, df_L1)
summary(L1_cohort_int_anxiety_age_gender) #no
L1_cohort_int_depression_age_gender <- lm(mean_L1_lower_cingulum_mask ~ cohort * additional_hads_depression + gender + age, df_L1)
summary(L1_cohort_int_depression_age_gender) #no
L1_cohort_int_story_d_age_gender <- lm(mean_L1_lower_cingulum_mask ~ cohort * story_d + gender + age, df_L1)
summary(L1_cohort_int_story_d_age_gender) #no
L1_cohort_int_mmse_age_gender <- lm(mean_L1_lower_cingulum_mask ~ cohort * homeint_mmse_cal + age + gender, df_L1)
summary(L1_cohort_int_mmse_age_gender) #no

ctl_L1 <- df_L1 %>% filter(cohort == 'ctl')
scd_L1 <- df_L1 %>% filter(cohort == 'scd')
ctl_L1_age_gender <- lm(mean_L1_lower_cingulum_mask ~ age + gender, ctl_L1)
scd_L1_age_gender <- lm(mean_L1_lower_cingulum_mask ~ age + gender, scd_L1)

L1_cohort_int_age_gender_plot <- interact_plot(L1_cohort_int_age_gender, pred = age, modx = cohort, 
              partial.residuals = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
              modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean AD", title = "Bilateral Mean AD",
       subtitle = paste0("Corrected for Gender \n",
                         "interaction p < 0.001"
                         # "interaction p = ",
                         # signif(summary(L1_cohort_int_age_gender)$coefficients[5,4], 2)
       )
  ) +
  geom_richtext(aes(x = -10, y = 9.5e-04, vjust = -1, 
                    label = paste0(
                      "p = ", signif(summary(ctl_L1_age_gender)$coefficients[2,4], 2), 
                      ", adj-R<sup>2</sup> = ",signif(summary(ctl_L1_age_gender)$adj.r.squared, 2)),
                    color = "Control"), 
                show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  geom_richtext(aes(x = -10, y = 9.5e-04, vjust = 1, 
                    label = paste0(
                      "p = ", signif(summary(scd_L1_age_gender)$coefficients[2,4], 2), 
                      ", adj-R<sup>2</sup> = ",signif(summary(scd_L1_age_gender)$adj.r.squared, 2)),
                    color = "SCD"), 
                show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
L1_cohort_int_age_gender_plot

lc_interaction_pptx <- add_slide(lc_interaction_pptx)
lc_interaction_pptx <- ph_with(x = lc_interaction_pptx, L1_cohort_int_age_gender_plot, location = ph_location_type(type = "body"))

#anxiety interaction effects
L1_anxiety_int_age_gender <- lm(mean_L1_lower_cingulum_mask ~ additional_hads_anxiety * age + gender, df_L1)
summary(L1_anxiety_int_age_gender) #no
L1_anxiety_int_gender_age <- lm(mean_L1_lower_cingulum_mask ~ additional_hads_anxiety * gender + age, df_L1)
summary(L1_anxiety_int_gender_age) #no
L1_anxiety_int_depression_age_gender <- lm(mean_L1_lower_cingulum_mask ~ additional_hads_anxiety * additional_hads_depression + age + gender, df_L1)
summary(L1_anxiety_int_depression_age_gender) #no
L1_anxiety_int_story_age_gender <- lm(mean_L1_lower_cingulum_mask ~ additional_hads_anxiety * story_d + age + gender, df_L1)
summary(L1_anxiety_int_story_age_gender) #no
L1_anxiety_int_mmse_age_gender <- lm(mean_L1_lower_cingulum_mask ~ additional_hads_anxiety * homeint_mmse_cal + age + gender, df_L1)
summary(L1_anxiety_int_mmse_age_gender) #no

#depression interaction effects
L1_depression_int_age_gender <- lm(mean_L1_lower_cingulum_mask ~ additional_hads_depression * age + gender, df_L1)
summary(L1_depression_int_age_gender) #no
L1_depression_int_gender_age <- lm(mean_L1_lower_cingulum_mask ~ additional_hads_depression * gender + age, df_L1)
summary(L1_depression_int_gender_age) #no
L1_depression_int_story_age_gender <- lm(mean_L1_lower_cingulum_mask ~ additional_hads_depression * story_d + age + gender, df_L1)
summary(L1_depression_int_story_age_gender) #no
L1_depression_int_mmse_age_gender <- lm(mean_L1_lower_cingulum_mask ~ additional_hads_depression * homeint_mmse_cal + age + gender, df_L1)
summary(L1_depression_int_mmse_age_gender) #no

#story interaction effects
L1_story_d_int_age_gender <- lm(mean_L1_lower_cingulum_mask ~ story_d * age + gender, df_L1)
summary(L1_story_d_int_age_gender) #yes
L1_story_d_int_gender_age <- lm(mean_L1_lower_cingulum_mask ~ story_d * gender + age, df_L1)
summary(L1_story_d_int_gender_age) #no
L1_story_d_int_mmse_age_gender <- lm(mean_L1_lower_cingulum_mask ~ story_d * homeint_mmse_cal + age + gender, df_L1)
summary(L1_story_d_int_mmse_age_gender) #no

upper_story_L1 <- df_L1 %>% filter(story_d > 0)
lower_story_L1 <- df_L1 %>% filter(story_d < 0)
upper_story_L1_age_gender <- lm(mean_L1_lower_cingulum_mask ~ age + gender, upper_story_L1)
lower_story_L1_age_gender <- lm(mean_L1_lower_cingulum_mask ~ age + gender, lower_story_L1)

L1_story_d_int_age_gender_plot_1 <- interact_plot(L1_story_d_int_age_gender, pred = age, modx = story_d, modx.values = "plus-minus", 
              partial.residuals = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
              legend.main = 'Delayed Story Recall Score'
              ) +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean AD", title = "Bilateral Mean AD",
       subtitle = paste0("Corrected for Gender \n",
                         "interaction p = ",
                         signif(summary(L1_story_d_int_age_gender)$coefficients[5,4], 2)
       )
  ) +
  geom_richtext(aes(x = -10, y = 9.5e-04, vjust = -1, 
                    label = paste0(
                      "p < 0.001", 
                      ", adj-R<sup>2</sup> = ",signif(summary(upper_story_L1_age_gender)$adj.r.squared, 2))),
                color = "darkblue", show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  geom_richtext(aes(x = -10, y = 9.5e-04, vjust = 1, 
                    label = paste0(
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ",signif(summary(lower_story_L1_age_gender)$adj.r.squared, 2)
                    )),
                show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
L1_story_d_int_age_gender_plot_1

lc_interaction_pptx <- add_slide(lc_interaction_pptx)
lc_interaction_pptx <- ph_with(x = lc_interaction_pptx, L1_story_d_int_age_gender_plot_1, location = ph_location_type(type = "body"))

upper_age_L1 <- df_L1 %>% filter(age > 0)
lower_age_L1 <- df_L1 %>% filter(age < 0)
upper_age_L1_story_gender <- lm(mean_L1_lower_cingulum_mask ~ story_d + gender, upper_age_L1)
lower_age_L1_story_gender <- lm(mean_L1_lower_cingulum_mask ~ story_d + gender, lower_age_L1)

L1_story_d_int_age_gender_plot_2 <- interact_plot(L1_story_d_int_age_gender, pred = story_d, modx = age, modx.values = "plus-minus", 
                                                  partial.residuals = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                                                  legend.main = 'Age'
) +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Story Delayed Recall Score", y = "Mean AD", title = "Bilateral Mean AD",
       subtitle = paste0("Corrected for Gender \n",
                         "interaction p = ",
                         signif(summary(L1_story_d_int_age_gender)$coefficients[5,4], 2)
       )
  ) +
  geom_richtext(aes(x = 8, y = 9.5e-04, vjust = -1, 
                    label = paste0(
                      "p = ", signif(summary(upper_age_L1_story_gender)$coefficients[2,4], 2), 
                      ", adj-R<sup>2</sup> = ",signif(summary(upper_age_L1_story_gender)$adj.r.squared, 2))),
                color = "darkblue", show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  geom_richtext(aes(x = 8, y = 9.5e-04, vjust = 1, 
                    label = paste0(
                      "p = ", signif(summary(lower_age_L1_story_gender)$coefficients[2,4], 2) 
                      # ", adj-R<sup>2</sup> = ",signif(summary(lower_age_L1_story_gender)$adj.r.squared, 2)
                      )),
                show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
L1_story_d_int_age_gender_plot_2

lc_interaction_pptx <- add_slide(lc_interaction_pptx)
lc_interaction_pptx <- ph_with(x = lc_interaction_pptx, L1_story_d_int_age_gender_plot_2, location = ph_location_type(type = "body"))

#mmse interaction effects
L1_mmse_int_age_gender <- lm(mean_L1_lower_cingulum_mask ~ homeint_mmse_cal * age + gender, df_L1)
summary(L1_mmse_int_age_gender) #no
L1_mmse_int_gender_age <- lm(mean_L1_lower_cingulum_mask ~ homeint_mmse_cal * gender + age, df_L1)
summary(L1_mmse_int_gender_age) #no

#select variables and remove outliers
df_RD <- df 
df_RD$mean_RD_lower_cingulum_mask <- remove_outliers(df$mean_RD_lower_cingulum_mask)
df_RD <- df_RD %>% drop_na(mean_RD_lower_cingulum_mask)

#cohort interaction effects
RD_cohort_int_age_gender <- lm(mean_RD_lower_cingulum_mask ~ cohort * age + gender, df_RD)
summary(RD_cohort_int_age_gender) #yes
RD_cohort_int_gender_age <- lm(mean_RD_lower_cingulum_mask ~ cohort * gender + age, df_RD)
summary(RD_cohort_int_gender_age) #no
RD_cohort_int_anxiety_age_gender <- lm(mean_RD_lower_cingulum_mask ~ cohort * additional_hads_anxiety + gender + age, df_RD)
summary(RD_cohort_int_anxiety_age_gender) #no
RD_cohort_int_depression_age_gender <- lm(mean_RD_lower_cingulum_mask ~ cohort * additional_hads_depression + gender + age, df_RD)
summary(RD_cohort_int_depression_age_gender) #no
RD_cohort_int_story_d_age_gender <- lm(mean_RD_lower_cingulum_mask ~ cohort * story_d + gender + age, df_RD)
summary(RD_cohort_int_story_d_age_gender) #no
RD_cohort_int_mmse_age_gender <- lm(mean_RD_lower_cingulum_mask ~ cohort * homeint_mmse_cal + age + gender, df_RD)
summary(RD_cohort_int_mmse_age_gender) #no

ctl_RD <- df_RD %>% filter(cohort == 'ctl')
scd_RD <- df_RD %>% filter(cohort == 'scd')
ctl_RD_age_gender <- lm(mean_RD_lower_cingulum_mask ~ age + gender, ctl_RD)
scd_RD_age_gender <- lm(mean_RD_lower_cingulum_mask ~ age + gender, scd_RD)

RD_cohort_int_age_gender_plot <- interact_plot(RD_cohort_int_age_gender, pred = age, modx = cohort, 
              partial.residuals = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
              modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean RD", title = "Bilateral Mean RD",
       subtitle = paste0("Corrected for Gender \n",
                         "interaction p = ",
                         signif(summary(RD_cohort_int_age_gender)$coefficients[5,4], 2)
       )
  ) +
  geom_richtext(aes(x = -10, y = 6e-04, vjust = -1, 
                    label = paste0(
                      # "p = ", signif(summary(ctl_RD_age_gender)$coefficients[2,4], 2), 
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ",signif(summary(ctl_RD_age_gender)$adj.r.squared, 2)),
                    color = "Control"), 
                show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  geom_richtext(aes(x = -10, y = 6e-04, vjust = 1, 
                    label = paste0(
                      # "p = ", signif(summary(scd_RD_age_gender)$coefficients[2,4], 2), 
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ",signif(summary(scd_RD_age_gender)$adj.r.squared, 2)),
                    color = "SCD"), 
                show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
RD_cohort_int_age_gender_plot

lc_interaction_pptx <- add_slide(lc_interaction_pptx)
lc_interaction_pptx <- ph_with(x = lc_interaction_pptx, RD_cohort_int_age_gender_plot, location = ph_location_type(type = "body"))

#anxiety interaction effects
RD_anxiety_int_age_gender <- lm(mean_RD_lower_cingulum_mask ~ additional_hads_anxiety * age + gender, df_RD)
summary(RD_anxiety_int_age_gender) #no
RD_anxiety_int_gender_age <- lm(mean_RD_lower_cingulum_mask ~ additional_hads_anxiety * gender + age, df_RD)
summary(RD_anxiety_int_gender_age) #no
RD_anxiety_int_depression_age_gender <- lm(mean_RD_lower_cingulum_mask ~ additional_hads_anxiety * additional_hads_depression + age + gender, df_RD)
summary(RD_anxiety_int_depression_age_gender) #no
RD_anxiety_int_story_age_gender <- lm(mean_RD_lower_cingulum_mask ~ additional_hads_anxiety * story_d + age + gender, df_RD)
summary(RD_anxiety_int_story_age_gender) #no
RD_anxiety_int_mmse_age_gender <- lm(mean_RD_lower_cingulum_mask ~ additional_hads_anxiety * homeint_mmse_cal + age + gender, df_RD)
summary(RD_anxiety_int_mmse_age_gender) #no

#depression interaction effects
RD_depression_int_age_gender <- lm(mean_RD_lower_cingulum_mask ~ additional_hads_depression * age + gender, df_RD)
summary(RD_depression_int_age_gender) #no
RD_depression_int_gender_age <- lm(mean_RD_lower_cingulum_mask ~ additional_hads_depression * gender + age, df_RD)
summary(RD_depression_int_gender_age) #no
RD_depression_int_story_age_gender <- lm(mean_RD_lower_cingulum_mask ~ additional_hads_depression * story_d + age + gender, df_RD)
summary(RD_depression_int_story_age_gender) #no
RD_depression_int_mmse_age_gender <- lm(mean_RD_lower_cingulum_mask ~ additional_hads_depression * homeint_mmse_cal + age + gender, df_RD)
summary(RD_depression_int_mmse_age_gender) #no

#story interaction effects
RD_story_d_int_age_gender <- lm(mean_RD_lower_cingulum_mask ~ story_d * age + gender, df_RD)
summary(RD_story_d_int_age_gender) #no
RD_story_d_int_gender_age <- lm(mean_RD_lower_cingulum_mask ~ story_d * gender + age, df_RD)
summary(RD_story_d_int_gender_age) #no
RD_story_d_int_mmse_age_gender <- lm(mean_RD_lower_cingulum_mask ~ story_d * homeint_mmse_cal + age + gender, df_RD)
summary(RD_story_d_int_mmse_age_gender) #no

#mmse interaction effects
RD_mmse_int_age_gender <- lm(mean_RD_lower_cingulum_mask ~ homeint_mmse_cal * age + gender, df_RD)
summary(RD_mmse_int_age_gender) #no
RD_mmse_int_gender_age <- lm(mean_RD_lower_cingulum_mask ~ homeint_mmse_cal * gender + age, df_RD)
summary(RD_mmse_int_gender_age) #no

#select variables and remove outliers
df_OD <- df 
df_OD$mean_OD_lower_cingulum_mask <- remove_outliers(df$mean_OD_lower_cingulum_mask)
df_OD <- df_OD %>% drop_na(mean_OD_lower_cingulum_mask)

#cohort interaction effects
OD_cohort_int_age_gender <- lm(mean_OD_lower_cingulum_mask ~ cohort * age + gender, df_OD)
summary(OD_cohort_int_age_gender) #no
OD_cohort_int_gender_age <- lm(mean_OD_lower_cingulum_mask ~ cohort * gender + age, df_OD)
summary(OD_cohort_int_gender_age) #no
OD_cohort_int_anxiety_age_gender <- lm(mean_OD_lower_cingulum_mask ~ cohort * additional_hads_anxiety + gender + age, df_OD)
summary(OD_cohort_int_anxiety_age_gender) #no
OD_cohort_int_depression_age_gender <- lm(mean_OD_lower_cingulum_mask ~ cohort * additional_hads_depression + gender + age, df_OD)
summary(OD_cohort_int_depression_age_gender) #no
OD_cohort_int_story_d_age_gender <- lm(mean_OD_lower_cingulum_mask ~ cohort * story_d + gender + age, df_OD)
summary(OD_cohort_int_story_d_age_gender) #no
OD_cohort_int_mmse_age_gender <- lm(mean_OD_lower_cingulum_mask ~ cohort * homeint_mmse_cal + age + gender, df_OD)
summary(OD_cohort_int_mmse_age_gender) #no

#anxiety interaction effects
OD_anxiety_int_age_gender <- lm(mean_OD_lower_cingulum_mask ~ additional_hads_anxiety * age + gender, df_OD)
summary(OD_anxiety_int_age_gender) #no
OD_anxiety_int_gender_age <- lm(mean_OD_lower_cingulum_mask ~ additional_hads_anxiety * gender + age, df_OD)
summary(OD_anxiety_int_gender_age) #no
OD_anxiety_int_depression_age_gender <- lm(mean_OD_lower_cingulum_mask ~ additional_hads_anxiety * additional_hads_depression + age + gender, df_OD)
summary(OD_anxiety_int_depression_age_gender) #no
OD_anxiety_int_story_age_gender <- lm(mean_OD_lower_cingulum_mask ~ additional_hads_anxiety * story_d + age + gender, df_OD)
summary(OD_anxiety_int_story_age_gender) #no
OD_anxiety_int_mmse_age_gender <- lm(mean_OD_lower_cingulum_mask ~ additional_hads_anxiety * homeint_mmse_cal + age + gender, df_OD)
summary(OD_anxiety_int_mmse_age_gender) #no

#depression interaction effects
OD_depression_int_age_gender <- lm(mean_OD_lower_cingulum_mask ~ additional_hads_depression * age + gender, df_OD)
summary(OD_depression_int_age_gender) #no
OD_depression_int_gender_age <- lm(mean_OD_lower_cingulum_mask ~ additional_hads_depression * gender + age, df_OD)
summary(OD_depression_int_gender_age) #no
OD_depression_int_story_age_gender <- lm(mean_OD_lower_cingulum_mask ~ additional_hads_depression * story_d + age + gender, df_OD)
summary(OD_depression_int_story_age_gender) #no
OD_depression_int_mmse_age_gender <- lm(mean_OD_lower_cingulum_mask ~ additional_hads_depression * homeint_mmse_cal + age + gender, df_OD)
summary(OD_depression_int_mmse_age_gender) #no

#story interaction effects
OD_story_d_int_age_gender <- lm(mean_OD_lower_cingulum_mask ~ story_d * age + gender, df_OD)
summary(OD_story_d_int_age_gender) #no
OD_story_d_int_gender_age <- lm(mean_OD_lower_cingulum_mask ~ story_d * gender + age, df_OD)
summary(OD_story_d_int_gender_age) #no
OD_story_d_int_mmse_age_gender <- lm(mean_OD_lower_cingulum_mask ~ story_d * homeint_mmse_cal + age + gender, df_OD)
summary(OD_story_d_int_mmse_age_gender) #no

#mmse interaction effects
OD_mmse_int_age_gender <- lm(mean_OD_lower_cingulum_mask ~ homeint_mmse_cal * age + gender, df_OD)
summary(OD_mmse_int_age_gender) #no
OD_mmse_int_gender_age <- lm(mean_OD_lower_cingulum_mask ~ homeint_mmse_cal * gender + age, df_OD)
summary(OD_mmse_int_gender_age) #no

#select variables and remove outliers
df_ISOVF <- df 
df_ISOVF$mean_ISOVF_lower_cingulum_mask <- remove_outliers(df$mean_ISOVF_lower_cingulum_mask)
df_ISOVF <- df_ISOVF %>% drop_na(mean_ISOVF_lower_cingulum_mask)

#cohort interaction effects
ISOVF_cohort_int_age_gender <- lm(mean_ISOVF_lower_cingulum_mask ~ cohort * age + gender, df_ISOVF)
summary(ISOVF_cohort_int_age_gender) #no
ISOVF_cohort_int_gender_age <- lm(mean_ISOVF_lower_cingulum_mask ~ cohort * gender + age, df_ISOVF)
summary(ISOVF_cohort_int_gender_age) #no
ISOVF_cohort_int_anxiety_age_gender <- lm(mean_ISOVF_lower_cingulum_mask ~ cohort * additional_hads_anxiety + gender + age, df_ISOVF)
summary(ISOVF_cohort_int_anxiety_age_gender) #no
ISOVF_cohort_int_depression_age_gender <- lm(mean_ISOVF_lower_cingulum_mask ~ cohort * additional_hads_depression + gender + age, df_ISOVF)
summary(ISOVF_cohort_int_depression_age_gender) #no
ISOVF_cohort_int_story_d_age_gender <- lm(mean_ISOVF_lower_cingulum_mask ~ cohort * story_d + gender + age, df_ISOVF)
summary(ISOVF_cohort_int_story_d_age_gender) #no
ISOVF_cohort_int_mmse_age_gender <- lm(mean_ISOVF_lower_cingulum_mask ~ cohort * homeint_mmse_cal + age + gender, df_ISOVF)
summary(ISOVF_cohort_int_mmse_age_gender) #no

#anxiety interaction effects
ISOVF_anxiety_int_age_gender <- lm(mean_ISOVF_lower_cingulum_mask ~ additional_hads_anxiety * age + gender, df_ISOVF)
summary(ISOVF_anxiety_int_age_gender) #no
ISOVF_anxiety_int_gender_age <- lm(mean_ISOVF_lower_cingulum_mask ~ additional_hads_anxiety * gender + age, df_ISOVF)
summary(ISOVF_anxiety_int_gender_age) #no
ISOVF_anxiety_int_depression_age_gender <- lm(mean_ISOVF_lower_cingulum_mask ~ additional_hads_anxiety * additional_hads_depression + age + gender, df_ISOVF)
summary(ISOVF_anxiety_int_depression_age_gender) #no
ISOVF_anxiety_int_story_age_gender <- lm(mean_ISOVF_lower_cingulum_mask ~ additional_hads_anxiety * story_d + age + gender, df_ISOVF)
summary(ISOVF_anxiety_int_story_age_gender) #no
ISOVF_anxiety_int_mmse_age_gender <- lm(mean_ISOVF_lower_cingulum_mask ~ additional_hads_anxiety * homeint_mmse_cal + age + gender, df_ISOVF)
summary(ISOVF_anxiety_int_mmse_age_gender) #no

#depression interaction effects
ISOVF_depression_int_age_gender <- lm(mean_ISOVF_lower_cingulum_mask ~ additional_hads_depression * age + gender, df_ISOVF)
summary(ISOVF_depression_int_age_gender) #no
ISOVF_depression_int_gender_age <- lm(mean_ISOVF_lower_cingulum_mask ~ additional_hads_depression * gender + age, df_ISOVF)
summary(ISOVF_depression_int_gender_age) #no
ISOVF_depression_int_story_age_gender <- lm(mean_ISOVF_lower_cingulum_mask ~ additional_hads_depression * story_d + age + gender, df_ISOVF)
summary(ISOVF_depression_int_story_age_gender) #no
ISOVF_depression_int_mmse_age_gender <- lm(mean_ISOVF_lower_cingulum_mask ~ additional_hads_depression * homeint_mmse_cal + age + gender, df_ISOVF)
summary(ISOVF_depression_int_mmse_age_gender) #no

#story interaction effects
ISOVF_story_d_int_age_gender <- lm(mean_ISOVF_lower_cingulum_mask ~ story_d * age + gender, df_ISOVF)
summary(ISOVF_story_d_int_age_gender) #yes
ISOVF_story_d_int_gender_age <- lm(mean_ISOVF_lower_cingulum_mask ~ story_d * gender + age, df_ISOVF)
summary(ISOVF_story_d_int_gender_age) #no
ISOVF_story_d_int_mmse_age_gender <- lm(mean_ISOVF_lower_cingulum_mask ~ story_d * homeint_mmse_cal + age + gender, df_ISOVF)
summary(ISOVF_story_d_int_mmse_age_gender) #no

upper_story_ISOVF <- df_ISOVF %>% filter(story_d > 0)
lower_story_ISOVF <- df_ISOVF %>% filter(story_d < 0)
upper_story_ISOVF_age_gender <- lm(mean_ISOVF_lower_cingulum_mask ~ age + gender, upper_story_ISOVF)
lower_story_ISOVF_age_gender <- lm(mean_ISOVF_lower_cingulum_mask ~ age + gender, lower_story_ISOVF)

ISOVF_story_d_int_age_gender_plot_1 <- interact_plot(ISOVF_story_d_int_age_gender, pred = age, modx = story_d, modx.values = "plus-minus", 
                                                  partial.residuals = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                                                  legend.main = 'Delayed Story Recall Score'
) +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean ISOVF", title = "Bilateral Mean ISOVF",
       subtitle = paste0("Corrected for Gender \n",
                         "interaction p = ",
                         signif(summary(ISOVF_story_d_int_age_gender)$coefficients[5,4], 2)
       )
  ) +
  geom_richtext(aes(x = -13, y = 0.6, vjust = -1, 
                    label = paste0(
                      "p < 0.001", 
                      ", adj-R<sup>2</sup> = ",signif(summary(upper_story_ISOVF_age_gender)$adj.r.squared, 2))),
                color = "darkblue", show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  geom_richtext(aes(x = -13, y = 0.6, vjust = 1, 
                    label = paste0(
                      "p < 0.001",
                      ", adj-R<sup>2</sup> = ",signif(summary(lower_story_ISOVF_age_gender)$adj.r.squared, 2)
                    )),
                show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ISOVF_story_d_int_age_gender_plot_1

lc_interaction_pptx <- add_slide(lc_interaction_pptx)
lc_interaction_pptx <- ph_with(x = lc_interaction_pptx, ISOVF_story_d_int_age_gender_plot_1, location = ph_location_type(type = "body"))

upper_age_ISOVF <- df_ISOVF %>% filter(age > 0)
lower_age_ISOVF <- df_ISOVF %>% filter(age < 0)
upper_age_ISOVF_story_gender <- lm(mean_ISOVF_lower_cingulum_mask ~ story_d + gender, upper_age_ISOVF)
lower_age_ISOVF_story_gender <- lm(mean_ISOVF_lower_cingulum_mask ~ story_d + gender, lower_age_ISOVF)

ISOVF_story_d_int_age_gender_plot_2 <- interact_plot(ISOVF_story_d_int_age_gender, pred = story_d, modx = age, modx.values = "plus-minus", 
                                                  partial.residuals = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                                                  legend.main = 'Age'
) +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Story Delayed Recall Score", y = "Mean ISOVF", title = "Bilateral Mean ISOVF",
       subtitle = paste0("Corrected for Gender \n",
                         "interaction p = ",
                         signif(summary(ISOVF_story_d_int_age_gender)$coefficients[5,4], 2)
       )
  ) +
  geom_richtext(aes(x = 8, y = 0.3, vjust = -1, 
                    label = paste0(
                      "p = ", signif(summary(upper_age_ISOVF_story_gender)$coefficients[2,4], 2), 
                      ", adj-R<sup>2</sup> = ",signif(summary(upper_age_ISOVF_story_gender)$adj.r.squared, 2))),
                color = "darkblue", show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  geom_richtext(aes(x = 8, y = 0.3, vjust = 1, 
                    label = paste0(
                      "p = ", signif(summary(lower_age_ISOVF_story_gender)$coefficients[2,4], 2), 
                      ", adj-R<sup>2</sup> = ",signif(summary(lower_age_ISOVF_story_gender)$adj.r.squared, 2)
                    )),
                show.legend = F, fill = NA, label.color = NA, label.padding = grid::unit(rep(0,4), "pt"))  +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ISOVF_story_d_int_age_gender_plot_2

lc_interaction_pptx <- add_slide(lc_interaction_pptx)
lc_interaction_pptx <- ph_with(x = lc_interaction_pptx, ISOVF_story_d_int_age_gender_plot_2, location = ph_location_type(type = "body"))

#mmse interaction effects
ISOVF_mmse_int_age_gender <- lm(mean_ISOVF_lower_cingulum_mask ~ homeint_mmse_cal * age + gender, df_ISOVF)
summary(ISOVF_mmse_int_age_gender) #no
ISOVF_mmse_int_gender_age <- lm(mean_ISOVF_lower_cingulum_mask ~ homeint_mmse_cal * gender + age, df_ISOVF)
summary(ISOVF_mmse_int_gender_age) #no

#select variables and remove outliers
df_ICVF <- df 
df_ICVF$mean_ICVF_lower_cingulum_mask <- remove_outliers(df_ICVF$mean_ICVF_lower_cingulum_mask)
df_ICVF <- df_ICVF %>% drop_na(mean_ICVF_lower_cingulum_mask)

#cohort interaction effects
df_ICVF$additional_hads_anxiety <- remove_outliers(df_ICVF$additional_hads_anxiety)
df_ICVF <- df_ICVF %>% drop_na(additional_hads_anxiety)

ICVF_cohort_int_age_gender <- lm(mean_ICVF_lower_cingulum_mask ~ cohort * age + gender, df_ICVF)
summary(ICVF_cohort_int_age_gender) #no
ICVF_cohort_int_gender_age <- lm(mean_ICVF_lower_cingulum_mask ~ cohort * gender + age, df_ICVF)
summary(ICVF_cohort_int_gender_age) #yes
ICVF_cohort_int_anxiety_age_gender <- lm(mean_ICVF_lower_cingulum_mask ~ cohort * additional_hads_anxiety + gender + age, df_ICVF)
summary(ICVF_cohort_int_anxiety_age_gender) #yes
ICVF_cohort_int_depression_age_gender <- lm(mean_ICVF_lower_cingulum_mask ~ cohort * additional_hads_depression + gender + age, df_ICVF)
summary(ICVF_cohort_int_depression_age_gender) #no
ICVF_cohort_int_story_d_age_gender <- lm(mean_ICVF_lower_cingulum_mask ~ cohort * story_d + gender + age, df_ICVF)
summary(ICVF_cohort_int_story_d_age_gender) #no
ICVF_cohort_int_mmse_age_gender <- lm(mean_ICVF_lower_cingulum_mask ~ cohort * homeint_mmse_cal + age + gender, df_ICVF)
summary(ICVF_cohort_int_mmse_age_gender) #no

male_ICVF <- df_ICVF %>% filter(gender == 'male')
female_ICVF <- df_ICVF %>% filter(gender == 'female')
male_ICVF_cohort_age <- lm(mean_ICVF_lower_cingulum_mask ~ cohort + age, male_ICVF)
female_ICVF_cohort_age <- lm(mean_ICVF_lower_cingulum_mask ~ cohort + age, female_ICVF)

# ICVF_cohort_int_gender_age_plot_1 <- cat_plot(ICVF_cohort_int_gender_age, pred = gender, modx = cohort, 
#               partial.residuals = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
#               modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
#   theme(legend.position = 'none') +
#   labs(x = "Gender", y = "Mean ICVF", title = "Bilateral Mean ICVF",
#        subtitle = paste0("Corrected for Age \n",
#                          "interaction p = ",
#                          signif(summary(ICVF_cohort_int_gender_age)$coefficients[5,4], 2)
#        )
#   ) +
#   geom_text(aes(x = 'male', y = 1, vjust = 1,
#                 label = paste0("p = ", signif(summary(male_ICVF_cohort_age)$coefficients[2,4], 2))), show.legend = F) +
#   geom_text(aes(x = 'female', y = 1, vjust = 1,
#                 label = paste0("p = ", signif(summary(female_ICVF_cohort_age)$coefficients[2,4], 2))), show.legend = F) +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5))
# ICVF_cohort_int_gender_age_plot_1
# 
# lc_interaction_pptx <- add_slide(lc_interaction_pptx)
# lc_interaction_pptx <- ph_with(x = lc_interaction_pptx, ICVF_cohort_int_gender_age_plot_1, location = ph_location_type(type = "body"))

ctl_ICVF <- df_ICVF %>% filter(cohort == 'ctl')
scd_ICVF <- df_ICVF %>% filter(cohort == 'scd')
ctl_ICVF_gender_age <- lm(mean_ICVF_lower_cingulum_mask ~ gender + age, ctl_ICVF)
scd_ICVF_gender_age <- lm(mean_ICVF_lower_cingulum_mask ~ gender + age, scd_ICVF)

ICVF_cohort_int_gender_age_plot_2 <- cat_plot(ICVF_cohort_int_gender_age, pred = cohort, modx = gender, 
                                              partial.residuals = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                                              modx.labels = c('Female', 'Male'), legend.main = 'Gender') +
  theme(legend.position = 'none') +
  labs(x = "Cohort", y = "Mean ICVF", title = "Bilateral Mean ICVF",
       subtitle = paste0("Corrected for Age \n",
                         "interaction p = ",
                         signif(summary(ICVF_cohort_int_gender_age)$coefficients[5,4], 2)
       )
  ) +
  geom_text(aes(x = 'ctl', y = 1, vjust = 1,
                # label = paste0("p = ", signif(summary(ctl_ICVF_gender_age)$coefficients[2,4], 2))),
                label = "p < 0.001"),
            color = "black", show.legend = F) +
  geom_text(aes(x = 'scd', y = 1, vjust = 1,
                label = paste0("p = ", signif(summary(scd_ICVF_gender_age)$coefficients[2,4], 2))), 
            color = "black", show.legend = F) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ICVF_cohort_int_gender_age_plot_2

lc_interaction_pptx <- add_slide(lc_interaction_pptx)
lc_interaction_pptx <- ph_with(x = lc_interaction_pptx, ICVF_cohort_int_gender_age_plot_2, location = ph_location_type(type = "body"))
#there is only a difference between genders in controls, not in SCD

ctl_ICVF <- df_ICVF %>% filter(cohort == 'ctl')
scd_ICVF <- df_ICVF %>% filter(cohort == 'scd')
ctl_ICVF_anxiety_age_gender <- lm(mean_ICVF_lower_cingulum_mask ~ additional_hads_anxiety + age + gender, ctl_ICVF)
scd_ICVF_anxiety_age_gender <- lm(mean_ICVF_lower_cingulum_mask ~ additional_hads_anxiety + age + gender, scd_ICVF)

ICVF_cohort_int_anxiety_age_gender_plot <- interact_plot(ICVF_cohort_int_anxiety_age_gender, pred = additional_hads_anxiety, modx = cohort, 
              partial.residuals = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
              modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Anxiety (Mean Centered HADS)", y = "Mean ICVF", title = "Mean ICVF in Bilateral Lower Cingulum",
       subtitle = paste0("Corrected for Age and Gender \n",
                         # "interaction p < 0.001"
                         "interaction p = ",
                         signif(summary(ICVF_cohort_int_anxiety_age_gender)$coefficients[6,4], 2)
       )
  ) +
  geom_text(aes(x = 14, y = 0.85, vjust = -1, 
                label = paste0("p = ", signif(summary(ctl_ICVF_anxiety_age_gender)$coefficients[2,4], 2)), 
                color = "Control"), show.legend = F) +
  geom_text(aes(x = 14, y = 0.85, vjust = 1, 
                label = paste0("p = ", signif(summary(scd_ICVF_anxiety_age_gender)$coefficients[2,4], 2)), 
                color = "SCD"), show.legend = F) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ICVF_cohort_int_anxiety_age_gender_plot
#anxiety related to different processes in SCD and controls?

lc_interaction_pptx <- add_slide(lc_interaction_pptx)
lc_interaction_pptx <- ph_with(x = lc_interaction_pptx, ICVF_cohort_int_anxiety_age_gender_plot, location = ph_location_type(type = "body"))

#anxiety interaction effects
ICVF_anxiety_int_age_gender <- lm(mean_ICVF_lower_cingulum_mask ~ additional_hads_anxiety * age + gender, df_ICVF)
summary(ICVF_anxiety_int_age_gender) #yes
ICVF_anxiety_int_gender_age <- lm(mean_ICVF_lower_cingulum_mask ~ additional_hads_anxiety * gender + age, df_ICVF)
summary(ICVF_anxiety_int_gender_age) #no
ICVF_anxiety_int_depression_age_gender <- lm(mean_ICVF_lower_cingulum_mask ~ additional_hads_anxiety * additional_hads_depression + age + gender, df_ICVF)
summary(ICVF_anxiety_int_depression_age_gender) #no
ICVF_anxiety_int_story_age_gender <- lm(mean_ICVF_lower_cingulum_mask ~ additional_hads_anxiety * story_d + age + gender, df_ICVF)
summary(ICVF_anxiety_int_story_age_gender) #no
ICVF_anxiety_int_mmse_age_gender <- lm(mean_ICVF_lower_cingulum_mask ~ additional_hads_anxiety * homeint_mmse_cal + age + gender, df_ICVF)
summary(ICVF_anxiety_int_mmse_age_gender) #no

upper_anxiety_ICVF <- df_ICVF %>% filter(additional_hads_anxiety > 0)
lower_anxiety_ICVF <- df_ICVF %>% filter(additional_hads_anxiety < 0)
upper_anxiety_ICVF_age_gender <- lm(mean_ICVF_lower_cingulum_mask ~ age + gender, upper_anxiety_ICVF)
lower_anxiety_ICVF_age_gender <- lm(mean_ICVF_lower_cingulum_mask ~ age + gender, lower_anxiety_ICVF)

ICVF_anxiety_int_age_gender_plot_1 <- interact_plot(ICVF_anxiety_int_age_gender, pred = age, modx = additional_hads_anxiety, modx.values = "plus-minus", 
                                                     partial.residuals = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                                                     legend.main = 'Anxiety (HADS)'
) +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Mean ICVF", title = "Bilateral Mean ICVF",
       subtitle = paste0("Corrected for Gender \n",
                         "interaction p = ",
                         signif(summary(ICVF_anxiety_int_age_gender)$coefficients[5,4], 2)
       )
  ) +
  geom_text(aes(x = 15, y = 0.85, vjust = -1,
                label = paste0("p = ", signif(summary(upper_anxiety_ICVF_age_gender)$coefficients[2,4], 2))),
                # label = "p < 0.001"),
            color = "darkblue", show.legend = F) +
  geom_text(aes(x = 15, y = 0.85, vjust = 1,
                # label = paste0("p = ", signif(summary(lower_anxiety_ICVF_age_gender)$coefficients[2,4], 2))),
                label = "p < 0.001"),
            show.legend = F) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ICVF_anxiety_int_age_gender_plot_1

lc_interaction_pptx <- add_slide(lc_interaction_pptx)
lc_interaction_pptx <- ph_with(x = lc_interaction_pptx, ICVF_anxiety_int_age_gender_plot_1, location = ph_location_type(type = "body"))

upper_age_ICVF <- df_ICVF %>% filter(age > 0)
lower_age_ICVF <- df_ICVF %>% filter(age < 0)
upper_age_ICVF_anxiety_gender <- lm(mean_ICVF_lower_cingulum_mask ~ additional_hads_anxiety + gender, upper_age_ICVF)
lower_age_ICVF_anxiety_gender <- lm(mean_ICVF_lower_cingulum_mask ~ additional_hads_anxiety + gender, lower_age_ICVF)

ICVF_anxiety_int_age_gender_plot_2 <- interact_plot(ICVF_anxiety_int_age_gender, pred = additional_hads_anxiety, modx = age, modx.values = "plus-minus", 
                                                     partial.residuals = TRUE, interval = TRUE, point.alpha = 1, vary.lty = FALSE,
                                                     legend.main = 'Age'
) +
  theme(legend.position = 'none') +
  labs(x = "Anxiety (Mean Centered HADS)", y = "Mean ICVF", title = "Bilateral Mean ICVF",
       subtitle = paste0("Corrected for Gender \n",
                         "interaction p = ",
                         signif(summary(ICVF_anxiety_int_age_gender)$coefficients[5,4], 2)
       )
  ) +
  geom_text(aes(x = 8, y = 0.85, vjust = -1,
                label = paste0("p = ", signif(summary(upper_age_ICVF_anxiety_gender)$coefficients[2,4], 2))),
            # label = "p < 0.001"), 
            color = "darkblue", show.legend = F) +
  geom_text(aes(x = 8, y = 0.85, vjust = 1,
                label = paste0("p = ", signif(summary(lower_age_ICVF_anxiety_gender)$coefficients[2,4], 2))),
            # label = "p < 0.001"),
            show.legend = F) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ICVF_anxiety_int_age_gender_plot_2

lc_interaction_pptx <- add_slide(lc_interaction_pptx)
lc_interaction_pptx <- ph_with(x = lc_interaction_pptx, ICVF_anxiety_int_age_gender_plot_2, location = ph_location_type(type = "body"))

#depression interaction effects
ICVF_depression_int_age_gender <- lm(mean_ICVF_lower_cingulum_mask ~ additional_hads_depression * age + gender, df_ICVF)
summary(ICVF_depression_int_age_gender) #no
ICVF_depression_int_gender_age <- lm(mean_ICVF_lower_cingulum_mask ~ additional_hads_depression * gender + age, df_ICVF)
summary(ICVF_depression_int_gender_age) #no
ICVF_depression_int_story_age_gender <- lm(mean_ICVF_lower_cingulum_mask ~ additional_hads_depression * story_d + age + gender, df_ICVF)
summary(ICVF_depression_int_story_age_gender) #no
ICVF_depression_int_mmse_age_gender <- lm(mean_ICVF_lower_cingulum_mask ~ additional_hads_depression * homeint_mmse_cal + age + gender, df_ICVF)
summary(ICVF_depression_int_mmse_age_gender) #no

#story interaction effects
ICVF_story_d_int_age_gender <- lm(mean_ICVF_lower_cingulum_mask ~ story_d * age + gender, df_ICVF)
summary(ICVF_story_d_int_age_gender) #no
ICVF_story_d_int_gender_age <- lm(mean_ICVF_lower_cingulum_mask ~ story_d * gender + age, df_ICVF)
summary(ICVF_story_d_int_gender_age) #no
ICVF_story_d_int_mmse_age_gender <- lm(mean_ICVF_lower_cingulum_mask ~ story_d * homeint_mmse_cal + age + gender, df_ICVF)
summary(ICVF_story_d_int_mmse_age_gender) #no

#mmse interaction effects
ICVF_mmse_int_age_gender <- lm(mean_ICVF_lower_cingulum_mask ~ homeint_mmse_cal * age + gender, df_ICVF)
summary(ICVF_mmse_int_age_gender) #no
ICVF_mmse_int_gender_age <- lm(mean_ICVF_lower_cingulum_mask ~ homeint_mmse_cal * gender + age, df_ICVF)
summary(ICVF_mmse_int_gender_age) #no

print(lc_interaction_pptx, target = "lc_interaction.pptx")
