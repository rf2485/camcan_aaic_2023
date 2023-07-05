library(tidyverse)
library(interactions)
source('cleaning.R')

#group differences in anxiety and depression?
cohort_anxiety <- lm(additional_hads_anxiety ~ cohort, df)
summary(cohort_anxiety) #yes
cohort_depression <- lm(additional_hads_depression ~ cohort, df)
summary(cohort_depression) #yes
gender_depression <- lm(additional_hads_depression ~ gender, df)
summary(gender_depression)
gender_anxiety <- lm(additional_hads_anxiety ~ gender, df)
summary(gender_anxiety)
boxplot(additional_hads_anxiety~gender, data=df)
#should we correct for age, gender, and interactions with anxiety?
cohort_anxiety_age <- lm(additional_hads_anxiety ~ cohort + age, df)
anova(cohort_anxiety, cohort_anxiety_age) #age improves model
cohort_anxiety_age_gender <- lm(additional_hads_anxiety ~ cohort + age + gender, df)
anova(cohort_anxiety_age, cohort_anxiety_age_gender) #gender improves model
cohort_anxiety_int_age_gender <- lm(additional_hads_anxiety ~ cohort * age + gender, df)
anova(cohort_anxiety_age_gender, cohort_anxiety_int_age_gender) #no improvement
cohort_anxiety_int_gender_age <- lm(additional_hads_anxiety ~ cohort * gender + age, df)
anova(cohort_anxiety_age_gender, cohort_anxiety_int_gender_age) #no improvement
summary(cohort_anxiety_age_gender) #significant group differences
#should we correct for age, gender, and interactions with depression?
cohort_depression_age <- lm(additional_hads_depression ~ cohort + age, df)
anova(cohort_depression, cohort_depression_age) #no improvement
cohort_depression_gender <- lm(additional_hads_depression ~ cohort + gender, df)
anova(cohort_depression, cohort_depression_gender) #no improvement
cohort_depression_int_age <- lm(additional_hads_depression ~ cohort * age, df)
anova(cohort_depression, cohort_depression_int_age) #no improvement
cohort_depression_int_gender <- lm(additional_hads_depression ~ cohort * gender, df)
anova(cohort_depression, cohort_depression_int_gender) #no improvement
summary(cohort_depression) #significant group differences

#group differences in memory?
cohort_story_i <- lm(homeint_storyrecall_i ~ cohort, df)
summary(cohort_story_i) #yes
cohort_story_d <- lm(story_d ~ cohort, df)
summary(cohort_story_d) #yes
cohort_mmse <- lm(homeint_mmse_cal ~ cohort, df)
summary(cohort_mmse) #no
cohort_objprpos <- lm(objprpos ~ cohort, df)
summary(cohort_objprpos) #no
cohort_objprneg <- lm(objprneg ~ cohort, df)
summary(cohort_objprneg) #no
cohort_objprneu <- lm(objprneu ~ cohort, df)
summary(cohort_objprneu) #no
cohort_valprpos <- lm(valprpos ~ cohort, df)
summary(cohort_valprpos) #no
cohort_valprneg <- lm(valprneg ~ cohort, df)
summary(cohort_valprneg)
cohort_valprneu <- lm(valprneu ~ cohort, df)
summary(cohort_valprneu) #no
#should we correct for age, gender, and interactions with memory?
cohort_story_age <- lm(story_d ~ cohort + age, df)
anova(cohort_story_d, cohort_story_age) #age improves model
cohort_story_age_gender <- lm(story_d ~ cohort + age + gender, df)
anova(cohort_story_age, cohort_story_age_gender) #no improvement
cohort_story_int_age <- lm(story_d ~ cohort * age, df)
anova(cohort_story_age, cohort_story_int_age) #age_int_story improves model
cohort_story_int_age_int_gender <- lm(story_d ~ cohort * age * gender, df)
anova(cohort_story_int_age, cohort_story_int_age_int_gender) #no improvement
summary(cohort_story_int_age) #significant interaction effect

interact_plot(cohort_story_int_age, pred = age, modx = cohort, 
              plot.points = T, interval = T, point.alpha = 1, vary.lty = FALSE,
              modx.labels = c('Control', 'SCD'), legend.main = 'Cohort') +
  theme(legend.position = 'none') +
  labs(x = "Mean Centered Age", y = "Delayed Story Recall", title = "Age vs Story Delayed Recall",
       subtitle = paste0( "interaction p = ",
                         signif(summary(cohort_story_int_age)$coefficients[4,4], 2)
       )
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

cohort_mmse_age <- lm(homeint_mmse_cal ~ cohort + age, df)
anova(cohort_mmse, cohort_mmse_age) #age improves model
cohort_mmse_age_gender <- lm(homeint_mmse_cal ~ cohort + age + gender, df)
anova(cohort_mmse_age, cohort_mmse_age_gender) #no improvement
cohort_mmse_int_age <- lm(homeint_mmse_cal ~ cohort * age, df)
anova(cohort_mmse_age, cohort_mmse_int_age) #no improvement
cohort_mmse_int_gender_age <- lm(homeint_mmse_cal ~ cohort * gender + age, df)
anova(cohort_mmse_age, cohort_mmse_int_gender_age) #no improvement
summary(cohort_mmse_age) #group differences still not significant

#group differences in RT?
cohort_rtsimple <- lm(rtmean_simple ~ cohort, df)
summary(cohort_rtsimple) #no
cohort_rtchoice <- lm(rtmean_choice ~ cohort, df)
summary(cohort_rtchoice) #no

#anxiety and depression correlates with memory?
anxiety_story_d <- lm(additional_hads_anxiety ~ story_d, df)
summary(anxiety_story_d) #no
anxiety_story_i <- lm(additional_hads_anxiety ~ homeint_storyrecall_i, df)
summary(anxiety_story_i) #no
anxiety_mmse <- lm(additional_hads_anxiety ~ homeint_mmse_cal, df)
summary(anxiety_mmse) #no

depression_story_d <- lm(additional_hads_depression ~ story_d, df)
summary(depression_story_d) #no
depression_story_i <- lm(additional_hads_depression ~ homeint_storyrecall_i, df)
summary(depression_story_i) #no
depression_mmse <- lm(additional_hads_depression ~ homeint_mmse_cal, df)
summary(depression_mmse) #no

