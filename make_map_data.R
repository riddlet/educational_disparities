library(dplyr)
library(tidyr)

county_means <- read.csv('/Users/travis/Documents/gits/educational_disparities/output/county_means.csv')
county_means_sex <- read.csv('/Users/travis/Documents/gits/educational_disparities/output/county_means_sexuality.csv')
names(county_means_sex)[c(5:8)] <- c('n_bias_obs_sex', 'n_explicit_obs_sex', 'weighted_bias_sex', 'weighted_explicit_sex')
model.data <- read.csv('/Users/travis/Documents/gits/educational_disparities/output/full_model_data.csv')

model.data %>%
  filter(exclude==FALSE) %>%
  group_by(county_id, metric, group) %>%
  summarise(total = sum(number),
            total_students = sum(total_number)) %>%
  mutate(prop=total/total_students) %>%
  select(county_id, metric, group, prop) %>%
  spread(group, prop) %>%
  ungroup() %>%
  mutate(relative_risk = case_when(black == 0 & white == 0 ~ 1, 
                              TRUE ~ black/white)) %>%
  select(-black, -white) %>%
  spread(metric, relative_risk) -> relative_risks

model.data %>%
  filter(exclude==FALSE) %>%
  group_by(county_id, metric, group) %>%
  summarise(total = sum(number)) %>%
  ungroup() %>%
  mutate(gmet=paste(metric, group, sep='_')) %>%
  select(county_id, gmet, total) %>%
  spread(gmet, total) -> number_incidents

model.data %>%
  filter(exclude==FALSE) %>%
  group_by(county_id, group) %>%
  summarise(total = sum(total_number)) %>%
  ungroup() %>%
  spread(group, total) -> number_students

county_means %>%
  select(county_id, n_bias_obs:weighted_bias, weighted_explicit_diff) %>%
  left_join(county_means_sex) %>%
  select(-X, -bias, -explicit) %>%
  left_join(relative_risks) %>%
  left_join(number_incidents) %>%
  left_join(number_students) %>%
  mutate(implicit_obs = n_bias_obs,
         explicit_obs = n_explicit_obs,
         implicit = weighted_bias,
         explicit = weighted_explicit_diff,
         implicit_sex_obs = n_bias_obs_sex,
         explicit_sex_obs = n_explicit_obs_sex,
         implicit_sex = weighted_bias_sex,
         explicit_sex = weighted_explicit_sex) %>%
  select(county_id, expulsion_combined:explicit_sex) -> outdat

write.csv(outdat, file='/Users/travis/Documents/gits/educational_disparities/output/map_data.csv', row.names = F)
