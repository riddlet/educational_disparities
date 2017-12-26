library(dplyr)
set.seed(42)
dfs <- read.csv('output/full_model_data.csv')

### write cross validating data & model files
dfs %>% 
  group_by(county_id) %>% 
  select(COMBOKEY) %>% 
  distinct() -> school_state_combos #every unique school

school_state_combos %>%
  ungroup() %>%
  mutate(grouping = sample(1:175, n(), replace=T)) -> group_assignments

dfs %>%
  left_join(group_assignments) -> grouped_dat #append the grouping information

write.csv(grouped_dat, file='data/cross_val_grouping.csv', row.names = F)

#write files
string1 <- "library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(rstanarm)

options(mc.cores = parallel::detectCores())

df <- read.csv('/home/triddle/educational_disparities/cluster/data/cross_val_grouping.csv')

df %>% filter(grouping=="

string2 <-  ") %>% select(county_id, bias:weighted_warmth, total_pop, unemp_rate:b.w.ratio) %>% distinct() %>% mutate_at(vars(-county_id), scale) -> scaled_county_level

df %>% filter(grouping=="

string3 <- ") %>% select(county_id, COMBOKEY:metric) %>% left_join(scaled_county_level) -> mod.dat

m <- stan_glmer(cbind(number, total_number-number) ~ group + bias + warmth + 
                  group:bias + group:warmth + total_pop + unemp_rate + 
                  med_income + poverty_rate + col_grads + white_prop + 
                  black_prop + b.w.ratio + 
                  (group|county_id) + 
                  (group + bias + warmth + group:bias + group:warmth + 
                    total_pop + unemp_rate + med_income + poverty_rate + 
                    col_grads + white_prop + black_prop + b.w.ratio|metric) + 
                  (group|COMBOKEY), 
                data=mod.dat, prior = normal(0,5), 
                prior_intercept = normal(0,5), family=binomial, adapt_delta=.99)

save(m, file='/tigress/triddle/educational_disparities/cross_val_raw/m"

for(i in 1:175){
  fs <- paste(string1, i, string2, i, string3, i, ".rdata')", sep='')
  fileConn <- file(paste("model_scripts/cross_val_raw/m", i, ".R", sep=''))
  writeLines(fs, fileConn)
  close(fileConn)
}

#### cross validation with iat data
#write files
string1 <- "library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(rstanarm)

options(mc.cores = parallel::detectCores())

df <- read.csv('/home/triddle/educational_disparities/cluster/data/cross_val_grouping.csv')

df %>% filter(grouping=="

string2 <-  ") %>% select(county_id, bias:weighted_warmth, total_pop, unemp_rate:b.w.ratio) %>% distinct() %>% mutate_at(vars(-county_id), scale) -> scaled_county_level

df %>% filter(grouping=="

string3 <- ") %>% select(county_id, COMBOKEY:metric) %>% left_join(scaled_county_level) -> mod.dat

m <- stan_glmer(cbind(number, total_number-number) ~ group + weighted_bias + 
                  weighted_warmth + group:weighted_bias + 
                  group:weighted_warmth + total_pop + unemp_rate + med_income + 
                  poverty_rate + col_grads + white_prop + black_prop + 
                  b.w.ratio + 
                  (group|county_id) + 
                  (group + weighted_bias + weighted_warmth + 
                    group:weighted_bias + group:weighted_warmth + total_pop + 
                    unemp_rate + med_income + poverty_rate + col_grads + 
                    white_prop + black_prop + b.w.ratio|metric) + 
                  (group|COMBOKEY), 
                data=mod.dat, prior = normal(0,5), 
                prior_intercept = normal(0,5), family=binomial, adapt_delta=.99)

save(m, file='/tigress/triddle/educational_disparities/cross_val_weighted/m"

for(i in 1:175){
  fs <- paste(string1, i, string2, i, string3, i, ".rdata')", sep='')
  fileConn <- file(paste("model_scripts/cross_val_weighted/m", i, ".R", sep=''))
  writeLines(fs, fileConn)
  close(fileConn)
}

#for i in model_scripts/cross_val_raw/*.R; do sbatch batch_submit.cmd $i; done;