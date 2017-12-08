library(dplyr)

df <- read.csv('data/full_model_data.csv')

### write cross validating data & model files
df %>% 
  group_by(LEA_STATE) %>% 
  select(COMBOKEY) %>% 
  distinct() -> school_state_combos #every unique school

labs <- paste('g', seq(1:100), sep='_')

school_state_combos %>%
  ungroup() %>%
  mutate(arr_column = runif(n(),0,1)) %>%
  group_by(LEA_STATE) %>%
  arrange(arr_column) %>%
  mutate(r_num = row_number()) %>%
  mutate(grouping = cut(r_num, 100, labels = labs)) -> group_assignments

df %>%
  select(LEA_STATE, bias, warmth, COMBOKEY, number, total_number, metric, group) %>%
  left_join(group_assignments) %>%
  group_by(grouping) %>%
  mutate(n_states = n_distinct(LEA_STATE)) %>%
  ungroup() %>%
  filter(n_states==51) %>%
  mutate(grouping = droplevels(grouping)) -> grouped_dat #append the grouping information

write.csv(grouped_dat, file='data/cross_val_grouping.csv', row.names = F)

#write files
string1 <- "library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(rstanarm)

options(mc.cores = parallel::detectCores())

df <- read.csv('/home/triddle/educational_disparities/cluster/data/cross_val_grouping.csv')
df %>% filter(grouping=='g_"

string2 <-  "') -> mod.dat

m <- stan_glmer(cbind(number, total_number-number) ~ group + (group|LEA_STATE) + 
                  (group|metric) + (1|COMBOKEY), 
                data=mod.dat, prior = normal(0,1), 
                prior_intercept = normal(0,1), family=binomial, adapt_delta=.99)

save(m, file='/tigress/triddle/educational_disparities/cross_val/m"

for(i in 1:100){
  fs <- paste(string1, i, string2, i, ".rdata')", sep='')
  fileConn <- file(paste("model_scripts/cross_val/m", i, ".R", sep=''))
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
df %>% filter(grouping=='g_"

string2 <-  "') -> mod.dat

m <- stan_glmer(cbind(number, total_number-number) ~ group*bias + (group|LEA_STATE) + 
(group*bias|metric) + (1|COMBOKEY), 
data=mod.dat, prior = normal(0,1), 
prior_intercept = normal(0,1), family=binomial, adapt_delta=.99)

save(m, file='/tigress/triddle/educational_disparities/cross_val_bias/m"

for(i in 1:100){
  fs <- paste(string1, i, string2, i, ".rdata')", sep='')
  fileConn <- file(paste("model_scripts/cross_val_bias/m", i, ".R", sep=''))
  writeLines(fs, fileConn)
  close(fileConn)
}
