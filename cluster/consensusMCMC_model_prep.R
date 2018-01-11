library(dplyr)
set.seed(42)
dfs <- read.csv('output/full_model_data.csv')
dfss <- read.csv('output/selected_model_data_ucla_excl.csv')
dfss_diff <- read.csv('output/selected_model_data_ucla_excl_exp_diff.csv')
dft <- read.csv('output/teacher_model_data.csv')

######## write consensus MCMC files
dfs %>% 
  select(county_id) %>% 
  distinct() -> counties

dft %>%
  select(county_id) %>%
  distinct() -> teacher_counties

counties %>% 
  mutate(grouping=sample(1:11, n(), replace=T)) -> county_assignments

teacher_counties %>%
  mutate(grouping=sample(1:9, n(), replace=T)) -> teacher_county_assignments

dfs %>% 
  left_join(county_assignments) -> county_groups #append the grouping information
dfss %>%
  left_join(county_assignments) -> subcounty_groups
dfss_diff %>%
  left_join(county_assignments) -> subcounty_groups_diff
dft %>%
  left_join(teacher_county_assignments) -> teacher_groups

write.csv(county_groups, file='data/county_grouping.csv', row.names = F)
write.csv(subcounty_groups, file='data/subcounty_grouping_ucla_excl.csv', row.names=F)
write.csv(subcounty_groups_diff, file='data/subcounty_grouping_diff.csv', row.names=F)
write.csv(teacher_groups, file='data/county_teacher_grouping.csv', row.names=F)


######################## write raw files #############

string1 <- "library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(rstanarm)

options(mc.cores = parallel::detectCores())

df <- read.csv('/home/triddle/educational_disparities/cluster/data/county_grouping.csv')

df %>%  filter(metric=='"
string2 <- "') %>% select(county_id, bias:weighted_warmth, total_pop, unemp_rate:b.w.ratio) %>% distinct() %>% mutate_at(vars(-county_id), scale) -> scaled_county_level

df %>% filter(grouping=="

string3 <- ") %>% filter(metric=='"
string4 <- "') %>% select(county_id, COMBOKEY:metric) %>% left_join(scaled_county_level) -> mod.dat

m <- stan_glmer(cbind(number, total_number-number) ~ group + bias + warmth + 
group:bias + group:warmth + total_pop + unemp_rate + 
med_income + poverty_rate + col_grads + white_prop + 
black_prop + b.w.ratio + 
(group|county_id), data=mod.dat, prior = normal(0,5), 
prior_intercept = normal(0,5), family=binomial, adapt_delta=.99)

save(m, file='/scratch/network/triddle/educational_disparities/metrics_raw/"

for(i in rownames(table(dfs$metric))){
  for (j in 1:10){
    fs <- paste(string1, i, string2, j, string3, i, string4, i, "/m", j, ".rdata')", sep='')
    fileConn <- file(paste("model_scripts/metrics_raw/",i,"/m", j, ".R", sep=''))
    writeLines(fs, fileConn)
    close(fileConn)
  }
}

######################## write weighted files #############

#write files
string1 <- "library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(rstanarm)

options(mc.cores = parallel::detectCores())

df <- read.csv('/home/triddle/educational_disparities/cluster/data/county_grouping.csv')

df %>% filter(metric=='"

string2 <- "') %>% select(county_id, bias:weighted_warmth, total_pop, unemp_rate:b.w.ratio) %>% distinct() %>% mutate_at(vars(-county_id), scale) -> scaled_county_level

df %>% filter(grouping=="

string3 <- ") %>% filter(metric=='"
string4 <- "') %>% select(county_id, COMBOKEY:metric) %>% left_join(scaled_county_level) -> mod.dat

m <- stan_glmer(cbind(number, total_number-number) ~ group + weighted_bias + 
weighted_warmth + group:weighted_bias + group:weighted_warmth + total_pop + 
unemp_rate + med_income + poverty_rate + col_grads + white_prop + 
black_prop + b.w.ratio + 
(group|county_id), data=mod.dat, prior = normal(0,5), 
prior_intercept = normal(0,5), family=binomial, adapt_delta=.99)

save(m, file='/tigress/triddle/educational_disparities/metrics_weighted/"


for(i in rownames(table(dfs$metric))){
  for (j in 1:10){
    fs <- paste(string1, i, string2, j, string3, i, string4, i, "/m", j, ".rdata')", sep='')
    fileConn <- file(paste("model_scripts/metrics_weighted/",i,"/m", j, ".R", sep=''))
    writeLines(fs, fileConn)
    close(fileConn)
  }
}


######################## write teacher files #############
string1 <- "library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(rstanarm)

options(mc.cores = parallel::detectCores())

df <- read.csv('/home/triddle/educational_disparities/cluster/data/county_teacher_grouping.csv')

df %>% filter(metric=='"

string2 <- "') %>% select(county_id, county_bias:county_warmth, total_pop, unemp_rate:b.w.ratio) %>% distinct() %>% mutate_at(vars(-county_id), scale) -> scaled_county_level

df %>% filter(grouping=="

string3 <- ") %>% filter(metric=='"
string4 <- "') %>% select(COMBOKEY:metric, county_id) %>% left_join(scaled_county_level) -> mod.dat

m <- stan_glmer(cbind(number, total_number-number) ~ group + county_bias + 
county_warmth + group:county_bias + group:county_warmth + total_pop + 
unemp_rate + med_income + poverty_rate + col_grads + white_prop + black_prop + b.w.ratio + 
(group|county_id), data=mod.dat, prior = normal(0,5), 
prior_intercept = normal(0,5), family=binomial, adapt_delta=.99)

save(m, file='/tigress/triddle/educational_disparities/teacher_metrics_della/"


for(i in rownames(table(dft$metric))){
  for (j in 1:9){
    fs <- paste(string1, i, string2, j, string3, i, string4, i, "/m", j, ".rdata')", sep='')
    fileConn <- file(paste("model_scripts/teacher_metrics/",i,"/m", j, ".R", sep=''))
    writeLines(fs, fileConn)
    close(fileConn)
  }
}

######################## write weighted files for UCLA exclusions #############

#write files
string1 <- "library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(rstanarm)

options(mc.cores = parallel::detectCores())

df <- read.csv('/home/triddle/educational_disparities/cluster/data/subcounty_grouping_ucla_excl.csv')

df %>% filter(metric=='"

string2 <- "') %>% select(county_id, bias:weighted_warmth, total_pop, unemp_rate:b.w.ratio) %>% distinct() %>% mutate_at(vars(-county_id), scale) -> scaled_county_level

df %>% filter(grouping=="

string3 <- ") %>% filter(metric=='"
string4 <- "') %>% select(county_id, COMBOKEY:metric) %>% left_join(scaled_county_level) -> mod.dat

m <- stan_glmer(cbind(number, total_number-number) ~ group + weighted_bias + 
weighted_warmth + group:weighted_bias + group:weighted_warmth + total_pop + 
unemp_rate + med_income + poverty_rate + col_grads + white_prop + 
black_prop + b.w.ratio + 
(group|county_id), data=mod.dat, prior = normal(0,5), 
prior_intercept = normal(0,5), family=binomial, adapt_delta=.99)

save(m, file='/tigress/triddle/educational_disparities/mw_uclaexcl/"


for(i in rownames(table(dfss$metric))){
  for (j in 1:10){
    fs <- paste(string1, i, string2, j, string3, i, string4, i, "/m", j, ".rdata')", sep='')
    fileConn <- file(paste("model_scripts/mw_uclaexcl/",i,"/m", j, ".R", sep=''))
    writeLines(fs, fileConn)
    close(fileConn)
  }
}

######### write weighted files for UCLA exclusions & explicit diff #############

#write files
string1 <- "library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(rstanarm)

options(mc.cores = parallel::detectCores())

df <- read.csv('/home/triddle/educational_disparities/cluster/data/subcounty_grouping_diff.csv')

df %>% filter(metric=='"

string2 <- "') %>% select(county_id, bias:weighted_warmth, total_pop, unemp_rate:b.w.ratio) %>% distinct() %>% mutate_at(vars(-county_id), scale) -> scaled_county_level

df %>% filter(grouping=="

string3 <- ") %>% filter(metric=='"
string4 <- "') %>% select(county_id, COMBOKEY:metric) %>% left_join(scaled_county_level) -> mod.dat

m <- stan_glmer(cbind(number, total_number-number) ~ group + weighted_bias + 
weighted_warmth + group:weighted_bias + group:weighted_warmth + total_pop + 
unemp_rate + med_income + poverty_rate + col_grads + white_prop + 
black_prop + b.w.ratio + 
(group|county_id), data=mod.dat, prior = normal(0,5), 
prior_intercept = normal(0,5), family=binomial, adapt_delta=.99)

save(m, file='/tigress/triddle/educational_disparities/mw_uclaexcl_diff/"


for(i in rownames(table(dfss$metric))){
  for (j in 1:11){
    fs <- paste(string1, i, string2, j, string3, i, string4, i, "/m", j, ".rdata')", sep='')
    fileConn <- file(paste("model_scripts/mw_uclaexcl_diff//",i,"/m", j, ".R", sep=''))
    writeLines(fs, fileConn)
    close(fileConn)
  }
}
