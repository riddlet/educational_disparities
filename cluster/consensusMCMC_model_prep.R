library(dplyr)
set.seed(42)
dfs <- read.csv('output/full_model_data.csv')

### write consensus MCMC files
dfs %>% 
  select(county_id) %>% 
  distinct() -> counties

counties %>% 
  mutate(grouping=sample(1:10, n(), replace=T)) -> county_assignments

dfs %>% 
  left_join(county_assignments) -> county_groups #append the grouping information

write.csv(county_groups, file='data/county_grouping.csv', row.names = F)

#write files
string1 <- "library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(rstanarm)

options(mc.cores = parallel::detectCores())

df <- read.csv('/home/triddle/educational_disparities/cluster/data/county_grouping.csv')

df %>% filter(grouping=="

string2 <-  ") %>% filter(metric=='"
string3 <- "') %>% select(county_id, bias:weighted_warmth, total_pop, unemp_rate:b.w.ratio) %>% distinct() %>% mutate_at(vars(-county_id), scale) -> scaled_county_level

df %>% filter(grouping=="

string4 <- ") %>% filter(metric=='"
string5 <- "') %>% select(county_id, COMBOKEY:metric) %>% left_join(scaled_county_level) -> mod.dat

m <- stan_glmer(cbind(number, total_number-number) ~ group + bias + warmth + 
group:bias + group:warmth + total_pop + unemp_rate + 
med_income + poverty_rate + col_grads + white_prop + 
black_prop + b.w.ratio + 
(group|county_id), data=mod.dat, prior = normal(0,5), 
prior_intercept = normal(0,5), family=binomial, adapt_delta=.99)

save(m, file='/tigress/triddle/educational_disparities/metrics_raw/"

for(i in rownames(table(dfs$metric))){
  for (j in 1:10){
    fs <- paste(string1, j, string2, i, string3, j, string4, i, string5, i, "/m", j, ".rdata')", sep='')
    fileConn <- file(paste("model_scripts/metrics_raw/",i,"/m", j, ".R", sep=''))
    writeLines(fs, fileConn)
    close(fileConn)
  }
}

#write files
string1 <- "library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(rstanarm)

options(mc.cores = parallel::detectCores())

df <- read.csv('/home/triddle/educational_disparities/cluster/data/county_grouping.csv')

df %>% filter(grouping=="

string2 <-  ") %>% filter(metric=='"
string3 <- "') %>% select(county_id, bias:weighted_warmth, total_pop, unemp_rate:b.w.ratio) %>% distinct() %>% mutate_at(vars(-county_id), scale) -> scaled_county_level

df %>% filter(grouping=="

string4 <- ") %>% filter(metric=='"
string5 <- "') %>% select(county_id, COMBOKEY:metric) %>% left_join(scaled_county_level) -> mod.dat

m <- stan_glmer(cbind(number, total_number-number) ~ group + weighted_bias + 
weighted_warmth + group:weighted_bias + group:weighted_warmth + total_pop + 
unemp_rate + med_income + poverty_rate + col_grads + white_prop + 
black_prop + b.w.ratio + 
(group|county_id), data=mod.dat, prior = normal(0,5), 
prior_intercept = normal(0,5), family=binomial, adapt_delta=.99)

save(m, file='/tigress/triddle/educational_disparities/metrics_weighted/"


for(i in rownames(table(dfs$metric))){
  for (j in 1:10){
    fs <- paste(string1, j, string2, i, string3, j, string4, i, string5, i, "/m", j, ".rdata')", sep='')
    fileConn <- file(paste("model_scripts/metrics_weighted/",i,"/m", j, ".R", sep=''))
    writeLines(fs, fileConn)
    close(fileConn)
  }
}