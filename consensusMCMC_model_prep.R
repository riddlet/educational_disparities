library(dplyr)
set.seed(42)
dfs <- read.csv('/Users/travis/Documents/gits/educational_disparities/output/full_model_data.csv')
dft <- read.csv('/Users/travis/Documents/gits/educational_disparities/output/teacher_model_data.csv')

######## write consensus MCMC files
dfs %>% 
  select(county_id) %>% 
  distinct() -> counties

dft %>%
  select(county_id) %>%
  distinct() -> teacher_counties

counties %>% 
  mutate(grouping=sample(1:15, n(), replace=T)) -> county_assignments

teacher_counties %>%
  mutate(grouping=sample(1:12, n(), replace=T)) -> teacher_county_assignments

dfs %>% 
  left_join(county_assignments) -> county_groups #append the grouping information
dft %>%
  left_join(teacher_county_assignments) -> teacher_groups

write.csv(county_groups, file='/Users/travis/Documents/gits/educational_disparities/cluster/data/county_grouping.csv', row.names = F)
write.csv(teacher_groups, file='data/county_teacher_grouping.csv', row.names=F)

#### note that the script writing below is from when the model dataframes were written as separate files

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
group:bias + group:warmth + + 
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
  for (j in 1:11){
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
  for (j in 1:12){
    fs <- paste(string1, i, string2, j, string3, i, string4, i, "/m", j, ".rdata')", sep='')
    fileConn <- file(paste("model_scripts/mw_uclaexcl/",i,"/m", j, ".R", sep=''))
    writeLines(fs, fileConn)
    close(fileConn)
  }
}

######################## write weighted files for UCLA exclusions & explicit diff #############

#write files
string1 <- "library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(rstanarm)

options(mc.cores = parallel::detectCores())

df <- read.csv('/home/triddle/educational_disparities/cluster/data/county_grouping.csv')

df %>% filter(exclude==FALSE) %>% filter(metric=='"

string2 <- "') %>% select(county_id, bias:weighted_explicit_diff) %>% distinct() %>% mutate_at(vars(-county_id), scale) -> scaled_county_level

df %>% filter(grouping=="

string3 <- ") %>% filter(exclude==FALSE) %>% filter(metric=='"
string4 <- "') %>% select(county_id, total_pop:poverty_rate, crime_rate:b.w.ratio, COMBOKEY:metric) %>% left_join(scaled_county_level) -> mod.dat

m <- stan_glmer(cbind(number, total_number-number) ~ group + weighted_bias + 
weighted_explicit_diff + group:weighted_bias + group:weighted_explicit_diff + total_pop + 
unemp_rate + med_income + poverty_rate + col_grads + white_prop + 
black_prop + b.w.ratio + mobility + crime_rate + density + 
(group|county_id), data=mod.dat, prior = normal(0,5), 
prior_intercept = normal(0,5), family=binomial, adapt_delta=.99)

save(m, file='/tigress/triddle/educational_disparities/mw_uclaexcl_diff_extra_covs_extra_excl/"


for(i in rownames(table(dfs$metric))){
  for (j in 1:15){
    fs <- paste(string1, i, string2, j, string3, i, string4, i, "/m", j, ".rdata')", sep='')
    fileConn <- file(paste("/Users/travis/Documents/gits/educational_disparities/cluster/model_scripts/mw_uclaexcl_diff_extracovs_extra_excl/",i,"/m", j, ".R", sep=''))
    writeLines(fs, fileConn)
    close(fileConn)
  }
}

######################## write teacher files for explicit diffs #############
string1 <- "library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(rstanarm)

options(mc.cores = parallel::detectCores())

df <- read.csv('/home/triddle/educational_disparities/cluster/data/county_teacher_grouping_expdiff.csv')

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

save(m, file='/tigress/triddle/educational_disparities/teacher_metrics_expdiff/"


for(i in rownames(table(dft$metric))){
  for (j in 1:12){
    fs <- paste(string1, i, string2, j, string3, i, string4, i, "/m", j, ".rdata')", sep='')
    fileConn <- file(paste("/Users/travis/Documents/gits/educational_disparities/cluster/model_scripts/teacher_metrics_expdiff/",i,"/m", j, ".R", sep=''))
    writeLines(fs, fileConn)
    close(fileConn)
  }
}

######################## write weighted files w/3 way interaction #############

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

m <- stan_glmer(cbind(number, total_number-number) ~ group*weighted_bias*weighted_warmth
+ total_pop + unemp_rate + med_income + poverty_rate + col_grads + white_prop + 
black_prop + b.w.ratio + 
(group|county_id), data=mod.dat, prior = normal(0,5), 
prior_intercept = normal(0,5), family=binomial, adapt_delta=.99)

save(m, file='/tigress/triddle/educational_disparities/metrics_weighted_3way/"


for(i in rownames(table(dfs$metric))){
  for (j in 1:12){
    fs <- paste(string1, i, string2, j, string3, i, string4, i, "/m", j, ".rdata')", sep='')
    fileConn <- file(paste("/Users/travis/Documents/gits/educational_disparities/cluster/model_scripts/metrics_weighted_3way/",i,"/m", j, ".R", sep=''))
    writeLines(fs, fileConn)
    close(fileConn)
  }
}


######################## write weighted files for UCLA exclusions, explicit diff, nested intercept #############

#write files
string1 <- "library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(rstanarm)

options(mc.cores = parallel::detectCores())

df <- read.csv('/home/triddle/educational_disparities/cluster/data/county_grouping.csv')

df %>% filter(metric=='"

string2 <- "') %>% filter(exclude==FALSE) %>% select(county_id, bias:weighted_explicit_diff, total_pop, unemp_rate:b.w.ratio) %>% distinct() %>% mutate_at(vars(-county_id), scale) -> scaled_county_level

df %>% filter(grouping=="

string3 <- ") %>% filter(metric=='"
string4 <- "') %>% filter(exclude==FALSE) %>% select(county_id, COMBOKEY:metric) %>% left_join(scaled_county_level) -> mod.dat

m <- stan_glmer(cbind(number, total_number-number) ~ group + weighted_bias + 
weighted_explicit_diff + group:weighted_bias + group:weighted_explicit_diff + 
total_pop + unemp_rate + med_income + poverty_rate + col_grads + white_prop + 
black_prop + b.w.ratio + (group|county_id) + (1|county_id:COMBOKEY), 
data=mod.dat, prior = normal(0,5), prior_intercept = normal(0,5), 
family=binomial, adapt_delta=.99)

save(m, file='/tigress/triddle/educational_disparities/mw_uclaexcl_diff_nested/"


for(i in rownames(table(dfs$metric))){
  for (j in 1:14){
    fs <- paste(string1, i, string2, j, string3, i, string4, i, "/m", j, ".rdata')", sep='')
    fileConn <- file(paste("/Users/travis/Documents/gits/educational_disparities/cluster/model_scripts/mw_uclaexcl_diff_nested/",i,"/m", j, ".R", sep=''))
    writeLines(fs, fileConn)
    close(fileConn)
  }
}
