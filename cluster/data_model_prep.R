library(dplyr)

df <- read.csv('data/full_model_data.csv')

### write testing file - crossed
df %>% 
  group_by(LEA_STATE) %>% 
  select(COMBOKEY) %>% 
  distinct() -> school_state_combos #every unique school

labs <- paste('g', seq(1:525), sep='_')

school_state_combos %>%
  ungroup() %>%
  mutate(arr_column = runif(n(),0,1)) %>%
  group_by(LEA_STATE) %>%
  arrange(arr_column) %>%
  mutate(r_num = row_number()) %>%
  mutate(grouping = cut(r_num, 525, labels = labs)) -> group_assignments

df %>%
  select(LEA_STATE, bias, warmth, COMBOKEY, number, total_number, metric, group) %>%
  left_join(group_assignments) %>%
  group_by(grouping) %>%
  mutate(n_states = n_distinct(LEA_STATE)) %>%
  ungroup() %>%
  filter(n_states==51) %>%
  mutate(grouping = droplevels(grouping)) -> grouped_dat #append the grouping information

sampled_groups <- sample(rownames(table(grouped_dat$grouping)), 5)

grouped_dat %>%
  filter(grouping %in% sampled_groups) -> testdat

write.csv(testdat, file='data/big_bayes_testingdata4.csv')

### write testing file - distinct metrics (corporal)
df %>% 
  select(LEA_STATE) %>%
  distinct() -> states #every unique state

states$grouping <- sample(c(1:5), size = 51, replace=T)

df %>%
  filter(metric=='corporal') %>%
  select(LEA_STATE, bias, warmth, COMBOKEY, number, total_number, metric, group) %>%
  left_join(states) %>%
  group_by(grouping) %>%
  mutate(n_states = n_distinct(LEA_STATE)) -> grouped_dat #append the grouping information

write.csv(grouped_dat, file='data/big_bayes_testingdata5.csv')

### write testing file - distinct metrics (susp)
df %>% 
  select(LEA_STATE) %>%
  distinct() -> states #every unique state

states$grouping <- sample(c(1:5), size = 51, replace=T)

df %>%
  select(COMBOKEY) %>%
  distinct() -> schools

sampled_schools <- sample(schools$COMBOKEY, 7500)

df %>%
  filter(metric=='oos_susp') %>%
  select(LEA_STATE, bias, warmth, COMBOKEY, number, total_number, metric, group) %>%
  left_join(states) %>%
  group_by(grouping) %>%
  mutate(n_states = n_distinct(LEA_STATE)) -> grouped_dat #append the grouping information

grouped_dat %>%
  ungroup() %>%
  filter(COMBOKEY %in% sampled_schools) -> testdat

write.csv(testdat, file='data/big_bayes_testingdata6.csv')


##### generate model scripts\
# corporal can do full data
# pre_school can do ~5 samples
# all others ~ 10 samples
#all others 
df %>% 
  group_by(LEA_STATE) %>% 
  select(COMBOKEY) %>% 
  distinct() -> school_state_combos #every unique school
  
labs5 <- paste('g', seq(1:5), sep='_')
labs10 <- paste('g', seq(1:10), sep='_')

school_state_combos %>%
  ungroup() %>%
  mutate(arr_column = runif(n(),0,1)) %>% #to randomize the assignment
  group_by(LEA_STATE) %>%
  arrange(arr_column) %>%
  mutate(r_num = row_number()) %>%
  mutate(grouping_5 = cut(r_num, 5, labels = labs5),
         grouping_10 = cut(r_num, 10, labels=labs10)) -> group_assignments
  
df %>%
  select(LEA_STATE, bias, warmth, COMBOKEY, number, total_number, metric, group) %>%
  left_join(group_assignments) -> grouped_dat #append the grouping information

write.csv(grouped_dat, file='data/grouped_data.csv', row.names = F)


#### Write files (gap)
filestring1 <- "
library(dplyr)
library(tidyr)
library(rstanarm)

options(mc.cores = parallel::detectCores())

df <- read.csv('/home/triddle/educational_disparities/cluster/data/grouped_data.csv')
df %>% filter(metric == '"

filestring2 <- "', grouping_5=='g_"

filestring3 <- "') -> subdat

m <- stan_glmer(cbind(number, total_number-number) ~ group +
(group|LEA_STATE) + (1|COMBOKEY), data=subdat,
prior = normal(0,1), prior_intercept = normal(0,1),
family=binomial, adapt_delta=.99)

save(m, file='/home/triddle/educational_disparities/cluster/gap/"

filestring4 <- "/output/s"

#1.rdata')
#"
for(j in c('preschool_expulsion', 'preschool_susp')){
  for(i in 1:5){
    fs <- paste(filestring1, j, filestring2, i, filestring3, j, filestring4, i, ".rdata')", sep='')
    fileConn <- file(paste("model_scripts/gap/",j,'/s', i, '.R', sep=''))
    writeLines(fs, fileConn)
    close(fileConn)
  }
}

filestring2 <- "', grouping_10=='g_"

for(j in c('expulsion_0_tolerance', 'expulsion_w_ed', 'expulsion_wo_ed',
           'in_school_arrest', 'inschool_susp', 'law_enforcement', 
           'mechanical_restraint', 'oos_susp', 'physical_restraint', 'seclusion')){
  for(i in 1:10){
    fs <- paste(filestring1, j, filestring2, i, filestring3, j, filestring4, i, ".rdata')", sep='')
    fileConn <- file(paste("model_scripts/gap/",j,'/s', i, '.R', sep=''))
    writeLines(fs, fileConn)
    close(fileConn)
  }
}

### write files (gap)

filestring3 <- "') -> subdat

m <- stan_glmer(cbind(number, total_number-number) ~ group*bias +
(group|LEA_STATE) + (1|COMBOKEY), data=subdat,
prior = normal(0,1), prior_intercept = normal(0,1),
family=binomial, adapt_delta=.99)

save(m, file='/home/triddle/educational_disparities/cluster/bias/"

for(j in c('preschool_expulsion', 'preschool_susp')){
  for(i in 1:5){
    fs <- paste(filestring1, j, filestring2, i, filestring3, j, filestring4, i, ".rdata')", sep='')
    fileConn <- file(paste("model_scripts/bias/",j,'/s', i, '.R', sep=''))
    writeLines(fs, fileConn)
    close(fileConn)
  }
}

filestring2 <- "', grouping_10=='g_"

for(j in c('expulsion_0_tolerance', 'expulsion_w_ed', 'expulsion_wo_ed',
           'in_school_arrest', 'inschool_susp', 'law_enforcement', 
           'mechanical_restraint', 'oos_susp', 'physical_restraint', 'seclusion')){
  for(i in 1:10){
    fs <- paste(filestring1, j, filestring2, i, filestring3, j, filestring4, i, ".rdata')", sep='')
    fileConn <- file(paste("model_scripts/bias/",j,'/s', i, '.R', sep=''))
    writeLines(fs, fileConn)
    close(fileConn)
  }
}

### write files (warmth)

filestring3 <- "') -> subdat

m <- stan_glmer(cbind(number, total_number-number) ~ group*warmth +
(group|LEA_STATE) + (1|COMBOKEY), data=subdat,
prior = normal(0,1), prior_intercept = normal(0,1),
family=binomial, adapt_delta=.99)

save(m, file='/home/triddle/educational_disparities/cluster/warmth/"

for(j in c('preschool_expulsion', 'preschool_susp')){
  for(i in 1:5){
    fs <- paste(filestring1, j, filestring2, i, filestring3, j, filestring4, i, ".rdata')", sep='')
    fileConn <- file(paste("model_scripts/bias/",j,'/s', i, '.R', sep=''))
    writeLines(fs, fileConn)
    close(fileConn)
  }
}

filestring2 <- "', grouping_10=='g_"

for(j in c('expulsion_0_tolerance', 'expulsion_w_ed', 'expulsion_wo_ed',
           'in_school_arrest', 'inschool_susp', 'law_enforcement', 
           'mechanical_restraint', 'oos_susp', 'physical_restraint', 'seclusion')){
  for(i in 1:10){
    fs <- paste(filestring1, j, filestring2, i, filestring3, j, filestring4, i, ".rdata')", sep='')
    fileConn <- file(paste("model_scripts/bias/",j,'/s', i, '.R', sep=''))
    writeLines(fs, fileConn)
    close(fileConn)
  }
}
