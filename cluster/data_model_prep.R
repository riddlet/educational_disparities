library(dplyr)

df <- read.csv('data/full_model_data.csv')

### write testing file
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

##### generate full file
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
  left_join(group_assignments) -> grouped_dat #append the grouping information

write.csv(grouped_dat, file='data/grouped_data.csv', row.names = F)

filestring1 <- "
library(dplyr)
library(tidyr)
library(rstanarm)

options(mc.cores = parallel::detectCores())

df <- read.csv('/home/triddle/educational_disparities/big_bayes/grouped_data.csv')
df %>% filter(grouping==g_"

filestring2 <- ") -> subdat

m <- stan_glmer(cbind(number, total_number-number) ~ group +
(group|LEA_STATE) + (group|metric) + (group|COMBOKEY), data=subdat,
prior = normal(0,1), prior_intercept = normal(0,1),
family=binomial, adapt_delta=.99)

save(m, file='/home/triddle/educational_disparities/big_bayes/output/big_bayes_s"

#1.rdata')
#"

for(i in 1:100){
  fs <- paste(filestring1, i, filestring2, i, ".rdata')", sep='')
  fileConn <- file(paste("model_scripts/gap/s", i, '.R', sep=''))
  writeLines(fs, fileConn)
  close(fileConn)
}
