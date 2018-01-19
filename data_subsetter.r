library(dplyr)

df <- read.csv('../output/full_model_data.csv')

df %>%
  sample_n(20000) %>%
  group_by(COMBOKEY) %>% 
  mutate(group = sample(c(1:5), 1)) -> subdat

write.csv(subdat, file='big_bayes_testingdata.csv', row.names = F)
