
library(dplyr)
library(tidyr)
library(rstanarm)

options(mc.cores = parallel::detectCores())

df <- read.csv('/home/triddle/educational_disparities/cluster/data/grouped_data.csv')
df %>% filter(metric == 'preschool_expulsion', grouping_10=='g_2') -> subdat
m <- stan_glmer(cbind(number, total_number-number) ~ group*bias +
(group|LEA_STATE) + (1|COMBOKEY), data=subdat,
prior = normal(0,1), prior_intercept = normal(0,1),
family=binomial, adapt_delta=.99)
save(m, file='/home/triddle/educational_disparities/cluster/bias/preschool_expulsion/output/s2.rdata')
