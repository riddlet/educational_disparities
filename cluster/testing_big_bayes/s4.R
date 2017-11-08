library(dplyr)
library(tidyr)
library(rstanarm)

options(mc.cores = parallel::detectCores())
df <- read.csv('/home/triddle/educational_disparities/cluster/data/big_bayes_testingdata6.csv')
#selected_group <- rownames(table(df$grouping))[4]
df %>% filter(grouping==4) -> subdat

m_s4 <- stan_glmer(cbind(number, total_number-number) ~ group + 
	(group|LEA_STATE) + (1|COMBOKEY), data=subdat, 
	prior = normal(0,1), prior_intercept = normal(0,1), 
	family=binomial, adapt_delta=.99)

save(m_s4, file='/home/triddle/educational_disparities/cluster/output/m_s4.rdata')