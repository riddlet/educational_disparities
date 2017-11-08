library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(rstanarm)
library(lme4)

options(mc.cores = parallel::detectCores())

df <- read.csv('/home/triddle/educational_disparities/cluster/data/big_bayes_testingdata5.csv')

m_fulldat <- stan_glmer(cbind(number, total_number-number) ~ group + 
	(group|LEA_STATE) + (1|COMBOKEY), data=df, 
	prior = normal(0,1), prior_intercept = normal(0,1), 
	family=binomial, adapt_delta=.99)

save(m_fulldat, file='/home/triddle/educational_disparities/cluster/output/m_fulldat.rdata')