library(parallelMCMCcombine)
library(dplyr)
library(tidyr)

load('/Users/travis/Documents/gits/educational_disparities/cluster/output/big_bayes/testrun6 (adroit)/m_s1.rdata')
load('/Users/travis/Documents/gits/educational_disparities/cluster/output/big_bayes/testrun6 (adroit)/m_s2.rdata')
load('/Users/travis/Documents/gits/educational_disparities/cluster/output/big_bayes/testrun6 (adroit)/m_s3.rdata')
load('/Users/travis/Documents/gits/educational_disparities/cluster/output/big_bayes/testrun6 (adroit)/m_s4.rdata')
load('/Users/travis/Documents/gits/educational_disparities/cluster/output/big_bayes/testrun6 (adroit)/m_s5.rdata')
load('/Users/travis/Documents/gits/educational_disparities/cluster/output/big_bayes/testrun6 (adroit)/m_fulldat.rdata')

temp <- as.data.frame(m)
temp2 <- as.data.frame(m_s2)
temp3 <- as.data.frame(m_s3)
temp4 <- as.data.frame(m_s4)
temp5 <- as.data.frame(m_s5)
tempfull <- as.data.frame(m_fulldat)


## first, look at overall fixed effects
posterior_combo <- array(cbind(rbind(temp$`(Intercept)`, temp$groupwhite,
                                     temp$`Sigma[COMBOKEY:(Intercept),(Intercept)]`,
                                     temp$`Sigma[LEA_STATE:(Intercept),(Intercept)]`,
                                     temp$`Sigma[LEA_STATE:groupwhite,(Intercept)]`,
                                     temp$`Sigma[LEA_STATE:groupwhite,groupwhite]`),
                               rbind(temp2$`(Intercept)`, temp2$groupwhite,
                                     temp2$`Sigma[COMBOKEY:(Intercept),(Intercept)]`,
                                     temp2$`Sigma[LEA_STATE:(Intercept),(Intercept)]`,
                                     temp2$`Sigma[LEA_STATE:groupwhite,(Intercept)]`,
                                     temp2$`Sigma[LEA_STATE:groupwhite,groupwhite]`),
                               rbind(temp3$`(Intercept)`, temp3$groupwhite,
                                     temp3$`Sigma[COMBOKEY:(Intercept),(Intercept)]`,
                                     temp3$`Sigma[LEA_STATE:(Intercept),(Intercept)]`,
                                     temp3$`Sigma[LEA_STATE:groupwhite,(Intercept)]`,
                                     temp3$`Sigma[LEA_STATE:groupwhite,groupwhite]`),
                               rbind(temp4$`(Intercept)`, temp4$groupwhite,
                                     temp4$`Sigma[COMBOKEY:(Intercept),(Intercept)]`,
                                     temp4$`Sigma[LEA_STATE:(Intercept),(Intercept)]`,
                                     temp4$`Sigma[LEA_STATE:groupwhite,(Intercept)]`,
                                     temp4$`Sigma[LEA_STATE:groupwhite,groupwhite]`),
                               rbind(temp5$`(Intercept)`, temp5$groupwhite,
                                     temp5$`Sigma[COMBOKEY:(Intercept),(Intercept)]`,
                                     temp5$`Sigma[LEA_STATE:(Intercept),(Intercept)]`,
                                     temp5$`Sigma[LEA_STATE:groupwhite,(Intercept)]`,
                                     temp5$`Sigma[LEA_STATE:groupwhite,groupwhite]`)),
                         dim=c(6,4000, 5))

p_avg <- sampleAvg(posterior_combo)
p_cons <- consensusMCindep(posterior_combo)
p_cons_cov <- consensusMCcov(posterior_combo)
p_semi <- semiparamDPE(posterior_combo, anneal = T)

collected_dat <- data.frame(m1_int = temp$`(Intercept)`,
                            m1_group = temp$groupwhite,
                            m2_int = temp2$`(Intercept)`,
                            m2_group = temp2$groupwhite,
                            m3_int = temp3$`(Intercept)`,
                            m3_group = temp3$groupwhite,
                            m4_int = temp4$`(Intercept)`,
                            m4_group = temp4$groupwhite,
                            m5_int = temp5$`(Intercept)`,
                            m5_group = temp5$groupwhite,
                            full.avg_int = p_avg[1,],
                            full.avg_group = p_avg[2,],
                            full.cons_int = p_cons[1,],
                            full.cons_group = p_cons[2,],
                            full.cons.cov_int = p_cons_cov[1,],
                            full.cons.cov_group = p_cons_cov[2,],
                            full.semi_int = p_semi[1,],
                            full.semi_group = p_semi[2,],
                            true_int = tempfull$`(Intercept)`,
                            true_group = tempfull$groupwhite)

collected_dat %>%
  gather(var, val) %>%
  separate(var, into=c('model', 'par'), sep='_') %>%
  ggplot(aes(x=val, group=model, color=model)) +
  geom_density() +
  facet_wrap(~par, scales='free')

collected_dat %>%
  select(starts_with('full'), true_int, true_group) %>%
  gather(var, val) %>%
  separate(var, into=c('model', 'par'), sep='_') %>%
  ggplot(aes(x=val, group=model, color=model)) +
  geom_density() +
  facet_wrap(~par, scales='free')

collected_dat %>%
  select(starts_with('m')) %>%
  gather(var, val) %>%
  separate(var, into=c('model', 'par'), sep='_') %>%
  ggplot(aes(x=val, group=model, color=model)) +
  geom_density() +
  facet_wrap(~par, scales='free')

collected_dat %>%
  select(starts_with('full'), true_int, true_group) %>%
  gather(var, val) %>%
  separate(var, into=c('model', 'par'), sep='_') %>%
  filter(par=='int') %>%
  ggplot(aes(x=val, group=model, color=model)) +
  geom_density() +
  facet_wrap(~par, scales='free')
