library(parallelMCMCcombine)
library(dplyr)
library(tidyr)

load('/Users/travis/Documents/gits/educational_disparities/cluster/output/big_bayes/testrun4 (adroit)/m_s1.rdata')
load('/Users/travis/Documents/gits/educational_disparities/cluster/output/big_bayes/testrun4 (adroit)/m_s2.rdata')
load('/Users/travis/Documents/gits/educational_disparities/cluster/output/big_bayes/testrun4 (adroit)/m_s3.rdata')
load('/Users/travis/Documents/gits/educational_disparities/cluster/output/big_bayes/testrun4 (adroit)/m_s4.rdata')
load('/Users/travis/Documents/gits/educational_disparities/cluster/output/big_bayes/testrun4 (adroit)/m_s5.rdata')
load('/Users/travis/Documents/gits/educational_disparities/cluster/output/big_bayes/testrun4 (adroit)/m_fulldat.rdata')
  
temp <- as.data.frame(m)
temp2 <- as.data.frame(m_s2)
temp3 <- as.data.frame(m_s3)
temp4 <- as.data.frame(m_s4)
temp5 <- as.data.frame(m_s5)
tempfull <- as.data.frame(m_fulldat)


## first, look at overall fixed effects
posterior_combo <- array(cbind(rbind(temp$`(Intercept)`, temp$groupwhite),
                           rbind(temp2$`(Intercept)`, temp2$groupwhite),
                           rbind(temp3$`(Intercept)`, temp3$groupwhite),
                           rbind(temp4$`(Intercept)`, temp4$groupwhite),
                           rbind(temp5$`(Intercept)`, temp5$groupwhite)),
                           dim=c(2,4000, 5))

p_avg <- sampleAvg(posterior_combo, shuff=T)
p_cons <- consensusMCindep(posterior_combo, shuff = T)
p_cons_cov <- consensusMCcov(posterior_combo, shuff=T)
p_semi <- semiparamDPE(posterior_combo)

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
  facet_wrap(~par)

collected_dat %>%
  select(starts_with('full'), true_int, true_group) %>%
  gather(var, val) %>%
  separate(var, into=c('model', 'par'), sep='_') %>%
  ggplot(aes(x=val, group=model, color=model)) +
  geom_density() +
  facet_wrap(~par)

## random effect of state
posterior_combo <- array(cbind(rbind(temp$`b[(Intercept) LEA_STATE:AK]`, temp$`b[groupwhite LEA_STATE:AK]`,
                                     temp$`b[(Intercept) LEA_STATE:CA]`, temp$`b[groupwhite LEA_STATE:CA]`),
                               rbind(temp2$`b[(Intercept) LEA_STATE:AK]`, temp2$`b[groupwhite LEA_STATE:AK]`,
                                     temp2$`b[(Intercept) LEA_STATE:CA]`, temp2$`b[groupwhite LEA_STATE:CA]`),
                               rbind(temp3$`b[(Intercept) LEA_STATE:AK]`, temp3$`b[groupwhite LEA_STATE:AK]`,
                                     temp3$`b[(Intercept) LEA_STATE:CA]`, temp3$`b[groupwhite LEA_STATE:CA]`),
                               rbind(temp4$`b[(Intercept) LEA_STATE:AK]`, temp4$`b[groupwhite LEA_STATE:AK]`,
                                     temp4$`b[(Intercept) LEA_STATE:CA]`, temp4$`b[groupwhite LEA_STATE:CA]`),
                               rbind(temp5$`b[(Intercept) LEA_STATE:AK]`, temp5$`b[groupwhite LEA_STATE:AK]`,
                                     temp5$`b[(Intercept) LEA_STATE:CA]`, temp5$`b[groupwhite LEA_STATE:CA]`)),
                         dim=c(4,4000, 5))

p_avg <- sampleAvg(posterior_combo, shuff=T)
p_cons <- consensusMCindep(posterior_combo, shuff = T)
p_cons_cov <- consensusMCcov(posterior_combo, shuff=T)
p_semi <- semiparamDPE(posterior_combo)

collected_dat <- data.frame(m1_int_AK = temp$`b[(Intercept) LEA_STATE:AK]`,
                            m1_group_AK = temp$`b[groupwhite LEA_STATE:AK]`,
                            m1_int_CA = temp$`b[(Intercept) LEA_STATE:CA]`,
                            m1_group_CA = temp$`b[groupwhite LEA_STATE:CA]`,
                            m2_int_AK = temp2$`b[(Intercept) LEA_STATE:AK]`,
                            m2_group_AK = temp2$`b[groupwhite LEA_STATE:AK]`,
                            m2_int_CA = temp2$`b[(Intercept) LEA_STATE:CA]`,
                            m2_group_CA = temp2$`b[groupwhite LEA_STATE:CA]`,
                            m3_int_AK = temp3$`b[(Intercept) LEA_STATE:AK]`,
                            m3_group_AK = temp3$`b[groupwhite LEA_STATE:AK]`,
                            m3_int_CA = temp3$`b[(Intercept) LEA_STATE:CA]`,
                            m3_group_CA = temp3$`b[groupwhite LEA_STATE:CA]`,
                            m4_int_AK = temp4$`b[(Intercept) LEA_STATE:AK]`,
                            m4_group_AK = temp4$`b[groupwhite LEA_STATE:AK]`,
                            m4_int_CA = temp4$`b[(Intercept) LEA_STATE:CA]`,
                            m4_group_CA = temp4$`b[groupwhite LEA_STATE:CA]`,
                            m5_int_AK = temp5$`b[(Intercept) LEA_STATE:AK]`,
                            m5_group_AK = temp5$`b[groupwhite LEA_STATE:AK]`,
                            m5_int_CA = temp5$`b[(Intercept) LEA_STATE:CA]`,
                            m5_group_CA = temp5$`b[groupwhite LEA_STATE:CA]`,
                            full.avg_int_AK = p_avg[1,],
                            full.avg_group_AK = p_avg[2,],
                            full.avg_int_CA = p_avg[3,],
                            full.avg_group_CA = p_avg[4,],
                            full.cons_int_AK = p_cons[1,],
                            full.cons_group_AK = p_cons[2,],
                            full.cons_int_CA = p_cons[3,],
                            full.cons_group_CA = p_cons[4,],
                            full.cons.cov_int_AK = p_cons_cov[1,],
                            full.cons.cov_group_AK = p_cons_cov[2,],
                            full.cons.cov_int_CA = p_cons_cov[3,],
                            full.cons.cov_group_CA = p_cons_cov[4,],
                            full.semi_int_AK = p_semi[1,],
                            full.semi_group_AK = p_semi[2,],
                            full.semi_int_CA = p_semi[3,],
                            full.semi_group_CA = p_semi[4,],
                            true_int_AK = tempfull$`b[(Intercept) LEA_STATE:AK]`,
                            true_group_AK = tempfull$`b[groupwhite LEA_STATE:AK]`,
                            true_int_CA = tempfull$`b[(Intercept) LEA_STATE:CA]`,
                            true_group_CA = tempfull$`b[groupwhite LEA_STATE:CA]`)

collected_dat %>%
  gather(var, val) %>%
  separate(var, into=c('model', 'par', 'state'), sep='_') %>%
  ggplot(aes(x=val, group=model, color=model)) +
  geom_density() +
  facet_grid(state~par)

collected_dat %>%
  gather(var, val) %>%
  separate(var, into=c('model', 'par', 'state'), sep='_') %>%
  filter(par=='group') %>%
  ggplot(aes(x=val, group=model, color=model)) +
  geom_density() +
  facet_grid(~state)

collected_dat %>%
  gather(var, val) %>%
  separate(var, into=c('model', 'par', 'state'), sep='_') %>%
  filter(par=='int') %>%
  ggplot(aes(x=val, group=model, color=model)) +
  geom_density() +
  facet_grid(~state)

collected_dat %>%
  gather(var, val) %>%
  separate(var, into=c('model', 'par', 'state'), sep='_') %>%
  filter(state=='AK') %>%
  ggplot(aes(x=val, group=model, color=model)) +
  geom_density() +
  facet_grid(~par)

collected_dat %>%
  select(starts_with('full'), starts_with('true')) %>%
  gather(var, val) %>%
  separate(var, into=c('model', 'par', 'state'), sep='_') %>%
  ggplot(aes(x=val, group=model, color=model)) +
  geom_density() +
  facet_grid(state~par)

## random effect of metric
posterior_combo <- array(cbind(rbind(temp$`b[(Intercept) metric:corporal]`, temp$`b[groupwhite metric:corporal]`,
                                     temp$`b[(Intercept) metric:oos_susp]`, temp$`b[groupwhite metric:oos_susp]`),
                               rbind(temp2$`b[(Intercept) metric:corporal]`, temp2$`b[groupwhite metric:corporal]`,
                                     temp2$`b[(Intercept) metric:oos_susp]`, temp2$`b[groupwhite metric:oos_susp]`),
                               rbind(temp3$`b[(Intercept) metric:corporal]`, temp3$`b[groupwhite metric:corporal]`,
                                     temp3$`b[(Intercept) metric:oos_susp]`, temp3$`b[groupwhite metric:oos_susp]`),
                               rbind(temp4$`b[(Intercept) metric:corporal]`, temp4$`b[groupwhite metric:corporal]`,
                                     temp4$`b[(Intercept) metric:oos_susp]`, temp4$`b[groupwhite metric:oos_susp]`),
                               rbind(temp5$`b[(Intercept) metric:corporal]`, temp5$`b[groupwhite metric:corporal]`,
                                     temp5$`b[(Intercept) metric:oos_susp]`, temp5$`b[groupwhite metric:oos_susp]`)),
                         dim=c(4,4000, 5))

p_avg <- sampleAvg(posterior_combo, shuff=T)
p_cons <- consensusMCindep(posterior_combo, shuff = T)
p_cons_cov <- consensusMCcov(posterior_combo, shuff=T)
p_semi <- semiparamDPE(posterior_combo)

collected_dat <- data.frame(m1_int_corp = temp$`b[(Intercept) metric:corporal]`,
                            m1_group_corp = temp$`b[groupwhite metric:corporal]`,
                            m1_int_susp = temp$`b[(Intercept) metric:oos_susp]`,
                            m1_group_susp = temp$`b[groupwhite metric:oos_susp]`,
                            m2_int_corp = temp2$`b[(Intercept) metric:corporal]`,
                            m2_group_corp = temp2$`b[groupwhite metric:corporal]`,
                            m2_int_susp = temp2$`b[(Intercept) metric:oos_susp]`,
                            m2_group_susp = temp2$`b[groupwhite metric:oos_susp]`,
                            m3_int_corp = temp3$`b[(Intercept) metric:corporal]`,
                            m3_group_corp = temp3$`b[groupwhite metric:corporal]`,
                            m3_int_susp = temp3$`b[(Intercept) metric:oos_susp]`,
                            m3_group_susp = temp3$`b[groupwhite metric:oos_susp]`,
                            m4_int_corp = temp4$`b[(Intercept) metric:corporal]`,
                            m4_group_corp = temp4$`b[groupwhite metric:corporal]`,
                            m4_int_susp = temp4$`b[(Intercept) metric:oos_susp]`,
                            m4_group_susp = temp4$`b[groupwhite metric:oos_susp]`,
                            m5_int_corp = temp5$`b[(Intercept) metric:corporal]`,
                            m5_group_corp = temp5$`b[groupwhite metric:corporal]`,
                            m5_int_susp = temp5$`b[(Intercept) metric:oos_susp]`,
                            m5_group_susp = temp5$`b[groupwhite metric:oos_susp]`,
                            full.avg_int_corp = p_avg[1,],
                            full.avg_group_corp = p_avg[2,],
                            full.avg_int_susp = p_avg[3,],
                            full.avg_group_susp = p_avg[4,],
                            full.cons_int_corp = p_cons[1,],
                            full.cons_group_corp = p_cons[2,],
                            full.cons_int_susp = p_cons[3,],
                            full.cons_group_susp = p_cons[4,],
                            full.cons.cov_int_corp = p_cons_cov[1,],
                            full.cons.cov_group_corp = p_cons_cov[2,],
                            full.cons.cov_int_susp = p_cons_cov[3,],
                            full.cons.cov_group_susp = p_cons_cov[4,],
                            full.semi_int_corp = p_semi[1,],
                            full.semi_group_corp = p_semi[2,],
                            full.semi_int_susp = p_semi[3,],
                            full.semi_group_susp = p_semi[4,],
                            true_int_corp = tempfull$`b[(Intercept) metric:corporal]`,
                            true_group_corp = tempfull$`b[groupwhite metric:corporal]`,
                            true_int_susp = tempfull$`b[(Intercept) metric:oos_susp]`,
                            true_group_susp = tempfull$`b[groupwhite metric:oos_susp]`)

collected_dat %>%
  select(-starts_with('full')) %>%
  gather(var, val) %>%
  separate(var, into=c('model', 'par', 'metric'), sep='_') %>%
  ggplot(aes(x=val, group=model, color=model)) +
  geom_density() +
  facet_grid(metric~par)

collected_dat %>%
  gather(var, val) %>%
  separate(var, into=c('model', 'par', 'metric'), sep='_') %>%
  filter(par=='group') %>%
  ggplot(aes(x=val, group=model, color=model)) +
  geom_density() +
  facet_grid(~metric)

collected_dat %>%
  gather(var, val) %>%
  separate(var, into=c('model', 'par', 'metric'), sep='_') %>%
  filter(par=='int') %>%
  ggplot(aes(x=val, group=model, color=model)) +
  geom_density() +
  facet_grid(~metric)

collected_dat %>%
  gather(var, val) %>%
  separate(var, into=c('model', 'par', 'metric'), sep='_') %>%
  filter(metric=='corp') %>%
  ggplot(aes(x=val, group=model, color=model)) +
  geom_density() +
  facet_grid(~par)

collected_dat %>%
  select(starts_with('full'), starts_with('true')) %>%
  gather(var, val) %>%
  separate(var, into=c('model', 'par', 'metric'), sep='_') %>%
  ggplot(aes(x=val, group=model, color=model)) +
  geom_density() +
  facet_grid(metric~par)
