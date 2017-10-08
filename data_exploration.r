library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(rstanarm)

options(mc.cores = parallel::detectCores())

district_content <- read.csv('../Data/crdc201314csv/CRDC2013_14_LEA_content.csv')
df_district <- read.csv('../Data/crdc201314csv/CRDC2013_14_LEA.csv')
school_content <- read.csv('../Data/crdc201314csv/CRDC2013_14_SCH_content.csv')
df_school <- read.csv('../Data/crdc201314csv/CRDC2013_14_SCH.csv')


# Get enrollment figures
df_school %>%
  select(COMBOKEY, LEA_STATE:SCH_NAME, SCH_ENR_HI_M:TOT_ENR_F) %>%
  gather(group, total_number, SCH_ENR_HI_M:TOT_ENR_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>%
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='NR_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> enrollment

## Gifted and talented
df_school %>% 
  select(LEA_STATE:SCH_NAME, SCH_GT_IND:TOT_GTENR_F) %>% 
  filter(SCH_GT_IND=='YES') %>%
  gather(group, number, SCH_GTENR_HI_M:TOT_GTENR_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>%
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='NR_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> gifted_and_talented

gifted_and_talented %>%
  left_join(enrollment) %>%
  group_by(LEA_STATE, LEA_NAME, SCH_NAME, group) %>%
  summarise(number = sum(number),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  mutate(group=droplevels(group)) -> subdat

m <- lmer(proportion~group + (group|LEA_STATE), data=subdat)

m_meanfield <- stan_glmer(cbind(number, total_number-number) ~ group + (group|LEA_STATE), data=subdat, 
                          prior = normal(0,.5), prior_intercept = normal(0,.5), 
                          algorithm = 'meanfield', family=binomial)

#m_meanfield2 <- stan_glmer(cbind(number, total_number-number) ~ group + (group|LEA_STATE), data=subdat, 
#                          prior = normal(0,.5), prior_intercept = normal(0,.5), 
#                          algorithm = 'fullrank', family=binomial)

newdat <- expand.grid(group = c('black', 'white'),
                      LEA_STATE = rownames(table(subdat$LEA_STATE)),
                      number=10,
                      total_number=100)
newdat$index <- as.character(paste('V', seq(1, length(newdat$group)), sep=''))

temp <- as.data.frame(
  predict(m_meanfield, newdata = newdat,
                    re.form = ~(group|LEA_STATE)))

temp$sample <- seq(1, length(temp$`V1`))
temp %>%
  gather(index, prediction, -sample) %>%
  left_join(newdat) %>%
  group_by(group, LEA_STATE) %>%
  summarise(est = mean(prediction),
            lower = quantile(prediction, .025),
            upper = quantile(prediction, .975)) -> plot.dat

newdat.fixed <- expand.grid(group = c('black', 'white'),
                            number=10,
                            total_number=100)
newdat.fixed$index <- as.character(paste('V', 
                                         seq(1, length(newdat.fixed$group)), 
                                         sep=''))
temp.fixed <- as.data.frame(
  predict(m_meanfield, newdata = newdat.fixed,
                    re.form = NA))

temp.fixed$sample <- seq(1, length(temp.fixed$`V1`))
temp.fixed %>%
  gather(index, prediction, -sample) %>%
  left_join(newdat.fixed) %>%
  mutate(LEA_STATE = 'Overall') %>%
  group_by(group, LEA_STATE) %>%
  summarise(est = mean(prediction),
            lower = quantile(prediction, .025),
            upper = quantile(prediction, .975)) %>%
  rbind(plot.dat) %>%
  mutate(overall = LEA_STATE=='Overall') %>%
  ungroup() -> plot.dat

plot.dat %>%
  filter(group=='black') %>%
  mutate(LEA_STATE = reorder(LEA_STATE, est)) %>%
  rbind(plot.dat[which(plot.dat$group=='white'),]) -> plot.dat

p <- ggplot(plot.dat, aes(x=LEA_STATE, y=est, group=group, alpha=group, color=overall)) +
  geom_point(size=2, position=position_dodge(.4)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), position=position_dodge(.4)) + 
  coord_flip() +
  theme_classic() +
  scale_alpha_manual(values=c('white' = .25, 'black'=1)) +
  scale_color_manual(values=c('TRUE'='red', 'FALSE'='black')) +
  ylab('Number of students in gifted and talented, per 100 of each group') +
  xlab('State') +
  guides(color=FALSE)

ggsave('figs/gnt_group.jpeg', p)

temp %>%
  gather(index, prediction, -sample) %>%
  left_join(newdat) %>%
  select(-index) %>%
  spread(group, prediction) %>%
  mutate(diff = white-black) %>%
  group_by(LEA_STATE) %>%
  summarise(est = mean(diff, na.rm=T),
            lower = quantile(diff, .025, na.rm=T),
            upper = quantile(diff, .975, na.rm=T)) -> plot.dat

temp.fixed %>%
  gather(index, prediction, -sample) %>%
  left_join(newdat.fixed) %>%
  select(-index) %>%
  spread(group, prediction) %>%
  mutate(diff = white-black) %>%
  mutate(LEA_STATE = 'Overall') %>%
  group_by(LEA_STATE) %>%
  summarise(est = mean(diff),
            lower = quantile(diff, .025),
            upper = quantile(diff, .975)) %>%
  rbind(plot.dat) %>%
  mutate(overall = LEA_STATE=='Overall') %>%
  ungroup() -> plot.dat


plot.dat %>%
  mutate(LEA_STATE = reorder(LEA_STATE, est)) -> plot.dat

p <- ggplot(plot.dat, aes(x=LEA_STATE, y=est, color=overall)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=lower, ymax=upper)) + 
  coord_flip() +
  theme_classic() +
  scale_color_manual(values=c('TRUE'='red', 'FALSE'='black')) +
  ylab('N_white - N_black students in gifted and talented, per 100 of each group') +
  xlab('State') +
  guides(color=FALSE) +
  geom_hline(yintercept = 0)

ggsave('figs/gnt_gap.jpeg', p)

## AP enrollment
df_school %>% 
  select(LEA_STATE:SCH_NAME, SCH_APENR_IND, SCH_APENR_HI_M:TOT_APENR_F) %>% 
  filter(SCH_APENR_IND=='YES') %>%
  gather(group, number, SCH_APENR_HI_M:TOT_APENR_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>%
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='NR_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> AP

AP %>%
  left_join(enrollment) %>%
  group_by(LEA_STATE, LEA_NAME, SCH_NAME, group) %>%
  summarise(number = sum(number),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  mutate(group=droplevels(group)) -> subdat

#m <- lmer(proportion~group + (group|LEA_STATE), data=subdat)

m_AP <- stan_glmer(cbind(number, total_number-number) ~ group + (group|LEA_STATE), data=subdat, 
                          prior = normal(0,.5), prior_intercept = normal(0,.5), 
                          algorithm = 'meanfield', family=binomial)

#m_meanfield2 <- stan_glmer(cbind(number, total_number-number) ~ group + (group|LEA_STATE), data=subdat, 
#                          prior = normal(0,.5), prior_intercept = normal(0,.5), 
#                          algorithm = 'fullrank', family=binomial)

newdat <- expand.grid(group = c('black', 'white'),
                      LEA_STATE = rownames(table(subdat$LEA_STATE)),
                      number=10,
                      total_number=100)
newdat$index <- as.character(paste('V', seq(1, length(newdat$group)), sep=''))

temp <- as.data.frame(
  predict(m_AP, newdata = newdat,
                    re.form = ~(group|LEA_STATE)))

temp$sample <- seq(1, length(temp$`V1`))
temp %>%
  gather(index, prediction, -sample) %>%
  left_join(newdat) %>%
  group_by(group, LEA_STATE) %>%
  summarise(est = mean(prediction),
            lower = quantile(prediction, .025),
            upper = quantile(prediction, .975)) -> plot.dat

newdat.fixed <- expand.grid(group = c('black', 'white'),
                            number=10,
                            total_number=100)
newdat.fixed$index <- as.character(paste('V', 
                                         seq(1, length(newdat.fixed$group)), 
                                         sep=''))
temp.fixed <- as.data.frame(
  predict(m_AP, newdata = newdat.fixed,
                    re.form = NA))

temp.fixed$sample <- seq(1, length(temp.fixed$`V1`))
temp.fixed %>%
  gather(index, prediction, -sample) %>%
  left_join(newdat.fixed) %>%
  mutate(LEA_STATE = 'Overall') %>%
  group_by(group, LEA_STATE) %>%
  summarise(est = mean(prediction),
            lower = quantile(prediction, .025),
            upper = quantile(prediction, .975)) %>%
  rbind(plot.dat) %>%
  mutate(overall = LEA_STATE=='Overall') %>%
  ungroup() -> plot.dat

plot.dat %>%
  filter(group=='black') %>%
  mutate(LEA_STATE = reorder(LEA_STATE, est)) %>%
  rbind(plot.dat[which(plot.dat$group=='white'),]) -> plot.dat

p <- ggplot(plot.dat, aes(x=LEA_STATE, y=est, group=group, alpha=group, color=overall)) +
  geom_point(size=2, position=position_dodge(.4)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), position=position_dodge(.4)) + 
  coord_flip() +
  theme_classic() +
  scale_alpha_manual(values=c('white' = .25, 'black'=1)) +
  scale_color_manual(values=c('TRUE'='red', 'FALSE'='black')) +
  ylab('Number of students in AP classes, per 100 of each group') +
  xlab('State') +
  guides(color=FALSE)

ggsave('figs/ap_group.jpeg', p)

temp %>%
  gather(index, prediction, -sample) %>%
  left_join(newdat) %>%
  select(-index) %>%
  spread(group, prediction) %>%
  mutate(diff = white-black) %>%
  group_by(LEA_STATE) %>%
  summarise(est = mean(diff, na.rm=T),
            lower = quantile(diff, .025, na.rm=T),
            upper = quantile(diff, .975, na.rm=T)) -> plot.dat

temp.fixed %>%
  gather(index, prediction, -sample) %>%
  left_join(newdat.fixed) %>%
  select(-index) %>%
  spread(group, prediction) %>%
  mutate(diff = white-black) %>%
  mutate(LEA_STATE = 'Overall') %>%
  group_by(LEA_STATE) %>%
  summarise(est = mean(diff),
            lower = quantile(diff, .025),
            upper = quantile(diff, .975)) %>%
  rbind(plot.dat) %>%
  mutate(overall = LEA_STATE=='Overall') %>%
  ungroup() -> plot.dat


plot.dat %>%
  mutate(LEA_STATE = reorder(LEA_STATE, est)) -> plot.dat

p <- ggplot(plot.dat, aes(x=LEA_STATE, y=est, color=overall)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=lower, ymax=upper)) + 
  coord_flip() +
  theme_classic() +
  scale_color_manual(values=c('TRUE'='red', 'FALSE'='black')) +
  ylab('N_white - N_black students in AP classes, per 100 of each group') +
  xlab('State') +
  guides(color=FALSE) +
  geom_hline(yintercept = 0)

ggsave('figs/ap_gap.jpeg', p)

###### corporal punishment
df_school %>% 
  select(LEA_STATE:SCH_NAME, SCH_CORPINSTANCES_IND:TOT_DISCWODIS_CORP_F) %>% 
  filter(SCH_CORPINSTANCES_IND=='YES') %>%
  gather(group, number, SCH_DISCWODIS_CORP_HI_M:TOT_DISCWODIS_CORP_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>%
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='RP_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> corporal

corporal %>%
  left_join(enrollment) %>%
  group_by(LEA_STATE, LEA_NAME, SCH_NAME, group) %>%
  summarise(number = sum(number),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  mutate(group=droplevels(group)) %>%
  mutate(LEA_STATE=droplevels(LEA_STATE)) -> subdat

m_corporal3 <- stan_glmer(cbind(number, total_number-number) ~ group + (group|LEA_STATE), data=subdat, 
                          prior = normal(0,1), prior_intercept = normal(0,1), 
                          family=binomial)

#save(m_corporal3, file='corporal.rdata')
load('corporal.rdata')

newdat <- expand.grid(group = c('black', 'white'),
                      LEA_STATE = rownames(table(subdat$LEA_STATE)),
                      number=0,
                      total_number=1)
newdat$index <- as.character(seq(1, length(newdat$group)), sep='')

temp <- as.data.frame(
  posterior_linpred(m_corporal3, newdata = newdat,
                    re.form = ~(group|LEA_STATE), transform=T))

temp$sample <- seq(1, length(temp$`1`))
temp %>%
  gather(index, prediction, -sample) %>%
  left_join(newdat) %>%
  group_by(group, LEA_STATE) %>%
  summarise(est = mean(prediction),
            lower = quantile(prediction, .025),
            upper = quantile(prediction, .975)) -> plot.dat

newdat.fixed <- expand.grid(group = c('black', 'white'),
                            number=0,
                            total_number=1)
newdat.fixed$index <- as.character(seq(1, length(newdat.fixed$group)), 
                                         sep='')
temp.fixed <- as.data.frame(
  posterior_linpred(m_corporal3, newdata = newdat.fixed,
                    re.form = NA, transform=T))

temp.fixed$sample <- seq(1, length(temp.fixed$`1`))
temp.fixed %>%
  gather(index, prediction, -sample) %>%
  left_join(newdat.fixed) %>%
  mutate(LEA_STATE = 'Overall') %>%
  group_by(group, LEA_STATE) %>%
  summarise(est = mean(prediction),
            lower = quantile(prediction, .025),
            upper = quantile(prediction, .975)) %>%
  rbind(plot.dat) %>%
  mutate(overall = LEA_STATE=='Overall') %>%
  ungroup() -> plot.dat

plot.dat %>%
  filter(group=='black') %>%
  mutate(LEA_STATE = reorder(LEA_STATE, est)) %>%
  rbind(plot.dat[which(plot.dat$group=='white'),]) -> plot.dat

p <- ggplot(plot.dat, aes(x=LEA_STATE, y=est, group=group, alpha=group, color=overall)) +
  geom_point(size=2, position=position_dodge(.4)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), position=position_dodge(.4)) + 
  coord_flip() +
  theme_classic() +
  scale_alpha_manual(values=c('white' = .25, 'black'=1)) +
  scale_color_manual(values=c('TRUE'='red', 'FALSE'='black')) +
  ylab('Probability of a student receiving corporal punishment') +
  xlab('State') +
  guides(color=FALSE)

ggsave('figs/corp_group.jpeg', p)

temp %>%
  gather(index, prediction, -sample) %>%
  left_join(newdat) %>%
  select(-index) %>%
  spread(group, prediction) %>%
  mutate(diff = white-black) %>%
  group_by(LEA_STATE) %>%
  summarise(est = mean(diff, na.rm=T),
            lower = quantile(diff, .025, na.rm=T),
            upper = quantile(diff, .975, na.rm=T)) -> plot.dat

temp.fixed %>%
  gather(index, prediction, -sample) %>%
  left_join(newdat.fixed) %>%
  select(-index) %>%
  spread(group, prediction) %>%
  mutate(diff = white-black) %>%
  mutate(LEA_STATE = 'Overall') %>%
  group_by(LEA_STATE) %>%
  summarise(est = mean(diff),
            lower = quantile(diff, .025),
            upper = quantile(diff, .975)) %>%
  rbind(plot.dat) %>%
  mutate(overall = LEA_STATE=='Overall') %>%
  ungroup() -> plot.dat


plot.dat %>%
  mutate(LEA_STATE = reorder(LEA_STATE, est)) -> plot.dat

p <- ggplot(plot.dat, aes(x=LEA_STATE, y=est, color=overall)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=lower, ymax=upper)) + 
  coord_flip() +
  theme_classic() +
  scale_color_manual(values=c('TRUE'='red', 'FALSE'='black')) +
  ylab('N_white - N_black students in gifted and talented, per 100 of each group') +
  xlab('State') +
  geom_hline(yintercept=0) +
  guides(color=FALSE)

ggsave('figs/corp_gap.jpeg', p)

###### preschool out-of-school suspension one or more
df_school %>% 
  select(COMBOKEY, LEA_STATE:SCH_NAME, SCH_GRADE_PS, SCH_PSDISC_SINGOOS_HI_M:TOT_PSDISC_SINGOOS_F) %>% 
  filter(SCH_GRADE_PS=='YES') %>%
  gather(group, number, SCH_PSDISC_SINGOOS_HI_M:TOT_PSDISC_SINGOOS_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>%
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='OS_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> ps_susp

df_school %>% 
  select(COMBOKEY, LEA_STATE:SCH_NAME, SCH_GRADE_PS, SCH_PSDISC_MULTOOS_HI_M:TOT_PSDISC_MULTOOS_F) %>% 
  filter(SCH_GRADE_PS=='YES') %>%
  gather(group, number_2, SCH_PSDISC_MULTOOS_HI_M:TOT_PSDISC_MULTOOS_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>%
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='OS_', 
                          biracial='TR_', white='WH_')) %>%
  select(COMBOKEY, group, gender, number_2) %>%
  right_join(ps_susp) %>%
  filter(number>=0 & number_2 >= 0) %>%
  mutate(number = number+number_2) -> ps_susp

ps_susp %>%
  left_join(enrollment) %>%
  group_by(LEA_STATE, LEA_NAME, SCH_NAME, group) %>%
  summarise(number = sum(number),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  mutate(group=droplevels(group)) -> subdat

#m_p_susp3 <- stan_glmer(cbind(number, total_number-number) ~ group + (group|LEA_STATE), data=subdat,
#                         prior = normal(0,1), prior_intercept = normal(0,1),
#                         family=binomial)

#save(m_p_susp3, file='ps_susp.rdata')
load('ps_susp.rdata')


newdat <- expand.grid(group = c('black', 'white'),
                      LEA_STATE = rownames(table(droplevels(subdat$LEA_STATE))),
                      number=0,
                      total_number=1)
newdat$index <- as.character(seq(1, length(newdat$group)))

temp <- as.data.frame(
  posterior_linpred(m_p_susp3, newdata = newdat,
                    re.form = ~(group|LEA_STATE), transform=T))

temp$sample <- seq(1, length(temp$`1`))
temp %>%
  gather(index, prediction, -sample) %>%
  left_join(newdat) %>%
  group_by(group, LEA_STATE) %>%
  summarise(est = mean(prediction),
            lower = quantile(prediction, .025),
            upper = quantile(prediction, .975)) -> plot.dat

newdat.fixed <- expand.grid(group = c('black', 'white'),
                            number=0,
                            total_number=1)
newdat.fixed$index <- as.character(seq(1, length(newdat.fixed$group)))

temp.fixed <- as.data.frame(
  posterior_linpred(m_p_susp3, newdata = newdat.fixed,
                    re.form = NA, transform=T))

temp.fixed$sample <- seq(1, length(temp.fixed$`1`))
temp.fixed %>%
  gather(index, prediction, -sample) %>%
  left_join(newdat.fixed) %>%
  mutate(LEA_STATE = 'Overall') %>%
  group_by(group, LEA_STATE) %>%
  summarise(est = mean(prediction),
            lower = quantile(prediction, .025),
            upper = quantile(prediction, .975)) %>%
  rbind(plot.dat) %>%
  mutate(overall = LEA_STATE=='Overall') %>%
  ungroup() -> plot.dat

plot.dat %>%
  filter(group=='black') %>%
  mutate(LEA_STATE = reorder(LEA_STATE, est)) %>%
  rbind(plot.dat[which(plot.dat$group=='white'),]) -> plot.dat

p <- ggplot(plot.dat, aes(x=LEA_STATE, y=est, group=group, alpha=group, color=overall)) +
  geom_point(size=2, position=position_dodge(.4)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), position=position_dodge(.4)) + 
  coord_flip() +
  theme_classic() +
  scale_alpha_manual(values=c('white' = .25, 'black'=1)) +
  scale_color_manual(values=c('TRUE'='red', 'FALSE'='black')) +
  ylab('Probability of a preschooler receiving a suspension') +
  xlab('State') +
  guides(color=FALSE)

ggsave('figs/ps_susp_group.jpeg', p)

temp %>%
  gather(index, prediction, -sample) %>%
  left_join(newdat) %>%
  select(-index) %>%
  spread(group, prediction) %>%
  mutate(diff = white-black) %>%
  group_by(LEA_STATE) %>%
  summarise(est = mean(diff, na.rm=T),
            lower = quantile(diff, .025, na.rm=T),
            upper = quantile(diff, .975, na.rm=T)) -> plot.dat

temp.fixed %>%
  gather(index, prediction, -sample) %>%
  left_join(newdat.fixed) %>%
  select(-index) %>%
  spread(group, prediction) %>%
  mutate(diff = white-black) %>%
  mutate(LEA_STATE = 'Overall') %>%
  group_by(LEA_STATE) %>%
  summarise(est = mean(diff),
            lower = quantile(diff, .025),
            upper = quantile(diff, .975)) %>%
  rbind(plot.dat) %>%
  mutate(overall = LEA_STATE=='Overall') %>%
  ungroup() -> plot.dat


plot.dat %>%
  mutate(LEA_STATE = reorder(LEA_STATE, est)) -> plot.dat

p <- ggplot(plot.dat, aes(x=LEA_STATE, y=est, color=overall)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=lower, ymax=upper)) + 
  coord_flip() +
  theme_classic() +
  scale_color_manual(values=c('TRUE'='red', 'FALSE'='black')) +
  ylab('Preschool suspension gap, in terms of p_black - p_white') +
  xlab('State') +
  geom_hline(yintercept=0) +
  guides(color=FALSE)

ggsave('figs/ps_susp_gap.jpeg', p)

###
df_school %>% 
  select(COMBOKEY, LEA_STATE:SCH_NAME, SCH_DISCWODIS_ISS_HI_M:TOT_DISCWODIS_ISS_F) %>% 
  gather(group, number, SCH_DISCWODIS_ISS_HI_M:TOT_DISCWODIS_ISS_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='IS_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> susp_inschool

susp_inschool %>%
  left_join(enrollment) %>%
  group_by(LEA_STATE, LEA_NAME, SCH_NAME, group) %>%
  summarise(number = sum(number),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  mutate(group=droplevels(group)) -> subdat

#m_susp_inschool <- stan_glmer(cbind(number, total_number-number) ~ group + (group|LEA_STATE), data=subdat,
#                          prior = normal(0,1), prior_intercept = normal(0,1),
#                          family=binomial)

#save(m_susp_inschool, file='susp_inschool.rdata')
load('susp_inschool.rdata')

newdat <- expand.grid(group = c('black', 'white'),
                      LEA_STATE = rownames(table(droplevels(subdat$LEA_STATE))),
                      number=0,
                      total_number=1)
newdat$index <- as.character(paste('V', seq(1, length(newdat$group)), sep=''))

temp <- as.data.frame(
  posterior_linpred(m_susp_inschool, newdata = newdat,
          re.form = ~(group|LEA_STATE), transform=T))

temp$sample <- seq(1, length(temp$`1`))
temp %>%
  gather(index, prediction, -sample) %>%
  mutate(index = paste('V', index, sep='')) %>%
  left_join(newdat) %>%
  group_by(group, LEA_STATE) %>%
  summarise(est = mean(prediction),
            lower = quantile(prediction, .025),
            upper = quantile(prediction, .975)) -> plot.dat

newdat.fixed <- expand.grid(group = c('black', 'white'),
                            number=0,
                            total_number=1)
newdat.fixed$index <- as.character(paste('V', 
                                         seq(1, length(newdat.fixed$group)), 
                                         sep=''))
temp.fixed <- as.data.frame(
  posterior_linpred(m_susp_inschool, newdata = newdat.fixed,
          re.form = NA, transform=T))

temp.fixed$sample <- seq(1, length(temp.fixed$`1`))
temp.fixed %>%
  gather(index, prediction, -sample) %>%
  mutate(index = paste('V', index, sep='')) %>%
  left_join(newdat.fixed) %>%
  mutate(LEA_STATE = 'Overall') %>%
  group_by(group, LEA_STATE) %>%
  summarise(est = mean(prediction),
            lower = quantile(prediction, .025),
            upper = quantile(prediction, .975)) %>%
  rbind(plot.dat) %>%
  mutate(overall = LEA_STATE=='Overall') %>%
  ungroup() -> plot.dat

plot.dat %>%
  filter(group=='black') %>%
  mutate(LEA_STATE = reorder(LEA_STATE, est)) %>%
  rbind(plot.dat[which(plot.dat$group=='white'),]) -> plot.dat

p <- ggplot(plot.dat, aes(x=LEA_STATE, y=est, group=group, alpha=group, color=overall)) +
  geom_point(size=2, position=position_dodge(.4)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), position=position_dodge(.4)) + 
  coord_flip() +
  theme_classic() +
  scale_alpha_manual(values=c('white' = .25, 'black'=1)) +
  scale_color_manual(values=c('TRUE'='red', 'FALSE'='black')) +
  ylab('Probability of a student receiving an in-school suspension') +
  xlab('State') +
  guides(color=FALSE)

temp %>%
  gather(index, prediction, -sample) %>%
  mutate(index = paste('V', index, sep='')) %>%
  left_join(newdat) %>%
  select(-index) %>%
  spread(group, prediction) %>%
  mutate(diff = white-black) %>%
  group_by(LEA_STATE) %>%
  summarise(est = mean(diff, na.rm=T),
            lower = quantile(diff, .025, na.rm=T),
            upper = quantile(diff, .975, na.rm=T)) -> plot.dat

temp.fixed %>%
  gather(index, prediction, -sample) %>%
  mutate(index = paste('V', index, sep='')) %>%
  left_join(newdat.fixed) %>%
  select(-index) %>%
  spread(group, prediction) %>%
  mutate(diff = white-black) %>%
  mutate(LEA_STATE = 'Overall') %>%
  group_by(LEA_STATE) %>%
  summarise(est = mean(diff),
            lower = quantile(diff, .025),
            upper = quantile(diff, .975)) %>%
  rbind(plot.dat) %>%
  mutate(overall = LEA_STATE=='Overall') %>%
  ungroup() -> plot.dat


plot.dat %>%
  mutate(LEA_STATE = reorder(LEA_STATE, est)) -> plot.dat

p <- ggplot(plot.dat, aes(x=LEA_STATE, y=est, color=overall)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=lower, ymax=upper)) + 
  coord_flip() +
  theme_classic() +
  scale_color_manual(values=c('TRUE'='red', 'FALSE'='black')) +
  ylab('In-school suspension gap, in terms of p_black - p_white') +
  xlab('State') +
  geom_hline(yintercept=0) +
  guides(color=FALSE)

