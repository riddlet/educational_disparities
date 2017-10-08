.libPaths('/vega/psych/users/tar2119/rpackages/')

library(dplyr)
library(tidyr)
library(rstanarm)

options(mc.cores = 5)

district_content <- read.csv('/vega/psych/users/tar2119/Data/crdc201314csv/CRDC2013_14_LEA_content.csv')
df_district <- read.csv('/vega/psych/users/tar2119/Data/crdc201314csv/CRDC2013_14_LEA.csv')
school_content <- read.csv('/vega/psych/users/tar2119/Data/crdc201314csv/CRDC2013_14_SCH_content.csv')
df_school <- read.csv('/vega/psych/users/tar2119/Data/crdc201314csv/CRDC2013_14_SCH.csv')

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
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, SCH_NAME, group) %>%
  summarise(number = sum(number),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  mutate(group=droplevels(group)) %>%
  ungroup() %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) -> subdat

m_p_susp <- stan_glmer(cbind(number, total_number-number) ~ group + (group|LEA_STATE), data=subdat,
                         prior = normal(0,1), prior_intercept = normal(0,1),
                         family=binomial)

save(m_p_susp, file='ps_susp.rdata')