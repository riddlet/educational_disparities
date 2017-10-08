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
  select(COMBOKEY, LEA_STATE:SCH_NAME, SCH_CORPINSTANCES_IND:TOT_DISCWODIS_CORP_F) %>% 
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
  mutate(LEA_STATE=droplevels(LEA_STATE)) -> subdat

m_corporal <- stan_glmer(cbind(number, total_number-number) ~ group + (group|LEA_STATE), data=subdat, 
                          prior = normal(0,1), prior_intercept = normal(0,1), 
                          family=binomial)

save(m_corporal, file='corporal.rdata')