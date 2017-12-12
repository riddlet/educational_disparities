library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(rstanarm)
library(lme4)
library(sp)
library(rgdal)

options(mc.cores = parallel::detectCores())

district_content <- read.csv('../../Data/crdc201314csv/CRDC2013_14_LEA_content.csv')
df_district <- read.csv('../../Data/crdc201314csv/CRDC2013_14_LEA.csv', colClasses = 'character')
school_content <- read.csv('../../Data/crdc201314csv/CRDC2013_14_SCH_content.csv')
df_school <- read.csv('../../Data/crdc201314csv/CRDC2013_14_SCH.csv')
MSA_means <- read.csv('/Users/travis/Documents/gits/educational_disparities/output/MSA_means.csv')

#resume at line 445

# Get enrollment figures
df_school %>%
  select(LEA_STATE:LEAID, SCH_ENR_HI_M:TOT_ENR_F) %>%
  gather(group, total_number, SCH_ENR_HI_M:TOT_ENR_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>%
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='NR_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> enrollment

################ 
# all metrics #

df_school %>% 
  select(LEA_STATE:LEAID, SCH_CORPINSTANCES_IND:TOT_DISCWODIS_CORP_F) %>% 
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
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID, SCH_NAME, group) %>%
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
  mutate(metric='corporal') %>%
  mutate(LEA_STATE=droplevels(LEA_STATE)) -> subdat

df_school %>% 
  select(LEA_STATE:LEAID, SCH_GRADE_PS, 
         SCH_PSDISC_SINGOOS_HI_M:TOT_PSDISC_SINGOOS_F) %>% 
  filter(SCH_GRADE_PS=='YES') %>%
  gather(group, number, SCH_PSDISC_SINGOOS_HI_M:TOT_PSDISC_SINGOOS_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>%
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='OS_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> ps_susp

df_school %>% 
  select(LEA_STATE:LEAID, SCH_GRADE_PS, 
         SCH_PSDISC_MULTOOS_HI_M:TOT_PSDISC_MULTOOS_F) %>% 
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
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID, SCH_NAME, group) %>%
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
  mutate(metric='preschool_susp') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

df_school %>% 
  select(LEA_STATE:LEAID, SCH_DISCWODIS_ISS_HI_M:TOT_DISCWODIS_ISS_F) %>% 
  gather(group, number, SCH_DISCWODIS_ISS_HI_M:TOT_DISCWODIS_ISS_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='SS_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> susp_inschool

susp_inschool %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID, SCH_NAME, group) %>%
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
  mutate(metric='inschool_susp') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

df_school %>% 
  select(LEA_STATE:LEAID, SCH_DISCWODIS_SINGOOS_HI_M:TOT_DISCWODIS_SINGOOS_F) %>% 
  gather(group, number, SCH_DISCWODIS_SINGOOS_HI_M:TOT_DISCWODIS_SINGOOS_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>%
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='OS_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> oos_susp

df_school %>% 
  select(LEA_STATE:LEAID, SCH_DISCWODIS_MULTOOS_HI_M:TOT_DISCWODIS_MULTOOS_F) %>% 
  gather(group, number_2, SCH_DISCWODIS_MULTOOS_HI_M:TOT_DISCWODIS_MULTOOS_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>%
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='OS_', 
                          biracial='TR_', white='WH_')) %>%
  select(COMBOKEY, group, gender, number_2) %>%
  right_join(oos_susp) %>%
  filter(number>=0 & number_2 >= 0) %>%
  mutate(number = number+number_2) -> oos_susp

oos_susp %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID, SCH_NAME, group) %>%
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
  mutate(metric='oos_susp') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

df_school %>% 
  select(LEA_STATE:LEAID, SCH_PSDISC_EXP_HI_M:TOT_PSDISC_EXP_F) %>% 
  gather(group, number, SCH_PSDISC_EXP_HI_M:TOT_PSDISC_EXP_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='XP_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> ps_exp

ps_exp %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID, SCH_NAME, group) %>%
  summarise(number = sum(number),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  ungroup() %>%
  mutate(metric='preschool_expulsion') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

df_school %>% 
  select(LEA_STATE:LEAID, SCH_DISCWODIS_EXPWE_HI_M:TOT_DISCWODIS_EXPWE_F) %>% 
  gather(group, number, SCH_DISCWODIS_EXPWE_HI_M:TOT_DISCWODIS_EXPWE_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='WE_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> exp_w_ed

exp_w_ed %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID, SCH_NAME, group) %>%
  summarise(number = sum(number),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  ungroup() %>%
  mutate(metric='expulsion_w_ed') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

df_school %>% 
  select(LEA_STATE:LEAID, SCH_DISCWODIS_EXPWOE_HI_M:TOT_DISCWODIS_EXPWOE_F) %>% 
  gather(group, number, SCH_DISCWODIS_EXPWOE_HI_M:TOT_DISCWODIS_EXPWOE_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='OE_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> exp_wo_ed

exp_wo_ed %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID, SCH_NAME, group) %>%
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
  mutate(metric='expulsion_wo_ed') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

df_school %>% 
  select(LEA_STATE:LEAID, SCH_DISCWODIS_EXPZT_HI_M:TOT_DISCWODIS_EXPZT_F) %>% 
  gather(group, number, SCH_DISCWODIS_EXPZT_HI_M:TOT_DISCWODIS_EXPZT_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='ZT_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> exp_zero

exp_zero %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID, SCH_NAME, group) %>%
  summarise(number = sum(number),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  ungroup() %>%
  mutate(metric='expulsion_0_tolerance') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

df_school %>% 
  select(LEA_STATE:LEAID, SCH_DISCWODIS_REF_HI_M:TOT_DISCWODIS_REF_F) %>% 
  gather(group, number, SCH_DISCWODIS_REF_HI_M:TOT_DISCWODIS_REF_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='EF_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> law_enf

law_enf %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID, SCH_NAME, group) %>%
  summarise(number = sum(number),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  ungroup() %>%
  mutate(metric='law_enforcement') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

df_school %>% 
  select(LEA_STATE:LEAID, SCH_DISCWODIS_ARR_HI_M:TOT_DISCWODIS_ARR_F) %>% 
  gather(group, number, SCH_DISCWODIS_ARR_HI_M:TOT_DISCWODIS_ARR_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='RR_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> in_school_arrest

in_school_arrest %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID, SCH_NAME, group) %>%
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
  mutate(metric='in_school_arrest') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

df_school %>% 
  select(LEA_STATE:LEAID, SCH_RS_NONIDEA_MECH_HI_M:TOT_RS_NONIDEA_MECH_F) %>% 
  gather(group, number, SCH_RS_NONIDEA_MECH_HI_M:TOT_RS_NONIDEA_MECH_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='CH_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> mech_rest

mech_rest %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID, SCH_NAME, group) %>%
  summarise(number = sum(number),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  ungroup() %>%
  mutate(metric='mechanical_restraint') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

df_school %>% 
  select(LEA_STATE:LEAID, SCH_RS_NONIDEA_PHYS_HI_M:TOT_RS_NONIDEA_PHYS_F) %>% 
  gather(group, number, SCH_RS_NONIDEA_PHYS_HI_M:TOT_RS_NONIDEA_PHYS_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='YS_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> phys_rest

phys_rest %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID, SCH_NAME, group) %>%
  summarise(number = sum(number),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  ungroup() %>%
  mutate(metric='physical_restraint') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

df_school %>% 
  select(LEA_STATE:LEAID, SCH_RS_NONIDEA_SECL_HI_M:TOT_RS_NONIDEA_SECL_F) %>% 
  gather(group, number, SCH_RS_NONIDEA_SECL_HI_M:TOT_RS_NONIDEA_SECL_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='CL_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> seclusion

seclusion %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID, SCH_NAME, group) %>%
  summarise(number = sum(number),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  ungroup() %>%
  mutate(metric='seclusion') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

df_district %>%
  select(LEAID, LEA_ZIP) %>%
  right_join(subdat) %>%
  mutate(LEA_ZIP = as.character(LEA_ZIP)) -> tempout

## next, append the msa number as seen on the DOL website
msa_dat <- read.csv('../../Data/fs13_gpci_by_msa-ZIP.csv', skip=10, colClasses = 'character')
msa_dat %>%
  mutate(LEA_ZIP = ZIP.CODE) %>%
  select(LEA_ZIP, MSA.No.) %>%
  right_join(tempout) -> tempout

#some zip codes in district data do not seem to exist at all (eg 91705, 30335) 
#or in the DOL data (eg 30305)
MSA_means %>%
  filter(raceomb=='White') %>%
  mutate(MSA.No. = as.character(MSANo)) %>%
  select(MSANo, MSA.No., bias, warmth) %>%
  mutate(bias = scale(bias)[,1],
         warmth = scale(warmth)[,1]) %>%
  right_join(tempout) %>%
  filter(!is.na(MSA.No.)) %>%
  filter(!is.na(bias)) %>%
  select(MSA.No., bias, warmth, COMBOKEY, group, number, total_number, metric) -> tempout


df_acs <- read.csv('../../Data/ACS/ACS_14_5YR_DP05/ACS_14_5YR_DP05_with_ann.csv',skip = 1)

covs <- df_acs[,c(3, 4, 128, 132)]
names(covs) <- c('LEA_ZIP', 'total_pop', 'white_pop', 'black_pop')
covs$LEA_ZIP <- substr(covs$LEA_ZIP, 7,13)

df_acs <- read.csv('../../Data/ACS/ACS_14_5YR_DP03/ACS_14_5YR_DP03_with_ann.csv',skip = 1)

covs2 <- df_acs[,c(2, 22, 248, 478)]
names(covs2) <- c('CBSAFP', 'unemp_rate', 'med_income', 'poverty_rate')

covs3 <- left_join(covs, covs2)
covs3$CBSAFP <- as.character(covs3$CBSAFP)

tempthis <- left_join(tempout, covs3)

write.csv(tempout, file='output/full_model_data.csv', row.names = FALSE)

