library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(rstanarm)
library(lme4)
library(httr)
library(stringr)

options(mc.cores = parallel::detectCores())

district_content <- read.csv('../../Data/crdc201314csv/CRDC2013_14_LEA_content.csv')
df_district <- read.csv('../../Data/crdc201314csv/CRDC2013_14_LEA.csv')
school_content <- read.csv('../../Data/crdc201314csv/CRDC2013_14_SCH_content.csv')
df_school <- read.csv('../../Data/crdc201314csv/CRDC2013_14_SCH.csv')
county_means <- read.csv('/Users/travis/Documents/gits/educational_disparities/output/county_means.csv', 
                         colClasses = 'character')


# Get enrollment figures
df_school %>%
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, SCH_ENR_HI_M:TOT_ENR_F) %>%
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
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_CORPINSTANCES_IND:TOT_DISCWODIS_CORP_F) %>% 
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
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, 
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
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
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_GRADE_PS, SCH_PSDISC_SINGOOS_HI_M:TOT_PSDISC_SINGOOS_F) %>% 
  filter(SCH_GRADE_PS=='YES') %>%
  gather(group, number, SCH_PSDISC_SINGOOS_HI_M:TOT_PSDISC_SINGOOS_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>%
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='OS_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> ps_susp

df_school %>% 
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD,
         SCH_GRADE_PS, SCH_PSDISC_MULTOOS_HI_M:TOT_PSDISC_MULTOOS_F) %>% 
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
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, 
           CCD_LATCOD, CCD_LONCOD,  SCH_NAME, group) %>%
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
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_DISCWODIS_ISS_HI_M:TOT_DISCWODIS_ISS_F) %>% 
  gather(group, number, SCH_DISCWODIS_ISS_HI_M:TOT_DISCWODIS_ISS_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='SS_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> susp_inschool

susp_inschool %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, 
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
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
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_DISCWODIS_SINGOOS_HI_M:TOT_DISCWODIS_SINGOOS_F) %>% 
  gather(group, number, SCH_DISCWODIS_SINGOOS_HI_M:TOT_DISCWODIS_SINGOOS_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>%
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='OS_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> oos_susp

df_school %>% 
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_DISCWODIS_MULTOOS_HI_M:TOT_DISCWODIS_MULTOOS_F) %>% 
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
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, 
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
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
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_PSDISC_EXP_HI_M:TOT_PSDISC_EXP_F) %>% 
  gather(group, number, SCH_PSDISC_EXP_HI_M:TOT_PSDISC_EXP_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='XP_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> ps_exp

ps_exp %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, 
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
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
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_DISCWODIS_EXPWE_HI_M:TOT_DISCWODIS_EXPWE_F) %>% 
  gather(group, number, SCH_DISCWODIS_EXPWE_HI_M:TOT_DISCWODIS_EXPWE_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='WE_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> exp_w_ed

exp_w_ed %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, 
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
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
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_DISCWODIS_EXPWOE_HI_M:TOT_DISCWODIS_EXPWOE_F) %>% 
  gather(group, number, SCH_DISCWODIS_EXPWOE_HI_M:TOT_DISCWODIS_EXPWOE_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='OE_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> exp_wo_ed

exp_wo_ed %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, 
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
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
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_DISCWODIS_EXPZT_HI_M:TOT_DISCWODIS_EXPZT_F) %>% 
  gather(group, number, SCH_DISCWODIS_EXPZT_HI_M:TOT_DISCWODIS_EXPZT_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='ZT_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> exp_zero

exp_zero %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, 
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
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
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_DISCWODIS_REF_HI_M:TOT_DISCWODIS_REF_F) %>% 
  gather(group, number, SCH_DISCWODIS_REF_HI_M:TOT_DISCWODIS_REF_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='EF_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> law_enf

law_enf %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, 
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
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
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_DISCWODIS_ARR_HI_M:TOT_DISCWODIS_ARR_F) %>% 
  gather(group, number, SCH_DISCWODIS_ARR_HI_M:TOT_DISCWODIS_ARR_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='RR_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> in_school_arrest

in_school_arrest %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, 
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
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
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_RS_NONIDEA_MECH_HI_M:TOT_RS_NONIDEA_MECH_F) %>% 
  gather(group, number, SCH_RS_NONIDEA_MECH_HI_M:TOT_RS_NONIDEA_MECH_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='CH_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> mech_rest

mech_rest %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, 
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
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
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_RS_NONIDEA_PHYS_HI_M:TOT_RS_NONIDEA_PHYS_F) %>% 
  gather(group, number, SCH_RS_NONIDEA_PHYS_HI_M:TOT_RS_NONIDEA_PHYS_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='YS_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> phys_rest

phys_rest %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, 
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
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
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_RS_NONIDEA_SECL_HI_M:TOT_RS_NONIDEA_SECL_F) %>% 
  gather(group, number, SCH_RS_NONIDEA_SECL_HI_M:TOT_RS_NONIDEA_SECL_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='CL_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> seclusion

seclusion %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, 
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
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

rm(corporal, exp_w_ed, exp_wo_ed, exp_zero, in_school_arrest, law_enf, 
   mech_rest, oos_susp, phys_rest, ps_exp, ps_susp, seclusion, susp_inschool)

subdat %>%
  select(COMBOKEY, LEA_STATE, LEA_NAME, CCD_LATCOD, CCD_LONCOD) %>%
  filter(!is.na(CCD_LATCOD)) %>%
  distinct() -> schools_loc

get_fips_code <- function(lat, long) {
  url <- paste('http://data.fcc.gov/api/block/find?latitude=',
               lat, '&longitude=', long, sep='')
  r <- httr::GET(url)
  suppressMessages(out <- xml2::as_list(content(r)))
  out <- paste(attr(out$County, "FIPS"),
               attr(out$County, "name"),
               attr(out$State, "FIPS"),
               attr(out$State, "code"),
               sep='||')
  return(out)
}

# schools_loc$fips_api <- NA
# for(i in 1:length(schools_loc$CCD_LATCOD)){
#   schools_loc$fips_api[i] <- get_fips_code(schools_loc$CCD_LATCOD[i],
#                                            schools_loc$CCD_LONCOD[i])
#   print(i)
#   Sys.sleep(.1)
# }

#write.csv(schools_loc, file='output/schools_w_fips.csv', row.names = F)
schools_loc <- read.csv('output/schools_w_fips.csv')
schools_loc <- left_join(subdat, schools_loc)  
schools_loc %>%
  filter(!is.na(fips_api)) %>%
  separate(fips_api, into = c('full_fips', 'county_name', 
                              'state_fips', 'state_abb'), sep='\\|\\|') %>%
  mutate(county_fips = substr(full_fips, 3, 5)) %>%
  mutate(county_id = paste(state_abb, county_fips, sep='-')) -> schools_loc

schools_loc$county_name[which(schools_loc$county_id=='MD-510')] <- 'Baltimore city'
schools_loc$county_name[which(schools_loc$county_id=='VA-600')] <- 'Fairfax city'
schools_loc$county_name[which(schools_loc$county_id=='VA-620')] <- 'Franklin city'
schools_loc$county_name[which(schools_loc$county_id=='VA-760')] <- 'Richmond city'
schools_loc$county_name[which(schools_loc$county_id=='VA-770')] <- 'Roanoke city'
schools_loc$county_name[which(schools_loc$county_id=='MO-510')] <- 'St. Louis city'
schools_loc$county_name[which(schools_loc$county_id=='VA-510')] <- 'Alexandria city'

# counties that are also cities
# 1   Baltimore        MD     2 MD-510/MD-005
# 2     Fairfax        VA     2 VA-600/VA-059
# 3    Franklin        VA     2 VA-620/VA-067
# 4    Richmond        VA     2 VA-760/VA-159
# 5     Roanoke        VA     2 VA-770/VA-161
# 6   St. Louis        MO     2 MO-510/MO-189

county_means %>%
  select(-X) %>%
  right_join(schools_loc) %>%
  select(county_id, county_name, state_abb, 
         bias, warmth, weighted_bias, weighted_warmth, 
         COMBOKEY, group, number, total_number, metric) -> tempout


df_acs <- read.csv('../../Data/ACS/county_ethnicity/ACS_14_5YR_B02001_with_ann.csv',skip = 1)

covs1 <- df_acs[,c(3, 4, 6, 8)]
names(covs1) <- c('county', 'total_pop', 'white_pop', 'black_pop')

df_acs <- read.csv('../../Data/ACS/county_poverty_emp/ACS_14_5YR_DP03_with_ann.csv',skip = 1)

covs2 <- df_acs[,c(3, 22, 248, 478)]
names(covs2) <- c('county', 'unemp_rate', 'med_income', 'poverty_rate')

df_acs <- read.csv('../../Data/ACS/county_education/ACS_14_5YR_S1501_with_ann.csv',skip=1)
covs3 <- df_acs[,c(3, 28)]
names(covs3) <- c('county', 'col_grads')

covs1 %>%
  left_join(covs2) %>%
  left_join(covs3) %>%
  mutate(white_prop = white_pop/total_pop,
         black_prop = black_pop/total_pop) %>%
  mutate(b.w.ratio = black_prop/white_prop) -> covs

covs %>%
  separate(county, into=c('county_name', 'state'), sep=', ') %>%
  filter(state!='Puerto Rico') %>% #no PR in the education data
  mutate(county_name = 
           str_replace(county_name, 
                       ' County| Borough| Census Area| Parish| Municipality| City and Borough', '')) -> covs
covs$county_name[1803] <- 'Doña Ana' #unicode woes

#counties w/o IAT data: 
#Aleutians East, Hoonah-Angoon, Prince of Wales-Hyder, Skagway, Wrangell, 
#Hinsdale, Hodgeman, Berkshire, Petroleum, Powder River, Treasure, Banner, Loup,
#Billings, Rolette, Borden, Culberson, Foard, Glassock, Irion, Kenedy, Piute,
#Greensville

#counties w/o schools:
#kalawao, Issaquena, Mora, Divide, Loving

states <- data.frame(state = c(state.name, 'District of Columbia'), 
                     state_abb = c(state.abb, 'DC'))

covs <- left_join(covs, states)

tempthis <- left_join(tempout, covs)

# tempthis %>% 
#   select(county_id, county_name, state_abb, state) %>%
#   distinct() %>%
#   write.csv(., file='output/county_linking_table.csv', row.names=FALSE)

write.csv(tempthis, file='output/full_model_data.csv', row.names = FALSE)

