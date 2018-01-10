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
state_teacher_means <- read.csv('/Users/travis/Documents/gits/educational_disparities/output/state_teacher_means.csv')

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
# relevant metrics #

df_school %>% 
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_DISCWODIS_ISS_HI_M:TOT_DISCWODIS_ISS_F, 
         SCH_DISCWDIS_ISS_IDEA_HI_M:TOT_DISCWDIS_ISS_IDEA_F) %>% 
  gather(group, number, SCH_DISCWODIS_ISS_HI_M:TOT_DISCWDIS_ISS_IDEA_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  separate(prefix, into=c('prefix', 'disability'), -3) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='SS_', 
                          total='EA_', biracial='TR_', white='WH_')) %>%
  mutate(disability=fct_recode(disability, not_disabled='_I', disabled='ID',
                               not_disabled='S_', disabled='A_')) %>%
  select(-prefix) -> susp_inschool

susp_inschool %>%
  left_join(enrollment) -> tempout

tempout$number[which(tempout$number<0)] <- NA

tempout %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, 
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(total_number>number) %>%
  ungroup() %>%
  mutate(metric='inschool_susp') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) -> subdat

df_school %>% 
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_DISCWODIS_SINGOOS_HI_M:TOT_DISCWODIS_SINGOOS_F,
         SCH_DISCWDIS_SINGOOS_IDEA_HI_M:TOT_DISCWDIS_SINGOOS_IDEA_F) %>% 
  gather(group, number, SCH_DISCWODIS_SINGOOS_HI_M:TOT_DISCWDIS_SINGOOS_IDEA_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>%
  separate(group, into=c('prefix', 'group'), -4) %>%
  separate(prefix, into=c('prefix', 'disability'), -3) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='OS_', 
                          total='EA_', biracial='TR_', white='WH_')) %>%
  mutate(disability=fct_recode(disability, not_disabled='GO', disabled='ID',
                               not_disabled='S_', disabled='A_')) %>%
  select(-prefix) -> oos_susp

df_school %>% 
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_DISCWODIS_MULTOOS_HI_M:TOT_DISCWODIS_MULTOOS_F,
         SCH_DISCWDIS_MULTOOS_IDEA_HI_M:TOT_DISCWDIS_MULTOOS_IDEA_F) %>% 
  gather(group, number_2, SCH_DISCWODIS_MULTOOS_HI_M:TOT_DISCWDIS_MULTOOS_IDEA_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>%
  separate(group, into=c('prefix', 'group'), -4) %>%
  separate(prefix, into=c('prefix', 'disability'), -3) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='OS_', 
                          biracial='TR_', white='WH_')) %>%
  mutate(disability=fct_recode(disability, not_disabled='TO', disabled='ID',
                               not_disabled='S_', disabled='A_')) %>%
  select(COMBOKEY, group, gender, number_2) %>%
  right_join(oos_susp) %>%
  filter(number>=0 & number_2 >= 0) %>%
  mutate(number = number+number_2) -> oos_susp

oos_susp %>%
  left_join(enrollment) -> tempout

tempout$number[which(tempout$number<0)] <- NA

tempout %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, 
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  ungroup() %>%
  mutate(metric='oos_susp') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

df_school %>% 
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_DISCWODIS_EXPWE_HI_M:TOT_DISCWODIS_EXPWE_F,
         SCH_DISCWDIS_EXPWE_IDEA_HI_M:TOT_DISCWDIS_EXPWE_IDEA_F) %>% 
  gather(group, number, SCH_DISCWODIS_EXPWE_HI_M:TOT_DISCWDIS_EXPWE_IDEA_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  separate(prefix, into=c('prefix', 'disability'), -3) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='WE_', 
                          biracial='TR_', white='WH_', total='EA_')) %>%
  mutate(disability=fct_recode(disability, not_disabled='XP', disabled='ID',
                               not_disabled='E_', disabled='A_')) %>%
  select(-prefix) -> exp_w_ed

df_school %>% 
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_DISCWODIS_EXPWOE_HI_M:TOT_DISCWODIS_EXPWOE_F,
         SCH_DISCWDIS_EXPWOE_IDEA_HI_M:TOT_DISCWDIS_EXPWOE_IDEA_F) %>% 
  gather(group, number, SCH_DISCWODIS_EXPWOE_HI_M:TOT_DISCWDIS_EXPWOE_IDEA_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  separate(prefix, into=c('prefix', 'disability'), -3) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='OE_', 
                          biracial='TR_', white='WH_', total='EA_')) %>%
  mutate(disability=fct_recode(disability, not_disabled='PW', disabled='ID',
                               not_disabled='E_', disabled='A_')) %>%
  select(-prefix) -> exp_wo_ed

exp_w_ed %>%
  mutate(w_ed_num = number) %>%
  select(-number) %>%
  left_join(exp_wo_ed) %>%
  left_join(enrollment) -> tempout

tempout$number[which(tempout$number<0)] <- NA
tempout$w_ed_num[which(tempout$w_ed_num<0)] <- NA
tempout %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, 
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T),
            w_ed_number = sum(w_ed_num, na.rm=T),
            total_number = sum(total_number)) %>%
  mutate(number=number+w_ed_number) %>%
  select(-w_ed_number) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(total_number>number) %>%
  ungroup() %>%
  mutate(metric='expulsion_combined') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

df_school %>% 
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_DISCWODIS_REF_HI_M:TOT_DISCWODIS_REF_F,
         SCH_DISCWDIS_REF_IDEA_HI_M:TOT_DISCWDIS_REF_IDEA_F) %>% 
  gather(group, number, SCH_DISCWODIS_REF_HI_M:TOT_DISCWDIS_REF_IDEA_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  separate(prefix, into=c('prefix', 'disability'), -3) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='EA_', 
                          biracial='TR_', white='WH_', total='EF_')) %>%
  mutate(disability=fct_recode(disability, not_disabled='_R', disabled='ID',
                               not_disabled='F_', disabled='A_')) %>%
  select(-prefix) -> law_enf

law_enf %>%
  left_join(enrollment) -> tempout

tempout$number[which(tempout$number<0)] <- NA
tempout %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, 
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T),
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
         SCH_DISCWODIS_ARR_HI_M:TOT_DISCWODIS_ARR_F,
         SCH_DISCWDIS_ARR_IDEA_HI_M:TOT_DISCWDIS_ARR_IDEA_F) %>% 
  gather(group, number, SCH_DISCWODIS_ARR_HI_M:TOT_DISCWDIS_ARR_IDEA_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  separate(prefix, into=c('prefix', 'disability'), -3) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='RR_', 
                          total='EA_', biracial='TR_', white='WH_')) %>%
  mutate(disability=fct_recode(disability, not_disabled='_A', disabled='ID',
                               not_disabled='R_', disabled='A_')) %>%
  select(-prefix) -> in_school_arrest

in_school_arrest %>%
  left_join(enrollment) -> tempout

tempout$number[which(tempout$number<0)] <- NA
tempout %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, 
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  ungroup() %>%
  mutate(metric='in_school_arrest') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

rm(exp_w_ed, exp_wo_ed, in_school_arrest, law_enf, oos_susp, susp_inschool)

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

### write with teacher data
county_teacher_estimates <- read.csv('/Users/travis/Documents/gits/educational_disparities/output/county_teacher_means.csv')

schools_loc %>%
  left_join(county_teacher_estimates) %>%
  filter(!is.na(county_bias)) %>%
  left_join(covs) -> out

write.csv(out, file='output/teacher_model_data.csv', row.names=FALSE)
