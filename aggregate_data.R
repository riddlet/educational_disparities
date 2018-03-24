library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(rstanarm)
library(lme4)
library(httr)
library(stringr)

options(mc.cores = parallel::detectCores())

"%ni%" <- Negate("%in%")

district_content <- read.csv('/Users/travis/Documents/gits/Data/crdc201314csv/CRDC2013_14_LEA_content.csv')
df_district <- read.csv('/Users/travis/Documents/gits/Data/crdc201314csv/CRDC2013_14_LEA.csv')
school_content <- read.csv('/Users/travis/Documents/gits/Data/crdc201314csv/CRDC2013_14_SCH_content.csv')
df_school <- read.csv('/Users/travis/Documents/gits/Data/crdc201314csv/CRDC2013_14_SCH.csv')
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
  group_by(COMBOKEY, group) %>%
  mutate(total_number = sum(total_number)) %>%
  ungroup() %>%
  select(-prefix, -gender) %>%
  distinct() -> enrollment

# relevant metrics #
################ 

# in-school suspension
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

susp_inschool$number[which(susp_inschool$number<0)] <- NA

susp_inschool %>%
  #filter(group=='black'|group=='white') %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T)) %>%
  left_join(enrollment) %>%
  mutate(proportion = number/total_number) -> tempout

tempout %>% 
  ungroup() %>%
  mutate(impossible = number>total_number) %>%
  group_by(COMBOKEY) %>%
  mutate(impossible_school = sum(impossible)>0) %>%
  ungroup() %>%
  filter(impossible_school==F) %>%
  filter(group=='black'|group=='white') %>%
  mutate(metric='inschool_susp') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  distinct() -> subdat

#out of school suspension
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

oos_susp$number[which(oos_susp$number<0)] <- NA

oos_susp %>%
  #filter(group=='black'|group=='white') %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T)) %>%
  left_join(enrollment) %>%
  mutate(proportion = number/total_number) -> tempout

tempout %>% 
  ungroup() %>%
  mutate(impossible = number>total_number) %>%
  group_by(COMBOKEY) %>%
  mutate(impossible_school = sum(impossible)>0) %>%
  ungroup() %>%
  filter(impossible_school==F) %>%
  filter(group=='black'|group=='white') %>%
  mutate(metric='oos_susp') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  distinct() %>%
  rbind(subdat) -> subdat

#expulsion with education
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

#expulsion without education
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

exp_w_ed$number[which(exp_w_ed$number<0)] <- NA
exp_wo_ed$number[which(exp_wo_ed$number<0)] <- NA

exp_w_ed %>%
  mutate(w_ed_number = number) %>%
  select(-number) %>%
  left_join(exp_wo_ed) %>%
  mutate(number = number + w_ed_number) %>%
  #filter(group=='black'|group=='white') %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T)) %>%
  left_join(enrollment) %>%
  mutate(proportion = number/total_number) -> tempout

tempout %>% 
  ungroup() %>%
  mutate(impossible = number>total_number) %>%
  group_by(COMBOKEY) %>%
  mutate(impossible_school = sum(impossible)>0) %>%
  ungroup() %>%
  filter(impossible_school==F) %>%
  filter(group=='black'|group=='white') %>%
  mutate(metric='expulsion_combined') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  distinct() %>%
  rbind(subdat) -> subdat

#law enforcement
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

law_enf$number[which(law_enf$number<0)] <- NA

law_enf %>%
  #filter(group=='black'|group=='white') %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T)) %>%
  left_join(enrollment) %>%
  mutate(proportion = number/total_number) -> tempout

tempout %>% 
  ungroup() %>%
  mutate(impossible = number>total_number) %>%
  group_by(COMBOKEY) %>%
  mutate(impossible_school = sum(impossible)>0) %>%
  ungroup() %>%
  filter(impossible_school==F) %>%
  filter(group=='black'|group=='white') %>%
  mutate(metric='law_enforcement') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  distinct() %>%
  rbind(subdat) -> subdat

#inschool arrest
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

in_school_arrest$number[which(in_school_arrest$number<0)] <- NA

in_school_arrest %>%
  #filter(group=='black'|group=='white') %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T)) %>%
  left_join(enrollment) %>%
  mutate(proportion = number/total_number) -> tempout

tempout %>% 
  ungroup() %>%
  mutate(impossible = number>total_number) %>%
  group_by(COMBOKEY) %>%
  mutate(impossible_school = sum(impossible)>0) %>%
  ungroup() %>%
  filter(impossible_school==F) %>%
  filter(group=='black'|group=='white') %>%
  mutate(metric='in_school_arrest') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  distinct() %>%
  rbind(subdat) -> subdat


################
# transform county information #
################

rm(exp_w_ed, exp_wo_ed, in_school_arrest, law_enf, oos_susp, susp_inschool)

subdat %>%
  select(COMBOKEY, LEA_STATE, LEA_NAME, CCD_LATCOD, CCD_LONCOD, LEAID) %>%
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
schools_loc <- read.csv('/Users/travis/Documents/gits/educational_disparities/output/schools_w_fips.csv')
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
  select(county_id, county_name, state_abb, state_fips, county_fips, bias, 
         explicit, explicit_diff, weighted_bias, weighted_explicit, 
         weighted_explicit_diff, COMBOKEY, group, number, total_number, metric, 
         LEAID) -> tempout

################
# Exclude schools #
################

# Get exclusion schools
df_school %>%
  select(COMBOKEY, LEAID, JJ) %>%
  filter(JJ == 'YES') %>%
  mutate(LEAID = droplevels(LEAID))-> exclude

# these are LEAID numbers of schools to exclude
# error_elem <- c('06CC088', '06CC027', '0623340', '0625470', '0629370', '0633600', 
#                 '0634410', '0637050', '1201710', '1500030', '2802580', '3620580', 
#                 '4218990', '5308700', '1201860', '0500394') #taken from elementary spreadsheet at:
#https://civilrightsproject.ucla.edu/resources/projects/center-for-civil-rights-remedies/school-to-prison-folder/federal-reports/are-we-closing-the-school-discipline-gap

# error_second <- c('0100002', '0500394', '0500390', '0409734', '0400144', 
#                   '0400617', '0691007', '06CC087', '06CC121', '0623340',
#                   '0625470', '0629370', '0691025', '0600094', '0633600',
#                   '0634410', '0691037', '0637050', '0638640', '08SOP01',
#                   '11DOJ02', '1200030', '1200120', '1200510', '1200002',
#                   '1200960', '1200990', '1201200', '1201710', '1201860',
#                   '1202011', '1300026', '1500030', '9999088', '19SOP03',
#                   '1600148', '1600016', '1600144', '1709810', '1712060',
#                   '1700006', '18DOJ15', '2000352', '2000008', '22DOJ06',
#                   '2400060', '24SOP02', '2300056', '2600166', '2604290',
#                   '26DOJ08', '2600316', '2600968', '2700272', '2700260',
#                   '28DOJ01', '2802580', '3000091', '3700157', '3800005',
#                   '3100051', '3100046', '33SOP01', '3303271', '35DOJ03',
#                   '32SOP01', '3620580', '3600131', '3900488', '40SOP01',
#                   '4100043', '4200091', '42DOJ26', '4209960', '4289110',
#                   '4209932', '4289280', '4218990', '4200028', '4209934',
#                   '4503420', '4600035', '47SOP04', '4800196', '4800189',
#                   '4800223', '4800250', '4800048', '4800182', '5100070',
#                   '5000005', '5308700', '5500035', '5600015', '5680251')

tempout %>%
  mutate(exclude = COMBOKEY %in% exclude$COMBOKEY ) -> tempout

################
# append covariates #
################

df_acs <- read.csv('/Users/travis/Documents/gits/Data/ACS/county_ethnicity/ACS_14_5YR_B02001_with_ann.csv',skip = 1)

covs_pop <- df_acs[,c(3, 4, 6, 8)]
names(covs_pop) <- c('county', 'total_pop', 'white_pop', 'black_pop')

df_acs <- read.csv('/Users/travis/Documents/gits/Data/ACS/county_poverty_emp/ACS_14_5YR_DP03_with_ann.csv',skip = 1)

covs_emp <- df_acs[,c(3, 22, 248, 478)]
names(covs_emp) <- c('county', 'unemp_rate', 'med_income', 'poverty_rate')

df_acs <- read.csv('/Users/travis/Documents/gits/Data/ACS/county_education/ACS_14_5YR_S1501_with_ann.csv',skip=1)
covs_ed <- df_acs[,c(3, 28)]
names(covs_ed) <- c('county', 'col_grads')

df_acs <- read.csv('/Users/travis/Documents/gits/Data/ACS/county_mobility/ACS_14_5YR_S0701_with_ann.csv', skip=1)
covs_mob <- df_acs[,c(3, 158, 160, 162)]
covs_mob$mobility <- as.numeric(as.character(covs_mob[,2])) + 
  as.numeric(as.character(covs_mob[,3])) + 
  as.numeric(as.character(covs_mob[,4]))
covs_mob$mobility[which(is.na(covs_mob$mobility))] <- 0
covs_mob <- covs_mob[,c(1,5)]
names(covs_mob)[1] <- 'county'

load(file='/Users/travis/Documents/gits/Data/FBI/ICPSR_33523/DS0001/33523-0001-Data.rda')
load(file='/Users/travis/Documents/gits/Data/FBI/ICPSR_34582/DS0001/34582-0001-Data.rda')
load(file='/Users/travis/Documents/gits/Data/FBI/ICPSR_35019/DS0001/35019-0001-Data.rda')
load(file='/Users/travis/Documents/gits/Data/FBI/ICPSR_36117/DS0001/36117-0001-Data.rda')
load(file='/Users/travis/Documents/gits/Data/FBI/ICPSR_36399/DS0001/36399-0001-Data.rda')

df_fbi <- rbind(da33523.0001, da34582.0001, 
                da35019.0001, da36117.0001, da36399.0001)
df_fbi %>%
  select(FIPS_ST, FIPS_CTY, CPOPARST, P1VLNT) %>%
  mutate(state_fips = formatC(FIPS_ST, width = 2, format = "d", flag = "0"),
         county_fips = formatC(FIPS_CTY, width = 3, format = "d", flag = "0"),
         crime_rate = P1VLNT/CPOPARST) %>%
  group_by(state_fips, county_fips) %>%
  summarise(crime_rate = mean(crime_rate, na.rm=T)) %>%
  ungroup() %>%
  mutate(county_fips = paste(state_fips, county_fips, sep='')) %>%
  select(county_fips, crime_rate) -> covs_fbi

df_acs <- read.csv('/Users/travis/Documents/gits/Data/ACS/county_housing/DEC_10_SF1_GCTPH1.CY07_with_ann.csv',skip=1)
covs_hous <- df_acs[,c(3, 6, 7, 14)]
names(covs_hous) <- c('county', 'county1', 'county2', 'density')
covs_hous %>%
  filter(as.character(county1) == as.character(county2)) %>%
  mutate(density = as.numeric(as.character(density))) %>%
  select(county, density) -> covs_hous

df_haley <- read.csv('/Users/travis/Documents/gits/Data/Haley_countylinks/_Master Spreadsheet.csv')
names(df_haley)[6] <- 'county'

covs_pop %>%
  left_join(covs_ed) %>%
  left_join(covs_emp) %>%
  left_join(df_haley) %>%
  left_join(covs_fbi) %>% 
  left_join(covs_hous) %>%
  left_join(covs_mob) %>%
  mutate(white_prop = white_pop/total_pop,
         black_prop = black_pop/total_pop) %>%
  mutate(b.w.ratio = black_prop/white_prop) -> covs

#counties w/o IAT data: 
#Aleutians East, Hoonah-Angoon, Prince of Wales-Hyder, Skagway, Wrangell, 
#Hinsdale, Hodgeman, Berkshire, Petroleum, Powder River, Treasure, Banner, Loup,
#Billings, Rolette, Borden, Culberson, Foard, Glassock, Irion, Kenedy, Piute,
#Greensville

#counties w/o schools:
#kalawao, Issaquena, Mora, Divide, Loving

covs %>%
  mutate(total_pop = scale(total_pop)[,1],
         col_grads = scale(col_grads)[,1],
         unemp_rate = scale(unemp_rate)[,1],
         med_income = scale(med_income)[,1],
         poverty_rate = scale(poverty_rate)[,1],
         crime_rate = scale(crime_rate)[,1],
         density = scale(density)[,1],
         mobility = scale(mobility)[,1],
         white_prop = scale(white_prop)[,1],
         black_prop = scale(black_prop)[,1],
         b.w.ratio = scale(b.w.ratio)[,1]) %>%
  mutate(county_id = paste(state_code, 
                           stringr::str_sub(county_fips, -3, -1), sep='-')) %>%
  right_join(tempout, by='county_id') -> mod.dat

# tempthis %>% 
#   select(county_id, county_name, state_abb, state) %>%
#   distinct() %>%
#   write.csv(., file='output/county_linking_table.csv', row.names=FALSE)

################
# write file #
################

#change the name of the file, depending on what model it is for
write.csv(mod.dat, file='/Users/travis/Documents/gits/educational_disparities/output/full_model_data.csv', row.names = FALSE)


################
# teacher data #
################
county_teacher_estimates <- read.csv('/Users/travis/Documents/gits/educational_disparities/output/county_teacher_means.csv')

schools_loc %>%
  left_join(county_teacher_estimates) %>%
  filter(!is.na(teacher_bias)) %>%
  left_join(states) %>%
  left_join(covs) %>%
  mutate(exclude = COMBOKEY %in% exclude$COMBOKEY | 
           LEAID %in% error_elem |
           LEAID %in% error_second) -> out

write.csv(out, file='/Users/travis/Documents/gits/educational_disparities/output/teacher_model_data.csv', row.names=FALSE)

#################
# other metrics #
#################
# corporal punishment
df_school %>% 
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_DISCWODIS_CORP_HI_M:TOT_DISCWODIS_CORP_F, 
         SCH_DISCWDIS_CORP_IDEA_HI_M:TOT_DISCWDIS_CORP_IDEA_F) %>% 
  gather(group, number, SCH_DISCWODIS_CORP_HI_M:TOT_DISCWDIS_CORP_IDEA_F) %>%
  mutate(g=group) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  separate(prefix, into=c('prefix', 'disability'), -3) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='RP_', 
                          total='EA_', biracial='TR_', white='WH_')) %>%
  mutate(disability=fct_recode(disability, not_disabled='P_', disabled='ID',
                               not_disabled='CO', disabled='A_')) %>%
  select(-prefix) -> corp

corp %>%
  filter(group=='black'|group=='white') %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T)) %>%
  left_join(enrollment) %>%
  mutate(proportion = number/total_number) -> tempout

tempout$number[which(tempout$number<0)] <- NA

tempout %>% 
  ungroup() %>%
  filter(is.finite(proportion)) %>%
  filter(total_number>number) %>%
  mutate(metric='corporal') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  distinct() -> subdat

# expulsion zero tolerance
df_school %>% 
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_DISCWODIS_EXPZT_HI_M:TOT_DISCWODIS_EXPZT_F, 
         SCH_DISCWDIS_EXPZT_IDEA_HI_M:TOT_DISCWDIS_EXPZT_IDEA_F) %>% 
  gather(group, number, SCH_DISCWODIS_EXPZT_HI_M:TOT_DISCWDIS_EXPZT_IDEA_F) %>%
  mutate(g=group) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  separate(prefix, into=c('prefix', 'disability'), -3) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='ZT_', 
                          total='EA_', biracial='TR_', white='WH_')) %>%
  mutate(disability=fct_recode(disability, not_disabled='T_', disabled='ID',
                               not_disabled='XP', disabled='A_')) %>%
  select(-prefix) -> exp_0_tolerance

exp_0_tolerance %>%
  filter(group=='black'|group=='white') %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T)) %>%
  left_join(enrollment) %>%
  mutate(proportion = number/total_number) -> tempout

tempout$number[which(tempout$number<0)] <- NA

tempout %>% 
  ungroup() %>%
  filter(is.finite(proportion)) %>%
  filter(total_number>number) %>%
  mutate(metric='expulsion_0_tolerance') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  distinct() %>%
  rbind(subdat) -> subdat

#expulsion with ed
df_school %>% 
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_DISCWODIS_EXPWE_HI_M:TOT_DISCWODIS_EXPWE_F, 
         SCH_DISCWDIS_EXPWE_IDEA_HI_M:TOT_DISCWDIS_EXPWE_IDEA_F) %>% 
  gather(group, number, SCH_DISCWODIS_EXPWE_HI_M:TOT_DISCWDIS_EXPWE_IDEA_F) %>%
  mutate(g=group) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  separate(prefix, into=c('prefix', 'disability'), -3) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='WE_', 
                          total='EA_', biracial='TR_', white='WH_')) %>%
  mutate(disability=fct_recode(disability, not_disabled='T_', disabled='ID',
                               not_disabled='XP', disabled='E_')) %>%
  select(-prefix) -> exp_w_ed

exp_w_ed %>%
  filter(group=='black'|group=='white') %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T)) %>%
  left_join(enrollment) %>%
  mutate(proportion = number/total_number) -> tempout

tempout$number[which(tempout$number<0)] <- NA

tempout %>% 
  ungroup() %>%
  filter(is.finite(proportion)) %>%
  filter(total_number>number) %>%
  mutate(metric='expulsion_w_ed') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  distinct() %>%
  rbind(subdat) -> subdat

#expulsion without ed
df_school %>% 
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_DISCWODIS_EXPWOE_HI_M:TOT_DISCWODIS_EXPWOE_F, 
         SCH_DISCWDIS_EXPWOE_IDEA_HI_M:TOT_DISCWDIS_EXPWOE_IDEA_F) %>% 
  gather(group, number, SCH_DISCWODIS_EXPWOE_HI_M:TOT_DISCWDIS_EXPWOE_IDEA_F) %>%
  mutate(g=group) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  separate(prefix, into=c('prefix', 'disability'), -3) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='OE_', 
                          total='EA_', biracial='TR_', white='WH_')) %>%
  mutate(disability=fct_recode(disability, not_disabled='E_', disabled='ID',
                               not_disabled='PW', disabled='A_')) %>%
  select(-prefix) -> exp_wo_ed

exp_wo_ed %>%
  filter(group=='black'|group=='white') %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T)) %>%
  left_join(enrollment) %>%
  mutate(proportion = number/total_number) -> tempout

tempout$number[which(tempout$number<0)] <- NA

tempout %>% 
  ungroup() %>%
  filter(is.finite(proportion)) %>%
  filter(total_number>number) %>%
  mutate(metric='expulsion_wo_ed') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  distinct() %>%
  rbind(subdat) -> subdat

#in-school arrest
df_school %>% 
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_DISCWODIS_ARR_HI_M:TOT_DISCWODIS_ARR_F, 
         SCH_DISCWDIS_ARR_IDEA_HI_M:TOT_DISCWDIS_ARR_IDEA_F) %>% 
  gather(group, number, SCH_DISCWODIS_ARR_HI_M:TOT_DISCWDIS_ARR_IDEA_F) %>%
  mutate(g=group) %>%
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
  filter(group=='black'|group=='white') %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T)) %>%
  left_join(enrollment) %>%
  mutate(proportion = number/total_number) -> tempout

tempout$number[which(tempout$number<0)] <- NA

tempout %>% 
  ungroup() %>%
  filter(is.finite(proportion)) %>%
  filter(total_number>number) %>%
  mutate(metric='in_school_arrest') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  distinct() %>%
  rbind(subdat) -> subdat

#in-school suspension
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
  filter(group=='black'|group=='white') %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T)) %>%
  left_join(enrollment) %>%
  mutate(proportion = number/total_number) -> tempout

tempout$number[which(tempout$number<0)] <- NA

tempout %>% 
  ungroup() %>%
  filter(is.finite(proportion)) %>%
  filter(total_number>number) %>%
  mutate(metric='inschool_susp') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  distinct() -> subdat

#law enforcement
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
  filter(group=='black'|group=='white') %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T)) %>%
  left_join(enrollment) %>%
  mutate(proportion = number/total_number) -> tempout

tempout %>% 
  ungroup() %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  ungroup() %>%
  mutate(metric='law_enforcement') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  distinct() %>%
  rbind(subdat) -> subdat

#mechanical restraint
df_school %>% 
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_RS_NONIDEA_MECH_HI_M:TOT_RS_NONIDEA_MECH_F,
         SCH_RS_IDEA_MECH_HI_M:TOT_RS_IDEA_MECH_F) %>% 
  gather(group, number, SCH_RS_NONIDEA_MECH_HI_M:TOT_RS_IDEA_MECH_F) %>%
  mutate(g=group) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  separate(prefix, into=c('prefix', 'disability'), -12) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='CH_', 
                          biracial='TR_', white='WH_')) %>%
  mutate(disability=fct_recode(disability, not_disabled='_NONIDEA_ME', 
                               disabled='_IDEA_MECH_',
                               not_disabled='NIDEA_MECH_', 
                               disabled='_RS_IDEA_ME')) %>%
  select(-prefix) -> mechanical_restraint

mechanical_restraint %>%
  filter(group=='black'|group=='white') %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T)) %>%
  left_join(enrollment) %>%
  mutate(proportion = number/total_number) -> tempout

tempout %>% 
  ungroup() %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  ungroup() %>%
  mutate(metric='mechanical_restraint') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  distinct() %>%
  rbind(subdat) -> subdat

#oos suspension
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
  filter(group=='black'|group=='white') %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T)) %>%
  left_join(enrollment) %>%
  mutate(proportion = number/total_number) -> tempout

tempout$number[which(tempout$number<0)] <- NA

tempout %>% 
  ungroup() %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  mutate(metric='oos_susp') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  distinct() %>%
  rbind(subdat) -> subdat

#physical restraint
df_school %>% 
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_RS_NONIDEA_PHYS_HI_M:TOT_RS_NONIDEA_PHYS_F,
         SCH_RS_IDEA_PHYS_HI_M:TOT_RS_IDEA_PHYS_F) %>% 
  gather(group, number, SCH_RS_NONIDEA_PHYS_HI_M:TOT_RS_IDEA_PHYS_F) %>%
  mutate(g=group) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  separate(prefix, into=c('prefix', 'disability'), -12) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='YS_', 
                          biracial='TR_', white='WH_')) %>%
  mutate(disability=fct_recode(disability, not_disabled='_NONIDEA_PH', 
                               disabled='_IDEA_PHYS_',
                               not_disabled='NIDEA_PHYS_', 
                               disabled='_RS_IDEA_PH')) %>%
  select(-prefix) -> physical_restraint

physical_restraint %>%
  filter(group=='black'|group=='white') %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T)) %>%
  left_join(enrollment) %>%
  mutate(proportion = number/total_number) -> tempout

tempout %>% 
  ungroup() %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  ungroup() %>%
  mutate(metric='physical_restraint') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  distinct() %>%
  rbind(subdat) -> subdat

## Preschool enrollment
df_school %>%
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, SCH_PSENR_HI_M:TOT_PSENR_F) %>%
  gather(group, total_number, SCH_PSENR_HI_M:TOT_PSENR_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>%
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='NR_', 
                          biracial='TR_', white='WH_')) %>%
  filter(total_number>=0) %>%
  group_by(COMBOKEY, group) %>%
  mutate(total_number = sum(total_number)) %>%
  ungroup() %>%
  select(-prefix, -gender) %>%
  distinct() -> ps_enrollment

#preschool expulsion
df_school %>% 
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_PSDISC_EXP_HI_M:TOT_PSDISC_EXP_F) %>% 
  gather(group, number, SCH_PSDISC_EXP_HI_M:TOT_PSDISC_EXP_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='XP_', 
                          biracial='TR_', white='WH_')) %>%
  filter(number>=0) %>%
  select(-prefix) -> ps_exp

ps_exp %>%
  filter(group=='black'|group=='white') %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T)) %>%
  left_join(ps_enrollment) %>%
  mutate(proportion = number/total_number) -> tempout

tempout %>% 
  ungroup() %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  ungroup() %>%
  mutate(metric='preschool_expulsion') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  distinct() %>%
  rbind(subdat) -> subdat

#preschool suspension
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
  filter(group=='black'|group=='white') %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T)) %>%
  left_join(ps_enrollment) %>%
  mutate(proportion = number/total_number) -> tempout

tempout %>% 
  ungroup() %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  ungroup() %>%
  mutate(metric='preschool_susp') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  distinct() %>%
  rbind(subdat) -> subdat

#seclusion
df_school %>% 
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_RS_NONIDEA_SECL_HI_M:TOT_RS_NONIDEA_SECL_F,
         SCH_RS_IDEA_SECL_HI_M:TOT_RS_IDEA_SECL_F) %>% 
  gather(group, number, SCH_RS_NONIDEA_SECL_HI_M:TOT_RS_IDEA_SECL_F) %>%
  mutate(g=group) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  separate(prefix, into=c('prefix', 'disability'), -12) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='CL_', 
                          biracial='TR_', white='WH_')) %>%
  mutate(disability=fct_recode(disability, not_disabled='_NONIDEA_SE', 
                               disabled='_IDEA_SECL_',
                               not_disabled='NIDEA_SECL_', 
                               disabled='_RS_IDEA_SE')) %>%
  select(-prefix) -> seclusion

seclusion %>%
  filter(group=='black'|group=='white') %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T)) %>%
  left_join(enrollment) %>%
  mutate(proportion = number/total_number) -> tempout

tempout %>% 
  ungroup() %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  ungroup() %>%
  mutate(metric='seclusion') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  distinct() %>%
  rbind(subdat) -> subdat

