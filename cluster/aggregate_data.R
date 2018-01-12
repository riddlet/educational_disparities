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
df_school <- read.csv('../../Data/crdc201314csv/CRDC2013_14_SCH.csv')
county_means <- read.csv('/Users/travis/Documents/gits/educational_disparities/output/county_means_explicit_diff.csv', 
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
  filter(group=='black'|group=='white') %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
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
  filter(group=='black'|group=='white') %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
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
  filter(group=='black'|group=='white') %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T),
            w_ed_number = sum(w_ed_num, na.rm=T),
            total_number = sum(total_number)) %>%
  mutate(number=number+w_ed_number) %>%
  select(-w_ed_number) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
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
  filter(group=='black'|group=='white') %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
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
  filter(group=='black'|group=='white') %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  ungroup() %>%
  mutate(metric='in_school_arrest') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

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
         COMBOKEY, group, number, total_number, metric, LEAID) -> tempout

# Get exclusion schools
df_school %>%
  select(COMBOKEY, LEAID, JJ) %>%
  filter(JJ == 'YES') %>%
  mutate(LEAID = droplevels(LEAID))-> exclude

# these are LEAID numbers of schools to exclude
error_elem <- c('06CC088', '06CC027', '0623340', '0625470', '0629370', '0633600', 
                '0634410', '0637050', '1201710', '1500030', '2802580', '3620580', 
                '4218990', '5308700', '1201860', '0500394') #taken from elementary spreadsheet at:
#https://civilrightsproject.ucla.edu/resources/projects/center-for-civil-rights-remedies/school-to-prison-folder/federal-reports/are-we-closing-the-school-discipline-gap

error_second <- c('0100002', '0500394', '0500390', '0409734', '0400144', 
                  '0400617', '0691007', '06CC087', '06CC121', '0623340',
                  '0625470', '0629370', '0691025', '0600094', '0633600',
                  '0634410', '0691037', '0637050', '0638640', '08SOP01',
                  '11DOJ02', '1200030', '1200120', '1200510', '1200002',
                  '1200960', '1200990', '1201200', '1201710', '1201860',
                  '1202011', '1300026', '1500030', '9999088', '19SOP03',
                  '1600148', '1600016', '1600144', '1709810', '1712060',
                  '1700006', '18DOJ15', '2000352', '2000008', '22DOJ06',
                  '2400060', '24SOP02', '2300056', '2600166', '2604290',
                  '26DOJ08', '2600316', '2600968', '2700272', '2700260',
                  '28DOJ01', '2802580', '3000091', '3700157', '3800005',
                  '3100051', '3100046', '33SOP01', '3303271', '35DOJ03',
                  '32SOP01', '3620580', '3600131', '3900488', '40SOP01',
                  '4100043', '4200091', '42DOJ26', '4209960', '4289110',
                  '4209932', '4289280', '4218990', '4200028', '4209934',
                  '4503420', '4600035', '47SOP04', '4800196', '4800189',
                  '4800223', '4800250', '4800048', '4800182', '5100070',
                  '5000005', '5308700', '5500035', '5600015', '5680251')

tempout %>%
  filter(COMBOKEY %ni% exclude$COMBOKEY) %>%
  filter(LEAID %ni% error_elem) %>%
  filter(LEAID %ni% error_second) -> tempout

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

covs %>%
  left_join(states) %>%
  right_join(tempout) -> mod.dat

# tempthis %>% 
#   select(county_id, county_name, state_abb, state) %>%
#   distinct() %>%
#   write.csv(., file='output/county_linking_table.csv', row.names=FALSE)



write.csv(mod.dat, file='output/selected_model_data_ucla_excl_exp_diff.csv', row.names = FALSE)

### write with teacher data
county_teacher_estimates <- read.csv('/Users/travis/Documents/gits/educational_disparities/output/county_teacher_means.csv')

schools_loc %>%
  left_join(county_teacher_estimates) %>%
  filter(!is.na(county_bias)) %>%
  left_join(states) %>%
  left_join(covs) %>%
  filter(COMBOKEY %ni% exclude$COMBOKEY) %>%
  filter(LEAID %ni% error_elem) %>%
  filter(LEAID %ni% error_second) -> out

write.csv(out, file='output/teacher_model_data.csv', row.names=FALSE)
