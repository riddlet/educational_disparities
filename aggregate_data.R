library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(rstanarm)
library(lme4)
library(httr)
library(stringr)


"%ni%" <- Negate("%in%")

#read in data
df_school <- read.csv('/Users/travis/Documents/gits/Data/crdc201314csv/CRDC2013_14_SCH.csv')
county_means <- read.csv('/Users/travis/Documents/gits/educational_disparities/output/county_means.csv')
df_haley <- read.csv('/Users/travis/Documents/gits/Data/linking_files/_Master Spreadsheet.csv')
county_means_sex <- read.csv('/Users/travis/Documents/gits/educational_disparities/output/county_means_sexuality.csv')

# Get enrollment figures by race for all schools
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
#################### 

# in-school suspension
df_school %>% 
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_DISCWODIS_ISS_HI_M:TOT_DISCWODIS_ISS_F, 
         SCH_DISCWDIS_ISS_IDEA_HI_M:TOT_DISCWDIS_ISS_IDEA_F) %>%  #get relevant columns
  gather(group, number, SCH_DISCWODIS_ISS_HI_M:TOT_DISCWDIS_ISS_IDEA_F) %>% #"tidy"
  #separate out gender, group, & disability from column names
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  separate(prefix, into=c('prefix', 'disability'), -3) %>% 
  #rename nonsensical "groups"
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='SS_', 
                          total='EA_', biracial='TR_', white='WH_')) %>% 
  #rename nonsensical "disabled"
  mutate(disability=fct_recode(disability, not_disabled='_I', disabled='ID',
                               not_disabled='S_', disabled='A_')) %>% 
  select(-prefix) -> susp_inschool

susp_inschool$number[which(susp_inschool$number<0)] <- NA #-9 & -5 is code for missing

susp_inschool %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  #total number of incidents at each school for each group
  summarise(number = sum(number, na.rm=T)) %>%
  #join with enrollment totals
  left_join(enrollment) %>%
  mutate(proportion = number/total_number) -> susp_inschool #proportions just because


susp_inschool %>% 
  ungroup() %>%
  mutate(impossible = number>total_number) %>% #mark impossible observations
  group_by(COMBOKEY) %>%
  mutate(impossible_school = sum(impossible)>0) %>% #any impossible numbers?
  ungroup() %>%
  filter(group=='black'|group=='white') %>% #only keep black and white students
  mutate(metric='inschool_susp') %>% #label all these with their metric
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>% #housekeeping
  distinct() -> subdat

#out of school suspension
# these are grouped into students who were suspended once, and those who were 
# suspended multiple times
#first, the single offenders
df_school %>% 
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_DISCWODIS_SINGOOS_HI_M:TOT_DISCWODIS_SINGOOS_F,
         SCH_DISCWDIS_SINGOOS_IDEA_HI_M:TOT_DISCWDIS_SINGOOS_IDEA_F) %>%  #get relevant columns
  gather(group, number, SCH_DISCWODIS_SINGOOS_HI_M:TOT_DISCWDIS_SINGOOS_IDEA_F) %>% #"tidy"
  separate(group, into=c('group', 'gender'), -2) %>%
  separate(group, into=c('prefix', 'group'), -4) %>%
  separate(prefix, into=c('prefix', 'disability'), -3) %>% #separate out gender, group, & disab.
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='OS_', 
                          total='EA_', biracial='TR_', white='WH_')) %>% #rename nonsensical groups
  mutate(disability=fct_recode(disability, not_disabled='GO', disabled='ID',
                               not_disabled='S_', disabled='A_')) %>% #rename nonsensical disab.
  select(-prefix) -> oos_susp

#do the same for the multi-offenders
df_school %>% 
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_DISCWODIS_MULTOOS_HI_M:TOT_DISCWODIS_MULTOOS_F,
         SCH_DISCWDIS_MULTOOS_IDEA_HI_M:TOT_DISCWDIS_MULTOOS_IDEA_F) %>% #gather relevant columns
  gather(group, number_2, SCH_DISCWODIS_MULTOOS_HI_M:TOT_DISCWDIS_MULTOOS_IDEA_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>%
  separate(group, into=c('prefix', 'group'), -4) %>%
  separate(prefix, into=c('prefix', 'disability'), -3) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='OS_', 
                          biracial='TR_', white='WH_', total='EA_')) %>%
  mutate(disability=fct_recode(disability, not_disabled='TO', disabled='ID',
                               not_disabled='S_', disabled='A_')) %>%
  select(COMBOKEY, group, gender, disability, number_2) %>%
  right_join(oos_susp) -> oos_susp

#there are 16 instances (across groups, genders & schools) where they report one 
#but not the other. because it does not make sense to aggregate single & 
#multiple for all schools but these, we need to remove these schools
oos_susp$number[which(oos_susp$number<0)] <- NA
oos_susp$number_2[which(oos_susp$number_2<0)] <- NA
oos_susp$number <- oos_susp$number+oos_susp$number_2
#the 16 (along with the others who don't report any) now have NA

#summarise across gender
oos_susp %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T)) %>%
  left_join(enrollment) %>%
  mutate(proportion = number/total_number) -> oos_susp

#mark impossible schools
oos_susp %>% 
  ungroup() %>%
  mutate(impossible = number>total_number) %>% #mark impossible groups
  group_by(COMBOKEY) %>%
  mutate(impossible_school = sum(impossible)>0) %>% #any impossible groups in each school?
  ungroup() %>%
  filter(group=='black'|group=='white') %>% #now limit the data to target group
  mutate(metric='oos_susp') %>% #mark which metric this is
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  distinct() %>%
  rbind(subdat) -> subdat #housecleaning & append

#### expulsion with education
df_school %>% 
  select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_DISCWODIS_EXPWE_HI_M:TOT_DISCWODIS_EXPWE_F,
         SCH_DISCWDIS_EXPWE_IDEA_HI_M:TOT_DISCWDIS_EXPWE_IDEA_F) %>% 
  gather(group, w_ed_number, 
         SCH_DISCWODIS_EXPWE_HI_M:TOT_DISCWDIS_EXPWE_IDEA_F) %>%
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
  gather(group, wo_ed_number, 
         SCH_DISCWODIS_EXPWOE_HI_M:TOT_DISCWDIS_EXPWOE_IDEA_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  separate(prefix, into=c('prefix', 'disability'), -3) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='OE_', 
                          biracial='TR_', white='WH_', total='EA_')) %>%
  mutate(disability=fct_recode(disability, not_disabled='PW', disabled='ID',
                               not_disabled='E_', disabled='A_')) %>%
  select(-prefix) -> exp_wo_ed

exp_w_ed$w_ed_number[which(exp_w_ed$w_ed_number<0)] <- NA
exp_wo_ed$wo_ed_number[which(exp_wo_ed$wo_ed_number<0)] <- NA

#combine w and wo ed, then sum over gender
#as above with suspensions, there are a number of instances in which the data 
#is missing for one, but not the other. The counts here are much higher 
#(18,577 observations across all groups, genders, and schools)

exp_w_ed %>%
  left_join(exp_wo_ed) %>%
  mutate(number = wo_ed_number + w_ed_number) %>% #combined
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T)) %>%
  left_join(enrollment) %>%
  mutate(proportion = number/total_number) -> expulsions

expulsions %>% 
  ungroup() %>%
  mutate(impossible = number>total_number) %>% #mark impossible groups
  group_by(COMBOKEY) %>%
  mutate(impossible_school = sum(impossible)>0) %>% #any groups impossible?
  ungroup() %>%
  filter(group=='black'|group=='white') %>% #keep target groups
  mutate(metric='expulsion_combined') %>% #mark\ the metric
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  distinct() %>%
  rbind(subdat) -> subdat #housekeeping & append

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

#missing data/na's
law_enf$number[which(law_enf$number<0)] <- NA

law_enf %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T)) %>% #sum over genders
  left_join(enrollment) %>%
  mutate(proportion = number/total_number) -> law_enf

law_enf %>% 
  ungroup() %>%
  mutate(impossible = number>total_number) %>% #mark impossible groups
  group_by(COMBOKEY) %>%
  mutate(impossible_school = sum(impossible)>0) %>% #any impossible groups?
  ungroup() %>%
  filter(group=='black'|group=='white') %>% #filter to target groups
  mutate(metric='law_enforcement') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  distinct() %>%
  rbind(subdat) -> subdat #housekeeping & append

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

#missing/NA's
in_school_arrest$number[which(in_school_arrest$number<0)] <- NA

#sum over gender
in_school_arrest %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, LEAID,
           CCD_LATCOD, CCD_LONCOD, SCH_NAME, group) %>%
  summarise(number = sum(number, na.rm=T)) %>%
  left_join(enrollment) %>%
  mutate(proportion = number/total_number) -> in_school_arrest


in_school_arrest %>% 
  ungroup() %>%
  mutate(impossible = number>total_number) %>% #mark impossible groups
  group_by(COMBOKEY) %>%
  mutate(impossible_school = sum(impossible)>0) %>% #any impossible groups?
  ungroup() %>%
  filter(group=='black'|group=='white') %>% #filter to target group
  mutate(metric='in_school_arrest') %>% #mark the metric
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  distinct() %>%
  rbind(subdat) -> subdat #housekeeping & append


################
# transform county information #
################

rm(exp_w_ed, exp_wo_ed, in_school_arrest, law_enf, oos_susp, susp_inschool)

#just get lat, long & identifying info
df_school %>%
  select(COMBOKEY, LEA_STATE, LEA_NAME, CCD_LATCOD, CCD_LONCOD, LEAID) %>%
  filter(!is.na(CCD_LATCOD)) %>%
  distinct() -> schools_loc

#function to retrieve FIPS code
get_fips_code <- function(lat, long) {
  url <- paste('https://geo.fcc.gov/api/census/block/find?latitude=',
               lat, '&longitude=', long, '&format=xml', sep='')
  r <- httr::GET(url)
  suppressMessages(out <- xml2::as_list(content(r)))
  out <- paste(attr(out$County, "FIPS"),
               attr(out$County, "name"),
               attr(out$State, "FIPS"),
               attr(out$State, "code"),
               sep='||')
  return(out)
}
#it takes forever to run, so I ran it once, and saved the results
#schools_loc$fips_api <- NA
#for(i in 1:length(schools_loc$CCD_LATCOD)){
# schools_loc$fips_api[i] <- get_fips_code(schools_loc$CCD_LATCOD[i],
#                                          schools_loc$CCD_LONCOD[i])
# print(i)
# Sys.sleep(.05)
#}

#write.csv(schools_loc, file='/Users/travis/Documents/gits/educational_disparities/output/schools_w_fips.csv', row.names = F)
schools_loc <- read.csv('/Users/travis/Documents/gits/educational_disparities/output/schools_w_fips.csv')
schools_loc <- left_join(subdat, schools_loc)  
schools_loc %>%
  filter(!is.na(fips_api)) %>%
  separate(fips_api, into = c('county_fips', 'co_name', 
                              'state_fips', 'state_abb'), sep='\\|\\|') %>%
  mutate(co_fips = substr(county_fips, 3, 5)) %>%
  mutate(county_id = paste(state_abb, co_fips, sep='-')) -> schools_loc

df_haley %>%
  mutate(state_fips = formatC(state_fips, width = 2, format = "d", flag = "0")) %>%
  right_join(schools_loc) %>%
  select(-co_name, -co_fips) -> schools_loc

county_means %>%
  select(-X) %>%
  right_join(schools_loc) %>%
  select(county_id, county_name, state_abb, state_fips, county_fips, bias, 
         explicit, explicit_diff, weighted_bias, weighted_explicit, 
         weighted_explicit_diff, COMBOKEY, group, number, total_number, metric, 
         LEAID) -> full_data

county_means_sex %>%
  select(-X) %>%
  mutate(sex_bias = bias,
         sex_explicit = explicit,
         weighted_bias_sex = weighted_bias,
         weighted_explicit_sex = weighted_explicit) %>%
  select(county_id, sex_bias, sex_explicit, 
         weighted_bias_sex, weighted_explicit_sex) %>%
  right_join(full_data) -> full_data

################
# Exclude schools #
################

# Get exclusion schools
df_school %>%
  select(COMBOKEY, LEAID, JJ) %>%
  filter(JJ == 'YES') %>%
  mutate(LEAID = droplevels(LEAID)) %>%
  select(COMBOKEY) -> exclude

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

subdat %>%
  select(COMBOKEY, impossible_school, metric) %>%
  distinct() %>%
  group_by(COMBOKEY) %>%
  summarise(exclude=sum(impossible_school)>2) %>% #exclude any that have problems at more than 2 metrics
  filter(exclude==T) %>% #keep the excluded ones only
  select(COMBOKEY) %>%
  rbind(exclude) %>%
  distinct() %>%
  ungroup() -> exclude

full_data %>%
  filter(number<=total_number) %>%
  mutate(exclude = COMBOKEY %in% exclude$COMBOKEY) -> full_data

################
# append covariates #
################
#df_haley <- read.csv('/Users/travis/Documents/gits/Data/Haley_countylinks/_Master Spreadsheet.csv')
#names(df_haley)[6] <- 'county'

df_acs <- read.csv('/Users/travis/Documents/gits/Data/ACS/county_ethnicity/ACS_14_5YR_B02001_with_ann.csv',skip = 1)

covs_pop <- df_acs[,c(2, 3, 4, 6, 8)]
names(covs_pop) <- c('county_fips', 'county', 'total_pop', 'white_pop', 'black_pop')
covs_pop %>%
  mutate(county_fips = formatC(county_fips, width = 5, format = "d", flag = "0")) -> covs_pop

#df_acs <- read.csv('/Users/travis/Documents/gits/Data/ACS/county_poverty_emp/ACS_14_5YR_DP03_with_ann.csv',skip = 1)
df_acs <- read.csv('/Users/travis/Documents/gits/Data/ACS/county_poverty_emp/ACS_14_5YR_S2301_with_ann.csv',skip = 1)
df_acs2 <- read.csv('/Users/travis/Documents/gits/Data/ACS/county_poverty_emp/ACS_14_5YR_S1903_with_ann.csv', skip=1)
df_acs3 <- read.csv('/Users/travis/Documents/gits/Data/ACS/county_poverty_emp/ACS_14_5YR_S1701_with_ann.csv', skip=1)
df_acs4 <- read.csv('/Users/travis/Documents/gits/Data/ACS/county_education/ACS_14_5YR_C15002A_with_ann.csv',skip=1)
df_acs5 <- read.csv('/Users/travis/Documents/gits/Data/ACS/county_education/ACS_14_5YR_C15002B_with_ann.csv', skip=1)

#covs_emp <- df_acs[,c(3, 22, 248, 478)]
covs_emp <- df_acs[,c(2, 3, 82, 90)]
names(covs_emp) <- c('county_fips', 'county', 'unemp_rate_w', 'unemp_rate_b')
covs_emp %>%
 mutate(county_fips = formatC(county_fips, width = 5, format = "d", flag = "0"),
        unemp_rate_b = as.numeric(as.character(unemp_rate_b))) -> covs_emp

covs_inc <- df_acs2[,c(3, 10, 14)]
names(covs_inc) <- c('county', 'med_inc_w', 'med_inc_b')
covs_inc$med_inc_b <- as.numeric(as.character(covs_inc$med_inc_b))

covs_pov <- df_acs3[,c(3, 56, 62)]
names(covs_pov) <- c('county', 'poverty_rate_w', 'poverty_rate_b')
covs_pov$poverty_rate_b <- as.numeric(as.character(covs_pov$poverty_rate_b))

covs_ed_w <- df_acs4[,c(3, 4, 14, 24)]
names(covs_ed_w) <- c('county', 'total', 'col_grads_wm', 'col_grads_ww')

covs_ed_b <- df_acs5[,c(3, 4, 14, 24)]
names(covs_ed_b) <- c('county', 'total', 'col_grads_bm', 'col_grads_bw')

covs_ed_w$col_grads_w <- (covs_ed_w$col_grads_wm+covs_ed_w$col_grads_ww)/covs_ed_w$total
covs_ed_b$col_grads_b <- (covs_ed_b$col_grads_bm+covs_ed_b$col_grads_bw)/covs_ed_b$total

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

df_seg <- read.csv('/Users/travis/Documents/gits/Data/ACS/county_segregation/ACS_14_5YR_B02001_with_ann.csv', 
                   skip=1, stringsAsFactors = F)
#state-level segregation index
df_seg <- df_seg[,c(1:4, 6,8)]
names(df_seg) <- c('ID', 'ID2', 'Geo', 'Total', 'White', 'Black')
df_seg %>%
  mutate(FIPS = stringr::str_sub(ID, -11, -1)) %>%
  mutate(state_fips = stringr::str_sub(FIPS, 1, 2),
         county_fips = stringr::str_sub(FIPS, 3, 5),
         census_fips = stringr::str_sub(FIPS, 6, 12)) %>%
  select(-ID, -ID2) %>%
  group_by(state_fips, county_fips) %>%
  mutate(county_total = sum(Total, na.rm=T),
         county_white = sum(White, na.rm=T),
         county_black = sum(Black, na.rm=T)) %>%
  mutate(black_prop = Black/county_black,
         white_prop = White/county_white) %>%
  mutate(bw_diff = abs(black_prop-white_prop)) %>% 
  group_by(state_fips, county_fips) %>%
  mutate(dissim = sum(bw_diff, na.rm=T)*.5) %>% 
  separate(Geo, c('tract', 'county', 'state_name'), sep=',') %>%
  ungroup() %>%
  select(state_name, county, county_fips, state_fips, dissim) %>%
  mutate(state_name = stringr::str_trim(state_name)) %>%
  distinct() %>%
  arrange(desc(dissim)) -> covs_seg

df_states <- data.frame(state_fips = maps::state.fips$fips,
                        state_abb = maps::state.fips$abb)

df_states <- distinct(right_join(df_states, 
                       rbind(data.frame(state_abb=c(state.abb, 'DC'),
                                  state = c(state.name, 'District of Columbia')))))

df_states$state_fips[c(2,11)] <- c(2, 15)
df_states$state_fips <- formatC(df_states$state_fips, width = 2, format = "d", flag = "0")

covs_pop %>%
  left_join(covs_emp) %>%
  left_join(covs_inc) %>%
  left_join(covs_pov) %>%
  left_join(covs_ed_w) %>%
  left_join(covs_ed_b[,c('county', 'col_grads_b')]) %>%
  left_join(covs_fbi) %>% 
  left_join(covs_hous) %>%
  left_join(covs_mob) %>%
  separate(county_fips, into = c('state_fips', 'county_fips'), sep = 2) %>%
  left_join(df_states) %>%
  mutate(white_prop = white_pop/total_pop,
         black_prop = black_pop/total_pop) %>%
  mutate(b.w.ratio = black_prop/white_prop) %>%
  mutate(county_id = paste(state_abb, county_fips, sep='-')) %>%
  left_join(covs_seg[,c('county_fips', 'state_fips', 'dissim')]) -> covs

write.csv(covs, file='/Users/travis/Documents/gits/educational_disparities/output/covariates.csv', row.names = FALSE)

imp <- mice(covs[,c(4:13, 16:20, 23:25, 27)])
covs_imputed <- covs
covs_imputed[,c(4:13, 16:20, 23:25, 27)] <- complete(imp)

write.csv(covs_imputed, file='/Users/travis/Documents/gits/educational_disparities/output/covariates_imputed.csv', row.names = FALSE)

#counties w/o IAT data: 
#Aleutians East, Hoonah-Angoon, Prince of Wales-Hyder, Skagway, Wrangell, 
#Hinsdale, Hodgeman, Berkshire, Petroleum, Powder River, Treasure, Banner, Loup,
#Billings, Rolette, Borden, Culberson, Foard, Glassock, Irion, Kenedy, Piute,
#Greensville

#counties w/o schools:
#kalawao, Issaquena, Mora, Divide, Loving

#pca on covariates
covs_imputed %>%
  select(county_id, unemp_rate_w, unemp_rate_b, med_inc_w, med_inc_b, 
         poverty_rate_w, poverty_rate_b, col_grads_w, col_grads_b) %>%
  mutate(col_grads_diff = scale(col_grads_w-col_grads_b)[,1],
         unemp_rate_diff = scale(unemp_rate_w - unemp_rate_b)[,1],
         med_inc_diff = scale(med_inc_w-med_inc_b)[,1],
         poverty_rate_diff = scale(poverty_rate_w-poverty_rate_b)[,1]) %>%
  mutate(col_grads_w = scale(col_grads_w)[,1],
         unemp_rate_w = scale(unemp_rate_w)[,1],
         med_inc_w = scale(med_inc_w)[,1],
         poverty_rate_w = scale(poverty_rate_w)[,1],
         col_grads_b = scale(col_grads_b)[,1],
         unemp_rate_b = scale(unemp_rate_b)[,1],
         med_inc_b = scale(med_inc_b)[,1],
         poverty_rate_b = scale(poverty_rate_b)[,1]) -> ses_covs
pca_cov_w <- FactoMineR::PCA(ses_covs[,c(2, 4, 6, 8)])
pca_cov_b <- FactoMineR::PCA(ses_covs[,c(3, 5, 7, 9)])
pca_cov_diff <- FactoMineR::PCA(ses_covs[,c(10:13)])

covs_imputed$ses_w <- pca_cov_w$ind$coord[,1]
covs_imputed$ses_b <- pca_cov_b$ind$coord[,1]
covs_imputed$ses_gap <- pca_cov_diff$ind$coord[,1]

covs_imputed %>%
  mutate(total_pop = scale(total_pop)[,1],
         col_grads_w = scale(col_grads_w)[,1],
         col_grads_b = scale(col_grads_b)[,1],
         unemp_rate_w = scale(unemp_rate_w)[,1],
         unemp_rate_b = scale(unemp_rate_b)[,1],
         med_inc_w = scale(med_inc_w)[,1],
         med_inc_b = scale(med_inc_b)[,1],
         poverty_rate_w = scale(poverty_rate_w)[,1],
         poverty_rate_b = scale(poverty_rate_b)[,1],
         crime_rate = scale(crime_rate)[,1],
         density = scale(density)[,1],
         mobility = scale(mobility)[,1],
         white_prop = scale(white_prop)[,1],
         black_prop = scale(black_prop)[,1],
         b.w.ratio = scale(b.w.ratio)[,1],
         dissim = scale(dissim)[,1]) %>%
  right_join(full_data, by='county_id') -> mod.dat

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
  filter(!is.na(teacher_bias)) -> teacher_data

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
  select(county_id, total_pop, col_grads, unemp_rate, med_income, poverty_rate,
         crime_rate, density, mobility, white_prop, black_prop, b.w.ratio) %>%
  right_join(teacher_data) %>%
  mutate(exclude = COMBOKEY %in% exclude$COMBOKEY) -> out

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

