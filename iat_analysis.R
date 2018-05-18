library(haven)
library(dplyr)
library(ggplot2)
library(forcats)
library(rstanarm)
library(tidyr)
library(stringr)
library(lme4)

"%ni%" <- Negate("%in%")
######load datasets
#IAT
df <- rbind(read_sav('/Users/travis/Documents/gits/Data/iat_race/Race IAT.public.2013.sav'),
            read_sav('/Users/travis/Documents/gits/Data/iat_race/Race IAT.public.2012.sav'),
            read_sav('/Users/travis/Documents/gits/Data/iat_race/Race IAT.public.2010.sav'))

df2 <- read_sav('/Users/travis/Documents/gits/Data/iat_race/Race IAT.public.2011.sav')
df3 <- read_sav('/Users/travis/Documents/gits/Data/iat_race/Race IAT.public.2009.sav')
df4 <- read_sav('/Users/travis/Documents/gits/Data/iat_race/Race IAT.public.2008.sav')
df5 <- read_sav('/Users/travis/Documents/gits/Data/iat_race/Race IAT.public.2007.sav')
df6 <- read_sav('/Users/travis/Documents/gits/Data/iat_race/Race IAT.public.2006.sav')
df7 <- read_sav('/Users/travis/Documents/gits/Data/iat_race/Race IAT.public.2005.sav')
df8 <- read_sav('/Users/travis/Documents/gits/Data/iat_race/Race IAT.public.2004.sav')
df9 <- read_sav('/Users/travis/Documents/gits/Data/iat_race/Race IAT.public.2002-2003.sav')
df10 <- read_sav('/Users/travis/Documents/gits/Data/iat_race/Race IAT.public.2014.sav')

#County linking information
df_haley <- read.csv('/Users/travis/Documents/gits/Data/Haley_countylinks/_Master Spreadsheet.csv', stringsAsFactors = F)
df_haley[1791,6] <- 'Doña Ana County, New Mexico' #because of the "ñ"

df_states <- data.frame(state=c(state.name, 'District of Columbia'),
                        state_abb=c(state.abb, 'DC'))

#Covariates
df_acs <- read.csv('/Users/travis/Documents/gits/Data/ACS/county_age/ACS_14_5YR_DP05_with_ann.csv', skip=1)
df_acs_eth <- read.csv('/Users/travis/Documents/gits/Data/ACS/state_ethnicity/ACS_14_5YR_B02001_with_ann.csv',
                       skip = 1, stringsAsFactors = F)
df_acs_ed <- read.csv('/Users/travis/Documents/gits/Data/ACS/state_education/ACS_14_5YR_S1501_with_ann.csv',
                      skip = 1, stringsAsFactors = F)
df_acs_pov_emp <- read.csv('/Users/travis/Documents/gits/Data/ACS/state_poverty_emp/ACS_14_5YR_DP03_with_ann.csv',
                           skip = 1, stringsAsFactors = F)
df_acs_hous <- read.csv('/Users/travis/Documents/gits/Data/ACS/state_housing/DEC_10_SF1_GCTPH1.US04PR_with_ann.csv',
                        skip=1, stringsAsFactors = F)
df_acs_mob <- read.csv('/Users/travis/Documents/gits/Data/ACS/state_mobility/ACS_14_5YR_S0701_with_ann.csv',
                       skip=1, stringsAsFactors = F)
df_fbi <- read.csv('/Users/travis/Documents/gits/Data/FBI/state_crimes/CrimeTrendsInOneVar.csv')
df_seg <- read.csv('/Users/travis/Documents/gits/Data/ACS/county_segregation/ACS_14_5YR_B02001_with_ann.csv',
                   skip=1, stringsAsFactors = F)

#educators <- c('25-2000', '25-3000', '25-4000', '25-9000')
# per stacey's contact in education, limit to the following:
educators <- c('25-2000', '25-3000')

# '25-1000' - postsecondary teachers


## Get IAT data, limit observations to just those with county information, age, 
## and who identify as white
df %>%
  filter(CountyNo!='' &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0to10, raceomb, age) %>%
  filter(raceomb==6) -> subdat

df2 %>%
  filter(CountyNo!='' &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all,  
         tblack_0to10, twhite_0to10, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==6) -> subdat

df3 %>%
  filter(CountyNo!='' &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0to10, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==6) -> subdat

df4 %>%
  filter(CountyNo!='' &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0to10, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==6) -> subdat

df5 %>%
  filter(CountyNo!='' &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0to10, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==6) -> subdat

df6 %>%
  filter(CountyNo!='' &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0to10, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==6) -> subdat

df7 %>%
  filter(CountyNo!='' &
           !is.na(age)) %>%
  mutate(tblack_0to10=tblacks_0to10,
         twhite_0to10=twhites_0to10) %>%
  mutate(raceomb = ethnic) %>% #race was called something else in this one
  select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0to10, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==6) -> subdat

df8 %>%
  filter(CountyNo!='' &
           !is.na(age)) %>%
  mutate(tblack_0to10=tblacks_0to10,
         twhite_0to10=twhites_0to10) %>%
  mutate(raceomb = ethnic) %>% #race was called something else in this one
  select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0to10, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==6) -> subdat

df9 %>%
  filter(CountyNo!='' &
           !is.na(age)) %>%
  mutate(raceomb = ethnic) %>% #race was mislabeled
  mutate(tblack_0to10=tblacks_0to10, #change to be consistent with other files
         twhite_0to10=twhites_0to10) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0to10, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==6) -> subdat

df10 %>%
  filter(CountyNo!='' &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0to10, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==6) -> subdat

subdat %>%
  mutate(explicit_bias=tblack_0to10) %>%
  mutate(explicit_bias_diff = twhite_0to10 - tblack_0to10) %>%
  mutate(age_bin = cut(age, breaks=c(14, 24, 34, 54, 75, 120))) %>%
  filter(!is.na(age_bin)) %>%
  filter(STATE %ni% 
           c('AA', 'AE', 'AP', 'AS', 'FM', 'GU', 'MH', 'MP', 'PR', 'VI')) %>% #exclude territories, etc
  mutate(county_id = paste(STATE, CountyNo, sep='-')) -> individual_data

### Combine all covariates into one dataframe ###
#################################################
df_acs <- df_acs[,c(3, 28, 32, 36, 40, 44, 48, 52, 56, 60, 64)] #age population breakdown
df_acs$Geography <- as.character(df_acs$Geography)
df_acs$Geography[1803] <- 'Doña Ana County, New Mexico'

#county age distributions
df_acs %>%
  gather(age, num, -Geography) %>%
  mutate(name_from_census=Geography) %>% #make colun name match haley's
  select(-Geography) %>%
  left_join(df_haley) %>%
  mutate(age = substr(age, 25, 26)) %>% #pick out the lower bound of the category defined by the ACS
  mutate(age_bin = cut(as.numeric(age), 
                       breaks=c(14, 24, 34, 54, 75, 120))) %>% #convert to numeric that matches the MRP scheme
  group_by(county_fips, age_bin) %>%
  summarise(num = sum(num, na.rm=T)) %>% #total number of people for each age group & county
  left_join(df_haley[,c('state_name', 'state_fips', 
                        'state_code', 'county_fips')]) -> df_acs_counts

#white and african american state level population
covs <- df_acs_eth[,c(3, 4, 6, 8)] 
names(covs) <- c('state_name', 'total_pop', 'white_pop', 'black_pop')
covs %>%
  mutate(white_prop = white_pop/total_pop,
         black_prop = black_pop/total_pop) %>%
  mutate(b.w.ratio = black_prop/white_prop) -> covs

#percentage w/ba or higher @ state level
covs_ed <- df_acs_ed[,c(3, 28)] 
names(covs_ed) <- c('state_name', 'col_grads')

#state level unemployment, income & poverty level
covs_income <- df_acs_pov_emp[,c(3, 21, 248, 478)]
names(covs_income) <- c('state_name', 'unemp', 'income', 'poverty')

#state level housing density
covs_hous <- df_acs_hous[,c(5, 14)]
names(covs_hous) <- c('state_fips', 'housing_density')

#state-level mobility
covs_mob <- df_acs_mob[,c(3, 10, 12)]
names(covs_mob) <- c('state_name', 'moved_states', 'moved_abroad')
covs_mob$mobility <- covs_mob$moved_states + covs_mob$moved_abroad

#state-level crime rate
df_fbi %>%
  gather(state_name, crime, -Year) %>%
  mutate(state_name = stringr::str_replace_all(state_name, '\\.+', ' ')) %>%
  left_join(covs[,c('state_name', 'total_pop')]) %>%
  mutate(crime_rate = crime/total_pop) %>%
  group_by(state_name) %>%
  summarise(crime_rate = mean(crime_rate, na.rm=T)) -> covs_crime #averaged across 5 years

#state-level segregation index
df_seg <- df_seg[,c(1:4, 6,8)]
names(df_seg) <- c('ID', 'ID2', 'Geo', 'Total', 'White', 'Black')
df_seg %>%
  mutate(FIPS = stringr::str_sub(ID, -11, -1)) %>%
  mutate(state_fips = stringr::str_sub(FIPS, 1, 2),
         county_fips = stringr::str_sub(FIPS, 3, 5),
         census_fips = stringr::str_sub(FIPS, 6, 12)) %>%
  select(-ID, -ID2) %>%
  group_by(state_fips) %>%
  mutate(state_total = sum(Total, na.rm=T),
         state_white = sum(White, na.rm=T),
         state_black = sum(Black, na.rm=T)) %>%
  mutate(black_prop = Black/state_black,
         white_prop = White/state_white) %>%
  mutate(bw_diff = abs(black_prop-white_prop)) %>% 
  group_by(state_fips) %>%
  mutate(dissim = sum(bw_diff)*.5) %>% 
  separate(Geo, c('tract', 'county', 'state_name'), sep=',') %>%
  ungroup() %>%
  select(state_name, dissim) %>%
  mutate(state_name = stringr::str_trim(state_name)) %>%
  distinct() %>%
  arrange(desc(dissim)) -> covs_seg
  

#put 'em all together
df_acs_counts %>%
  left_join(covs) %>%
  left_join(covs_ed) %>%
  left_join(covs_income) %>%
  left_join(covs_hous) %>%
  left_join(covs_mob) %>%
  left_join(covs_crime) %>%
  left_join(covs_seg) %>%
  mutate(county_id = paste(
    state_code, stringr::str_sub(county_fips, -3, -1), sep='-') )-> df_acs_counts

#df_acs_counts has state-level covariates & distribution of age within county
df_acs_counts %>%
  ungroup() %>%
  select(state_name:housing_density, mobility:dissim) %>%
  distinct() %>%
  mutate_at(vars(total_pop:dissim), scale) -> state_covs

#rename to join with state covariates
names(individual_data)[2] <- 'state_code'
individual_data %>%
  left_join(state_covs) -> individual_data_base

# fit MRP models
# tutorial here: http://www.princeton.edu/~jkastell/mrp_primer.html
#no state-level predictors - these have trouble with convergence
#individual.model.bias <- lmer(D_biep.White_Good_all ~ (1|age_bin) + (1|county_id) + 
#                                (1|state_code), data=individual_data_base)

#individual.model.explicit <- lmer(explicit_bias ~ (1|age_bin) + (1|county_id) + 
#                                    (1|state_code), data=individual_data_base)

# individual_data_diffmod <- individual_data_base[!is.na(individual_data_base$explicit_bias_diff),]
# individual.model.explicit_diff <- lmer(explicit_bias_diff ~ (1|age_bin) +
#                                         (1|county_id) + (1|state_code),
#                                       data=individual_data_base)

#state-level predictors are the same predictors as in the final model:
individual.model.bias <- lmer(D_biep.White_Good_all ~ total_pop + white_prop + 
                                black_prop + b.w.ratio + col_grads + unemp + 
                                income + poverty + housing_density + mobility + 
                                crime_rate + dissim + (1|age_bin) + 
                                (1|county_id) + (1|state_code), 
                              data=individual_data_base)

individual.model.explicit <- lmer(explicit_bias ~ total_pop + white_prop + 
                                    black_prop + b.w.ratio + col_grads + 
                                    unemp + income + poverty + housing_density +
                                    mobility + crime_rate + dissim + 
                                    (1|age_bin) + (1|county_id) + 
                                    (1|state_code), data=individual_data_base)

individual.model.explicit_diff <- lmer(explicit_bias_diff ~ total_pop + 
                                         white_prop + black_prop + b.w.ratio + 
                                         col_grads + unemp + income + poverty + 
                                         housing_density + mobility + 
                                         crime_rate + dissim + (1|age_bin) + 
                                         (1|county_id) + (1|state_code), 
                                       data=individual_data_base)
#required to get rid of convergence warnings - run the model for more iterations
ss <- getME(individual.model.explicit_diff, c('theta', 'fixef'))
individual.model.explicit_diff <- update(individual.model.explicit_diff, 
                                         start=ss, 
                                         control=lmerControl(optCtrl=list(maxfun=2e4)))

#create dataframe to make predictions over
df_acs_counts %>%
  select(county_fips:state_code) %>%
  ungroup() %>%
  left_join(state_covs) %>%
  mutate(county_id = paste(state_code, 
                           str_sub(county_fips, -3, -1), sep='-')) -> scaled_counts

#predict implicit
scaled_counts$yhat_bias <- predict(individual.model.bias, 
                                   newdata=scaled_counts, allow.new.levels=T)
#predict explicit
scaled_counts$yhat_explicit <- predict(individual.model.explicit, 
                                     newdat=scaled_counts, allow.new.levels=T)
#reverse score explicit bias
scaled_counts$yhat_explicit <- scaled_counts$yhat_explicit*-1

#predict explicit diff score
scaled_counts$yhat_explicit_diff <- predict(individual.model.explicit_diff, 
                                            newdat=scaled_counts, 
                                            allow.new.levels=T)

scaled_counts %>% 
  ungroup() %>% 
  group_by(county_id) %>% 
  summarise(weighted_bias = weighted.mean(yhat_bias, num),
            weighted_explicit = weighted.mean(yhat_explicit, num),
            weighted_explicit_diff = weighted.mean(yhat_explicit_diff, num)) -> mrp_ests

#compute naiive means as well
individual_data %>%
  group_by(county_id) %>%
  summarise(bias = mean(D_biep.White_Good_all, na.rm=T),
            explicit = mean(explicit_bias, na.rm=T),
            explicit_diff = mean(explicit_bias_diff, na.rm=T),
            n_bias_obs = sum(!is.na(D_biep.White_Good_all)),
            n_explicit_obs = sum(!is.na(explicit_bias))) %>%
  left_join(mrp_ests) -> county_means

write.csv(county_means, '/Users/travis/Documents/gits/educational_disparities/output/county_means.csv')

###### write teacher data

#gather all dataframes who have occupation info
df %>%
  filter(CountyNo!='') %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         twhite_0to10, tblack_0to10, raceomb, age, occupation)  -> subdat

df2 %>%
  filter(CountyNo!='') %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         twhite_0to10, tblack_0to10, raceomb, age, occupation) %>%
  rbind(subdat) -> subdat

df3 %>%
  filter(CountyNo!='') %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         twhite_0to10, tblack_0to10, raceomb, age, occupation) %>%
  rbind(subdat) -> subdat

df4 %>%
  filter(CountyNo!='') %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         twhite_0to10, tblack_0to10, raceomb, age, occupation) %>%
  rbind(subdat) -> subdat

df5 %>%
  filter(CountyNo!='') %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         twhite_0to10, tblack_0to10, raceomb, age, occupation) %>%
  rbind(subdat) -> subdat

df6 %>%
  filter(CountyNo!='') %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         twhite_0to10, tblack_0to10, raceomb, age, occupation) %>%
  rbind(subdat) -> subdat

df10 %>%
  filter(CountyNo!='') %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         twhite_0to10, tblack_0to10, raceomb, age, occupation) %>%
  rbind(subdat) -> subdat

#compute difference score, reverse explicit score, filter out territories & non-educators
subdat %>%
  mutate(explicit_diff=twhite_0to10 - tblack_0to10,
         explicit = tblack_0to10*-1) %>%
  filter(STATE %ni% 
           c('AA', 'AE', 'AP', 'AS', 'FM', 'GU', 'MH', 'MP', 'PR', 'VI')) %>%
  filter(occupation %in% educators) %>%
  mutate(county_id = paste(STATE, CountyNo, sep='-')) -> individual_data

names(individual_data)[2] <- 'state_abb'

individual_data %>%
  filter(raceomb==6) %>% #whites only
  group_by(county_id) %>%
  mutate(teacher_bias = mean(D_biep.White_Good_all, na.rm=T),
         teacher_explicit = mean(explicit, na.rm=T),
         teacher_explicit_diff = mean(explicit_diff, na.rm=T),
         num_obs = n()) %>%
  filter(num_obs>49) %>%
  select(county_id, state_abb, teacher_bias, 
         teacher_explicit, teacher_explicit_diff, num_obs) %>%
  distinct() -> county_teacher_estimates

write.csv(county_teacher_estimates, row.names = F,
            file = '/Users/travis/Documents/gits/educational_disparities/output/county_teacher_means.csv')
