library(haven)
library(dplyr)
library(ggplot2)
library(forcats)
library(rstanarm)
library(tidyr)
library(stringr)
library(lme4)

#load datasets
df <- rbind(read_sav('/Users/travis/Documents/gits/Data/iat/Race IAT.public.2013.sav'),
            read_sav('/Users/travis/Documents/gits/Data/iat/Race IAT.public.2012.sav'),
            read_sav('/Users/travis/Documents/gits/Data/iat/Race IAT.public.2010.sav'))

df2 <- read_sav('/Users/travis/Documents/gits/Data/iat/Race IAT.public.2011.sav')
df3 <- read_sav('/Users/travis/Documents/gits/Data/iat/Race IAT.public.2009.sav')
df4 <- read_sav('/Users/travis/Documents/gits/Data/iat/Race IAT.public.2008.sav')
df5 <- read_sav('/Users/travis/Documents/gits/Data/iat/Race IAT.public.2007.sav')
df6 <- read_sav('/Users/travis/Documents/gits/Data/iat/Race IAT.public.2006.sav')
df7 <- read_sav('/Users/travis/Documents/gits/Data/iat/Race IAT.public.2005.sav')
df8 <- read_sav('/Users/travis/Documents/gits/Data/iat/Race IAT.public.2004.sav')
df9 <- read_sav('/Users/travis/Documents/gits/Data/iat/Race IAT.public.2002-2003.sav')
df10 <- read_sav('/Users/travis/Documents/gits/Data/iat/Race IAT.public.2014.sav')
df_acs <- read.csv('/Users/travis/Documents/gits/Data/ACS/county_age/ACS_14_5YR_DP05_with_ann.csv', skip=1)
df_county_linking_info <- read.csv('cluster/output/county_linking_table.csv', stringsAsFactors = F)
df_county_linking_info$county_name[1904] <- 'Doña Ana'
df_states <- data.frame(state=c(state.name, 'District of Columbia'),
                        state_abb=c(state.abb, 'DC'))
df_acs_eth <- read.csv('/Users/travis/Documents/gits/Data/ACS/state_ethnicity/ACS_14_5YR_B02001_with_ann.csv',
                       skip = 1, stringsAsFactors = F)
df_acs_ed <- read.csv('../Data/ACS/state_education/ACS_14_5YR_S1501_with_ann.csv',
                      skip = 1, stringsAsFactors = F)
df_acs_pov_emp <- read.csv('../Data/ACS/state_poverty_emp/ACS_14_5YR_DP03_with_ann.csv',
                           skip = 1, stringsAsFactors = F)

#educators <- c('25-2000', '25-3000', '25-4000', '25-9000')
# per stacey's contact in education, limit to the following:
educators <- c('25-2000', '25-3000')

# '25-1000' - postsecondary teachers

"%ni%" <- Negate("%in%")

df %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack_0to10) &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0to10, raceomb, age)  -> subdat

df2 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack_0to10) &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all,  
         tblack_0to10, twhites_0to10, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df3 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack_0to10) &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0to10, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df4 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack_0to10) &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0to10, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df5 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack_0to10) &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0to10, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df6 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack_0to10) &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0to10, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df7 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblacks_0to10) &
           !is.na(age)) %>%
  mutate(tblack_0to10=tblacks_0to10,
         twhite_0to10=twhites_0to10) %>%
  mutate(raceomb = ethnic) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0_to_10, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df8 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblacks_0to10) &
           !is.na(age)) %>%
  mutate(tblack_0to10=tblacks_0to10,
         twhite_0to10=twhites_0to10) %>%
  mutate(raceomb = ethnic) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0to10, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df9 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblacks_0to10) &
           !is.na(age)) %>%
  mutate(raceomb = ethnic) %>%
  mutate(tblack_0to10=tblacks_0to10,
         twhite_0to10=twhites_0to10) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0to10, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df10 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack_0to10) &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0to10, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

subdat %>%
  mutate(tblack=tblack_0to10) %>%
  mutate(explicit_bias = twhite_0to10 - tblack_0to10) %>%
  mutate(age_bin = cut(age, breaks=c(14, 24, 34, 54, 75, 120))) %>%
  mutate(race = fct_recode(as.character(raceomb), 
                           'Black'='5', 'White'='6')) %>%
  filter(!is.na(age_bin)) %>%
  filter(race=='White') %>%
  filter(STATE %ni% 
           c('AA', 'AE', 'AP', 'AS', 'FM', 'GU', 'MH', 'MP', 'PR', 'VI')) %>%
  mutate(county_id = paste(STATE, CountyNo, sep='-')) -> individual_data

df_acs <- df_acs[,c(3, 28, 32, 36, 40, 44, 48, 52, 56, 60, 64)]
df_acs$Geography <- as.character(df_acs$Geography)
df_acs$Geography[1803] <- 'Doña Ana County, New Mexico'

df_acs %>%
  gather(age, num, -Geography) %>%
  mutate(county=Geography) %>%
  select(-Geography) %>%
  separate(county, into=c('county_name', 'state'), sep=', ') %>%
  filter(state!='Puerto Rico') %>% #no PR in the education data
  mutate(county_name = 
           str_replace(county_name, 
                       ' County| Borough| Census Area| Parish| Municipality| City and Borough', '')) %>%
  mutate(age = substr(age, 25, 26)) %>%
  mutate(age_bin = cut(as.numeric(age), 
                       breaks=c(14, 24, 34, 54, 75, 120))) %>%
  group_by(county_name, age_bin) %>%
  summarise(num = sum(num)) %>%
  left_join(df_county_linking_info) -> df_acs_counts

covs <- df_acs_eth[,c(3, 4, 6, 8)]
names(covs) <- c('state', 'total_pop', 'white_pop', 'black_pop')
covs %>%
  mutate(white_prop = white_pop/total_pop,
         black_prop = black_pop/total_pop) %>%
  mutate(b.w.ratio = black_prop/white_prop) -> covs

covs_ed <- df_acs_ed[,c(3, 28)]
names(covs_ed) <- c('state', 'col_grads')

covs_income <- df_acs_pov_emp[,c(3, 21, 248, 478)]
names(covs_income) <- c('state', 'unemp', 'income', 'poverty')

df_acs_counts %>%
  left_join(covs) %>%
  left_join(covs_ed) %>%
  left_join(covs_income) -> df_acs_counts


#df_acs_counts[which(is.na(df_acs_counts$county_id)),] -> missings
#counties w/o IAT data: 
#Aleutians East, Wrangell, Berkshire, Billings, Rolette,
#Borden, Culberson, Foard, Glassock, Greensville, Calhoun, Campbell, Carter, 
#Buffalo, Blaine, Billings, Berkshire, Banner, Douglas, Franklin, Garfield, 
#Golden Valley, Grant, Greeley, Greensville, Hampshire, Harding, Hinsdale, 
#Hodgeman, Hoonah-Angoon, Irion, Jackson, Kenedy, King, Loup, Petroleum, Piute, 
#Potter, Powder River, Prairie, Prince of Wales-Hyder, Quitman, Roberts, Rock, 
#Rolette, Sheridan (ks), Sheridan (ND), Skagway, Thomas, Treasure

#counties w/o schools:
#kalawao, Issaquena, Mora, Divide, Loving, Marion, Miller,
names(individual_data)[2] <- 'state_abb'
individual_data %>%
  left_join(df_states) %>%
  left_join(df_acs_counts[,1:4], by=c('county_id', 'age_bin')) %>%
  left_join(covs, by='state') %>%
  left_join(covs_ed, by='state') %>%
  left_join(covs_income, by='state') %>%
  mutate_at(vars(white_prop:poverty), scale) -> individual_data

individual.model.bias <- lmer(D_biep.White_Good_all ~ white_prop + black_prop + 
                                b.w.ratio + col_grads + unemp + income + poverty +
                                (1|age_bin) + (1|county_id) + (1|state_abb), 
                         data=individual_data)
individual.model.explicit <- lmer(explicit_bias ~ white_prop + black_prop + 
                                  b.w.ratio + col_grads + unemp + income + poverty +
                                  (1|age_bin) + (1|county_id) + (1|state_abb), 
                                data=individual_data)

df_acs_countstemp <- df_acs_counts[which(!is.na(df_acs_counts$county_id)),]
#df_acs_counts <- left_join(df_acs_counts, df_states)
df_acs_countstemp %>%
  ungroup() %>%
  mutate_at(vars(white_prop:poverty), scale) -> scaled_counts

scaled_counts$yhat_bias <- predict(individual.model.bias, 
                                   newdata=scaled_counts, allow.new.levels=T)
scaled_counts$yhat_explicit <- predict(individual.model.explicit, 
                                     newdat=scaled_counts, allow.new.levels=T)

scaled_counts %>% 
  ungroup() %>% 
  group_by(county_id) %>% 
  summarise(weighted_bias = weighted.mean(yhat_bias, num),
            weighted_warmth = weighted.mean(yhat_explicit, num)) -> mrp_ests

individual_data %>%
  group_by(county_id) %>%
  summarise(bias = mean(explicit_bias),
            warmth = mean(tblack)) %>%
    left_join(mrp_ests) -> county_means

write.csv(county_means, '/Users/travis/Documents/gits/educational_disparities/output/county_means_explicit_diff.csv')

###### write teacher data
df %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack_0to10) &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         twhite_0to10, tblack_0to10, raceomb, age, occupation)  -> subdat

df2 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack_0to10) &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         twhite_0to10, tblack_0to10, raceomb, age, occupation) %>%
  rbind(subdat) -> subdat

df3 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack_0to10) &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         twhite_0to10, tblack_0to10, raceomb, age, occupation) %>%
  rbind(subdat) -> subdat

df4 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack_0to10) &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         twhite_0to10, tblack_0to10, raceomb, age, occupation) %>%
  rbind(subdat) -> subdat

df5 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack_0to10) &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         twhite_0to10, tblack_0to10, raceomb, age, occupation) %>%
  rbind(subdat) -> subdat

df6 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack_0to10) &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         twhite_0to10, tblack_0to10, raceomb, age, occupation) %>%
  rbind(subdat) -> subdat

df10 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack_0to10) &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, 
         twhite_0to10, tblack_0to10, raceomb, age, occupation) %>%
  rbind(subdat) -> subdat

subdat %>%
  mutate(tblack=twhite_0to10 - tblack_0to10) %>%
  filter(STATE %ni% 
           c('AA', 'AE', 'AP', 'AS', 'FM', 'GU', 'MH', 'MP', 'PR', 'VI')) %>%
  filter(occupation %in% educators) %>%
  mutate(county_id = paste(STATE, CountyNo, sep='-')) -> individual_data

names(individual_data)[2] <- 'state_abb'

individual_data %>%
  filter(raceomb==6) %>%
  group_by(county_id) %>%
  mutate(county_bias = mean(D_biep.White_Good_all),
         county_warmth = mean(tblack, na.rm=T),
         num_obs = n()) %>%
  filter(num_obs>49) %>%
  select(county_id, state_abb, county_bias, county_warmth, num_obs) %>%
  distinct() -> county_teacher_estimates

write.csv(county_teacher_estimates, row.names = F,
            file = '/Users/travis/Documents/gits/educational_disparities/output/county_teacher_means_expdiff.csv')
