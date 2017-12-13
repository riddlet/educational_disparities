library(haven)
library(dplyr)
library(ggplot2)
library(forcats)
library(rstanarm)
library(tidyr)
library(rgdal)

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

"%ni%" <- Negate("%in%")

df %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack) &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, tblack, raceomb, age)  -> subdat

df2 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack) &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, tblack, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df3 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack) &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, tblack, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df4 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack) &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, tblack, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df5 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack) &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, tblack, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df6 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack) &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, tblack, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df7 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblacks) &
           !is.na(age)) %>%
  mutate(tblack=tblacks) %>%
  mutate(raceomb = ethnic) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, tblack, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df8 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblacks) &
           !is.na(age)) %>%
  mutate(tblack=tblacks) %>%
  mutate(raceomb = ethnic) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, tblack, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df9 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblacks) &
           !is.na(age)) %>%
  mutate(raceomb = ethnic) %>%
  mutate(tblack=tblacks) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, tblack, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df10 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack) &
           !is.na(age)) %>%
  select(CountyNo, STATE, D_biep.White_Good_all, tblack, raceomb, age) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

subdat %>%
  mutate(age_bin = cut(age, breaks=c(15, 24, 34, 54, 75, 120))) %>%
  mutate(race = fct_recode(as.character(raceomb), 
                           'Black'='5', 'White'='6')) %>%
  filter(!is.na(age_bin)) -> individual_data

#individual.model <- lmer(D_biep.White_Good_all ~ (1|race) + (1|age_bin) + 
#                           (1|STATE), data=individual_data)

individual_data %>%
  group_by(raceomb, CountyNo, age_bin) %>%
  summarise(m_d_biep = mean(D_biep.White_Good_all, na.rm=T),
            m_tblack = mean(tblack, na.rm=T)) %>%
  ungroup() %>%
  select(-raceomb) -> categorized_bias

subdat %>%
  group_by(CountyNo, STATE, raceomb) %>%
  summarise(bias = mean(D_biep.White_Good_all),
            bias_se = sd(D_biep.White_Good_all)/sqrt(n()),
            warmth = mean(tblack),
            warmth_se = sd(tblack)/sqrt(n())) %>%
  mutate(bias_low = bias-1.96*bias_se,
         bias_high = bias+1.96*bias_se,
         warmth_low = warmth-1.96*warmth_se,
         warmth_high = warmth+1.96*warmth_se) %>%
  mutate(raceomb = fct_recode(as.character(raceomb), 
                              'African Americans'='5', 'White'='6')) -> county_means

write.csv(county_means, '/Users/travis/Documents/gits/educational_disparities/output/county_means.csv')
