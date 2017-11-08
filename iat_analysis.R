library(haven)
library(dplyr)
library(ggplot2)
library(forcats)
library(rstanarm)
library(tidyr)

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

25-2000, 25-3000, 25-4000, 25-9000
"%ni%" <- Negate("%in%")

df %>%
  filter(STATE!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack) &
           !is.na(age)) %>%
  select(STATE, D_biep.White_Good_all, tblack, raceomb, age) %>%
  filter(STATE %ni% c('AA', 'AE', 'AP', 'AS', 'FM', 'GU', 'MP', 'PR', 'VI', 'MH')) -> subdat

df2 %>%
  filter(STATE!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack) &
           !is.na(age)) %>%
  select(STATE, D_biep.White_Good_all, tblack, raceomb, age) %>%
  filter(STATE %ni% c('AA', 'AE', 'AP', 'AS', 'FM', 'GU', 'MP', 'PR', 'VI', 'MH')) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df3 %>%
  filter(STATE!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack) &
           !is.na(age)) %>%
  select(STATE, D_biep.White_Good_all, tblack, raceomb, age) %>%
  filter(STATE %ni% c('AA', 'AE', 'AP', 'AS', 'FM', 'GU', 'MP', 'PR', 'VI', 'MH')) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df4 %>%
  filter(STATE!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack) &
           !is.na(age)) %>%
  select(STATE, D_biep.White_Good_all, tblack, raceomb, age) %>%
  filter(STATE %ni% c('AA', 'AE', 'AP', 'AS', 'FM', 'GU', 'MP', 'PR', 'VI', 'MH')) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df5 %>%
  filter(STATE!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack) &
           !is.na(age)) %>%
  select(STATE, D_biep.White_Good_all, tblack, raceomb, age) %>%
  filter(STATE %ni% c('AA', 'AE', 'AP', 'AS', 'FM', 'GU', 'MP', 'PR', 'VI', 'MH')) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df6 %>%
  filter(STATE!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack) &
           !is.na(age)) %>%
  select(STATE, D_biep.White_Good_all, tblack, raceomb, age) %>%
  filter(STATE %ni% c('AA', 'AE', 'AP', 'AS', 'FM', 'GU', 'MP', 'PR', 'VI', 'MH')) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df7 %>%
  filter(STATE!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblacks) &
           !is.na(age)) %>%
  mutate(tblack=tblacks) %>%
  mutate(raceomb = ethnic) %>%
  select(STATE, D_biep.White_Good_all, tblack, raceomb, age) %>%
  filter(STATE %ni% c('AA', 'AE', 'AP', 'AS', 'FM', 'GU', 'MP', 'PR', 'VI', 
                      'MH', 'PW')) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df8 %>%
  filter(STATE!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblacks) &
           !is.na(age)) %>%
  mutate(tblack=tblacks) %>%
  mutate(raceomb = ethnic) %>%
  select(STATE, D_biep.White_Good_all, tblack, raceomb, age) %>%
  filter(STATE %ni% c('AA', 'AE', 'AP', 'AS', 'FM', 'GU', 'MP', 'PR', 'VI', 
                      'MH', 'PW')) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df9 %>%
  filter(STATE!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblacks) &
           !is.na(age)) %>%
  mutate(raceomb = ethnic) %>%
  mutate(tblack=tblacks) %>%
  select(STATE, D_biep.White_Good_all, tblack, raceomb, age) %>%
  filter(STATE %ni% c('AA', 'AE', 'AP', 'AS', 'FM', 'GU', 'MP', 'PR', 'VI', 
                      'MH', 'PW')) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df10 %>%
  filter(STATE!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack) &
           !is.na(age)) %>%
  select(STATE, D_biep.White_Good_all, tblack, raceomb, age) %>%
  filter(STATE %ni% c('AA', 'AE', 'AP', 'AS', 'FM', 'GU', 'MP', 'PR', 'VI', 
                      'MH', 'PW')) %>%
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
  group_by(raceomb, STATE, age_bin) %>%
  summarise(m_d_biep = mean(D_biep.White_Good_all, na.rm=T),
            m_tblack = mean(tblack, na.rm=T)) %>%
  ungroup() %>%
  select(-raceomb) -> categorized_bias

subdat %>%
  group_by(STATE, raceomb) %>%
  summarise(bias = mean(D_biep.White_Good_all),
            bias_se = sd(D_biep.White_Good_all)/sqrt(n()),
            warmth = mean(tblack),
            warmth_se = sd(tblack)/sqrt(n())) %>%
  mutate(bias_low = bias-1.96*bias_se,
         bias_high = bias+1.96*bias_se,
         warmth_low = warmth-1.96*warmth_se,
         warmth_high = warmth+1.96*warmth_se) %>%
  mutate(raceomb = fct_recode(as.character(raceomb), 
                              'African Americans'='5', 'White'='6')) -> state_means

write.csv(state_means, 'Documents/gits/educational_disparities/output/state_means.csv')

df %>%
  filter(STATE!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack) &
           !is.na(age),
         occupation %in% c('25-2000', '25-3000', '25-4000', '25-9000')) %>%
  select(STATE, D_biep.White_Good_all, tblack, raceomb, age, occupation) %>%
  filter(STATE %ni% c('AA', 'AE', 'AP', 'AS', 'FM', 'GU', 'MP', 'PR', 'VI', 'MH')) -> subdat

df2 %>%
  filter(STATE!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack) &
           !is.na(age),
         occupation %in% c('25-2000', '25-3000', '25-4000', '25-9000')) %>%
  select(STATE, D_biep.White_Good_all, tblack, raceomb, age, occupation) %>%
  filter(STATE %ni% c('AA', 'AE', 'AP', 'AS', 'FM', 'GU', 'MP', 'PR', 'VI', 'MH')) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df3 %>%
  filter(STATE!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack) &
           !is.na(age),
         occupation %in% c('25-2000', '25-3000', '25-4000', '25-9000')) %>%
  select(STATE, D_biep.White_Good_all, tblack, raceomb, age, occupation) %>%
  filter(STATE %ni% c('AA', 'AE', 'AP', 'AS', 'FM', 'GU', 'MP', 'PR', 'VI', 'MH')) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df4 %>%
  filter(STATE!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack) &
           !is.na(age),
         occupation %in% c('25-2000', '25-3000', '25-4000', '25-9000')) %>%
  select(STATE, D_biep.White_Good_all, tblack, raceomb, age, occupation) %>%
  filter(STATE %ni% c('AA', 'AE', 'AP', 'AS', 'FM', 'GU', 'MP', 'PR', 'VI', 'MH')) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df5 %>%
  filter(STATE!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack) &
           !is.na(age),
         occupation %in% c('25-2000', '25-3000', '25-4000', '25-9000')) %>%
  select(STATE, D_biep.White_Good_all, tblack, raceomb, age, occupation) %>%
  filter(STATE %ni% c('AA', 'AE', 'AP', 'AS', 'FM', 'GU', 'MP', 'PR', 'VI', 'MH')) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df6 %>%
  filter(STATE!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack) &
           !is.na(age),
         occupation %in% c('25-2000', '25-3000', '25-4000', '25-9000')) %>%
  select(STATE, D_biep.White_Good_all, tblack, raceomb, age, occupation) %>%
  filter(STATE %ni% c('AA', 'AE', 'AP', 'AS', 'FM', 'GU', 'MP', 'PR', 'VI', 'MH')) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

df10 %>%
  filter(STATE!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack) &
           !is.na(age),
         occupation %in% c('25-2000', '25-3000', '25-4000', '25-9000')) %>%
  select(STATE, D_biep.White_Good_all, tblack, raceomb, age, occupation) %>%
  filter(STATE %ni% c('AA', 'AE', 'AP', 'AS', 'FM', 'GU', 'MP', 'PR', 'VI', 
                      'MH', 'PW')) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

subdat %>%
  group_by(STATE, raceomb, occupation) %>%
  summarise(bias = mean(D_biep.White_Good_all),
            bias_se = sd(D_biep.White_Good_all)/sqrt(n()),
            warmth = mean(tblack),
            warmth_se = sd(tblack)/sqrt(n())) %>%
  mutate(bias_low = bias-1.96*bias_se,
         bias_high = bias+1.96*bias_se,
         warmth_low = warmth-1.96*warmth_se,
         warmth_high = warmth+1.96*warmth_se) %>%
  ungroup() %>%
  mutate(raceomb = fct_recode(as.character(raceomb), 
                              'African Americans'='5', 'White'='6'),
         occupation = fct_recode(as.character(occupation),
                                 'Primary, Secondary, & SpEd teachers'='25-2000',
                                 'Other teachers & instructors'='25-3000',
                                 'Librarians, Curators, & Archivists'='25-4000',
                                 'Other education, training, and library occupations'='25-9000')) -> state_teacher_means

write.csv(file='../output/state_teacher_means.csv', state_teacher_means, row.names = F)

subdat %>%
  mutate(age_bin = cut(age, breaks=c(15, 24, 34, 54, 75, 120))) %>%
  mutate(race = fct_recode(as.character(raceomb), 
                           'Black'='5', 'White'='6')) %>%
  filter(!is.na(age_bin)) -> individual_data
    
acs_white_09 <- read.csv('/Users/travis/Documents/gits/Data/ACS/ACS_09_5YR_B01001A/ACS_09_5YR_B01001A_with_ann.csv', skip = 1)  
acs_white_09$data <- 'White_09'
acs_black_09 <- read.csv('/Users/travis/Documents/gits/Data/ACS/ACS_09_5YR_B01001B/ACS_09_5YR_B01001B_with_ann.csv', skip = 1)  
acs_black_09$data <- 'Black_09'
acs_white_14 <- read.csv('/Users/travis/Documents/gits/Data/ACS/ACS_14_5YR_B01001A/ACS_14_5YR_B01001A_with_ann.csv', skip = 1)  
acs_white_14$data <- 'White_14'
acs_black_14 <- read.csv('/Users/travis/Documents/gits/Data/ACS/ACS_14_5YR_B01001B/ACS_14_5YR_B01001B_with_ann.csv', skip = 1)  
acs_black_14$data <- 'Black_14'

acs <- rbind(acs_black_09, acs_black_14, acs_white_09, acs_white_14)

acs %>%
  select(Id2, Geography, starts_with('Estimate'), data) %>%
  gather(group, value, Estimate..Male....Under.5.years:Estimate..Male....85.years.and.over, 
         Estimate..Female....Under.5.years:Estimate..Female....85.years.and.over) %>%
  mutate(race = str_extract(data, 'Black|White')) %>%
  select(-data) %>%
  separate(group, c('gender', 'age'), '\\.\\.\\.\\.') %>%
  mutate(gender = str_extract(gender, 'Male|Female'),
         age = str_sub(age, 1, 4)) %>%
  mutate(age_bin = fct_collapse(age, 
                                fifteen = c('15.t', '18.a', '20.t'),
                                none = c('10.t', 'Unde', '5.to'),
                                twentyfive = c('25.t', '30.t'),
                                thiryfive = c('35.t', '45.t'),
                                fiftyfive = c('55.t', '65.t'),
                                seventyfive = c('75.t', '85.y'))) %>%
  filter(age_bin != 'none') %>%
  group_by(Geography, race, gender, age) %>%
  mutate(total = sum(Estimate..Total.)) %>%
  ungroup() %>%
  group_by(Geography, race, age_bin) %>%
  mutate(count = sum(value)) %>%
  select(Geography, race, age_bin, total, count) %>%
  ungroup() %>%
  distinct() %>%
  mutate(weight = count/total) -> acs_weights

acs_weights$STATE <- state.abb[match(acs_weights$Geography, state.name)]

subdat %>%
  group_by(STATE, raceomb) %>%
  summarise(bias = mean(D_biep.White_Good_all),
            bias_se = sd(D_biep.White_Good_all)/sqrt(n()),
            warmth = mean(tblack),
            warmth_se = sd(tblack)/sqrt(n())) %>%
  mutate(bias_low = bias-1.96*bias_se,
         bias_high = bias+1.96*bias_se,
         warmth_low = warmth-1.96*warmth_se,
         warmth_high = warmth+1.96*warmth_se) %>%
  mutate(raceomb = fct_recode(as.character(raceomb), 
                              'African Americans'='5', 'White'='6')) %>%
  ggplot(aes(x=bias, y=warmth)) + 
  geom_point(alpha=.5) + 
  geom_errorbar(aes(ymin=warmth_low, ymax=warmth_high), alpha=.25) +
  geom_errorbarh(aes(xmin=bias_low, xmax=bias_high), alpha=.25) +
  theme_classic() +
  facet_wrap(~raceomb) +
  ylab('Warmth towards African Americans') +
  xlab('IAT bias against African Americans') -> p

ggsave('figs/iat_fixed.jpeg', p)

subdat %>%
  group_by(STATE, raceomb) %>%
  summarise(bias = mean(D_biep.White_Good_all),
            bias_se = sd(D_biep.White_Good_all)/sqrt(n()),
            warmth = mean(tblack),
            warmth_se = sd(tblack)/sqrt(n())) %>%
  mutate(bias_low = bias-1.96*bias_se,
         bias_high = bias+1.96*bias_se,
         warmth_low = warmth-1.96*warmth_se,
         warmth_high = warmth+1.96*warmth_se) %>%
  mutate(raceomb = fct_recode(as.character(raceomb), 
                              'African Americans'='5', 'White'='6')) %>%
  ggplot(aes(x=bias, y=warmth)) + 
  geom_point(alpha=.5) + 
  geom_errorbar(aes(ymin=warmth_low, ymax=warmth_high), alpha=.25) +
  geom_errorbarh(aes(xmin=bias_low, xmax=bias_high), alpha=.25) +
  theme_classic() +
  facet_wrap(~raceomb, scales = 'free') +
  ylab('Warmth towards African Americans') +
  xlab('IAT bias against African Americans') -> p

ggsave('figs/iat_free.jpeg', p)

### compare with discipline gaps
gaps <- read.csv('gaps.csv')

## iat
subdat %>%
  group_by(STATE, raceomb) %>%
  summarise(bias = mean(D_biep.White_Good_all),
            bias_se = sd(D_biep.White_Good_all)/sqrt(n()),
            warmth = mean(tblack),
            warmth_se = sd(tblack)/sqrt(n())) %>%
  mutate(bias_low = bias-1.96*bias_se,
         bias_high = bias+1.96*bias_se,
         warmth_low = warmth-1.96*warmth_se,
         warmth_high = warmth+1.96*warmth_se) %>%
  mutate(raceomb = fct_recode(as.character(raceomb), 
                              'African Americans'='5', 'White'='6')) %>%
  mutate(LEA_STATE = STATE) %>%
  right_join(gaps) %>%
  filter(variable=='corp_pun') %>%
  ungroup() %>%
  group_by(raceomb) %>%
  mutate(correl = cor(bias, est)) %>%
  ggplot(aes(x=bias, y=est)) + 
  geom_point(alpha=.5) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), alpha=.25) +
  geom_errorbarh(aes(xmin=bias_low, xmax=bias_high), alpha=.25) +
  theme_classic() +
  facet_wrap(~raceomb, scales = 'free') +
  ylab('Corporal Punishment gap') +
  xlab('IAT bias against African Americans') +
  geom_smooth(method='lm') +
  geom_text(x=-Inf, y=Inf, aes(label=paste('R = ', round(correl, 2))),
            hjust=-.2, vjust=1.2, alpha=.05) -> p

ggsave('figs/iat_corp_pun.jpeg', p)

subdat %>%
  group_by(STATE, raceomb) %>%
  summarise(bias = mean(D_biep.White_Good_all),
            bias_se = sd(D_biep.White_Good_all)/sqrt(n()),
            warmth = mean(tblack),
            warmth_se = sd(tblack)/sqrt(n())) %>%
  mutate(bias_low = bias-1.96*bias_se,
         bias_high = bias+1.96*bias_se,
         warmth_low = warmth-1.96*warmth_se,
         warmth_high = warmth+1.96*warmth_se) %>%
  mutate(raceomb = fct_recode(as.character(raceomb), 
                              'African Americans'='5', 'White'='6')) %>%
  mutate(LEA_STATE = STATE) %>%
  right_join(gaps) %>%
  filter(variable=='iss_susp') %>%
  ungroup() %>%
  group_by(raceomb) %>%
  mutate(correl = cor(bias, est)) %>%
  ggplot(aes(x=bias, y=est)) + 
  geom_point(alpha=.5) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), alpha=.25) +
  geom_errorbarh(aes(xmin=bias_low, xmax=bias_high), alpha=.25) +
  theme_classic() +
  facet_wrap(~raceomb, scales = 'free') +
  ylab('In-school suspension gap gap') +
  xlab('IAT bias against African Americans') +
  geom_smooth(method='lm') +
  geom_text(x=-Inf, y=Inf, aes(label=paste('R = ', round(correl, 2))),
            hjust=-.2, vjust=1.2, alpha=.03) -> p

ggsave('figs/iat_iss_susp.jpeg', p)

subdat %>%
  group_by(STATE, raceomb) %>%
  summarise(bias = mean(D_biep.White_Good_all),
            bias_se = sd(D_biep.White_Good_all)/sqrt(n()),
            warmth = mean(tblack),
            warmth_se = sd(tblack)/sqrt(n())) %>%
  mutate(bias_low = bias-1.96*bias_se,
         bias_high = bias+1.96*bias_se,
         warmth_low = warmth-1.96*warmth_se,
         warmth_high = warmth+1.96*warmth_se) %>%
  mutate(raceomb = fct_recode(as.character(raceomb), 
                              'African Americans'='5', 'White'='6')) %>%
  mutate(LEA_STATE = STATE) %>%
  right_join(gaps) %>%
  filter(variable=='oos_susp') %>%
  ungroup() %>%
  group_by(raceomb) %>%
  mutate(correl = cor(bias, est)) %>%
  ggplot(aes(x=bias, y=est)) + 
  geom_point(alpha=.5) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), alpha=.25) +
  geom_errorbarh(aes(xmin=bias_low, xmax=bias_high), alpha=.25) +
  theme_classic() +
  facet_wrap(~raceomb, scales = 'free') +
  ylab('Out-of-school suspension gap gap') +
  xlab('IAT bias against African Americans') +
  geom_smooth(method='lm') +
  geom_text(x=-Inf, y=Inf, aes(label=paste('R = ', round(correl, 2))),
            hjust=-.2, vjust=1.2, alpha=.03) -> p

ggsave('figs/iat_oos_susp.jpeg', p)

subdat %>%
  group_by(STATE, raceomb) %>%
  summarise(bias = mean(D_biep.White_Good_all),
            bias_se = sd(D_biep.White_Good_all)/sqrt(n()),
            warmth = mean(tblack),
            warmth_se = sd(tblack)/sqrt(n())) %>%
  mutate(bias_low = bias-1.96*bias_se,
         bias_high = bias+1.96*bias_se,
         warmth_low = warmth-1.96*warmth_se,
         warmth_high = warmth+1.96*warmth_se) %>%
  mutate(raceomb = fct_recode(as.character(raceomb), 
                              'African Americans'='5', 'White'='6')) %>%
  mutate(LEA_STATE = STATE) %>%
  right_join(gaps) %>%
  filter(variable=='ps_susp') %>%
  ungroup() %>%
  group_by(raceomb) %>%
  mutate(correl = cor(bias, est)) %>%
  ggplot(aes(x=bias, y=est)) + 
  geom_point(alpha=.5) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), alpha=.25) +
  geom_errorbarh(aes(xmin=bias_low, xmax=bias_high), alpha=.25) +
  theme_classic() +
  facet_wrap(~raceomb, scales = 'free') +
  ylab('Preschool suspension gap gap') +
  xlab('IAT bias against African Americans') +
  geom_smooth(method='lm') +
  geom_text(x=-Inf, y=Inf, aes(label=paste('R = ', round(correl, 2))),
            hjust=-.2, vjust=1.2, alpha=.03) -> p

ggsave('figs/iat_ps_susp.jpeg', p)

## explicit
subdat %>%
  group_by(STATE, raceomb) %>%
  summarise(bias = mean(D_biep.White_Good_all),
            bias_se = sd(D_biep.White_Good_all)/sqrt(n()),
            warmth = mean(tblack),
            warmth_se = sd(tblack)/sqrt(n())) %>%
  mutate(bias_low = bias-1.96*bias_se,
         bias_high = bias+1.96*bias_se,
         warmth_low = warmth-1.96*warmth_se,
         warmth_high = warmth+1.96*warmth_se) %>%
  mutate(raceomb = fct_recode(as.character(raceomb), 
                              'African Americans'='5', 'White'='6')) %>%
  mutate(LEA_STATE = STATE) %>%
  right_join(gaps) %>%
  filter(variable=='corp_pun') %>%
  ungroup() %>%
  group_by(raceomb) %>%
  mutate(correl = cor(warmth, est)) %>%
  ggplot(aes(x=warmth, y=est)) + 
  geom_point(alpha=.5) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), alpha=.25) +
  geom_errorbarh(aes(xmin=warmth_low, xmax=warmth_high), alpha=.25) +
  theme_classic() +
  facet_wrap(~raceomb, scales = 'free') +
  ylab('Corporal Punishment gap') +
  xlab('Explicit warmth towards African Americans') +
  geom_smooth(method='lm') +
  geom_text(x=-Inf, y=Inf, aes(label=paste('R = ', round(correl, 2))),
            hjust=-.2, vjust=1.2, alpha=.05) -> p

ggsave('figs/warmth_corp_pun.jpeg', p)

subdat %>%
  group_by(STATE, raceomb) %>%
  summarise(bias = mean(D_biep.White_Good_all),
            bias_se = sd(D_biep.White_Good_all)/sqrt(n()),
            warmth = mean(tblack),
            warmth_se = sd(tblack)/sqrt(n())) %>%
  mutate(bias_low = bias-1.96*bias_se,
         bias_high = bias+1.96*bias_se,
         warmth_low = warmth-1.96*warmth_se,
         warmth_high = warmth+1.96*warmth_se) %>%
  mutate(raceomb = fct_recode(as.character(raceomb), 
                              'African Americans'='5', 'White'='6')) %>%
  mutate(LEA_STATE = STATE) %>%
  right_join(gaps) %>%
  filter(variable=='iss_susp') %>%
  ungroup() %>%
  group_by(raceomb) %>%
  mutate(correl = cor(warmth, est)) %>%
  ggplot(aes(x=warmth, y=est)) + 
  geom_point(alpha=.5) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), alpha=.25) +
  geom_errorbarh(aes(xmin=warmth_low, xmax=warmth_high), alpha=.25) +
  theme_classic() +
  facet_wrap(~raceomb, scales = 'free') +
  ylab('In-school suspension gap gap') +
  xlab('Explicit warmth towards African Americans') +
  geom_smooth(method='lm') +
  geom_text(x=-Inf, y=Inf, aes(label=paste('R = ', round(correl, 2))),
            hjust=-.2, vjust=1.2, alpha=.03) -> p

ggsave('figs/warmth_iss_susp.jpeg', p)

subdat %>%
  group_by(STATE, raceomb) %>%
  summarise(bias = mean(D_biep.White_Good_all),
            bias_se = sd(D_biep.White_Good_all)/sqrt(n()),
            warmth = mean(tblack),
            warmth_se = sd(tblack)/sqrt(n())) %>%
  mutate(bias_low = bias-1.96*bias_se,
         bias_high = bias+1.96*bias_se,
         warmth_low = warmth-1.96*warmth_se,
         warmth_high = warmth+1.96*warmth_se) %>%
  mutate(raceomb = fct_recode(as.character(raceomb), 
                              'African Americans'='5', 'White'='6')) %>%
  mutate(LEA_STATE = STATE) %>%
  right_join(gaps) %>%
  filter(variable=='oos_susp') %>%
  ungroup() %>%
  group_by(raceomb) %>%
  mutate(correl = cor(warmth, est)) %>%
  ggplot(aes(x=warmth, y=est)) + 
  geom_point(alpha=.5) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), alpha=.25) +
  geom_errorbarh(aes(xmin=warmth_low, xmax=warmth_high), alpha=.25) +
  theme_classic() +
  facet_wrap(~raceomb, scales = 'free') +
  ylab('Out-of-school suspension gap gap') +
  xlab('Explicit warmth towards African Americans') +
  geom_smooth(method='lm') +
  geom_text(x=-Inf, y=Inf, aes(label=paste('R = ', round(correl, 2))),
            hjust=-.2, vjust=1.2, alpha=.03) -> p

ggsave('figs/warmth_oos_susp.jpeg', p)

subdat %>%
  group_by(STATE, raceomb) %>%
  summarise(bias = mean(D_biep.White_Good_all),
            bias_se = sd(D_biep.White_Good_all)/sqrt(n()),
            warmth = mean(tblack),
            warmth_se = sd(tblack)/sqrt(n())) %>%
  mutate(bias_low = bias-1.96*bias_se,
         bias_high = bias+1.96*bias_se,
         warmth_low = warmth-1.96*warmth_se,
         warmth_high = warmth+1.96*warmth_se) %>%
  mutate(raceomb = fct_recode(as.character(raceomb), 
                              'African Americans'='5', 'White'='6')) %>%
  mutate(LEA_STATE = STATE) %>%
  right_join(gaps) %>%
  filter(variable=='ps_susp') %>%
  ungroup() %>%
  group_by(raceomb) %>%
  mutate(correl = cor(warmth, est)) %>%
  ggplot(aes(x=warmth, y=est)) + 
  geom_point(alpha=.5) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), alpha=.25) +
  geom_errorbarh(aes(xmin=warmth_low, xmax=warmth_high), alpha=.25) +
  theme_classic() +
  facet_wrap(~raceomb, scales = 'free') +
  ylab('Preschool suspension gap gap') +
  xlab('Explicit warmth towards African Americans') +
  geom_smooth(method='lm') +
  geom_text(x=-Inf, y=Inf, aes(label=paste('R = ', round(correl, 2))),
            hjust=-.2, vjust=1.2, alpha=.03) -> p

ggsave('figs/warmth_ps_susp.jpeg', p)
