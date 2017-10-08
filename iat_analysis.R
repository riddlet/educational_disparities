library(haven)
library(dplyr)
library(ggplot2)

df <- rbind(read_sav('Documents/gits/Data/iat/Race IAT.public.2013.sav'),
            read_sav('Documents/gits/Data/iat/Race IAT.public.2012.sav'),
            read_sav('Documents/gits/Data/iat/Race IAT.public.2010.sav'))

df2 <- read_sav('Documents/gits/Data/iat/Race IAT.public.2011.sav')

df3 <- read_sav('Documents/gits/Data/iat/Race IAT.public.2010.sav')

"%ni%" <- Negate("%in%")

df %>%
  filter(STATE!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack)) %>%
  select(STATE, D_biep.White_Good_all, tblack, raceomb) %>%
  filter(STATE %ni% c('AA', 'AE', 'AP', 'AS', 'FM', 'GU', 'MP', 'PR', 'VI', 'MH')) -> subdat

df2 %>%
  filter(STATE!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack)) %>%
  select(STATE, D_biep.White_Good_all, tblack, raceomb) %>%
  filter(STATE %ni% c('AA', 'AE', 'AP', 'AS', 'FM', 'GU', 'MP', 'PR', 'VI', 'MH')) %>%
  rbind(subdat) %>%
  filter(raceomb==5 | raceomb==6) -> subdat

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
  ggplot(aes(x=bias, y=warmth)) + 
  geom_point() + 
  geom_errorbar(aes(ymin=warmth_low, ymax=warmth_high)) +
  geom_errorbarh(aes(xmin=bias_low, xmax=bias_high)) +
  theme_minimal() +
  facet_wrap(~raceomb, scales = 'free')
           