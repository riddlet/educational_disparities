library(ggplot2)
library(dplyr)

df <- data.frame(race = rep(c('white', 'black'), each=41),
                 perc = c(73.4, 73.6, 74.0, 74.1, 74.6, 74.4, 74.8, 75.1, 75.2,
                          75.3, 75.3, 75.4, 75.6, 75.6, 75.9, 76.1, 76.3, 76.5, 
                          76.3, 76.5, 76.5, 76.8, 77.1, 77.3, 77.3, 77.3, 77.5, 
                          77.5, 77.7, 78.1, 78.0, 78.3, 78.5, 78.5, 78.8, 78.9, 
                          79.0, 79.1, 79.0, 79.1, 79.0, 66.8, 67.2, 67.7, 68.1, 
                          68.5, 68.1, 68.9, 69.4, 69.4, 69.5, 69.3, 69.1, 69.1, 
                          68.9, 68.8, 69.1, 69.3, 69.6, 69.2, 69.5, 69.6, 70.2, 
                          71.1, 71.3, 71.4, 71.8, 72.0, 72.2, 72.4, 72.9, 73.0, 
                          73.4, 73.8, 74.3, 74.7, 75.1, 75.3, 75.5, 75.5, 75.6, 
                          75.5),
                 yr = rep(seq(1975, 2015, 1),2))

ggplot(df, aes(x=yr, y=perc, group=race, color=race)) + 
  theme_classic() +
  geom_line() +
  ggtitle('CDC national estimates for Life Expectancy') +
  ylab('Years') +
  ylim(60, 85) +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=14),
        axis.text = element_text(size=12),
        legend.text = element_text(size=12))

df <- data.frame(race = c('white', 'black'),
                 perc = c(4.9, 10.9))

ggplot(df, aes(x=race, y=perc, label=perc)) + 
  geom_point(aes(group=race, color=race), stat='identity', size=2) +
  theme_classic() +
  geom_line(aes(group=1)) +
  ggtitle('CDC national estimates for infant morality') +
  ylab('Deaths per 10,000 births') +
  ylim(0, 15) +
  geom_text(vjust=-.9, hjust=-.1, size=4.5) +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=14),
        axis.text = element_text(size=12),
        legend.text = element_text(size=12))

df <- data.frame(race = rep(c('white', 'black'), each=46),
                 perc = c(13.2, 13.4, 12.3, 11.6, 11.9, 11.4, 12.0, 11.9, 11.9,
                          12.0, 11.4, 11.3, 11.4, 11.1, 11.0, 10.4, 9.7, 10.4, 
                          9.6, 9.4, 9.0, 8.9, 7.7, 7.9, 7.7, 8.6, 7.3, 7.6, 7.7,
                          7.3, 6.9, 7.3, 6.5, 6.3, 6.8, 6.0, 5.8, 5.3, 4.8, 5.2,
                          5.1, 5.0, 4.3, 5.1, 5.2, 4.6, 27.9, 24.0, 21.3, 22.2,
                          21.2, 22.9, 20.5, 19.8, 20.2, 21.1, 19.1, 18.4, 18.4, 
                          18.0, 15.5, 15.2, 14.2, 14.1, 14.5, 13.9, 13.2, 13.6,
                          13.7, 13.6, 12.6, 12.1, 13.0, 13.4, 13.8, 12.6, 13.1, 
                          10.9, 11.3, 10.9, 11.8, 10.4, 10.7, 8.4, 9.9, 9.3, 
                          8.0, 7.3, 7.5, 7.3, 7.4, 6.5),
                 yr = rep(seq(1970, 2015, 1),2))

ggplot(df, aes(x=yr, y=perc, group=race, color=race)) + 
  theme_classic() +
  geom_line() +
  ggtitle('US Census Bureau national estimates for high school dropout rate') +
  ylab('Percentage of dropouts among 16-24 year-olds') +
  ylim(0, 35) +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=14),
        axis.text = element_text(size=12),
        legend.text = element_text(size=12))

df <- data.frame(race = rep(c('white', 'black'), each=22),
                 perc = c(2.7, 3.1, 1.9, 2.1, 2.5, 2.5, 2.5, 2.4, 2.5, 2.7,
                          2.7, 2.8, 2.4, 2.0, 1.8, 1.7, 1.6, 1.7, 1.5, 1.5, 
                          1.8, 1.8, 4.8, 4.9, 3.8, 4.2, 4.1, 4.0, 5.3, 4.9, 
                          3.9, 4.9, 5.4, 5.0, 4.2, 2.7, 3.0, 2.3, 2.5, 1.9,
                          2.7, 3.2, 3.3, 3.2),
                 yr = rep(seq(1994, 2015, 1),2))

ggplot(df, aes(x=yr, y=perc, group=race, color=race)) + 
  theme_classic() +
  geom_line() +
  ggtitle('US Census Bureau national estimate of annual retention, K-8') +
  ylab('Percentage of students retained') +
  ylim(0, 10) +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=14),
        axis.text = element_text(size=12),
        legend.text = element_text(size=12))

df <- data.frame(race = rep(c('white', 'black'), each=5),
                 perc = c(38.2, 30.9, 22.3, 6.7, 2,
                          33.5, 25.7, 27.1, 10.9, 2.8),
                 categ = rep(c('Excellent', 'Very Good', 'Good', 'Fair', 'Poor'), 2))
df$categ <- forcats::fct_relevel(df$categ, 'Excellent', 'Very Good', 'Good', 'Fair', 'Poor')                 

ggplot(df, aes(x=categ, y=perc, group=race, fill=race)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  theme_classic() +
  ggtitle('CDC national estimates for self-reported health status (2015)') +
  ylab('Estimated Percentage') +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=14),
        axis.text = element_text(size=12),
        legend.text = element_text(size=12))

df <- data.frame(race = rep(c('white', 'black'), each=2),
                 perc = c(7.7, 9.6, 16.3, 18),
                 yr = rep(c('1988-1994', '2011-2014'), 2))
              
ggplot(df, aes(x=yr, y=perc, group=race, color=race, label=perc)) + 
  geom_point(stat='identity', size=2) +
  theme_classic() +
  geom_line() +
  ggtitle('CDC national estimates for diabetes prevalence among adults') +
  ylab('Estimated Percentage (age adjusted)') +
  geom_text(vjust=-.75, size=4.5) +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=14),
        axis.text = element_text(size=12),
        legend.text = element_text(size=12))

df <- data.frame(race = rep(rep(c('white', 'black'), each=10),2),
                 perc = c(6.85, 6.27, 5.72, 5.65, 5.7, 6.22, 6.55, 7.16, 7.08, 7,
                          13.9, 13.19, 12.69, 12.65, 13.25, 13.13, 12.99, 13.59, 
                          13.21, 13.03, .95, .92, .9, .94, .95, 1.06, 1.14, 
                          1.20, 1.17, 1.12, 2.4, 2.4, 2.48, 2.71, 2.92, 2.97, 
                          3.07, 3.15, 2.9, 2.81),
                 yr = rep(c(1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 
                            2010, 2015), 4),
                 type = rep(c('Low Birthweight (<2,500g)', 
                              'Very Low Birthweight (<1,500g)'), each=20))


ggplot(df, aes(x=yr, y=perc, group=race, color=race)) + 
  geom_point(stat='identity', size=2) +
  theme_classic() +
  geom_line() +
  ggtitle('CDC national estimates for low birthweight') +
  ylab('Estimated Percentage (age adjusted)') +
  facet_wrap(~type, ncol=1, scales = 'free') +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=14),
        axis.text = element_text(size=12),
        legend.text = element_text(size=12),
        strip.text = element_text(size=12))

df <- data.frame(race = rep(c('white', 'black'), each=15),
                 perc = c(3.1, 2.5, 2.9, 3, 3, 3.1, 2.9, 2.7, 2.9, 3.2, 3.2,
                          3.2, 3.1, 3.4, 3.3, 4, 2.9, 3.1, 3.5, 3.4, 3.4, 3.6,
                          3.2, 3.2, 3.7, 3.8, 3.7, 3.3, 3.5, 3.4),
                 yr = rep(c(1997, 1999, 2000, 2001, 2002, 2003, 2004, 2006, 
                            2007, 2008, 2009, 2010, 2011, 2013, 2014), 2))


ggplot(df, aes(x=yr, y=perc, group=race, color=race)) + 
  geom_point(stat='identity', size=2) +
  theme_classic() +
  geom_line() +
  ylim(0, 5) +
  ggtitle('Serious psychological distress in the past 30 days') +
  ylab('Estimated Percentage of adults') +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=14),
        axis.text = element_text(size=12),
        legend.text = element_text(size=12),
        strip.text = element_text(size=12))

df <- data.frame(race = rep(c('white', 'black'), each=7),
                 perc = c(28.3, 29.4, 29.9, 29.9, 29.7, 28.6, 29.1,
                          42.3, 43.1, 43.4, 43.2, 42.7, 43.4, 43.3),
                 yr = rep(c(2002, 2004, 2006, 2008, 2010, 2012, 2014), 2))

ggplot(df, aes(x=yr, y=perc, group=race, color=race)) + 
  geom_point(stat='identity', size=2) +
  theme_classic() +
  geom_line() +
  ylim(0, 50) + 
  ggtitle('Rate of Hypertension') +
  ylab('Estimated Percentage of adults') +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=14),
        axis.text = element_text(size=12),
        legend.text = element_text(size=12),
        strip.text = element_text(size=12))
  
df <- data.frame(race = rep(c('white', 'black'), 2),
                 perc = c(19, 16.8, 4.3, 3.1),
                 type = c(rep('Any Mental Illness', 2),
                          rep('Serious Mental Illness', 2)))

ggplot(df, aes(x=type, y=perc, group=race, color=race, label=perc)) + 
  geom_point(stat='identity', size=2) +
  theme_classic() +
  geom_line() +
  ggtitle('Substance Abuse and Mental Health Services Administration',
          'Rates of mental illness, 2008-2012') +
  ylab('Estimated Annual Percentage') +
  geom_text(vjust=-.25, hjust=-.1, size=4) +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=14),
        axis.text = element_text(size=12),
        legend.text = element_text(size=12),
        strip.text = element_text(size=12))

df <- data.frame(race = rep(c('white', 'black'), each=4),
                 perc = c(46.3, 41, 25.7, 2.9, 29.8, 23.4, 18.7, 5.1),
                 type = rep(c('Any Service', 'Prescription', 'Outpatient', 
                              'Inpatient'),2))

ggplot(df, aes(x=race, y=perc, label=perc)) + 
  geom_point(stat='identity', size=2, aes(color=race)) +
  theme_classic() +
  geom_line(aes(group=1)) +
  ggtitle('Substance Abuse and Mental Health Services Administration',
          'Rate of Service use in the past year among adults with any mental illness') +
  ylab('Estimated Annual Percentage') +
  geom_text(hjust=-.5) +
  facet_wrap(~type, ncol=2, scales='free') + 
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=14),
        axis.text = element_text(size=12),
        legend.text = element_text(size=12),
        strip.text = element_text(size=12))

df <- data.frame(occupation = c('Manufacturing/Construction', 
                                'Retail trade',
                                'Finance/information/real estate',
                                'Professional/administrative/management',
                                'Educational services',
                                'Health care/social assistance',
                                'Accommodation/food services',
                                'Public administration/arts/other services'),
                 prev_ratio = c(1.14, 1.07, 1.44, 1.3, 1.39, 1.23, 1.05, 1.3),
                 lower = c(1.07, .96, 1.3, 1.18, 1.25, 1.14, .93, 1.21),
                 upper = c(1.2, 1.18, 1.59, 1.44, 1.54, 1.32, 1.19, 1.41))

df %>%
  mutate(occupation=reorder(occupation, prev_ratio)) %>%
  ggplot(aes(x=occupation, y=prev_ratio)) + 
  geom_point(stat='identity', size=2) +
  theme_classic() +
  geom_line() +
  geom_errorbar(aes(ymin=lower, ymax=upper)) +
  coord_flip() +
  geom_hline(yintercept = 1) +
  ggtitle('Short Sleeper black-white prevalence ratio',
          'Data from the National Health Interview Survey; Model described in Jackson et al., 2013') +
  ylab('Estimated Prevalence Ratio') +
  theme(legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=14),
        axis.text = element_text(size=12),
        legend.text = element_text(size=12),
        strip.text = element_text(size=12))

df <- data.frame(metric = rep(c('Percent of all KG held back', 
                            'Proportion of Preschoolers Suspended (n=6,751)'),
                            each=2),
                 per = c(5, 4, 46.9, 27.6),
                 race = c('black', 'white', 'black', 'white'))

df %>%
  ggplot(aes(x=race, y=per)) + 
  geom_point(stat='identity', size=2, aes(color=race)) +
  theme_classic() +
  geom_line(aes(group=1)) +
  ggtitle('Very early differences in educational attainment',
          'Data from the US department of Education, 2013-2014') +
  facet_wrap(~metric, scales='free', ncol=1) +
  theme(legend.title = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size=12),
        legend.text = element_text(size=12),
        strip.text = element_text(size=12))

"%ni%" <- Negate("%in%")

district_content <- read.csv('/Users/travis/Documents/gits/Data/crdc201314csv/CRDC2013_14_LEA_content.csv')
df_district <- read.csv('/Users/travis/Documents/gits/Data/crdc201314csv/CRDC2013_14_LEA.csv')
school_content <- read.csv('/Users/travis/Documents/gits/Data/crdc201314csv/CRDC2013_14_SCH_content.csv')
df_school <- read.csv('/Users/travis/Documents/gits/Data/crdc201314csv/CRDC2013_14_SCH.csv')
county_means <- read.csv('/Users/travis/Documents/gits/educational_disparities/output/county_means.csv', 
                         colClasses = 'character')

# Get enrollment figures
df_school %>%
  dplyr::select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, SCH_ENR_HI_M:TOT_ENR_F) %>%
  gather(group, total_number, SCH_ENR_HI_M:TOT_ENR_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>%
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='NR_', 
                          biracial='TR_', white='WH_')) %>%
  group_by(COMBOKEY, group) %>%
  mutate(total_number = sum(total_number)) %>%
  ungroup() %>%
  dplyr::select(-prefix, -gender) %>%
  distinct() -> enrollment

df_school %>%
  dplyr::select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, SCH_PSENR_HI_M:TOT_PSENR_F) %>%
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
  dplyr::select(-prefix, -gender) %>%
  distinct() -> ps_enrollment

df_school %>% 
  dplyr::select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_GRADE_PS, SCH_PSDISC_SINGOOS_HI_M:TOT_PSDISC_SINGOOS_F) %>% 
  filter(SCH_GRADE_PS=='YES') %>%
  gather(group, number, SCH_PSDISC_SINGOOS_HI_M:TOT_PSDISC_SINGOOS_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='OS_', 
                          biracial='TR_', white='WH_')) %>%
  dplyr::select(-prefix) -> ps_susp

df_school %>% 
  dplyr::select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD,
         SCH_GRADE_PS, SCH_PSDISC_MULTOOS_HI_M:TOT_PSDISC_MULTOOS_F) %>% 
  filter(SCH_GRADE_PS=='YES') %>%
  gather(group, number_2, SCH_PSDISC_MULTOOS_HI_M:TOT_PSDISC_MULTOOS_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>%
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='OS_', 
                          biracial='TR_', white='WH_')) %>%
  dplyr::select(COMBOKEY, group, gender, number_2) %>%
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
  group_by(group) %>%
  summarise(total_students = sum(total_number, na.rm=T),
            number_suspended = sum(number))

df_school %>% 
  dplyr::select(LEA_STATE:LEAID, CCD_LATCOD, CCD_LONCOD, 
         SCH_PSDISC_EXP_HI_M:TOT_PSDISC_EXP_F) %>% 
  gather(group, number, SCH_PSDISC_EXP_HI_M:TOT_PSDISC_EXP_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='XP_', 
                          biracial='TR_', white='WH_')) %>%
  filter(number>=0) %>%
  dplyr::select(-prefix) -> ps_exp

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

df.census <- read.csv('/Users/travis/Downloads/nc-est2016-agesex-res.csv')

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

raw_obs <- data.frame(implicit = c(df$D_biep.White_Good_all,
                                   df2$D_biep.White_Good_all,
                                   df3$D_biep.White_Good_all,
                                   df4$D_biep.White_Good_all,
                                   df5$D_biep.White_Good_all,
                                   df6$D_biep.White_Good_all,
                                   df7$D_biep.White_Good_all,
                                   df8$D_biep.White_Good_all,
                                   df9$D_biep.White_Good_all,
                                   df10$D_biep.White_Good_all),
                      w_white = c(df$twhite_0to10, df2$twhite_0to10,
                                  df3$twhite_0to10, df4$twhite_0to10,
                                  df5$twhite_0to10, df6$twhite_0to10,
                                  df7$twhites_0to10, df8$twhites_0to10,
                                  df9$twhites_0to10, df10$twhite_0to10),
                      w_black = c(df$tblack_0to10, df2$tblack_0to10,
                                  df3$tblack_0to10, df4$tblack_0to10,
                                  df5$tblack_0to10, df6$tblack_0to10,
                                  df7$tblacks_0to10, df8$tblacks_0to10,
                                  df9$tblacks_0to10, df10$tblack_0to10))

raw_obs$explicit <- raw_obs$w_white-raw_obs$w_black

df %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack_0to10) &
           !is.na(age)) %>%
  dplyr::select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0to10, raceomb, age)  -> subdat

df2 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack_0to10) &
           !is.na(age)) %>%
  dplyr::select(CountyNo, STATE, D_biep.White_Good_all,  
         tblack_0to10, twhite_0to10, raceomb, age) %>%
  rbind(subdat) -> subdat

df3 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack_0to10) &
           !is.na(age)) %>%
  dplyr::select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0to10, raceomb, age) %>%
  rbind(subdat) -> subdat

df4 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack_0to10) &
           !is.na(age)) %>%
  dplyr::select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0to10, raceomb, age) %>%
  rbind(subdat) -> subdat

df5 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack_0to10) &
           !is.na(age)) %>%
  dplyr::select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0to10, raceomb, age) %>%
  rbind(subdat) -> subdat

df6 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack_0to10) &
           !is.na(age)) %>%
  dplyr::select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0to10, raceomb, age) %>%
  rbind(subdat) -> subdat

df7 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblacks_0to10) &
           !is.na(age)) %>%
  mutate(tblack_0to10=tblacks_0to10,
         twhite_0to10=twhites_0to10) %>%
  mutate(raceomb = ethnic) %>%
  dplyr::select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0to10, raceomb, age) %>%
  rbind(subdat) -> subdat

df8 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblacks_0to10) &
           !is.na(age)) %>%
  mutate(tblack_0to10=tblacks_0to10,
         twhite_0to10=twhites_0to10) %>%
  mutate(raceomb = ethnic) %>%
  dplyr::select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0to10, raceomb, age) %>%
  rbind(subdat) -> subdat

df9 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblacks_0to10) &
           !is.na(age)) %>%
  mutate(raceomb = ethnic) %>%
  mutate(tblack_0to10=tblacks_0to10,
         twhite_0to10=twhites_0to10) %>%
  dplyr::select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0to10, raceomb, age) %>%
  rbind(subdat) -> subdat

df10 %>%
  filter(CountyNo!='' &
           !is.na(D_biep.White_Good_all) &
           !is.na(tblack_0to10) &
           !is.na(age)) %>%
  dplyr::select(CountyNo, STATE, D_biep.White_Good_all, 
         tblack_0to10, twhite_0to10, raceomb, age) %>%
  rbind(subdat) -> subdat

subdat %>%
  mutate(explicit_bias=tblack_0to10) %>%
  mutate(explicit_bias_diff = twhite_0to10 - tblack_0to10) %>%
  mutate(age_bin = cut(age, breaks=c(14, 24, 34, 54, 75, 120))) %>%
  mutate(race = fct_recode(as.character(raceomb), 
                           'Black'='5', 'White'='6')) %>%
  filter(!is.na(age_bin)) %>%
  filter(STATE %ni% 
           c('AA', 'AE', 'AP', 'AS', 'FM', 'GU', 'MH', 'MP', 'PR', 'VI')) %>%
  mutate(county_id = paste(STATE, CountyNo, sep='-')) -> individual_data

df.census %>%
  filter(AGE!=999) %>%
  filter(SEX==0) %>%
  mutate(age_5 = round(AGE/5)*5) %>%
  group_by(age_5) %>%
  summarise(number = sum(CENSUS2010POP)) %>%
  mutate(data='2010 Census') -> census_plot

individual_data %>%
  mutate(age_5 = round(age/5)*5) %>%
  group_by(age_5) %>%
  summarise(number = n()) %>%
  mutate(data='Project Implicit') %>%
  rbind(census_plot) %>%
  ggplot(aes(age_5, y=number)) + 
  geom_bar(stat='identity') +
  facet_wrap(~data, scales='free') +
  theme_classic() +
  xlab('Age')

ggplot(individual_data, aes(x=D_biep.White_Good_all, group=age_bin, color=age_bin)) + 
  geom_density() +
  theme_classic() +
  scale_color_discrete(name="Age Group") +
  xlab('Implicit Bias')

df_acs <- read.csv('/Users/travis/Documents/gits/Data/ACS/county_age/ACS_14_5YR_DP05_with_ann.csv', skip=1)
df_county_linking_info <- read.csv('/Users/travis/Documents/gits/educational_disparities/output/county_linking_table.csv', stringsAsFactors = F)
df_county_linking_info$county_name[1904] <- 'Doña Ana'
df_states <- data.frame(state=c(state.name, 'District of Columbia'),
                        state_abb=c(state.abb, 'DC'))
df_acs_eth <- read.csv('/Users/travis/Documents/gits/Data/ACS/state_ethnicity/ACS_14_5YR_B02001_with_ann.csv',
                       skip = 1, stringsAsFactors = F)
df_acs_ed <- read.csv('/Users/travis/Documents/gits/Data/ACS/state_education/ACS_14_5YR_S1501_with_ann.csv',
                      skip = 1, stringsAsFactors = F)
df_acs_pov_emp <- read.csv('/Users/travis/Documents/gits/Data/ACS/state_poverty_emp/ACS_14_5YR_DP03_with_ann.csv',
                           skip = 1, stringsAsFactors = F)

df_acs <- df_acs[,c(3, 28, 32, 36, 40, 44, 48, 52, 56, 60, 64)]
df_acs$Geography <- as.character(df_acs$Geography)
df_acs$Geography[1803] <- 'Doña Ana County, New Mexico'

df_acs %>%
  gather(age, num, -Geography) %>%
  mutate(county=Geography) %>%
  dplyr::select(-Geography) %>%
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



#counties w/o schools:
#kalawao, Issaquena, Mora, Divide, Loving, Marion, Miller,
names(individual_data)[2] <- 'state_abb'
individual_data %>%
  filter(raceomb==6) %>%
  left_join(df_states) %>%
  left_join(df_acs_counts[,1:4], by=c('county_id', 'age_bin')) %>%
  left_join(covs, by='state') %>%
  left_join(covs_ed, by='state') %>%
  left_join(covs_income, by='state') %>%
  mutate_at(vars(white_prop:poverty), scale) -> individual_data_base

library(ggrepel)
individual_data_base %>%
  filter(race=='White') %>%
  filter(state_abb!='PW') %>% 
  group_by(state_abb) %>%
  summarise(implicit = mean(D_biep.White_Good_all),
            explicit = mean(explicit_bias_diff, na.rm=T)) %>%
  mutate(implicit = scale(implicit)[,1],
         explicit = scale(explicit)[,1]) -> plotdat
plotdat$state_abb[which(plotdat$state_abb %ni% c('CA', 'NJ', 'NY', 'AK', 'DC', 
                                                 'MS', 'PA', 'MT', 'OR', 'ND'))] <- ''

ggplot(plotdat, aes(implicit, explicit, label=state_abb)) + 
  geom_point() + 
  geom_text_repel() +
  theme_classic()

county_means <- read.csv('/Users/travis/Documents/gits/educational_disparities/output/county_means.csv')

individual.model.bias <- lmer(D_biep.White_Good_all ~ white_prop + black_prop + 
                                b.w.ratio + col_grads + unemp + income + 
                                poverty + (1|age_bin) + (1|county_id) + 
                                (1|state_abb), data=individual_data_base)
individual.model.explicit <- lmer(explicit_bias ~ white_prop + black_prop + 
                                    b.w.ratio + col_grads + unemp + income + 
                                    poverty + (1|age_bin) + (1|county_id) + 
                                    (1|state_abb), data=individual_data_base)
individual_data %>%
  filter(race=='White') %>%
  filter(state_abb!='PW') %>% 
  left_join(df_states) %>%
  left_join(df_acs_counts[,1:4], by=c('county_id', 'age_bin')) %>%
  left_join(covs, by='state') %>%
  left_join(covs_ed, by='state') %>%
  left_join(covs_income, by='state') %>%
  filter(!is.na(explicit_bias_diff)) %>%
  mutate_at(vars(white_prop:poverty), scale) -> individual_data_diff

individual.model.explicit_diff <- lmer(explicit_bias_diff ~ white_prop + 
                                         black_prop + b.w.ratio + col_grads + 
                                         unemp + income + poverty + 
                                         (1|age_bin) + (1|county_id) + 
                                         (1|state_abb), data=individual_data_diff)

df_acs_countstemp <- df_acs_counts[which(!is.na(df_acs_counts$county_id)),]
#df_acs_counts <- left_join(df_acs_counts, df_states)
df_acs_countstemp %>%
  ungroup() %>%
  mutate_at(vars(white_prop:poverty), scale) -> scaled_counts

scaled_counts$yhat_bias <- predict(individual.model.bias, 
                                   newdata=scaled_counts, allow.new.levels=T)
scaled_counts$yhat_explicit <- predict(individual.model.explicit, 
                                       newdat=scaled_counts, allow.new.levels=T)
#reverse score explicit bias
scaled_counts$yhat_explicit <- scaled_counts$yhat_explicit*-1
scaled_counts$yhat_explicit_diff <- predict(individual.model.explicit_diff, 
                                            newdat=scaled_counts, 
                                            allow.new.levels=T)

scaled_counts %>%
  filter(county_id %in% c('MT-055', 'NJ-021', 'CA-037', 'NY-061', 'CA-075')) %>%
  ggplot(aes(x=age_bin, y=yhat_bias, group=county_name, color=county_name)) +
  geom_point(aes(size=num)) + 
  geom_line() +
  scale_size_continuous(guide=F) +
  guides(color=guide_legend(title=NULL)) + 
  theme_classic() +
  xlab('Age Group') +
  ylab('Estimated Implicit Bias')

scaled_counts %>%
  filter(county_id %in% c('MT-055', 'NJ-021', 'CA-037', 'NY-061', 'CA-075')) %>%
  group_by(county_id) %>%
  mutate(bias = weighted.mean(yhat_bias, w = num)) %>%
  ggplot(aes(x=age_bin, y=yhat_bias, group=county_name, color=county_name)) +
  geom_point(aes(size=num), alpha = .2) + 
  geom_line(alpha = .2) +
  geom_line(aes(x=age_bin, y=bias, color=county_name)) + 
  scale_size_continuous(guide=F) +
  guides(color=guide_legend(title=NULL)) + 
  theme_classic() +
  xlab('Age Group') +
  ylab('Estimated Implicit Bias')

ggplot(county_means, aes(x=explicit_diff, y=weighted_explicit_diff)) + 
  geom_point(alpha = .25) + 
  ylim(-.25, 1.75) + 
  xlim(-.25, 1.75) +
  theme_classic() +
  xlab('Raw Explicit mean') +
  ylab('Post-Stratified Explicit estimate') 

county_means$mercer <- county_means$county_id=='NJ-021'
ggplot(county_means, aes(x=weighted_bias, y=weighted_explicit_diff, 
                         color=mercer, alpha=mercer)) +
  geom_point() +
  theme_classic() +
  xlab('Implicit Bias') +
  ylab('Explicit Bias') +
  scale_color_manual(values=c('grey', 'darkorange'), guide=F) +
  scale_alpha_manual(values=c(.25, 1), guide=F) 

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

enrollment %>%
  mutate(exclude = COMBOKEY %in% exclude$COMBOKEY | 
           LEAID %in% error_elem |
           LEAID %in% error_second) %>%
  group_by(exclude, group) %>%
  summarise(ns=sum(total_number)) -> tempout

base_path <- '/Users/travis/Documents/gits/educational_disparities/cluster/output/mw_uclaexcl_diff/'
plot.dat <- data.frame(group = NA, weighted_bias = NA, est = NA, lower = NA, upper=NA, metric = NA)
plot.dat2 <- data.frame(weighted_bias = NA, est = NA, lower=NA, upper=NA, metric=NA)
plot.dat3 <- data.frame(est=NA, lower=NA, upper=NA, metric=NA)

# weighted bias w/exclusions & explicit difference
metrics <- list.files(base_path)
metrics <- c('expulsion_combined', 'inschool_susp', 'oos_susp')
for (i in metrics){
  pth <- paste(base_path, i, '/', sep='')
  
  files <- list.files(pth)
  j <- 1
  posterior_combo <- array(0, dim=c(14,4000,length(files)))
  raw_rate <- data.frame(county_id=NA, nschools=NA, weighted_bias=NA, group=NA, nincidents=NA, nstudents=NA, rate=NA)
  for (k in files){
    #print(i)
    load(paste(pth,k,sep=''))
    df <- as.matrix(m)
    m$data %>% 
      select(county_id, COMBOKEY) %>% 
      distinct() %>% 
      group_by(county_id) %>% 
      summarise(nschools=n()) -> n_schools
    m$data %>%
      group_by(county_id, group) %>%
      summarise(nincidents=sum(number), nstudents=sum(total_number)) %>%
      mutate(rate=nincidents/nstudents) -> rate
    n_schools %>%
      left_join(m$data[, c('county_id', 'weighted_bias')]) %>%
      distinct() %>%
      left_join(rate) %>%
      rbind(raw_rate) %>%
      filter(!is.na(county_id)) -> raw_rate
    posterior_combo[,,j] <- t(df[,c(1:14)])
    j <- j+1
  }
  p_cons <- parallelMCMCcombine::consensusMCcov(posterior_combo, shuff = T)
  
  for (p in 1:14){
    m$stanfit@sim$samples[[1]][[p]] <- p_cons[p,1:1000]
  }
  for (p in 1:14){
    m$stanfit@sim$samples[[2]][[p]] <- p_cons[p,1001:2000]
  }
  for (p in 1:14){
    m$stanfit@sim$samples[[3]][[p]] <- p_cons[p,2001:3000]
  }
  for (p in 1:14){
    m$stanfit@sim$samples[[4]][[p]] <- p_cons[p,3001:4000]
  }
  
  data.frame(est=mean(p_cons[13,]),
             lower=quantile(p_cons[13,], .025),
             upper=quantile(p_cons[13,], .975),
             metric=i) %>%
    rbind(plot.dat3) %>%
    filter(!is.na(est)) %>%
    mutate(metric=reorder(metric, est)) -> plot.dat3
}

plot.dat3$metric <- fct_recode(plot.dat3$metric, 
                               'Out-of-School Suspension' = 'oos_susp',
                               'In-school Suspension' = 'inschool_susp',
                               'School-Related Arrest' = 'in_school_arrest',
                               'Law Enf. Referral' = 'law_enforcement',
                               'Expulsion' = 'expulsion_combined')
plot.dat3_all <-plot.dat3
plot.dat3_all$bias <- 'Implicit'

#weighted warmth w/exclusions & explicit difference
plot.dat <- data.frame(group = NA, weighted_warmth = NA, est = NA, lower = NA, upper=NA, metric = NA)
plot.dat2 <- data.frame(weighted_warmth = NA, est = NA, lower=NA, upper=NA, metric=NA)
plot.dat3 <- data.frame(est=NA, lower=NA, upper=NA, metric=NA)

for(i in metrics){
  pth <- paste(base_path, i, '/', sep='')
  
  files <- list.files(pth)
  j <- 1
  posterior_combo <- array(0, dim=c(14,4000,length(files)))
  raw_rate_warmth <- data.frame(county_id=NA, nschools=NA, weighted_warmth=NA, group=NA, nincidents=NA, nstudents=NA, rate=NA)
  for (k in files){
    #print(i)
    load(paste(pth,k,sep=''))
    df <- as.matrix(m)
    m$data %>% 
      select(county_id, COMBOKEY) %>% 
      distinct() %>% 
      group_by(county_id) %>% 
      summarise(nschools=n()) -> n_schools
    m$data %>%
      group_by(county_id, group) %>%
      summarise(nincidents=sum(number), nstudents=sum(total_number)) %>%
      mutate(rate=nincidents/nstudents) -> rate
    n_schools %>%
      left_join(m$data[, c('county_id', 'weighted_warmth')]) %>%
      distinct() %>%
      left_join(rate) %>%
      rbind(raw_rate_warmth) %>%
      filter(!is.na(county_id)) -> raw_rate_warmth
    posterior_combo[,,j] <- t(df[,c(1:14)])
    j <- j+1
  }
  p_cons <- parallelMCMCcombine::consensusMCcov(posterior_combo, shuff = T)
  
  for (p in 1:14){
    m$stanfit@sim$samples[[1]][[p]] <- p_cons[p,1:1000]
  }
  for (p in 1:14){
    m$stanfit@sim$samples[[2]][[p]] <- p_cons[p,1001:2000]
  }
  for (p in 1:14){
    m$stanfit@sim$samples[[3]][[p]] <- p_cons[p,2001:3000]
  }
  for (p in 1:14){
    m$stanfit@sim$samples[[4]][[p]] <- p_cons[p,3001:4000]
  }
  
  data.frame(est=mean(p_cons[14,]),
             lower=quantile(p_cons[14,], .025),
             upper=quantile(p_cons[14,], .975),
             metric=i) %>%
    rbind(plot.dat3) %>%
    filter(!is.na(est)) %>%
    mutate(metric=reorder(metric, est)) -> plot.dat3
}

plot.dat3$bias <- 'Explicit'
plot.dat3$metric <- fct_recode(plot.dat3$metric, 
                               'Out-of-School Suspension' = 'oos_susp',
                               'In-school Suspension' = 'inschool_susp',
                               'School-Related Arrest' = 'in_school_arrest',
                               'Law Enf. Referral' = 'law_enforcement',
                               'Expulsion' = 'expulsion_combined')
plot.dat3_all <- rbind(plot.dat3_all, plot.dat3)
ggplot(plot.dat3_all, aes(x=metric, y=est, group=bias, color=bias)) +
  geom_point(position=position_dodge(width=.5), size=2) +
  geom_errorbar(aes(ymin=lower, ymax=upper), 
                width=.5, position=position_dodge(width=.5)) +
  theme_classic() +
  coord_flip() +
  geom_hline(yintercept=0) +
  ylab('Black-White Difference in log-odds slope') +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_text(size=14),
        axis.text = element_text(size=12),
        legend.text = element_text(size=12),
        strip.text = element_text(size=12))


i <- 'expulsion_combined'
pth <- paste(base_path, i, '/', sep='')

files <- list.files(pth)
j <- 1
posterior_combo <- array(0, dim=c(14,4000,length(files)))
raw_rate <- data.frame(county_id=NA, nschools=NA, weighted_bias=NA, 
                       weighted_warmth=NA, group=NA, nincidents=NA, 
                       nstudents=NA, rate=NA)
for (k in files){
  #print(i)
  load(paste(pth,k,sep=''))
  df <- as.matrix(m)
  m$data %>% 
    select(county_id, COMBOKEY) %>% 
    distinct() %>% 
    group_by(county_id) %>% 
    summarise(nschools=n()) -> n_schools
  m$data %>%
    group_by(county_id, group) %>%
    summarise(nincidents=sum(number), nstudents=sum(total_number)) %>%
    mutate(rate=nincidents/nstudents) -> rate
  n_schools %>%
    left_join(m$data[, c('county_id', 'weighted_bias', 'weighted_warmth')]) %>%
    distinct() %>%
    left_join(rate) %>%
    rbind(raw_rate) %>%
    filter(!is.na(county_id)) -> raw_rate
  posterior_combo[,,j] <- t(df[,c(1:14)])
  j <- j+1
}
p_cons <- parallelMCMCcombine::consensusMCcov(posterior_combo, shuff = T)

for (p in 1:14){
  m$stanfit@sim$samples[[1]][[p]] <- p_cons[p,1:1000]
}
for (p in 1:14){
  m$stanfit@sim$samples[[2]][[p]] <- p_cons[p,1001:2000]
}
for (p in 1:14){
  m$stanfit@sim$samples[[3]][[p]] <- p_cons[p,2001:3000]
}
for (p in 1:14){
  m$stanfit@sim$samples[[4]][[p]] <- p_cons[p,3001:4000]
}

newdat <- expand.grid(group = rownames(table(m$data$group)),
                      total_pop=0,
                      unemp_rate=0,
                      med_income=0,
                      poverty_rate=0,
                      col_grads=0,
                      white_prop=0,
                      black_prop=0,
                      b.w.ratio=0,
                      weighted_bias = seq(-2.25,2.25,.25),
                      weighted_warmth = 0,
                      number=0,
                      total_number=1)
y_hat <- rstanarm::posterior_linpred(m, newdata = newdat, re.form=~0, transform=T)

cbind(newdat, t(y_hat)) %>% 
  mutate(index=row_number()) %>%
  gather(sample, value, `1`:`4000`) -> posterior_distribution


posterior_distribution %>%
  select(group, weighted_bias, sample, value, index) %>%
  group_by(group, weighted_bias) %>%
  summarise(est = mean(value),
            lower = quantile(value, .025),
            upper = quantile(value, .975)) %>%
  ungroup() %>%
  mutate(bias = 'Implicit') -> plot.dat

newdat <- expand.grid(group = rownames(table(m$data$group)),
                      total_pop=0,
                      unemp_rate=0,
                      med_income=0,
                      poverty_rate=0,
                      col_grads=0,
                      white_prop=0,
                      black_prop=0,
                      b.w.ratio=0,
                      weighted_bias = 0,
                      weighted_warmth = seq(-2.25,2.25,.25),
                      number=0,
                      total_number=1)
y_hat <- rstanarm::posterior_linpred(m, newdata = newdat, re.form=~0, transform=T)

cbind(newdat, t(y_hat)) %>% 
  mutate(index=row_number()) %>%
  gather(sample, value, `1`:`4000`) -> posterior_distribution


posterior_distribution %>%
  select(group, weighted_warmth, sample, value, index) %>%
  mutate(weighted_bias = weighted_warmth) %>%
  group_by(group, weighted_bias) %>%
  summarise(est = mean(value),
            lower = quantile(value, .025),
            upper = quantile(value, .975)) %>%
  ungroup() %>%
  mutate(bias = 'Explicit') %>%
  rbind(plot.dat) -> plot.dat


ggplot(plot.dat, aes(x=weighted_bias, y=est, group=group)) +
  geom_line(aes(color=group)) +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
  theme_classic() +
  ylab('Probability of Expulsion') +
  xlab('Standardized Bias') + 
  facet_wrap(~bias, ncol=2) +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(size=14),
        axis.text = element_text(size=12),
        legend.text = element_text(size=12),
        strip.text = element_text(size=12)) -> p1

posterior_distribution %>%
  select(group, weighted_warmth, sample, value) %>%
  spread(group, value) %>%
  mutate(ratio = black/white) %>%
  mutate(weighted_bias = weighted_warmth) %>%
  group_by(weighted_bias) %>%
  summarise(est = mean(ratio),
            lower = quantile(ratio, .025),
            upper = quantile(ratio, .975)) %>%
  ungroup() %>%
  mutate(bias = 'Explicit') -> plot.dat

newdat <- expand.grid(group = rownames(table(m$data$group)),
                      total_pop=0,
                      unemp_rate=0,
                      med_income=0,
                      poverty_rate=0,
                      col_grads=0,
                      white_prop=0,
                      black_prop=0,
                      b.w.ratio=0,
                      weighted_bias = seq(-2.25,2.25,.25),
                      weighted_warmth = 0,
                      number=0,
                      total_number=1)
y_hat <- rstanarm::posterior_linpred(m, newdata = newdat, re.form=~0, transform=T)

cbind(newdat, t(y_hat)) %>% 
  mutate(index=row_number()) %>%
  gather(sample, value, `1`:`4000`) -> posterior_distribution

posterior_distribution %>%
  select(group, weighted_bias, sample, value) %>%
  spread(group, value) %>%
  mutate(ratio = black/white) %>%
  group_by(weighted_bias) %>%
  summarise(est = mean(ratio),
            lower = quantile(ratio, .025),
            upper = quantile(ratio, .975)) %>%
  ungroup() %>%
  mutate(bias = 'Implicit') %>%
  rbind(plot.dat) -> plot.dat

raw_rate %>%
  mutate(Implicit = weighted_bias,
         Explicit = weighted_warmth) %>%
  select(-weighted_bias, -weighted_warmth) %>%
  gather(bias, value, Implicit:Explicit) %>%
  select(county_id, nschools, bias, group, rate, value) %>%
  spread(group, rate) %>%
  mutate(odds_r = black/white) -> raw_or
  

raw_or$mercer_county <- FALSE
raw_or$mercer_county[which(raw_or$county_id=='NJ-021')] <- TRUE

ggplot(plot.dat, aes(x=weighted_bias, y=est)) +
  geom_point(data=raw_or, aes(x=value, y=odds_r, size=nschools,
                              color=mercer_county, alpha=mercer_county)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.25) +
  theme_classic() +
  scale_color_manual(values=c('grey', 'darkorange'), guide=F) + 
  scale_alpha_manual(values=c(.2, 1), guide=F) +
  ylim(0,8) +
  xlab('Standardized Bias') +
  ylab('Relative Risk Ratio') +
  geom_hline(yintercept=1) +
  theme(strip.text = element_text(size=14),
        axis.title = element_text(face='bold', size=16),
        axis.text = element_text(size=14),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14)) +
  facet_wrap(~bias)

p1$data %>% filter(weighted_bias%in%c(0,1)) %>% mutate(students = est*8000000)

i <- 'expulsion_combined'
pth <- paste(base_path, i, '/', sep='')

files <- list.files(pth)
j <- 1
posterior_combo <- array(0, dim=c(14,4000,length(files)))
raw_rate <- data.frame(county_id=NA, nschools=NA, weighted_bias=NA, 
                       weighted_warmth=NA, group=NA, nincidents=NA, 
                       nstudents=NA, rate=NA)
for (k in files){
  #print(i)
  load(paste(pth,k,sep=''))
  df <- as.matrix(m)
  m$data %>% 
    select(county_id, COMBOKEY) %>% 
    distinct() %>% 
    group_by(county_id) %>% 
    summarise(nschools=n()) -> n_schools
  m$data %>%
    group_by(county_id, group) %>%
    summarise(nincidents=sum(number), nstudents=sum(total_number)) %>%
    mutate(rate=nincidents/nstudents) -> rate
  n_schools %>%
    left_join(m$data[, c('county_id', 'weighted_bias', 'weighted_warmth')]) %>%
    distinct() %>%
    left_join(rate) %>%
    rbind(raw_rate) %>%
    filter(!is.na(county_id)) -> raw_rate
  posterior_combo[,,j] <- t(df[,c(1:14)])
  j <- j+1
}
p_cons <- parallelMCMCcombine::consensusMCcov(posterior_combo, shuff = T)

for (p in 1:14){
  m$stanfit@sim$samples[[1]][[p]] <- p_cons[p,1:1000]
}
for (p in 1:14){
  m$stanfit@sim$samples[[2]][[p]] <- p_cons[p,1001:2000]
}
for (p in 1:14){
  m$stanfit@sim$samples[[3]][[p]] <- p_cons[p,2001:3000]
}
for (p in 1:14){
  m$stanfit@sim$samples[[4]][[p]] <- p_cons[p,3001:4000]
}

newdat <- expand.grid(group = rownames(table(m$data$group)),
                      total_pop=0,
                      unemp_rate=0,
                      med_income=0,
                      poverty_rate=0,
                      col_grads=0,
                      white_prop=0,
                      black_prop=0,
                      b.w.ratio=0,
                      weighted_bias = seq(-2.25,2.25,.25),
                      weighted_warmth = 0,
                      number=0,
                      total_number=1)
y_hat <- rstanarm::posterior_linpred(m, newdata = newdat, re.form=~0, transform=T)

cbind(newdat, t(y_hat)) %>% 
  mutate(index=row_number()) %>%
  gather(sample, value, `1`:`4000`) -> posterior_distribution


posterior_distribution %>%
  select(group, weighted_bias, sample, value, index) %>%
  group_by(group, weighted_bias) %>%
  summarise(est = mean(value),
            lower = quantile(value, .025),
            upper = quantile(value, .975)) %>%
  ungroup() %>%
  mutate(bias = 'Implicit') -> plot.dat

newdat <- expand.grid(group = rownames(table(m$data$group)),
                      total_pop=0,
                      unemp_rate=0,
                      med_income=0,
                      poverty_rate=0,
                      col_grads=0,
                      white_prop=0,
                      black_prop=0,
                      b.w.ratio=0,
                      weighted_bias = 0,
                      weighted_warmth = seq(-2.25,2.25,.25),
                      number=0,
                      total_number=1)
y_hat <- rstanarm::posterior_linpred(m, newdata = newdat, re.form=~0, transform=T)

cbind(newdat, t(y_hat)) %>% 
  mutate(index=row_number()) %>%
  gather(sample, value, `1`:`4000`) -> posterior_distribution


posterior_distribution %>%
  select(group, weighted_warmth, sample, value, index) %>%
  mutate(weighted_bias = weighted_warmth) %>%
  group_by(group, weighted_bias) %>%
  summarise(est = mean(value),
            lower = quantile(value, .025),
            upper = quantile(value, .975)) %>%
  ungroup() %>%
  mutate(bias = 'Explicit') %>%
  rbind(plot.dat) -> plot.dat


ggplot(plot.dat, aes(x=weighted_bias, y=est, group=group)) +
  geom_line(aes(color=group)) +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
  theme_classic() +
  ylab('Probability of Receiving \n an Out-of-school Suspension') +
  xlab('Standardized Bias') + 
  facet_wrap(~bias, ncol=2) +
  theme(strip.text = element_text(size=14),
        axis.title = element_text(face='bold', size=14),
        axis.text = element_text(size=14),
        legend.title = element_blank(),
        legend.text = element_text(size=14)) -> p

posterior_distribution %>%
  select(group, weighted_warmth, sample, value) %>%
  spread(group, value) %>%
  mutate(ratio = black/white) %>%
  mutate(weighted_bias = weighted_warmth) %>%
  group_by(weighted_bias) %>%
  summarise(est = mean(ratio),
            lower = quantile(ratio, .025),
            upper = quantile(ratio, .975)) %>%
  ungroup() %>%
  mutate(bias = 'Explicit') -> plot.dat

newdat <- expand.grid(group = rownames(table(m$data$group)),
                      total_pop=0,
                      unemp_rate=0,
                      med_income=0,
                      poverty_rate=0,
                      col_grads=0,
                      white_prop=0,
                      black_prop=0,
                      b.w.ratio=0,
                      weighted_bias = seq(-2.25,2.25,.25),
                      weighted_warmth = 0,
                      number=0,
                      total_number=1)
y_hat <- rstanarm::posterior_linpred(m, newdata = newdat, re.form=~0, transform=T)

cbind(newdat, t(y_hat)) %>% 
  mutate(index=row_number()) %>%
  gather(sample, value, `1`:`4000`) -> posterior_distribution

posterior_distribution %>%
  select(group, weighted_bias, sample, value) %>%
  spread(group, value) %>%
  mutate(ratio = black/white) %>%
  group_by(weighted_bias) %>%
  summarise(est = mean(ratio),
            lower = quantile(ratio, .025),
            upper = quantile(ratio, .975)) %>%
  ungroup() %>%
  mutate(bias = 'Implicit') %>%
  rbind(plot.dat) -> plot.dat

raw_rate %>%
  mutate(Implicit = weighted_bias,
         Explicit = weighted_warmth) %>%
  select(-weighted_bias, -weighted_warmth) %>%
  gather(bias, value, Implicit:Explicit) %>%
  select(county_id, nschools, bias, group, rate, value) %>%
  spread(group, rate) %>%
  mutate(odds_r = black/white) -> raw_or

ggplot(plot.dat, aes(x=weighted_bias, y=est)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
  theme_classic() +
  facet_wrap(~bias)

raw_or$mercer_county <- FALSE
raw_or$mercer_county[which(raw_or$county_id=='NJ-021')] <- TRUE

ggplot(plot.dat, aes(x=weighted_bias, y=est)) +
  geom_point(data=raw_or, aes(x=value, y=odds_r, size=nschools,
                              color=mercer_county, alpha=mercer_county)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
  theme_classic() +
  scale_color_manual(values=c('grey', 'darkorange'), guide=F) + 
  scale_alpha_manual(values=c(.15, 1), guide=F) +
  ylim(0,8) +
  xlab('Standardized Bias') +
  ylab('Relative Risk Ratio') +
  geom_hline(yintercept=1) +
  theme(strip.text = element_text(size=14),
        axis.title = element_text(face='bold', size=16),
        axis.text = element_text(size=14),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14)) +
  facet_wrap(~bias)

p$data %>% filter(weighted_bias%in%c(0,1)) %>% mutate(students = est*8000000)
plot.dat %>% filter(weighted_bias%in%c(0,1))

df <- data.frame(accounts = rep(c('African American', 'News'), each=4),
                 percent = c(49, 6, 37, 8, 8, 23, 38, 31),
                 coded = rep(c('African-American', 'Other', 'White', 'Not Codeable'), 2))

ggplot(df, aes(accounts, y=percent/100, fill=coded)) + 
  geom_bar(stat='identity') +
  theme_classic() +
  xlab('Source Account') +
  ylab('Proportion')

load('/Users/travis/Documents/gits/Ferguson_tweets/output/yeti/output/overall/24hr/consensus_model.rdata')

newdat <- expand.grid(pre_shooting=levels(m$data$pre_shooting),
                      group = levels(m$data$group),
                      hr = levels(m$data$hr),
                      screen_name = 'triddle42')

y_hat <- rstanarm::posterior_linpred(m, newdata = newdat, re.form=~0, transform=T)

cbind(newdat, t(y_hat)) %>% 
  mutate(index=row_number()) %>%
  gather(sample, value, `1`:`4000`) -> posterior_distribution

posterior_distribution %>%
  dplyr::select(pre_shooting, group, hr, sample, value, index) %>%
  group_by(pre_shooting, group, hr) %>%
  summarise(est = mean(value),
            lower = quantile(value, .025),
            upper = quantile(value, .975)) %>%
  ungroup() -> plot.dat

ggplot(plot.dat, aes(x=hr, y=est, group=pre_shooting)) +
  geom_line(aes(color=pre_shooting)) +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
  theme_classic() +
  xlab('Hour of day') +
  ylab('# tweets for typical person over 2 weeks') +
  facet_wrap(~group, ncol = 1) + 
  scale_x_discrete(breaks=seq(0, 23, 2)) + 
  theme(strip.text = element_text(size=14),
        axis.title = element_text(face='bold', size=16),
        axis.text = element_text(size=14),
        legend.title = element_blank(),
        legend.text = element_text(size=15),
        legend.position='top') -> plot

ggsave('/Users/travis/Desktop/tweets.jpeg', plot)

posterior_distribution %>%
  dplyr::select(pre_shooting, group, hr, sample, value) %>%
  spread(pre_shooting, value) %>%
  mutate(ratio = Post/Pre) %>%
  group_by(group, hr) %>%
  summarise(est = mean(ratio, na.rm=T),
            lower = quantile(ratio, .025, na.rm=T),
            upper = quantile(ratio, .975, na.rm=T)) %>%
  ungroup() -> plot.dat

ggplot(plot.dat, aes(x=hr, y=est, group=group)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
  theme_classic() +
  xlab('Hour of day') +
  ylab('Ratio of post/pre tweets') +
  facet_wrap(~group, ncol = 1) +
  scale_x_discrete(breaks=seq(0, 23, 2)) + 
  theme(strip.text = element_text(size=14),
        axis.title = element_text(face='bold', size=16),
        axis.text = element_text(size=14),
        legend.title = element_blank(),
        legend.text = element_text(size=15))-> plot

ggsave('/Users/travis/Desktop/tweet_ratio.jpeg', plot)


f <- list.files('/Users/travis/Documents/gits/Ferguson_tweets/output/yeti/output/geog_simple/')
j <- 1
posterior_combo <- array(0, dim=c(50,4000,80))
for (i in f){
  #print(i)
  load(paste('/Users/travis/Documents/gits/Ferguson_tweets/output/yeti/output/geog_simple/',i,sep=''))
  df <- as.matrix(m)
  posterior_combo[,,j] <- t(df[,c(1:49,dim(df)[2]-1)])
  j <- j+1
}

p_cons <- parallelMCMCcombine::consensusMCcov(posterior_combo, shuff = T)
#p_cons <- parallelMCMCcombine::consensusMCindep(posterior_combo, shuff = T)
#p_cons <- parallelMCMCcombine::sampleAvg(posterior_combo, shuff = T)

disp_ix <- length(m$stanfit@sim$samples[[1]])-3


for (i in 1:49){
  m$stanfit@sim$samples[[1]][[i]] <- p_cons[i,1:1000]
}
m$stanfit@sim$samples[[1]][[disp_ix]] <- p_cons[50,1:1000]
for (i in 1:49){
  m$stanfit@sim$samples[[2]][[i]] <- p_cons[i,1001:2000]
}
m$stanfit@sim$samples[[2]][[disp_ix]] <- p_cons[50,1001:2000]
for (i in 1:49){
  m$stanfit@sim$samples[[3]][[i]] <- p_cons[i,2001:3000]
}
m$stanfit@sim$samples[[3]][[disp_ix]] <- p_cons[50,2001:3000]
for (i in 1:49){
  m$stanfit@sim$samples[[4]][[i]] <- p_cons[i,3001:4000]
}
m$stanfit@sim$samples[[4]][[disp_ix]] <- p_cons[50,3001:4000]

newdat <- expand.grid(pre_shooting = rownames(table(m$data$pre_shooting)),
                      group = rownames(table(m$data$group)),
                      time = rownames(table(m$data$time)),
                      screen_name='triddle42',
                      dist=c(seq(0,18)))

y_hat <- rstanarm::posterior_linpred(m, newdata = newdat, re.form=~0, transform=T)
tempout <- cbind(newdat, t(y_hat))
tempout %>% gather(sample, value, `1`:`4000`) -> posterior_distribution
posterior_distribution %>% 
  filter(group=='African-American'|group=='News_Organization') %>%
  spread(pre_shooting, value) %>% 
  mutate(ratio = Post/Pre) %>% 
  group_by(group, time, dist) %>% 
  summarise(rate=mean(ratio), 
            lower=quantile(ratio, .025), 
            upper=quantile(ratio, .975)) %>% 
  ggplot(aes(x=dist, y=rate, group=time)) + 
  geom_line(aes(color=time, linetype=time), size=1.5) + 
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
  geom_hline(yintercept=1) +
  theme_classic() +
  facet_wrap(~group) + 
  ylab('Relative increase in tweet rate after the shooting') +
  theme(legend.position='top',
        legend.title=element_blank(),
        legend.text = element_text(size=12),
        axis.text = element_text(size=12),
        strip.text = element_text(size=12),
        axis.title = element_text(size=14, face='bold')) +
  xlab('Distance to Ferguson (in hundreds of miles)')

load('/Users/travis/Documents/gits/Ferguson_tweets/output/yeti/output/sentiment/consensus_model.rdata')

newdat <- expand.grid(pre_shooting=levels(m$data$pre_shooting),
                      group = levels(m$data$group),
                      screen_name = 'triddle42')

y_hat <- rstanarm::posterior_linpred(m, newdata = newdat, re.form=~0, transform=T)

cbind(newdat, t(y_hat)) %>% 
  mutate(index=row_number()) %>%
  gather(sample, value, `1`:`4000`) -> posterior_distribution

posterior_distribution %>%
  dplyr::select(pre_shooting, group, sample, value, index) %>%
  group_by(pre_shooting, group) %>%
  summarise(est = mean(value),
            lower = quantile(value, .025),
            upper = quantile(value, .975)) %>%
  ungroup() -> plot.dat

plot.dat$pre_shooting <- relevel(plot.dat$pre_shooting, 'Pre')

ggplot(plot.dat, aes(x=pre_shooting, y=est, group=group, color=group)) +
  geom_point(aes(color=group), size=2) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.6) +
  theme_classic() +
  ylab('Estimated Sentiment') +
  theme(axis.title.y = element_text(face='bold', size=16),
        axis.title.x = element_blank(),
        axis.text = element_text(size=14),
        legend.title = element_blank(),
        legend.text = element_text(size=15),
        legend.position='top') -> plot

df <- read.csv('/Users/travis/Documents/gits/Ferguson_tweets/output/yeti/data/overall_diurnal_changes.csv')
df.dist <- read.csv('/Users/travis/Documents/gits/Ferguson_tweets/output/yeti/data/geog_diurnal_changes.csv')

df %>% group_by(group) %>% summarise(ns = sum(tweets))
df %>% group_by(group) %>% summarise(ns = n_distinct(screen_name))

df.dist %>% group_by(group) %>% summarise(ns=sum(tweets))
df.dist %>% group_by(group) %>% summarise(ns = n_distinct(screen_name))



