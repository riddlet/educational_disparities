library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(rstanarm)
library(lme4)

options(mc.cores = parallel::detectCores())

district_content <- read.csv('../../Data/crdc201314csv/CRDC2013_14_LEA_content.csv')
df_district <- read.csv('../../Data/crdc201314csv/CRDC2013_14_LEA.csv')
school_content <- read.csv('../../Data/crdc201314csv/CRDC2013_14_SCH_content.csv')
df_school <- read.csv('../../Data/crdc201314csv/CRDC2013_14_SCH.csv')
state_means <- read.csv('/home/triddle/Data/state_means.csv')


# Get enrollment figures
df_school %>%
  select(COMBOKEY, LEA_STATE:SCH_NAME, SCH_ENR_HI_M:TOT_ENR_F) %>%
  gather(group, total_number, SCH_ENR_HI_M:TOT_ENR_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>%
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='NR_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> enrollment

################ 
# all metrics #

df_school %>% 
  select(COMBOKEY, LEA_STATE:SCH_NAME, SCH_CORPINSTANCES_IND:TOT_DISCWODIS_CORP_F) %>% 
  filter(SCH_CORPINSTANCES_IND=='YES') %>%
  gather(group, number, SCH_DISCWODIS_CORP_HI_M:TOT_DISCWODIS_CORP_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>%
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='RP_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> corporal

corporal %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, SCH_NAME, group) %>%
  summarise(number = sum(number),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  mutate(group=droplevels(group)) %>%
  ungroup() %>%
  mutate(metric='corporal') %>%
  mutate(LEA_STATE=droplevels(LEA_STATE)) -> subdat

df_school %>% 
  select(COMBOKEY, LEA_STATE:SCH_NAME, SCH_GRADE_PS, SCH_PSDISC_SINGOOS_HI_M:TOT_PSDISC_SINGOOS_F) %>% 
  filter(SCH_GRADE_PS=='YES') %>%
  gather(group, number, SCH_PSDISC_SINGOOS_HI_M:TOT_PSDISC_SINGOOS_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>%
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='OS_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> ps_susp

df_school %>% 
  select(COMBOKEY, LEA_STATE:SCH_NAME, SCH_GRADE_PS, SCH_PSDISC_MULTOOS_HI_M:TOT_PSDISC_MULTOOS_F) %>% 
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
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, SCH_NAME, group) %>%
  summarise(number = sum(number),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  mutate(group=droplevels(group)) %>%
  ungroup() %>%
  mutate(metric='preschool_susp') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

df_school %>% 
  select(COMBOKEY, LEA_STATE:SCH_NAME, SCH_DISCWODIS_ISS_HI_M:TOT_DISCWODIS_ISS_F) %>% 
  gather(group, number, SCH_DISCWODIS_ISS_HI_M:TOT_DISCWODIS_ISS_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='SS_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> susp_inschool

susp_inschool %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, SCH_NAME, group) %>%
  summarise(number = sum(number),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  mutate(group=droplevels(group)) %>%
  ungroup() %>%
  mutate(metric='inschool_susp') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

df_school %>% 
  select(COMBOKEY, LEA_STATE:SCH_NAME, SCH_DISCWODIS_SINGOOS_HI_M:TOT_DISCWODIS_SINGOOS_F) %>% 
  gather(group, number, SCH_DISCWODIS_SINGOOS_HI_M:TOT_DISCWODIS_SINGOOS_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>%
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='OS_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> oos_susp

df_school %>% 
  select(COMBOKEY, LEA_STATE:SCH_NAME, SCH_DISCWODIS_MULTOOS_HI_M:TOT_DISCWODIS_MULTOOS_F) %>% 
  gather(group, number_2, SCH_DISCWODIS_MULTOOS_HI_M:TOT_DISCWODIS_MULTOOS_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>%
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='OS_', 
                          biracial='TR_', white='WH_')) %>%
  select(COMBOKEY, group, gender, number_2) %>%
  right_join(oos_susp) %>%
  filter(number>=0 & number_2 >= 0) %>%
  mutate(number = number+number_2) -> oos_susp

oos_susp %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, SCH_NAME, group) %>%
  summarise(number = sum(number),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  mutate(group=droplevels(group)) %>%
  ungroup() %>%
  mutate(metric='oos_susp') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

df_school %>% 
  select(COMBOKEY, LEA_STATE:SCH_NAME, SCH_PSDISC_EXP_HI_M:TOT_PSDISC_EXP_F) %>% 
  gather(group, number, SCH_PSDISC_EXP_HI_M:TOT_PSDISC_EXP_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='XP_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> ps_exp

ps_exp %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, SCH_NAME, group) %>%
  summarise(number = sum(number),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  ungroup() %>%
  mutate(metric='preschool_expulsion') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

df_school %>% 
  select(COMBOKEY, LEA_STATE:SCH_NAME, SCH_DISCWODIS_EXPWE_HI_M:TOT_DISCWODIS_EXPWE_F) %>% 
  gather(group, number, SCH_DISCWODIS_EXPWE_HI_M:TOT_DISCWODIS_EXPWE_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='WE_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> exp_w_ed

exp_w_ed %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, SCH_NAME, group) %>%
  summarise(number = sum(number),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  ungroup() %>%
  mutate(metric='expulsion_w_ed') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

df_school %>% 
  select(COMBOKEY, LEA_STATE:SCH_NAME, SCH_DISCWODIS_EXPWOE_HI_M:TOT_DISCWODIS_EXPWOE_F) %>% 
  gather(group, number, SCH_DISCWODIS_EXPWOE_HI_M:TOT_DISCWODIS_EXPWOE_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='OE_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> exp_wo_ed

exp_wo_ed %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, SCH_NAME, group) %>%
  summarise(number = sum(number),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  mutate(group=droplevels(group)) %>%
  ungroup() %>%
  mutate(metric='expulsion_wo_ed') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

df_school %>% 
  select(COMBOKEY, LEA_STATE:SCH_NAME, SCH_DISCWODIS_EXPZT_HI_M:TOT_DISCWODIS_EXPZT_F) %>% 
  gather(group, number, SCH_DISCWODIS_EXPZT_HI_M:TOT_DISCWODIS_EXPZT_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='ZT_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> exp_zero

exp_zero %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, SCH_NAME, group) %>%
  summarise(number = sum(number),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  ungroup() %>%
  mutate(metric='expulsion_0_tolerance') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

df_school %>% 
  select(COMBOKEY, LEA_STATE:SCH_NAME, SCH_DISCWODIS_REF_HI_M:TOT_DISCWODIS_REF_F) %>% 
  gather(group, number, SCH_DISCWODIS_REF_HI_M:TOT_DISCWODIS_REF_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='EF_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> law_enf

law_enf %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, SCH_NAME, group) %>%
  summarise(number = sum(number),
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
  select(COMBOKEY, LEA_STATE:SCH_NAME, SCH_DISCWODIS_ARR_HI_M:TOT_DISCWODIS_ARR_F) %>% 
  gather(group, number, SCH_DISCWODIS_ARR_HI_M:TOT_DISCWODIS_ARR_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='RR_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> in_school_arrest

in_school_arrest %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, SCH_NAME, group) %>%
  summarise(number = sum(number),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  mutate(group=droplevels(group)) %>%
  ungroup() %>%
  mutate(metric='in_school_arrest') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

df_school %>% 
  select(COMBOKEY, LEA_STATE:SCH_NAME, SCH_RS_NONIDEA_MECH_HI_M:TOT_RS_NONIDEA_MECH_F) %>% 
  gather(group, number, SCH_RS_NONIDEA_MECH_HI_M:TOT_RS_NONIDEA_MECH_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='CH_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> mech_rest

mech_rest %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, SCH_NAME, group) %>%
  summarise(number = sum(number),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  ungroup() %>%
  mutate(metric='mechanical_restraint') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

df_school %>% 
  select(COMBOKEY, LEA_STATE:SCH_NAME, SCH_RS_NONIDEA_PHYS_HI_M:TOT_RS_NONIDEA_PHYS_F) %>% 
  gather(group, number, SCH_RS_NONIDEA_PHYS_HI_M:TOT_RS_NONIDEA_PHYS_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='YS_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> phys_rest

phys_rest %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, SCH_NAME, group) %>%
  summarise(number = sum(number),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  ungroup() %>%
  mutate(metric='physical_restraint') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

df_school %>% 
  select(COMBOKEY, LEA_STATE:SCH_NAME, SCH_RS_NONIDEA_SECL_HI_M:TOT_RS_NONIDEA_SECL_F) %>% 
  gather(group, number, SCH_RS_NONIDEA_SECL_HI_M:TOT_RS_NONIDEA_SECL_F) %>%
  separate(group, into=c('group', 'gender'), -2) %>% 
  separate(group, into=c('prefix', 'group'), -4) %>%
  mutate(group=fct_recode(group, am_indian='AM_', asian='AS_', black='BL_',
                          hispanic='HI_', pac_isl='HP_', total='CL_', 
                          biracial='TR_', white='WH_')) %>%
  select(-prefix) -> seclusion

seclusion %>%
  left_join(enrollment) %>%
  group_by(COMBOKEY, LEA_STATE, LEA_NAME, SCH_NAME, group) %>%
  summarise(number = sum(number),
            total_number = sum(total_number)) %>%
  mutate(proportion = number/total_number) -> full_dat

full_dat %>% 
  filter(group=='black'|group=='white') %>%
  filter(is.finite(proportion)) %>%
  filter(number>=0) %>%
  filter(total_number>number) %>%
  ungroup() %>%
  mutate(metric='seclusion') %>%
  mutate(LEA_STATE = droplevels(LEA_STATE)) %>%
  rbind(subdat) -> subdat

state_means %>%
  filter(raceomb=='White') %>%
  mutate(LEA_STATE=STATE) %>%
  select(LEA_STATE, bias, warmth) %>%
  mutate(bias = scale(bias)[,1],
         warmth = scale(warmth)[,1]) %>%
  right_join(subdat) -> model_dat

write.csv(model_dat, file='output/full_model_data.csv', row.names = FALSE)
model_dat <- read.csv('output/full_model_data.csv')

###
fulldat_bias <- NA
bias_models <- NA
modelb_list <- list()
for(i in rownames(table(model_dat$metric))){
  print(i)
  model_dat %>%
    filter(metric==i) -> subdat
  m <- glmer(cbind(number, total_number-number) ~ group*bias + (group|LEA_STATE),
             data=subdat, family='binomial')
  modelb_list[[length(modelb_list)+1]] <- m
  subdat$lmer_yhat_fixed <- predict(m, re.form=NA, type='response')
  subdat$lmer_yhat_s <- predict(m, re.form=~(group|LEA_STATE), type='response')
  temp <- broom::tidy(m)
  temp$var <- i
  bias_models <- rbind(bias_models, temp)
  fulldat_bias <- rbind(fulldat_bias, subdat)
}

fulldat_warmth <- NA
warmth_models <- NA
modelw_list <- list()
for(i in rownames(table(model_dat$metric))){
  print(i)
  model_dat %>%
    filter(metric==i) -> subdat
  m <- glmer(cbind(number, total_number-number) ~ group*warmth + (group|LEA_STATE),
             data=subdat, family='binomial')
  modelw_list[[length(modelw_list)+1]] <- m
  subdat$lmer_yhat_fixed <- predict(m, re.form=NA, type='response')
  subdat$lmer_yhat_s <- predict(m, re.form=~(group|LEA_STATE), type='response')
  temp <- broom::tidy(m)
  temp$var <- i
  warmth_models <- rbind(warmth_models, temp)
  fulldat_warmth <- rbind(fulldat_warmth, subdat)
}

#fulldat_warmth <- fulldat_warmth[-1,]
#fulldat_bias <- fulldat_bias[-1,]

p <- ggplot(fulldat_warmth, aes(x=warmth, y=lmer_yhat_fixed, color=group, group=group)) + 
  geom_line() + 
  facet_wrap(~metric, scales = 'free') +
  theme_classic()

ggsave('../figs/individual_models/warmth.jpeg', p)
p <- ggplot(fulldat_bias, aes(x=bias, y=lmer_yhat_fixed, color=group, group=group)) + 
  geom_line() + 
  facet_wrap(~metric, scales = 'free') +
  theme_classic()

ggsave('../figs/individual_models/bias.jpeg', p)

load('output/meanfield/meanfield_model_statemeans.rdata')

m.bias <- stan_glmer(cbind(number, total_number-number) ~ group*bias + 
                  (group|LEA_STATE) + (group*bias|metric), data=model_dat, 
                  family='binomial', prior = normal(0,1), 
                  prior_intercept = normal(0,1), algorithm='meanfield')

m.warmth <- stan_glmer(cbind(number, total_number-number) ~ group*warmth + 
                  (group|LEA_STATE) + (group*warmth|metric), data=model_dat, 
                  family='binomial', prior = normal(0,1), 
                  prior_intercept = normal(0,1), algorithm='meanfield')


fulldat %>%
  select(group, LEA_STATE, metric) %>%
  distinct() %>%
  mutate(warmth_l2=-2,
         warmth_l1=-1,
         warmth_0=0,
         warmth_1=1,
         warmth_2=2) %>%
  gather(label, warmth, warmth_l2:warmth_2) %>%
  select(-label) %>%
  filter(!is.na(group))-> newdat

newdat$index <- as.character(seq(1, length(newdat$group)))

temp <- as.data.frame(
  posterior_linpred(m.warmth, newdata = newdat, re.form=~(group*warmth|metric),
                    transform=T))
temp$sample <- seq(1, length(temp$`1`))

temp %>%
  gather(index, prediction, -sample) %>%
  left_join(newdat) %>%
  group_by(group, metric, warmth) %>%
  summarise(est = mean(prediction),
            lower = quantile(prediction, .025),
            upper = quantile(prediction, .975)) -> plot.dat

p <- ggplot(plot.dat, aes(x=warmth, y=est, color=group, group=group)) + 
  geom_line() + 
  geom_errorbar(aes(ymin=lower, ymax=upper)) + 
  facet_wrap(~metric, scales = 'free') +
  theme_classic()

ggsave('../figs/individual_models/warmth_meanfield.jpeg', p)

fulldat %>%
  select(group, LEA_STATE, metric) %>%
  distinct() %>%
  mutate(bias_l2=-2,
         bias_l1=-1,
         bias_0=0,
         bias_1=1,
         bias_2=2) %>%
  gather(label, bias, bias_l2:bias_2) %>%
  select(-label) %>%
  filter(!is.na(group))-> newdat

newdat$index <- as.character(seq(1, length(newdat$group)))

temp <- as.data.frame(
  posterior_linpred(m.bias, newdata = newdat, re.form=~(group*bias|metric),
                    transform=T))
temp$sample <- seq(1, length(temp$`1`))

temp %>%
  gather(index, prediction, -sample) %>%
  left_join(newdat) %>%
  group_by(group, metric, bias) %>%
  summarise(est = mean(prediction),
            lower = quantile(prediction, .025),
            upper = quantile(prediction, .975)) -> plot.dat

p <- ggplot(plot.dat, aes(x=bias, y=est, color=group, group=group)) + 
  geom_line() + 
  geom_errorbar(aes(ymin=lower, ymax=upper)) + 
  facet_wrap(~metric, scales = 'free') +
  theme_classic()

ggsave('../figs/individual_models/bias_meanfield.jpeg', p)

slopes <- data.frame()
for(i in 1:length(modelw_list)){
  sl <- summary(lsmeans(modelw_list[[i]], ~warmth|group, var='warmth', cov.reduce=F), type='response')
  sl$metric <- rownames(table(model_dat$metric))[i]
  slopes <- rbind(slopes, as.data.frame(sl))
}

p <- ggplot(slopes, aes(x=warmth, y=prob, group=group, color=group)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL), alpha=.1, color='white') + 
  theme_classic() +
  facet_wrap(~metric, scales='free') +
  theme(legend.position = 'top')

ggsave('../figs/individual_models/warmth_se.jpeg', p)

slopes <- data.frame()
for(i in 1:length(modelb_list)){
  sl <- summary(lsmeans(modelb_list[[i]], ~bias|group, var='bias', cov.reduce=F), type='response')
  sl$metric <- rownames(table(model_dat$metric))[i]
  slopes <- rbind(slopes, as.data.frame(sl))
}

p <- ggplot(slopes, aes(x=bias, y=prob, group=group, color=group)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL), alpha=.1, color='white') + 
  theme_classic() +
  facet_wrap(~metric, scales='free') +
  theme(legend.position = 'top')

ggsave('../figs/individual_models/bias_se.jpeg', p)

slopes <- data.frame()
for(i in 1:length(modelb_list)){
  sl <- summary(lstrends(modelb_list[[i]], ~group, var='bias'))
  sl$metric <- rownames(table(model_dat$metric))[i]
  slopes <- rbind(slopes, as.data.frame(sl))
}

ggplot(slopes, aes(x=metric, y=bias.trend, group=group, color=group)) + 
  geom_point() + 
  geom_errorbar(aes(ymax=asymp.UCL, ymin=asymp.LCL)) +
  coord_flip() +
  theme_classic()


slopes <- data.frame()
for(i in 1:length(modelw_list)){
  sl <- summary(lstrends(modelw_list[[i]], ~group, var='warmth'))
  sl$metric <- rownames(table(model_dat$metric))[i]
  slopes <- rbind(slopes, as.data.frame(sl))
}

ggplot(slopes, aes(x=metric, y=warmth.trend, group=group, color=group)) + 
  geom_point() + 
  geom_errorbar(aes(ymax=asymp.UCL, ymin=asymp.LCL)) +
  coord_flip() +
  theme_classic()

state_teacher_means <- read.csv('../output/state_teacher_means.csv')
state_teacher_means %>%
  filter(raceomb=='White',
         occupation=='Primary, Secondary, & SpEd teachers') %>%
  mutate(LEA_STATE=STATE) %>%
  select(LEA_STATE, bias, warmth) %>%
  mutate(bias = scale(bias)[,1],
         warmth = scale(warmth)[,1]) %>%
  right_join(subdat) -> model_dat

###
fulldat_bias_t <- NA
bias_models_t <- NA
modelb_list_t <- list()
for(i in rownames(table(model_dat$metric))){
  print(i)
  model_dat %>%
    filter(metric==i) -> subdat
  m <- glmer(cbind(number, total_number-number) ~ group*bias + (group|LEA_STATE),
             data=subdat, family='binomial')
  modelb_list_t[[length(modelb_list_t)+1]] <- m
  subdat$lmer_yhat_fixed <- predict(m, re.form=NA, type='response')
  subdat$lmer_yhat_s <- predict(m, re.form=~(group|LEA_STATE), type='response')
  temp <- broom::tidy(m)
  temp$var <- i
  bias_models_t <- rbind(bias_models_t, temp)
  fulldat_bias_t <- rbind(fulldat_bias_t, subdat)
}

fulldat_warmth_t <- NA
warmth_models_t <- NA
modelw_list_t <- list()
for(i in rownames(table(model_dat$metric))){
  print(i)
  model_dat %>%
    filter(metric==i) -> subdat
  m <- glmer(cbind(number, total_number-number) ~ group*warmth + (group|LEA_STATE),
             data=subdat, family='binomial')
  modelw_list_t[[length(modelw_list_t)+1]] <- m
  subdat$lmer_yhat_fixed <- predict(m, re.form=NA, type='response')
  subdat$lmer_yhat_s <- predict(m, re.form=~(group|LEA_STATE), type='response')
  temp <- broom::tidy(m)
  temp$var <- i
  warmth_models_t <- rbind(warmth_models_t, temp)
  fulldat_warmth_t <- rbind(fulldat_warmth_t, subdat)
}

fulldat_warmth_t <- fulldat_warmth_t[-1,]
fulldat_bias_t <- fulldat_bias_t[-1,]

p <- ggplot(fulldat_warmth_t, aes(x=warmth, y=lmer_yhat_fixed, color=group, group=group)) + 
  geom_line() + 
  facet_wrap(~metric, scales = 'free') +
  theme_classic()

ggsave('../figs/individual_models/warmth_teachers.jpeg', p)
p <- ggplot(fulldat_bias_t, aes(x=bias, y=lmer_yhat_fixed, color=group, group=group)) + 
  geom_line() + 
  facet_wrap(~metric, scales = 'free') +
  theme_classic()

ggsave('../figs/individual_models/bias_teachers.jpeg', p)

slopes <- data.frame()
for(i in 1:length(modelw_list_t)){
  sl <- summary(lsmeans(modelw_list_t[[i]], ~warmth|group, var='warmth', cov.reduce=F), type='response')
  sl$metric <- rownames(table(model_dat$metric))[i]
  slopes <- rbind(slopes, as.data.frame(sl))
}

p <- ggplot(slopes, aes(x=warmth, y=prob, group=group, color=group)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL), alpha=.1, color='white') + 
  theme_classic() +
  facet_wrap(~metric, scales='free') +
  theme(legend.position = 'bottom') +
  ggtitle('teachers')

ggsave('../figs/individual_models/warmth_se_t.jpeg', p)

slopes <- data.frame()
for(i in 1:length(modelb_list_t)){
  print(i)
  sl <- summary(lsmeans(modelb_list_t[[i]], ~bias|group, var='bias', cov.reduce=F), type='response')
  sl$metric <- rownames(table(model_dat$metric))[i]
  slopes <- rbind(slopes, as.data.frame(sl))
}

p <- ggplot(slopes, aes(x=bias, y=prob, group=group, color=group)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL), alpha=.1, color='white') + 
  theme_classic() +
  facet_wrap(~metric, scales='free') +
  theme(legend.position = 'bottom') +
  ggtitle('teachers')

ggsave('../figs/individual_models/bias_se_t.jpeg', p)

slopes <- data.frame()
for(i in 1:length(modelb_list_t)){
  print(i)
  sl <- summary(lstrends(modelb_list_t[[i]], ~group, var='bias'))
  sl$metric <- rownames(table(model_dat$metric))[i]
  slopes <- rbind(slopes, as.data.frame(sl))
}

ggplot(slopes, aes(x=metric, y=bias.trend, group=group, color=group)) + 
  geom_point() + 
  geom_errorbar(aes(ymax=asymp.UCL, ymin=asymp.LCL)) +
  coord_flip() +
  theme_classic()


slopes <- data.frame()
for(i in 1:length(modelw_list_t)){
  print(i)
  sl <- summary(lstrends(modelw_list_t[[i]], ~group, var='warmth'))
  sl$metric <- rownames(table(model_dat$metric))[i]
  slopes <- rbind(slopes, as.data.frame(sl))
}

ggplot(slopes, aes(x=metric, y=warmth.trend, group=group, color=group)) + 
  geom_point() + 
  geom_errorbar(aes(ymax=asymp.UCL, ymin=asymp.LCL)) +
  coord_flip() +
  theme_classic()
