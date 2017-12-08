library(rstanarm)
library(dplyr)
library(tidyr)
library(ggplot2)
set.seed(42)

df <- read.csv('cluster/data/cross_val_grouping.csv')
f <- list.files('output/cross_val/')
summary_dat <- data.frame(file=f, 
                          r_2 = NA,
                          r_2_lower=NA,
                          r_2_upper=NA)
########################### in-sample performance
j=1
for (i in f){
  print(i)
  load(paste('output/cross_val/', i, sep=''))
  y_hat <- data.frame(t(posterior_predict(m, draws = 250)))
  y_hat$index <- seq(1, length(y_hat$X1))
  dat <- m$data
  dat <- cbind(dat, y_hat)
  dat %>%
    gather(sample, pred, X1:X250) %>%
    group_by(sample) %>%
    summarise(r = cor(number, pred)^2) %>%
    summarise(r_2 = mean(r),
              r_2_lower = quantile(r, .25),
              r_2_upper = quantile(r, .75)) -> r2
  summary_dat[j,2:4] <- r2
  j = j+1
}


plot.r2 <- cor(dat$number, dat$X1)^2
p <- ggplot(dat, aes(x=number, X1)) +
  geom_point() + 
  theme_classic() +
  ylab('Oberved number of disciplinary incidents') +
  xlab('Predicted number of discinplinary incidents') +
  ggtitle('One in-sample posterior prediction from one model', 
          paste('R-squared = ',round(plot.r2,2), sep=''))

ggsave(p, filename='figs/cross_val_gap/in_sample_pred_onemodel.jpeg')

summary_dat$variance <- summary_dat$r_2_upper-summary_dat$r_2_lower

p <- ggplot(summary_dat, aes(x=r_2, y=variance)) + 
  geom_point() +
  theme_classic() +
  ylab('Predictive variance (interquartile range)') +
  xlab('Model R-squared') +
  ggtitle('In-sample predictive performance')

ggsave(p, filename='figs/cross_val_gap/in_sample_perf.jpeg')

########################### out of sample performance
groups <- unique(df$grouping)
test_summary_dat <- data.frame(file=f, 
                               r_2 = NA,
                               r_2_lower=NA,
                               r_2_upper=NA)
j=1
for (i in f){
  print(i)
  load(paste('output/cross_val/', i, sep=''))
  dat <- m$data
  test_groups <- groups[groups!=dat$grouping[1]]
  df %>%
    filter(grouping %in% sample(test_groups, 10)) -> testing_data
  y_hat <- data.frame(t(posterior_predict(m, draws = 250, newdata = testing_data)))
  y_hat$index <- seq(1, length(y_hat$X1))
  testing_data <- cbind(testing_data, y_hat)
  testing_data %>%
    gather(sample, pred, X1:X250) %>%
    group_by(sample) %>%
    summarise(r = cor(number, pred)^2) %>%
    summarise(r_2 = mean(r),
              r_2_lower = quantile(r, .25),
              r_2_upper = quantile(r, .75)) -> r2
  test_summary_dat[j,2:4] <- r2
  j = j+1
}

plot.r2 <- cor(testing_data$number, testing_data$X1)^2
p <- ggplot(testing_data, aes(x=number, X1)) +
  geom_point() + 
  theme_classic() +
  ylab('Oberved number of disciplinary incidents') +
  xlab('Predicted number of disciplinary incidents') +
  ggtitle('One out-of-sample posterior prediction from one model', 
          paste('R-squared = ',round(plot.r2,2), sep=''))

ggsave(p, filename='figs/cross_val_gap/out_of_sample_pred_onemodel.jpeg')

test_summary_dat$variance <- test_summary_dat$r_2_upper-test_summary_dat$r_2_lower

p <- ggplot(test_summary_dat, aes(x=r_2, y=variance)) + 
  geom_point() +
  theme_classic() + 
  ylab('Predictive variance (interquartile range)') +
  xlab('Model R-squared') +
  ggtitle('Out-of-sample predictive performance')

ggsave(p, filename='figs/cross_val_gap/out_of_sample_perf.jpeg')

test_summary_dat$weighted_perf <- test_summary_dat$r_2*(1-test_summary_dat$variance)

test_summary_dat %>% 
  mutate(weighted_perf = r_2*(1-variance)) %>%
  arrange(desc(weighted_perf)) -> selected_model

########################### Model summary (fixed effect)
load(paste('output/cross_val/',selected_model$file[1],sep=''))

newdat <- expand.grid(group = rownames(table(m$data$group)))
newdat$index <- as.character(seq(1, length(newdat$group)))
temp <- as.data.frame(
  posterior_linpred(m, re.form = ~0, 
                    newdata = newdat, transform=T))
temp$sample <- seq(1, length(temp$`1`))

temp %>%
  gather(index, prediction, -sample) %>%
  left_join(newdat) %>%
  group_by(group) %>%
  summarise(est = mean(prediction),
            lower = quantile(prediction, .025),
            upper = quantile(prediction, .975)) -> plot.dat

samples <- sample(1:4000, 25)

temp %>%
  gather(index, prediction, -sample) %>%
  left_join(newdat) %>%
  filter(sample %in% sample(sample, 25)) -> plot.dat2

p_black <- prop.table(table(temp$`1`>temp$`2`))[2]
p <- ggplot(plot.dat, aes(x=group, y=est)) +
  geom_point(size=3) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.25) +
  geom_line(data=plot.dat2, aes(x=group, y=prediction, group=sample), alpha=.2) +
  theme_classic() +
  ylab('Estimated Disciplinary Rate') +
  ggtitle('Disciplinary rate, marginalizing across state & metric',
          paste(round(p_black,3)*100,'% of the posterior samples are consistent 
with Black students receiving more disciplinary action', sep=''))

ggsave(p, filename='figs/cross_val_gap/overall_discipline_1model.jpeg')

all_models <- data.frame()
for (i in f){
  print(i)
  load(paste('output/cross_val/', i, sep=''))
  temp <- as.data.frame(
    posterior_linpred(m, re.form = ~0, 
                      newdata = newdat, transform=T))
  temp$sample <- seq(1, length(temp$`1`))
  
  temp %>%
    gather(index, prediction, -sample) %>%
    left_join(newdat) %>%
    group_by(group) %>%
    summarise(est = mean(prediction),
              lower = quantile(prediction, .025),
              upper = quantile(prediction, .975)) -> out
  out$model <- i
  all_models <- rbind(all_models, out)
}

all_models %>%
  select(group, est, model) %>%
  spread(group, est) -> out

blacks_higher <- table(out$black>out$white)

p <- ggplot(plot.dat, aes(x=group, y=est)) +
  geom_point(size=3) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.25) +
  geom_line(data=all_models, aes(x=group, y=est, group=model), alpha=.2) +
  theme_classic() +
  ylab('Estimated Disciplinary Rate') +
  ggtitle('Disciplinary rate, marginalizing across state & metric',
          paste(blacks_higher,' of ', sum(blacks_higher), 
                ' models indicate blacks punished more frequently', sep=''))

ggsave(p, filename='figs/cross_val_gap/overall_discipline_all_models.jpeg')


########################### state-level effect
load(paste('output/cross_val/',selected_model$file[1],sep=''))

newdat <- expand.grid(group = rownames(table(m$data$group)),
                      LEA_STATE=rownames(table(m$data$LEA_STATE)),
                      metric=rownames(table(m$data$metric)),
                      COMBOKEY=rownames(table(m$data$metric)))
newdat$index <- as.character(seq(1, length(newdat$group)))
temp <- as.data.frame(
  posterior_linpred(m, re.form = ~(group|LEA_STATE), 
                    newdata = newdat, transform=T))
temp$sample <- seq(1, length(temp$`1`))

temp %>%
  gather(index, prediction, -sample) %>%
  left_join(newdat) %>%
  group_by(group, LEA_STATE) %>%
  summarise(est = mean(prediction),
            lower = quantile(prediction, .025),
            upper = quantile(prediction, .975)) -> plot.dat

plot.dat %>%
  filter(group=='black') %>%
  mutate(LEA_STATE = reorder(LEA_STATE, est)) %>%
  rbind(plot.dat[which(plot.dat$group=='white'),]) -> plot.dat

p <- ggplot(plot.dat, aes(x=LEA_STATE, y=est, group=group, alpha=group)) +
  geom_point(size=2, position=position_dodge(width=.75)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.25, position=position_dodge(width=.75)) +
  coord_flip() +
  theme_classic() +
  scale_alpha_manual(values=c('white' = .25, 'black'=1)) +
  ylab('Estimated Disciplinary Rate') +
  xlab('State') +
  ggtitle('Disciplinary rate, marginalizing across metric')

ggsave(p, filename='figs/cross_val_gap/state_discipline.jpeg')

temp %>%
  gather(index, prediction, -sample) %>%
  left_join(newdat) %>%
  select(sample, prediction, group, LEA_STATE) %>%
  distinct() %>%
  spread(group, prediction) %>%
  mutate(or = black/white) %>%
  group_by(LEA_STATE) %>%
  summarise(est = mean(or),
            lower = quantile(or, .025),
            upper = quantile(or, .975)) -> plot.dat

plot.dat %>%
  mutate(LEA_STATE = reorder(LEA_STATE, est)) -> plot.dat

p <- ggplot(plot.dat, aes(x=LEA_STATE, y=est)) +
  geom_point(size=2, position=position_dodge(width=.75)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.25, position=position_dodge(width=.75)) +
  coord_flip() +
  theme_classic() +
  ylab('Odds ratio for punishing black students, relative to whites') +
  xlab('State') +
  ggtitle('Disciplinary rate gap as odds ratio (p_black/p_white), marginalizing across metric') +
  geom_hline(yintercept=1)

ggsave(p, filename='figs/cross_val_gap/state_discipline_gap.jpeg')

########################### metric-level effect
load(paste('output/cross_val/',selected_model$file[1],sep=''))

newdat <- expand.grid(group = rownames(table(m$data$group)),
                      LEA_STATE=rownames(table(m$data$LEA_STATE)),
                      metric=rownames(table(m$data$metric)),
                      COMBOKEY=rownames(table(m$data$metric)))
newdat$index <- as.character(seq(1, length(newdat$group)))
temp <- as.data.frame(
  posterior_linpred(m, re.form = ~(group|metric), 
                    newdata = newdat, transform=T))
temp$sample <- seq(1, length(temp$`1`))

temp %>%
  gather(index, prediction, -sample) %>%
  left_join(newdat) %>%
  group_by(group, metric) %>%
  summarise(est = mean(prediction),
            lower = quantile(prediction, .025),
            upper = quantile(prediction, .975)) -> plot.dat

plot.dat %>%
  filter(group=='black') %>%
  mutate(metric = reorder(metric, est)) %>%
  rbind(plot.dat[which(plot.dat$group=='white'),]) -> plot.dat

p <- ggplot(plot.dat, aes(x=metric, y=est, group=group, alpha=group)) +
  geom_point(size=2, position=position_dodge(width=.75)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.25, position=position_dodge(width=.75)) +
  coord_flip() +
  theme_classic() +
  scale_alpha_manual(values=c('white' = .25, 'black'=1)) +
  ylab('Estimated Disciplinary Rate') +
  xlab('metric') +
  ggtitle('Disciplinary rate, marginalizing across state')

ggsave(p, filename='figs/cross_val_gap/metric_discipline.jpeg')

temp %>%
  gather(index, prediction, -sample) %>%
  left_join(newdat) %>%
  select(sample, prediction, group, metric) %>%
  distinct() %>%
  spread(group, prediction) %>%
  mutate(or = black/white) %>%
  group_by(metric) %>%
  summarise(est = mean(or),
            lower = quantile(or, .025),
            upper = quantile(or, .975)) -> plot.dat

plot.dat %>%
  mutate(metric = reorder(metric, est)) -> plot.dat

p <- ggplot(plot.dat, aes(x=metric, y=est)) +
  geom_point(size=2, position=position_dodge(width=.75)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.25, position=position_dodge(width=.75)) +
  coord_flip() +
  theme_classic() +
  ylab('Odds ratio for punishing black students, relative to whites') +
  xlab('metric') +
  ggtitle('Disciplinary rate gap as odds ratio (p_black/p_white), marginalizing across state') +
  geom_hline(yintercept=1)

ggsave(p, filename='figs/cross_val_gap/metric_discipline_gap.jpeg')
