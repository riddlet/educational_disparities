library(tidyr)
library(dplyr)
library(ggplot2)

#warmth: 0=very cold, 10=very warm
#bias: higher scores = more bias

base_path <- '/Users/travis/Documents/gits/educational_disparities/cluster/output/metrics_raw/'
plot.dat <- data.frame(group = NA, bias = NA, est = NA, lower = NA, upper=NA, metric = NA)
plot.dat2 <- data.frame(bias = NA, est = NA, lower=NA, upper=NA, metric=NA)
plot.dat3 <- data.frame(est=NA, lower=NA, upper=NA, metric=NA)


metrics <- list.files(base_path)
for (i in metrics){
  pth <- paste(base_path, i, '/', sep='')
  
  files <- list.files(pth)
  j <- 1
  posterior_combo <- array(0, dim=c(14,4000,length(files)))
  raw_rate <- data.frame(county_id=NA, nschools=NA, bias=NA, group=NA, nincidents=NA, nstudents=NA, rate=NA)
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
      left_join(m$data[, c('county_id', 'bias')]) %>%
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
                        bias = seq(-2,2,.25),
                        warmth = 0,
                        number=0,
                        total_number=1)
  y_hat <- rstanarm::posterior_linpred(m, newdata = newdat, re.form=~0, transform=T)
  
  cbind(newdat, t(y_hat)) %>% 
    mutate(index=row_number()) %>%
    gather(sample, value, `1`:`4000`) -> posterior_distribution
  
  
  posterior_distribution %>%
    select(group, bias, sample, value, index) %>%
    group_by(group, bias) %>%
    summarise(est = mean(value),
              lower = quantile(value, .025),
              upper = quantile(value, .975)) %>%
    ungroup() %>%
    mutate(metric=i) %>%
    rbind(plot.dat) %>% 
    filter(!is.na(group)) -> plot.dat
  
  ggplot(plot.dat, aes(x=bias, y=est, group=group)) +
    #geom_point(data=raw_rate, aes(x=bias, y=rate, group=group, color=group, size=nschools), alpha=.05)+
    geom_line(aes(color=group)) +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    #ylim(0,quantile(raw_rate$rate, .9))
    facet_wrap(~metric, scales = 'free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/bias.jpeg'
  
  ggsave(filename=plot_path, plot)
  
  ggplot(plot.dat, aes(x=bias, y=est, group=group)) +
    geom_point(data=raw_rate, aes(x=bias, y=rate, group=group, color=group, size=nschools), alpha=.05)+
    geom_line(aes(color=group)) +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    ylim(0,quantile(raw_rate$rate, .9)) +
    facet_wrap(~metric, scales = 'free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/bias_wdata.jpeg'
  
  ggsave(filename=plot_path, plot)
  
  posterior_distribution %>%
    select(group, bias, sample, value) %>%
    spread(group, value) %>%
    mutate(ratio = black/white) %>%
    select(bias, sample, ratio) %>%
    group_by(bias) %>%
    summarise(est = mean(ratio),
              lower = quantile(ratio, .025),
              upper = quantile(ratio, .975)) %>%
    ungroup() %>%
    mutate(metric=i) %>%
    rbind(plot.dat2) %>% 
    filter(!is.na(bias)) -> plot.dat2
  
  raw_rate %>% 
    select(county_id, nschools, bias, group, rate) %>% 
    spread(group, rate) %>%
    mutate(odds_r = black/white) -> raw_or
  
  ggplot(plot.dat2, aes(x=bias, y=est)) +
    geom_line() +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    #geom_point(data=raw_or, aes(x=bias, y=odds_r, size=nschools), alpha=.1) +
    #ylim(0,20)
    facet_wrap(~metric, scales='free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/bias_OR.jpeg'
  
  ggsave(filename=plot_path, plot)
  
  ggplot(plot.dat2, aes(x=bias, y=est)) +
    geom_point(data=raw_or, aes(x=bias, y=odds_r, size=nschools), alpha=.1) +
    geom_line() +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    ylim(0,quantile(raw_or$odds_r, na.rm=T, .9)) +
    facet_wrap(~metric, scales='free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/bias_OR_wdata.jpeg'
  
  ggsave(filename=plot_path, plot)  
  
  newdat <- expand.grid(group = rownames(table(m$data$group)),
                        total_pop=0,
                        unemp_rate=0,
                        med_income=0,
                        poverty_rate=0,
                        col_grads=0,
                        white_prop=0,
                        black_prop=0,
                        b.w.ratio=0,
                        bias = c(0,1),
                        warmth = 0,
                        number=0,
                        total_number=1)
  
  y_hat <- rstanarm::posterior_linpred(m, newdata = newdat, re.form=~0, transform=F)
  
  cbind(newdat, t(y_hat)) %>% 
    mutate(index=row_number()) %>%
    gather(sample, value, `1`:`4000`) -> posterior_distribution
  
  posterior_distribution %>%
    select(group, bias, sample, value) %>%
    spread(group, value) %>%
    mutate(ratio = black/white) %>%
    select(bias, sample, ratio) %>%
    spread(bias, ratio) %>%
    mutate(slope = `1`-`0`) %>%
    summarise(est = mean(slope),
              lower = quantile(slope, .025),
              upper = quantile(slope, .975)) %>%
    mutate(metric=i) %>%
    rbind(plot.dat3) %>% 
    filter(!is.na(est)) %>%
    mutate(metric=reorder(metric, est)) -> plot.dat3
  
    ggplot(plot.dat3, aes(x=metric, y=est)) +
    geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper)) +
    theme_classic() +
    coord_flip() +
    geom_hline(yintercept=0) -> plot
    
    plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/bias_OR_slope.jpeg'
    
    ggsave(filename=plot_path, plot)
  
}


plot.dat <- data.frame(group = NA, warmth = NA, est = NA, lower = NA, upper=NA, metric = NA)
plot.dat2 <- data.frame(warmth = NA, est = NA, lower=NA, upper=NA, metric=NA)
plot.dat3 <- data.frame(est=NA, lower=NA, upper=NA, metric=NA)

for (i in metrics){
  pth <- paste(base_path, i, '/', sep='')
  
  files <- list.files(pth)
  j <- 1
  posterior_combo <- array(0, dim=c(14,4000,length(files)))
  raw_rate <- data.frame(county_id=NA, nschools=NA, warmth=NA, group=NA, nincidents=NA, nstudents=NA, rate=NA)
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
      left_join(m$data[, c('county_id', 'warmth')]) %>%
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
                        bias = 0,
                        warmth = seq(-2,2,.25),
                        number=0,
                        total_number=1)
  y_hat <- rstanarm::posterior_linpred(m, newdata = newdat, re.form=~0, transform=T)
  
  cbind(newdat, t(y_hat)) %>% 
    mutate(index=row_number()) %>%
    gather(sample, value, `1`:`4000`) -> posterior_distribution
  
  posterior_distribution %>%
    select(group, warmth, sample, value, index) %>%
    group_by(group, warmth) %>%
    summarise(est = mean(value),
              lower = quantile(value, .025),
              upper = quantile(value, .975)) %>%
    ungroup() %>%
    mutate(metric=i) %>%
    rbind(plot.dat) %>% 
    filter(!is.na(group)) -> plot.dat
  
  ggplot(plot.dat, aes(x=warmth, y=est, group=group)) +
    geom_line(aes(color=group)) +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    facet_wrap(~metric, scales = 'free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/warmth.jpeg'
  
  ggsave(filename=plot_path, plot)
  
  ggplot(plot.dat, aes(x=warmth, y=est, group=group)) +
    geom_point(data=raw_rate, aes(x=warmth, y=rate, group=group, color=group, size=nschools), alpha=.05)+
    geom_line(aes(color=group)) +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    ylim(0,quantile(raw_rate$rate, .9)) +
    facet_wrap(~metric, scales = 'free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/warmth_wdata.jpeg'
  
  ggsave(filename=plot_path, plot)
  
  posterior_distribution %>%
    select(group, warmth, sample, value) %>%
    spread(group, value) %>%
    mutate(ratio = black/white) %>%
    select(warmth, sample, ratio) %>%
    group_by(warmth) %>%
    summarise(est = mean(ratio),
              lower = quantile(ratio, .025),
              upper = quantile(ratio, .975)) %>%
    ungroup() %>%
    mutate(metric=i) %>%
    rbind(plot.dat2) %>% 
    filter(!is.na(warmth)) -> plot.dat2
  
  raw_rate %>% 
    select(county_id, nschools, warmth, group, rate) %>% 
    spread(group, rate) %>%
    mutate(odds_r = black/white) -> raw_or
  
  ggplot(plot.dat2, aes(x=warmth, y=est)) +
    geom_line() +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    facet_wrap(~metric, scales='free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/warmth_OR.jpeg'
  
  ggsave(filename=plot_path, plot)
  
  ggplot(plot.dat2, aes(x=warmth, y=est)) +
    geom_point(data=raw_or, aes(x=warmth, y=odds_r, size=nschools), alpha=.1) +
    geom_line() +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    ylim(0,quantile(raw_or$odds_r, na.rm=T, .9)) +
    facet_wrap(~metric, scales='free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/warmth_OR_wdata.jpeg'
  
  ggsave(filename=plot_path, plot)  
  
  newdat <- expand.grid(group = rownames(table(m$data$group)),
                        total_pop=0,
                        unemp_rate=0,
                        med_income=0,
                        poverty_rate=0,
                        col_grads=0,
                        white_prop=0,
                        black_prop=0,
                        b.w.ratio=0,
                        bias = 0,
                        warmth = c(0,1),
                        number=0,
                        total_number=1)
  
  y_hat <- rstanarm::posterior_linpred(m, newdata = newdat, re.form=~0, transform=F)
  
  cbind(newdat, t(y_hat)) %>% 
    mutate(index=row_number()) %>%
    gather(sample, value, `1`:`4000`) -> posterior_distribution
  
  posterior_distribution %>%
    select(group, warmth, sample, value) %>%
    spread(group, value) %>%
    mutate(ratio = black/white) %>%
    select(warmth, sample, ratio) %>%
    spread(warmth, ratio) %>%
    mutate(slope = `1`-`0`) %>%
    summarise(est = mean(slope),
              lower = quantile(slope, .025),
              upper = quantile(slope, .975)) %>%
    mutate(metric=i) %>%
    rbind(plot.dat3) %>% 
    filter(!is.na(est)) %>%
    mutate(metric=reorder(metric, est)) -> plot.dat3
  
  ggplot(plot.dat3, aes(x=metric, y=est)) +
    geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper)) +
    theme_classic() +
    coord_flip() +
    geom_hline(yintercept=0) -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/warmth_OR_slope.jpeg'
  
  ggsave(filename=plot_path, plot)
}


posterior_distribution %>%
  select(group, warmth, sample, value) %>%
  spread(group, value) %>%
  mutate(ratio = black/white) %>%
  select(warmth, sample, ratio) %>%
  spread(warmth, ratio) %>%
  mutate(slope = `1`-`0`) %>%
  summarise(est = mean(slope),
            lower = quantile(slope, .025),
            upper = quantile(slope, .975)) %>%
  ggplot(aes(x=0, y=est)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper)) +
  theme_classic()



