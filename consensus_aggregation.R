library(tidyr)
library(dplyr)
library(ggplot2)

#warmth: 0=very cold, 10=very warm
#bias: higher scores = more bias

base_path <- '/Users/travis/Documents/gits/educational_disparities/cluster/output/metrics_raw/'
plot.dat <- data.frame(group = NA, bias = NA, est = NA, lower = NA, upper=NA, metric = NA)
plot.dat2 <- data.frame(bias = NA, est = NA, lower=NA, upper=NA, metric=NA)
plot.dat3 <- data.frame(est=NA, lower=NA, upper=NA, metric=NA)

# bias
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
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/metrics_raw/bias.jpeg'
  
  ggsave(filename=plot_path, plot)
  
  ggplot(plot.dat, aes(x=bias, y=est, group=group)) +
    geom_point(data=raw_rate, aes(x=bias, y=rate, group=group, color=group, size=nschools), alpha=.05)+
    geom_line(aes(color=group)) +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    ylim(0,quantile(raw_rate$rate, .9)) +
    facet_wrap(~metric, scales = 'free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/metrics_raw/bias_wdata.jpeg'
  
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
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/metrics_raw/bias_OR.jpeg'
  
  ggsave(filename=plot_path, plot)
  
  ggplot(plot.dat2, aes(x=bias, y=est)) +
    geom_point(data=raw_or, aes(x=bias, y=odds_r, size=nschools), alpha=.1) +
    geom_line() +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    ylim(0,quantile(raw_or$odds_r, na.rm=T, .9)) +
    facet_wrap(~metric, scales='free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/metrics_raw/bias_OR_wdata.jpeg'
  
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
  
data.frame(est=mean(p_cons[13,]),
           lower=quantile(p_cons[13,], .025),
           upper=quantile(p_cons[13,], .975),
           metric=i) %>%
    rbind(plot.dat3) %>%
    filter(!is.na(est)) %>%
    mutate(metric=reorder(metric, est)) -> plot.dat3
  
    ggplot(plot.dat3, aes(x=metric, y=est)) +
    geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper)) +
    theme_classic() +
    coord_flip() +
    geom_hline(yintercept=0) -> plot
    
    plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/metrics_raw/bias_OR_slope.jpeg'
    
    ggsave(filename=plot_path, plot)
  
}

#warmth
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
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/metrics_raw/warmth.jpeg'
  
  ggsave(filename=plot_path, plot)
  
  ggplot(plot.dat, aes(x=warmth, y=est, group=group)) +
    geom_point(data=raw_rate, aes(x=warmth, y=rate, group=group, color=group, size=nschools), alpha=.05)+
    geom_line(aes(color=group)) +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    ylim(0,quantile(raw_rate$rate, .9)) +
    facet_wrap(~metric, scales = 'free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/metrics_raw/warmth_wdata.jpeg'
  
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
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/metrics_raw/warmth_OR.jpeg'
  
  ggsave(filename=plot_path, plot)
  
  ggplot(plot.dat2, aes(x=warmth, y=est)) +
    geom_point(data=raw_or, aes(x=warmth, y=odds_r, size=nschools), alpha=.1) +
    geom_line() +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    ylim(0,quantile(raw_or$odds_r, na.rm=T, .9)) +
    facet_wrap(~metric, scales='free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/metrics_raw/warmth_OR_wdata.jpeg'
  
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
  
  data.frame(est=mean(p_cons[14,]),
             lower=quantile(p_cons[14,], .025),
             upper=quantile(p_cons[14,], .975),
             metric=i) %>%
    rbind(plot.dat3) %>%
    filter(!is.na(est)) %>%
    mutate(metric=reorder(metric, est)) -> plot.dat3
  
  ggplot(plot.dat3, aes(x=metric, y=est)) +
    geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper)) +
    theme_classic() +
    coord_flip() +
    geom_hline(yintercept=0) -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/metrics_raw/warmth_OR_slope.jpeg'
  
  ggsave(filename=plot_path, plot)
}

base_path <- '/Users/travis/Documents/gits/educational_disparities/cluster/output/metrics_weighted/'
plot.dat <- data.frame(group = NA, weighted_bias = NA, est = NA, lower = NA, upper=NA, metric = NA)
plot.dat2 <- data.frame(weighted_bias = NA, est = NA, lower=NA, upper=NA, metric=NA)
plot.dat3 <- data.frame(est=NA, lower=NA, upper=NA, metric=NA)

# weighted bias
metrics <- list.files(base_path)
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
  newdat <- expand.grid(group = rownames(table(m$data$group)),
                        total_pop=0,
                        unemp_rate=0,
                        med_income=0,
                        poverty_rate=0,
                        col_grads=0,
                        white_prop=0,
                        black_prop=0,
                        b.w.ratio=0,
                        weighted_bias = seq(-2,2,.25),
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
    mutate(metric=i) %>%
    rbind(plot.dat) %>% 
    filter(!is.na(group)) -> plot.dat
  
  ggplot(plot.dat, aes(x=weighted_bias, y=est, group=group)) +
    #geom_point(data=raw_rate, aes(x=bias, y=rate, group=group, color=group, size=nschools), alpha=.05)+
    geom_line(aes(color=group)) +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    #ylim(0,quantile(raw_rate$rate, .9))
    facet_wrap(~metric, scales = 'free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/metrics_weighted/bias.jpeg'
  
  ggsave(filename=plot_path, plot)
  
  ggplot(plot.dat, aes(x=weighted_bias, y=est, group=group)) +
    geom_point(data=raw_rate, aes(x=weighted_bias, y=rate, group=group, color=group, size=nschools), alpha=.05)+
    geom_line(aes(color=group)) +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    ylim(0,quantile(raw_rate$rate, .9)) +
    facet_wrap(~metric, scales = 'free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/metrics_weighted/bias_wdata.jpeg'
  
  ggsave(filename=plot_path, plot)
  
  posterior_distribution %>%
    select(group, weighted_bias, sample, value) %>%
    spread(group, value) %>%
    mutate(ratio = black/white) %>%
    select(weighted_bias, sample, ratio) %>%
    group_by(weighted_bias) %>%
    summarise(est = mean(ratio),
              lower = quantile(ratio, .025),
              upper = quantile(ratio, .975)) %>%
    ungroup() %>%
    mutate(metric=i) %>%
    rbind(plot.dat2) %>% 
    filter(!is.na(weighted_bias)) -> plot.dat2
  
  raw_rate %>% 
    select(county_id, nschools, weighted_bias, group, rate) %>% 
    spread(group, rate) %>%
    mutate(odds_r = black/white) -> raw_or
  
  ggplot(plot.dat2, aes(x=weighted_bias, y=est)) +
    geom_line() +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    #geom_point(data=raw_or, aes(x=bias, y=odds_r, size=nschools), alpha=.1) +
    #ylim(0,20)
    facet_wrap(~metric, scales='free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/metrics_weighted/bias_OR.jpeg'
  
  ggsave(filename=plot_path, plot)
  
  ggplot(plot.dat2, aes(x=weighted_bias, y=est)) +
    geom_point(data=raw_or, aes(x=weighted_bias, y=odds_r, size=nschools), alpha=.1) +
    geom_line() +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    ylim(0,quantile(raw_or$odds_r, na.rm=T, .9)) +
    facet_wrap(~metric, scales='free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/metrics_weighted/bias_OR_wdata.jpeg'
  
  ggsave(filename=plot_path, plot)  
  
  data.frame(est=mean(p_cons[13,]),
             lower=quantile(p_cons[13,], .025),
             upper=quantile(p_cons[13,], .975),
             metric=i) %>%
    rbind(plot.dat3) %>%
    filter(!is.na(est)) %>%
    mutate(metric=reorder(metric, est)) -> plot.dat3
  
  ggplot(plot.dat3, aes(x=metric, y=est)) +
    geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper)) +
    theme_classic() +
    coord_flip() +
    geom_hline(yintercept=0) -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/metrics_weighted/bias_OR_slope.jpeg'
  
  ggsave(filename=plot_path, plot)
  
}

#weighted warmth
plot.dat <- data.frame(group = NA, weighted_warmth = NA, est = NA, lower = NA, upper=NA, metric = NA)
plot.dat2 <- data.frame(weighted_warmth = NA, est = NA, lower=NA, upper=NA, metric=NA)
plot.dat3 <- data.frame(est=NA, lower=NA, upper=NA, metric=NA)

for (i in metrics){
  pth <- paste(base_path, i, '/', sep='')
  
  files <- list.files(pth)
  j <- 1
  posterior_combo <- array(0, dim=c(14,4000,length(files)))
  raw_rate <- data.frame(county_id=NA, nschools=NA, weighted_warmth=NA, group=NA, nincidents=NA, nstudents=NA, rate=NA)
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
                        weighted_bias = 0,
                        weighted_warmth = seq(-2,2,.25),
                        number=0,
                        total_number=1)
  y_hat <- rstanarm::posterior_linpred(m, newdata = newdat, re.form=~0, transform=T)
  
  cbind(newdat, t(y_hat)) %>% 
    mutate(index=row_number()) %>%
    gather(sample, value, `1`:`4000`) -> posterior_distribution
  
  posterior_distribution %>%
    select(group, weighted_warmth, sample, value, index) %>%
    group_by(group, weighted_warmth) %>%
    summarise(est = mean(value),
              lower = quantile(value, .025),
              upper = quantile(value, .975)) %>%
    ungroup() %>%
    mutate(metric=i) %>%
    rbind(plot.dat) %>% 
    filter(!is.na(group)) -> plot.dat
  
  ggplot(plot.dat, aes(x=weighted_warmth, y=est, group=group)) +
    geom_line(aes(color=group)) +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    facet_wrap(~metric, scales = 'free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/metrics_weighted/warmth.jpeg'
  
  ggsave(filename=plot_path, plot)
  
  ggplot(plot.dat, aes(x=weighted_warmth, y=est, group=group)) +
    geom_point(data=raw_rate, aes(x=weighted_warmth, y=rate, group=group, color=group, size=nschools), alpha=.05)+
    geom_line(aes(color=group)) +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    ylim(0,quantile(raw_rate$rate, .9)) +
    facet_wrap(~metric, scales = 'free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/metrics_weighted/warmth_wdata.jpeg'
  
  ggsave(filename=plot_path, plot)
  
  posterior_distribution %>%
    select(group, weighted_warmth, sample, value) %>%
    spread(group, value) %>%
    mutate(ratio = black/white) %>%
    select(weighted_warmth, sample, ratio) %>%
    group_by(weighted_warmth) %>%
    summarise(est = mean(ratio),
              lower = quantile(ratio, .025),
              upper = quantile(ratio, .975)) %>%
    ungroup() %>%
    mutate(metric=i) %>%
    rbind(plot.dat2) %>% 
    filter(!is.na(weighted_warmth)) -> plot.dat2
  
  raw_rate %>% 
    select(county_id, nschools, weighted_warmth, group, rate) %>% 
    spread(group, rate) %>%
    mutate(odds_r = black/white) -> raw_or
  
  ggplot(plot.dat2, aes(x=weighted_warmth, y=est)) +
    geom_line() +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    facet_wrap(~metric, scales='free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/metrics_weighted/warmth_OR.jpeg'
  
  ggsave(filename=plot_path, plot)
  
  ggplot(plot.dat2, aes(x=weighted_warmth, y=est)) +
    geom_point(data=raw_or, aes(x=weighted_warmth, y=odds_r, size=nschools), alpha=.1) +
    geom_line() +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    ylim(0,quantile(raw_or$odds_r, na.rm=T, .9)) +
    facet_wrap(~metric, scales='free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/metrics_weighted/warmth_OR_wdata.jpeg'
  
  ggsave(filename=plot_path, plot)  
  
  data.frame(est=mean(p_cons[14,]),
             lower=quantile(p_cons[14,], .025),
             upper=quantile(p_cons[14,], .975),
             metric=i) %>%
    rbind(plot.dat3) %>%
    filter(!is.na(est)) %>%
    mutate(metric=reorder(metric, est)) -> plot.dat3
  
  ggplot(plot.dat3, aes(x=metric, y=est)) +
    geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper)) +
    theme_classic() +
    coord_flip() +
    geom_hline(yintercept=0) -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/metrics_weighted/warmth_OR_slope.jpeg'
  
  ggsave(filename=plot_path, plot)
}

base_path <- '/Users/travis/Documents/gits/educational_disparities/cluster/output/teacher_metrics/'
plot.dat <- data.frame(group = NA, county_bias = NA, est = NA, lower = NA, upper=NA, metric = NA)
plot.dat2 <- data.frame(county_bias = NA, est = NA, lower=NA, upper=NA, metric=NA)
plot.dat3 <- data.frame(est=NA, lower=NA, upper=NA, metric=NA)

# teacher bias
metrics <- list.files(base_path)
for (i in metrics){
  pth <- paste(base_path, i, '/', sep='')

  files <- list.files(pth)
  j <- 1
  posterior_combo <- array(0, dim=c(14,4000,length(files)))
  #raw_rate <- data.frame(county_id=NA, nschools=NA, county_bias=NA, group=NA, nincidents=NA, nstudents=NA, rate=NA)
  if (length(files>1)){
    for (k in files){
      #print(i)
      load(paste(pth,k,sep=''))
      df <- as.matrix(m)
      m$data %>% 
        select(county_id, COMBOKEY) %>% 
        distinct() %>% 
        group_by(county_id) %>% 
        summarise(nschools=n()) -> n_schools
      # m$data %>% 
      #   group_by(county_id, group) %>% 
      #   summarise(nincidents=sum(number), nstudents=sum(total_number)) %>% 
      #   mutate(rate=nincidents/nstudents) -> rate
      # n_schools %>% 
      #   left_join(m$data[, c('county_id', 'county_bias')]) %>%
      #   distinct() %>%
      #   left_join(rate) %>%
      #   rbind(raw_rate) %>%
      #   filter(!is.na(county_id)) -> raw_rate
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
  }else{
    load(paste(pth,files,sep=''))
    df <- as.matrix(m)
    m$data %>% 
      select(county_id, COMBOKEY) %>% 
      distinct() %>% 
      group_by(county_id) %>% 
      summarise(nschools=n()) -> n_schools
    # m$data %>% 
    #   group_by(county_id, group) %>% 
    #   summarise(nincidents=sum(number), nstudents=sum(total_number)) %>% 
    #   mutate(rate=nincidents/nstudents) -> rate
    # n_schools %>% 
    #   left_join(m$data[, c('county_id', 'county_bias')]) %>%
    #   distinct() %>%
    #   left_join(rate) %>%
    #   rbind(raw_rate) %>%
    #   filter(!is.na(county_id)) -> raw_rate
    
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
                        county_bias = seq(-2,2,.25),
                        county_warmth = 0,
                        number=0,
                        total_number=1)
  y_hat <- rstanarm::posterior_linpred(m, newdata = newdat, re.form=~0, transform=T)
  
  cbind(newdat, t(y_hat)) %>% 
    mutate(index=row_number()) %>%
    gather(sample, value, `1`:`4000`) -> posterior_distribution
  
  
  posterior_distribution %>%
    select(group, county_bias, sample, value, index) %>%
    group_by(group, county_bias) %>%
    summarise(est = mean(value),
              lower = quantile(value, .025),
              upper = quantile(value, .975)) %>%
    ungroup() %>%
    mutate(metric=i) %>%
    rbind(plot.dat) %>% 
    filter(!is.na(group)) -> plot.dat
  
  ggplot(plot.dat, aes(x=county_bias, y=est, group=group)) +
    #geom_point(data=raw_rate, aes(x=bias, y=rate, group=group, color=group, size=nschools), alpha=.05)+
    geom_line(aes(color=group)) +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    facet_wrap(~metric, scales = 'free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/teachers/bias.jpeg'
  
  ggsave(filename=plot_path, plot)
  
  # ggplot(plot.dat, aes(x=county_bias, y=est, group=group)) +
  #   geom_point(data=raw_rate, aes(x=county_bias, y=rate, group=group, color=group, size=nschools), alpha=.05)+
  #   geom_line(aes(color=group)) +
  #   geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
  #   theme_classic() +
  #   ylim(0,quantile(raw_rate$rate, .9)) +
  #   facet_wrap(~metric, scales = 'free') -> plot
  # 
  # plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/teachers/bias_wdata.jpeg'
  # 
  # ggsave(filename=plot_path, plot)
  # 
  posterior_distribution %>%
    select(group, county_bias, sample, value) %>%
    spread(group, value) %>%
    mutate(ratio = black/white) %>%
    select(county_bias, sample, ratio) %>%
    group_by(county_bias) %>%
    summarise(est = mean(ratio),
              lower = quantile(ratio, .025),
              upper = quantile(ratio, .975)) %>%
    ungroup() %>%
    mutate(metric=i) %>%
    rbind(plot.dat2) %>% 
    filter(!is.na(county_bias)) -> plot.dat2
  
  # raw_rate %>% 
  #   select(county_id, nschools, county_bias, group, rate) %>% 
  #   spread(group, rate) %>%
  #   mutate(odds_r = black/white) -> raw_or
  
  ggplot(plot.dat2, aes(x=county_bias, y=est)) +
    geom_line() +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    facet_wrap(~metric, scales='free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/teachers/bias_OR.jpeg'
  
  ggsave(filename=plot_path, plot)
  
  # ggplot(plot.dat2, aes(x=county_bias, y=est)) +
  #   geom_point(data=raw_or, aes(x=county_bias, y=odds_r, size=nschools), alpha=.1) +
  #   geom_line() +
  #   geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
  #   theme_classic() +
  #   ylim(0,quantile(raw_or$odds_r, na.rm=T, .9)) +
  #   facet_wrap(~metric, scales='free') -> plot
  # 
  # plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/teachers/bias_OR_wdata.jpeg'
  
  ggsave(filename=plot_path, plot)  
  
  data.frame(est=mean(p_cons[13,]),
             lower=quantile(p_cons[13,], .025),
             upper=quantile(p_cons[13,], .975),
             metric=i) %>%
    rbind(plot.dat3) %>%
    filter(!is.na(est)) %>%
    mutate(metric=reorder(metric, est)) -> plot.dat3
  
  ggplot(plot.dat3, aes(x=metric, y=est)) +
    geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper)) +
    theme_classic() +
    coord_flip() +
    geom_hline(yintercept=0) -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/teachers/bias_OR_slope.jpeg'
  
  ggsave(filename=plot_path, plot)
}


#teacher warmth
plot.dat <- data.frame(group = NA, county_warmth = NA, est = NA, lower = NA, upper=NA, metric = NA)
plot.dat2 <- data.frame(county_warmth = NA, est = NA, lower=NA, upper=NA, metric=NA)
plot.dat3 <- data.frame(est=NA, lower=NA, upper=NA, metric=NA)

for (i in metrics){
  pth <- paste(base_path, i, '/', sep='')
  
  files <- list.files(pth)
  j <- 1
  posterior_combo <- array(0, dim=c(14,4000,length(files)))
  #raw_rate <- data.frame(county_id=NA, nschools=NA, county_warmth=NA, group=NA, nincidents=NA, nstudents=NA, rate=NA)
  if(length(files)>1){
    for (k in files){
      #print(i)
      load(paste(pth,k,sep=''))
      df <- as.matrix(m)
      m$data %>% 
        select(county_id, COMBOKEY) %>% 
        distinct() %>% 
        group_by(county_id) %>% 
        summarise(nschools=n()) -> n_schools
      # m$data %>% 
      #   group_by(county_id, group) %>% 
      #   summarise(nincidents=sum(number), nstudents=sum(total_number)) %>% 
      #   mutate(rate=nincidents/nstudents) -> rate
      # n_schools %>% 
      #   left_join(m$data[, c('county_id', 'county_warmth')]) %>%
      #   distinct() %>%
      #   left_join(rate) %>%
      #   rbind(raw_rate) %>%
      #   filter(!is.na(county_id)) -> raw_rate
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
  } else{
    load(paste(pth,files,sep=''))
    df <- as.matrix(m)
    m$data %>% 
      select(county_id, COMBOKEY) %>% 
      distinct() %>% 
      group_by(county_id) %>% 
      summarise(nschools=n()) -> n_schools
    # m$data %>% 
    #   group_by(county_id, group) %>% 
    #   summarise(nincidents=sum(number), nstudents=sum(total_number)) %>% 
    #   mutate(rate=nincidents/nstudents) -> rate
    # n_schools %>% 
    #   left_join(m$data[, c('county_id', 'county_warmth')]) %>%
    #   distinct() %>%
    #   left_join(rate) %>%
    #   rbind(raw_rate) %>%
    #   filter(!is.na(county_id)) -> raw_rate
    
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
                        county_bias = 0,
                        county_warmth = seq(-2,2,.25),
                        number=0,
                        total_number=1)
  y_hat <- rstanarm::posterior_linpred(m, newdata = newdat, re.form=~0, transform=T)
  
  cbind(newdat, t(y_hat)) %>% 
    mutate(index=row_number()) %>%
    gather(sample, value, `1`:`4000`) -> posterior_distribution
  
  posterior_distribution %>%
    select(group, county_warmth, sample, value, index) %>%
    group_by(group, county_warmth) %>%
    summarise(est = mean(value),
              lower = quantile(value, .025),
              upper = quantile(value, .975)) %>%
    ungroup() %>%
    mutate(metric=i) %>%
    rbind(plot.dat) %>% 
    filter(!is.na(group)) -> plot.dat
  
  ggplot(plot.dat, aes(x=county_warmth, y=est, group=group)) +
    geom_line(aes(color=group)) +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    facet_wrap(~metric, scales = 'free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/teachers/warmth.jpeg'
  
  ggsave(filename=plot_path, plot)
  
  # ggplot(plot.dat, aes(x=county_warmth, y=est, group=group)) +
  #   geom_point(data=raw_rate, aes(x=county_warmth, y=rate, group=group, color=group, size=nschools), alpha=.05)+
  #   geom_line(aes(color=group)) +
  #   geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
  #   theme_classic() +
  #   ylim(0,quantile(raw_rate$rate, .9)) +
  #   facet_wrap(~metric, scales = 'free') -> plot
  # 
  # plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/teacher_warmth_wdata.jpeg'
  # 
  # ggsave(filename=plot_path, plot)
  
  posterior_distribution %>%
    select(group, county_warmth, sample, value) %>%
    spread(group, value) %>%
    mutate(ratio = black/white) %>%
    select(county_warmth, sample, ratio) %>%
    group_by(county_warmth) %>%
    summarise(est = mean(ratio),
              lower = quantile(ratio, .025),
              upper = quantile(ratio, .975)) %>%
    ungroup() %>%
    mutate(metric=i) %>%
    rbind(plot.dat2) %>% 
    filter(!is.na(county_warmth)) -> plot.dat2
  
  # raw_rate %>% 
  #   select(county_id, nschools, county_warmth, group, rate) %>% 
  #   spread(group, rate) %>%
  #   mutate(odds_r = black/white) -> raw_or
  
  ggplot(plot.dat2, aes(x=county_warmth, y=est)) +
    geom_line() +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    facet_wrap(~metric, scales='free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/teachers/warmth_OR.jpeg'
  
  ggsave(filename=plot_path, plot)
  
  # ggplot(plot.dat2, aes(x=county_warmth, y=est)) +
  #   geom_point(data=raw_or, aes(x=county_warmth, y=odds_r, size=nschools), alpha=.1) +
  #   geom_line() +
  #   geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
  #   theme_classic() +
  #   ylim(0,quantile(raw_or$odds_r, na.rm=T, .9)) +
  #   facet_wrap(~metric, scales='free') -> plot
  # 
  # plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/teachers/warmth_OR_wdata.jpeg'
  # 
  # ggsave(filename=plot_path, plot)  
  
  data.frame(est=mean(p_cons[14,]),
             lower=quantile(p_cons[14,], .025),
             upper=quantile(p_cons[14,], .975),
             metric=i) %>%
    rbind(plot.dat3) %>%
    filter(!is.na(est)) %>%
    mutate(metric=reorder(metric, est)) -> plot.dat3
  
  ggplot(plot.dat3, aes(x=metric, y=est)) +
    geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper)) +
    theme_classic() +
    coord_flip() +
    geom_hline(yintercept=0) -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/teachers/warmth_OR_slope.jpeg'
  
  ggsave(filename=plot_path, plot)
}

base_path <- '/Users/travis/Documents/gits/educational_disparities/cluster/output/mw_uclaexcl/'
plot.dat <- data.frame(group = NA, weighted_bias = NA, est = NA, lower = NA, upper=NA, metric = NA)
plot.dat2 <- data.frame(weighted_bias = NA, est = NA, lower=NA, upper=NA, metric=NA)
plot.dat3 <- data.frame(est=NA, lower=NA, upper=NA, metric=NA)

# weighted bias w/exclusions
metrics <- list.files(base_path)
for (i in metrics){
  pth <- paste(base_path, i, '/', sep='')
  
  files <- list.files(pth)
  j <- 1
  posterior_combo <- array(0, dim=c(14,4000,length(files)))
  #raw_rate <- data.frame(county_id=NA, nschools=NA, weighted_bias=NA, group=NA, nincidents=NA, nstudents=NA, rate=NA)
  if (length(files>1)){
    for (k in files){
      #print(i)
      load(paste(pth,k,sep=''))
      df <- as.matrix(m)
      m$data %>% 
        select(county_id, COMBOKEY) %>% 
        distinct() %>% 
        group_by(county_id) %>% 
        summarise(nschools=n()) -> n_schools
      # m$data %>% 
      #   group_by(county_id, group) %>% 
      #   summarise(nincidents=sum(number), nstudents=sum(total_number)) %>% 
      #   mutate(rate=nincidents/nstudents) -> rate
      # n_schools %>% 
      #   left_join(m$data[, c('county_id', 'weighted_bias')]) %>%
      #   distinct() %>%
      #   left_join(rate) %>%
      #   rbind(raw_rate) %>%
      #   filter(!is.na(county_id)) -> raw_rate
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
  }else{
    load(paste(pth,files,sep=''))
    df <- as.matrix(m)
    m$data %>% 
      select(county_id, COMBOKEY) %>% 
      distinct() %>% 
      group_by(county_id) %>% 
      summarise(nschools=n()) -> n_schools
    # m$data %>% 
    #   group_by(county_id, group) %>% 
    #   summarise(nincidents=sum(number), nstudents=sum(total_number)) %>% 
    #   mutate(rate=nincidents/nstudents) -> rate
    # n_schools %>% 
    #   left_join(m$data[, c('county_id', 'weighted_bias')]) %>%
    #   distinct() %>%
    #   left_join(rate) %>%
    #   rbind(raw_rate) %>%
    #   filter(!is.na(county_id)) -> raw_rate
    
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
                        weighted_bias = seq(-2,2,.25),
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
    mutate(metric=i) %>%
    rbind(plot.dat) %>% 
    filter(!is.na(group)) -> plot.dat
  
  ggplot(plot.dat, aes(x=weighted_bias, y=est, group=group)) +
    geom_line(aes(color=group)) +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    #ylim(0,quantile(raw_rate$rate, .9))
    facet_wrap(~metric, scales = 'free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/mw_uclaexcl/bias.jpeg'
  
  ggsave(filename=plot_path, plot)
  
  # ggplot(plot.dat, aes(x=weighted_bias, y=est, group=group)) +
  #   geom_point(data=raw_rate, aes(x=weighted_bias, y=rate, group=group, color=group, size=nschools), alpha=.05)+
  #   geom_line(aes(color=group)) +
  #   geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
  #   theme_classic() +
  #   ylim(0,quantile(raw_rate$rate, .9)) +
  #   facet_wrap(~metric, scales = 'free') -> plot
  # 
  # plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/mw_uclaexcl/bias_wdata.jpeg'
  # 
  # ggsave(filename=plot_path, plot)
  
  posterior_distribution %>%
    select(group, weighted_bias, sample, value) %>%
    spread(group, value) %>%
    mutate(ratio = black/white) %>%
    select(weighted_bias, sample, ratio) %>%
    group_by(weighted_bias) %>%
    summarise(est = mean(ratio),
              lower = quantile(ratio, .025),
              upper = quantile(ratio, .975)) %>%
    ungroup() %>%
    mutate(metric=i) %>%
    rbind(plot.dat2) %>% 
    filter(!is.na(weighted_bias)) -> plot.dat2
  
  # raw_rate %>% 
  #   select(county_id, nschools, weighted_bias, group, rate) %>% 
  #   spread(group, rate) %>%
  #   mutate(odds_r = black/white) -> raw_or
  
  ggplot(plot.dat2, aes(x=weighted_bias, y=est)) +
    geom_line() +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    facet_wrap(~metric, scales='free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/mw_uclaexcl/bias_OR.jpeg'
  
  ggsave(filename=plot_path, plot)
  
  # ggplot(plot.dat2, aes(x=weighted_bias, y=est)) +
  #   geom_point(data=raw_or, aes(x=weighted_bias, y=odds_r, size=nschools), alpha=.1) +
  #   geom_line() +
  #   geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
  #   theme_classic() +
  #   ylim(0,quantile(raw_or$odds_r, na.rm=T, .9)) +
  #   facet_wrap(~metric, scales='free') -> plot
  # 
  # plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/mw_uclaexcl/bias_OR_wdata.jpeg'
  # 
  # ggsave(filename=plot_path, plot)  
  
  data.frame(est=mean(p_cons[13,]),
             lower=quantile(p_cons[13,], .025),
             upper=quantile(p_cons[13,], .975),
             metric=i) %>%
    rbind(plot.dat3) %>%
    filter(!is.na(est)) %>%
    mutate(metric=reorder(metric, est)) -> plot.dat3
  
  ggplot(plot.dat3, aes(x=metric, y=est)) +
    geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper)) +
    theme_classic() +
    coord_flip() +
    geom_hline(yintercept=0) -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/mw_uclaexcl/bias_OR_slope.jpeg'
  
  ggsave(filename=plot_path, plot)
}


#UCLA excl warmth
plot.dat <- data.frame(group = NA, weighted_warmth = NA, est = NA, lower = NA, upper=NA, metric = NA)
plot.dat2 <- data.frame(weighted_warmth = NA, est = NA, lower=NA, upper=NA, metric=NA)
plot.dat3 <- data.frame(est=NA, lower=NA, upper=NA, metric=NA)

for (i in metrics){
  pth <- paste(base_path, i, '/', sep='')
  
  files <- list.files(pth)
  j <- 1
  posterior_combo <- array(0, dim=c(14,4000,length(files)))
  #raw_rate <- data.frame(county_id=NA, nschools=NA, weighted_warmth=NA, group=NA, nincidents=NA, nstudents=NA, rate=NA)
  if(length(files)>1){
    for (k in files){
      #print(i)
      load(paste(pth,k,sep=''))
      df <- as.matrix(m)
      m$data %>% 
        select(county_id, COMBOKEY) %>% 
        distinct() %>% 
        group_by(county_id) %>% 
        summarise(nschools=n()) -> n_schools
      # m$data %>% 
      #   group_by(county_id, group) %>% 
      #   summarise(nincidents=sum(number), nstudents=sum(total_number)) %>% 
      #   mutate(rate=nincidents/nstudents) -> rate
      # n_schools %>% 
      #   left_join(m$data[, c('county_id', 'weighted_warmth')]) %>%
      #   distinct() %>%
      #   left_join(rate) %>%
      #   rbind(raw_rate) %>%
      #   filter(!is.na(county_id)) -> raw_rate
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
  } else{
    load(paste(pth,files,sep=''))
    df <- as.matrix(m)
    m$data %>% 
      select(county_id, COMBOKEY) %>% 
      distinct() %>% 
      group_by(county_id) %>% 
      summarise(nschools=n()) -> n_schools
    # m$data %>% 
    #   group_by(county_id, group) %>% 
    #   summarise(nincidents=sum(number), nstudents=sum(total_number)) %>% 
    #   mutate(rate=nincidents/nstudents) -> rate
    # n_schools %>% 
    #   left_join(m$data[, c('county_id', 'weighted_warmth')]) %>%
    #   distinct() %>%
    #   left_join(rate) %>%
    #   rbind(raw_rate) %>%
    #   filter(!is.na(county_id)) -> raw_rate
    
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
                        weighted_bias = 0,
                        weighted_warmth = seq(-2,2,.25),
                        number=0,
                        total_number=1)
  y_hat <- rstanarm::posterior_linpred(m, newdata = newdat, re.form=~0, transform=T)
  
  cbind(newdat, t(y_hat)) %>% 
    mutate(index=row_number()) %>%
    gather(sample, value, `1`:`4000`) -> posterior_distribution
  
  posterior_distribution %>%
    select(group, weighted_warmth, sample, value, index) %>%
    group_by(group, weighted_warmth) %>%
    summarise(est = mean(value),
              lower = quantile(value, .025),
              upper = quantile(value, .975)) %>%
    ungroup() %>%
    mutate(metric=i) %>%
    rbind(plot.dat) %>% 
    filter(!is.na(group)) -> plot.dat
  
  ggplot(plot.dat, aes(x=weighted_warmth, y=est, group=group)) +
    geom_line(aes(color=group)) +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    facet_wrap(~metric, scales = 'free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/mw_uclaexcl/warmth.jpeg'
  
  ggsave(filename=plot_path, plot)
  
  # ggplot(plot.dat, aes(x=weighted_warmth, y=est, group=group)) +
  #   geom_point(data=raw_rate, aes(x=weighted_warmth, y=rate, group=group, color=group, size=nschools), alpha=.05)+
  #   geom_line(aes(color=group)) +
  #   geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
  #   theme_classic() +
  #   ylim(0,quantile(raw_rate$rate, .9)) +
  #   facet_wrap(~metric, scales = 'free') -> plot
  # 
  # plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/mw_uclaexcl/warmth_wdata.jpeg'
  # 
  # ggsave(filename=plot_path, plot)
  
  posterior_distribution %>%
    select(group, weighted_warmth, sample, value) %>%
    spread(group, value) %>%
    mutate(ratio = black/white) %>%
    select(weighted_warmth, sample, ratio) %>%
    group_by(weighted_warmth) %>%
    summarise(est = mean(ratio),
              lower = quantile(ratio, .025),
              upper = quantile(ratio, .975)) %>%
    ungroup() %>%
    mutate(metric=i) %>%
    rbind(plot.dat2) %>% 
    filter(!is.na(weighted_warmth)) -> plot.dat2
  
  # raw_rate %>% 
  #   select(county_id, nschools, weighted_warmth, group, rate) %>% 
  #   spread(group, rate) %>%
  #   mutate(odds_r = black/white) -> raw_or
  
  ggplot(plot.dat2, aes(x=weighted_warmth, y=est)) +
    geom_line() +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
    theme_classic() +
    facet_wrap(~metric, scales='free') -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/mw_uclaexcl/warmth_OR.jpeg'
  
  ggsave(filename=plot_path, plot)
  
  # ggplot(plot.dat2, aes(x=weighted_warmth, y=est)) +
  #   geom_point(data=raw_or, aes(x=weighted_warmth, y=odds_r, size=nschools), alpha=.1) +
  #   geom_line() +
  #   geom_ribbon(aes(ymin=lower, ymax=upper), alpha=.15) +
  #   theme_classic() +
  #   ylim(0,quantile(raw_or$odds_r, na.rm=T, .9)) +
  #   facet_wrap(~metric, scales='free') -> plot
  # 
  # plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/mw_uclaexcl/warmth_OR_wdata.jpeg'
  # 
  # ggsave(filename=plot_path, plot)  
  
  data.frame(est=mean(p_cons[14,]),
             lower=quantile(p_cons[14,], .025),
             upper=quantile(p_cons[14,], .975),
             metric=i) %>%
    rbind(plot.dat3) %>%
    filter(!is.na(est)) %>%
    mutate(metric=reorder(metric, est)) -> plot.dat3
  
  ggplot(plot.dat3, aes(x=metric, y=est)) +
    geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper)) +
    theme_classic() +
    coord_flip() +
    geom_hline(yintercept=0) -> plot
  
  plot_path <- '/Users/travis/Documents/gits/educational_disparities/figs/individual_models/mw_uclaexcl/warmth_OR_slope.jpeg'
  
  ggsave(filename=plot_path, plot)
}
