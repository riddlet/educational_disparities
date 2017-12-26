
pth <- '/Users/travis/Documents/gits/educational_disparities/cluster/output/cross_val_raw/'

files <- list.files(pth)

"%ni%" <- Negate("%in%")

timed_out_models <- NA

for (i in 1:175){
  model <- paste('m',i,'.rdata', sep='')
  if (model %ni% files){
    timed_out_models <- c(timed_out_models, model)
  }
  timed_out_models <- timed_out_models[-1]
}

failed_models <- NA
for (i in files){
  load(paste(pth, i, sep=''))
  stan_fit <- rstan::get_sampler_params(m$stanfit)
  divs <- sum(c(stan_fit[[1]][,5], stan_fit[[2]][,5], 
                stan_fit[[3]][,5], stan_fit[[4]][,5]))
  print(i)
  print(divs)
  if (divs>0){
    failed_models <- c(failed_models, i)
  }
  failed_models <- failed_models[-1]
}
