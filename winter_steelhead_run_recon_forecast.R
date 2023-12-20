wd_functions<-"functions"
sapply(FUN = source, paste(wd_functions, list.files(wd_functions), sep="/"))

#pull and munge data 
dat<-get_data_fun(data_start_year = 0,data_end_year=2023)

#plot data
dat$ts_plot

#look at data summary
dat$data_summary

# fit DFA with MARSS to interpolate (takes ~2 minutes)
imputes<-impute_DFA_fun(dat$new.df)

#plot of imputed values
imputes$p2

#plot of annual sums (including imputed values) with numbers of missing (i.e. imputed) values per year
imputes$p3


# join annual sum of counts with mainstem fishing mortality and sume to calculated river mouth run size
RMR<-imputes$rollup %>% 
  left_join(dat$mainstem.morts %>% group_by(Year) %>% summarize(morts=sum(Fishing_Mortality))) %>% 
  mutate(runsize_obs=Counts +morts) 

View(RMR)


#calculated 5-year average river mouth run size
RMR %>% tail(5) %>% summarize(f=round(mean(runsize_obs),-2))  %>% pull(f)

#calculated 10-year average river mouth run size
RMR %>% tail(10) %>% summarize(mean(runsize_obs))

#fit ARIMA model to RMR time series
fit<-forecast::auto.arima(log(RMR$runsize_obs))
#forecast
exp(forecast(fit,h=1)$mean)
#plot on log scale
plot(forecast(fit,h=1))

     