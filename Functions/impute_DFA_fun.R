#This function fits a DFA to data and returns a dataset with the NAs interpolated based on the DFA. Based on DFA example here: https://atsa-es.github.io/atsa-labs/example-snotel-data.html

impute_DFA_fun<-function(new.df){
  
  
  #pivot wider with column for each population for MARSS modeling
  wide_df.ts<-new.df %>% 
    
    arrange(DPS_ESU,Population_Name) %>% 
    ##ALERT!!! filtering to only three populations for model testing.
    # filter(Population_Name%in%c("Clackamas River","Kalama","Sandy River")) %>% 
    dplyr::select(Year,Population_Name,Population_Escapement) %>% pivot_wider(names_from = Population_Name,values_from = Population_Escapement) %>% arrange(Year) %>% 
    #drop Year column and log for input into MARSS
    select(-1) %>% log
  
  
  
  # Z-score (subtract mean and devide by SD) escapements
  test.ts_z<-scale(wide_df.ts)
  att_Z<-attributes(test.ts_z) #save means and SDs for back-transform
  
  DPS_vec<-new.df$DPS_ESU[match(colnames(test.ts_z),new.df$Population_Name)]
  
# Specify MARSS DFA model
n_fac<-3
ns<-ncol(wide_df.ts)
  
B <- matrix(list(0), n_fac, n_fac)
diag(B)<-paste0("b",1:n_fac)


Q <- diag(1, n_fac)
R <- "diagonal and unequal"
U <- "zero"
x0 <- "zero"
Z <- matrix(list(0), ns, n_fac)
Z[1:(ns * n_fac)] <- paste0(rep(paste0("z",1:n_fac),each=ns),rep(1:ns,times=n_fac))

Z[upper.tri(Z)]<-0
A <- "zero"
mod.list.dfa = list(B = B, Z = Z, Q = Q, R = R, U = U, A = A, 
                    x0 = x0)



fit <- MARSS::MARSS(t(test.ts_z), model = mod.list.dfa, control = list(maxit = 1000),
                 inits = list(A = matrix(0, ns, 1)))



d <- fitted(fit, type = "ytT", interval = "prediction") %>%
  #create a new column "vals" with data when available and fitted values when data missing
  mutate(vals=ifelse(is.na(y),.fitted,y),interpolated=is.na(y))
d$Year <- d$t+ max(as.numeric(as.character(new.df$Year)))-max(d$t)
d$Station <- d$.rownames
#plot
p <- ggplot(data = d) + geom_line(aes(Year, vals)) + geom_point(aes(Year, 
                                                                    vals,col=interpolated))  +
  
  facet_wrap(~Station) + 
  xlab("") + ylab("Z-scored log counts")+ggplot2::scale_color_manual(values=c("black","red"))


log_z_vals<-d  %>% select(Year,vals,Station) %>% pivot_wider(values_from=vals,names_from   = Station) %>% arrange(Year) %>% select(-Year)

cnts<-t(exp(t(log_z_vals)*att_Z$`scaled:scale`+att_Z$`scaled:center`))


long_counts<-pivot_longer(as_tibble(cnts) %>% mutate(Year=new.df$Year %>%  unique() %>% sort()),!Year,names_to="Population",values_to = "Counts") %>% right_join(d %>% select(Year,Population=Station,interpolated))





#plot
p2 <- ggplot(data = long_counts) + geom_line(aes(Year, Counts)) + geom_point(aes(Year, 
                                                                       Counts,col=interpolated))  +
  
  facet_wrap(~Population,scale="free_y",ncol=3) + 
  xlab("") + ylab("Counts")+ggplot2::scale_color_manual(values=c("black","red"))+theme(legend.position = "top")+xlim(2000,2023)


rollup<-long_counts %>% group_by(Year) %>% summarize(Counts=sum(Counts),
                                                     n_interp=sum(interpolated))

#plot

p3<-ggplot(rollup,aes(x=Year,y=Counts))+geom_point()+geom_line()+geom_text(aes(x=Year,y=Counts,label=n_interp), nudge_y =500,nudge_x = .25)

return(list(
  fit=fit, #fitted MARSS model
  d=d, #long data grame of fitted values and data from the MARSS model
  p=p, # plot of dtransformed ata with interpolated values
  cnts=cnts, #wide version of counts with missing data filled in based on model
  long_counts=long_counts, # long version of counts with missing data filled in based on model
  p2=p2, # plot of counts with interpolated values
  p3=p3, # plot of annual totals with number of missing data points
  rollup=rollup #counts summed by year
))
}
