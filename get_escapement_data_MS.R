library("RSocrata")
library("tidyjson")
library("jsonlite")
library("tidyr")
library("dplyr")
library("ggplot2")
library("readxl")
library('lubridate')
library('purrr')
library('tidyverse')
library("forecast")

# Load mainsteam harvest data, tributary release mortality rates, and miscellaneous escapement data
##Wilamette Falls counts - painstakingly hand entered from PDF tables posted at https://myodfw.com/willamette-falls-fish-counts
willamette.upper.esc<-read_xls(path="data/Input_winter_steelhead_new.xls", sheet = "Escapement") %>% as_tibble() %>% 
filter(Population_Name == "Upper Willamette") %>% 
  select(Year,Population_Name,Escapement) %>% 
  mutate(across(c(Year,Escapement),as.integer))
##estimates of main stem columbia commercial and sport harvest and post-release mortality on the aggregate run
mainstem.morts<-read_xls(path="data/Input_winter_steelhead_new.xls", sheet = "FishingMortality_Mainstem") %>% as_tibble()
## harvest rates used every year, established at some point based on expert judgement I presume.
trib.mort.rates<-read_xls(path="data/Input_winter_steelhead_new.xls", sheet = "HarvestMortRate_Tributaries") %>% as_tibble()


# Populations from SASI
population.list<-c(#"Big White Salmon River Winter Steelhead", # no data available for Big White Salmon
  "Coweeman Winter Steelhead",
  "East Fork Lewis Winter Steelhead",
  "Elochoman-Skamokawa Winter Steelhead",
  "Grays-Chinook Winter Steelhead", 
  "Kalama Winter Steelhead",
  "Klickitat Summer and Winter Steelhead",
  "Lower Cowlitz Winter Steelhead",
  "Lower Gorge (Columbia) Winter Steelhead", 
  "Lower Cowlitz Winter Steelhead",
  "Mill-Abernathy-Germany Creeks Winter Steelhead",
  "North Fork Lewis Winter Steelhead", # no data available for North Fork Lewis
  "North Fork Toutle Winter Steelhead", 
  "Salmon Creek Winter Steelhead", #no data available for Salmon Creek
  "South Fork Toutle Winter Steelhead",
  "Tilton Winter Steelhead",
  "Upper Cowlitz and Cispus Winter Steelhead", 
  "Upper Gorge (Columbia) Winter Steelhead", 
  "Washougal Winter Steelhead")


# Load escapement data from SPI
wa.esc <- read.socrata("https://data.wa.gov/resource/fgyz-n3uk.json") %>% as_tibble() %>%  dplyr::filter(species %in% "Steelhead", production_type != "Hatchery")


#munge data: filter populations, natural-origin escapement estiamtes
wa.esc.df2<-wa.esc %>% 
  #pops in list
  filter(population_name%in%population.list,
         #escapement not pHOS
                        !grepl("pHOS",escapement_methodology), 
  #deal with some cases where more than one entry for a year x population
    ##get rid of duplicate kalamas selecting total spawners above and below falls
    !(population_name=="Kalama Winter Steelhead"&calculation_type!="NA"),
  ##North Fork Toutle had no methods descriptions so we choose the largest value (equal to the sum of the two smaller values, just like for the Kalama, which had methods description)
    !(population_name=="North Fork Toutle Winter Steelhead"&nchar(sub_population_name)<1),
  
  # For several populations we are using TASEJ (excluding jacks) instead of TSAIJ (including jacks - but the numbers are the same) and NOSAEJ (natural-origin only)
  !(population_name%in%c("Elochoman-Skamokawa Winter Steelhead","Grays-Chinook Winter Steelhead","Lower Cowlitz Winter Steelhead","Mill-Abernathy-Germany Creeks Winter Steelhead","Tilton Winter Steelhead","Upper Cowlitz and Cispus Winter Steelhead") &data_type!="TSAEJ"),
  
  # Using Total Escapement instead of Trap Count
  !(population_name=="Upper Gorge (Columbia) Winter Steelhead"&escapement_methodology!="Total Escapement"),
  
  #
  !(population_name=="Klickitat Summer and Winter Steelhead"&sub_population_name!="Klickitat Winter Steelhead")
  
  
  ) %>%
#get rid of any duplicate rows
    distinct() %>% 
  
  #looking for multiple entries per year x populations
  group_by(year,population_name,sub_population_name) %>%
  mutate(n=n()) %>% arrange(population_name,year) %>% 
  arrange(desc(n)) %>% 
  
  mutate(
         Population=ifelse(sub_population_name=="",population_name,sub_population_name),
         Population=sub(" Winter Steelhead","",Population))  %>% 
  #drop missing values
  filter(!is.na(abundance_qty)) %>% 
  # #add 2022 Kalama (Steve Grey pers. correspondence)
  # bind_rows(
  #   filter(.,population_name == "Kalama Winter Steelhead"&year==2021) %>% mutate(year="2022",abundance_qty="881")
  # ) %>% 
  distinct() %>% #get rid of any duplicates
    ungroup %>% 
  # columns needed for analysis: year, population, escapement
select(Year=year,Population_Name=Population,Escapement=abundance_qty) %>% 
  mutate(across(c(Year,Escapement),as.integer),
         source="WA_SPI")
  

# OR Escapements 

##read data from coordinated assesment: 
# StreamNet
sn.apikey <- "D661AF5F-543F-40F9-878E-42D9EBF2E7A8" # Tim's API key for streamnet

sn.nosa <- httr::GET(paste("https://api.streamnet.org/api/v1/ca.json?XApiKey=", sn.apikey, sep=""),query=list("table_id"="4EF09E86-2AA8-4C98-A983-A272C2C2C7E3",agency="ODFW",per_page=10000))

##code to extract information from the object downloaded from streamnet
sn.nosa<-httr::content(sn.nosa,as = "text")
nosa_df<-fromJSON(sn.nosa)
rm(sn.nosa)
or.esc.df<-nosa_df$records %>% as_tibble() %>% 
  filter(
    commonname=="Steelhead",
    run=="Winter",
    submitagency=="ODFW" 
    
  ) %>% 
  select(Year=spawningyear,Population_Name=commonpopname,Escapement=nosaij) %>% 
  mutate(Escapement=as.numeric(Escapement)) %>% 
  mutate(source="streamnet")
rm(nosa_df)

## populations to exclude from analysis (some because upstream of Wilamtter Falls, which we are using counts from, and Youngs Bay probably because of short time series length)
pop.exclude<-list("Calapooia River", "Cedar Creek", "Molalla River", "North Santiam River", "South Santiam River", "Youngs Bay")

# Combined WA and OR escapements
all.esc.df<-bind_rows(wa.esc.df2, or.esc.df,willamette.upper.esc %>% mutate(source="Willamette_falls")) %>% arrange(Population_Name, Year) %>% filter(!Population_Name %in% pop.exclude) %>% 
  group_by(Population_Name) %>%
  rename(Population_Escapement=Escapement) %>% 
  arrange(Population_Name, Year) %>% 
  transform(Population_Escapement = as.numeric(Population_Escapement))


#add tributary harvest rates (table of values originating from god knows where)
new.df<-all.esc.df %>% left_join(trib.mort.rates) %>% 
  filter(Year>=2001) %>% #years with more complete data
#expand escapement to account for tributary harvest
  mutate(Population_Escapement=Population_Escapement/(1-TribMortRate)) %>% filter(Population_Name!="Lower Gorge (Columbia)")


new.df<-all.esc.df %>% left_join(trib.mort.rates) %>% 
  # filter(between(Year,2001,2022)) %>% #years with more complete data
  filter(Year<=2022) %>%
  #expand escapement to account for tributary harvest
  mutate(Population_Escapement=Population_Escapement/(1-TribMortRate)) %>% filter(Population_Name!="Lower Gorge (Columbia)")


new.df  %>% group_by(Population_Name,source) %>% 
  summarize(min_year=min(Year),
            max_year=max(Year),
            missing_years=sum(is.na(Population_Escapement)),
            n_years=n(),
            mean_esc=round(mean(Population_Escapement,na.rm=T),2)) %>%View()# write_csv(file="winter_stlhd.csv")

#plot data time series by population
 ggplot(new.df,aes(x=Year,y=Population_Escapement))+geom_line()+geom_point()+facet_wrap(~Population_Name,scales="free_y")
 
#pivot wider with column for each population for MARSS modeling
wide_df.ts<-new.df %>% 
 
    arrange(DPS_ESU,Population_Name) %>% 
  ##ALERT!!! filtering to only three populations for model testing.
  # filter(Population_Name%in%c("Clackamas River","Kalama","Sandy River")) %>% 
  dplyr::select(Year,Population_Name,Population_Escapement) %>% pivot_wider(names_from = Population_Name,values_from = Population_Escapement) %>% arrange(Year) %>% 
  #drop Year column and log for input into MARSS
  select(-1) %>% log




#look at correlations between population escapement time series
cor.ts<-cor(wide_df.ts,use="pairwise.complete.obs")


# Z-score escapements
test.ts_z<-scale(wide_df.ts)
att_Z<-attributes(test.ts_z) #save means and SDs for back-transform

DPS_vec<-new.df$DPS_ESU[match(colnames(test.ts_z),new.df$Population_Name)]


#fit MARSS model to log escapement

ns <- ncol(wide_df.ts)
B <- "diagonal and equal"
Q <- "unconstrained"
R <- diag(0.01, ns)
U <- "zero"
A <- "zero"
x0 <- "unequal"
mod.list.ar1 = list(B = B, Q = Q, R = R, U = U, x0 = x0, A = A, 
                    tinitx = 1)



library(MARSS)
# m <- apply(test.ts_z, 2, mean, na.rm = TRUE)
fit.ar1 <- MARSS(t(test.ts_z), model = mod.list.ar1, control = list(maxit = 5000),method="BFGS")




fit <- fit.ar1
d <- fitted(fit, interval = "prediction", type = "ytT")
d$Year <- d$t + 1980
d$Station <- d$.rownames
p <- ggplot(data = d) + geom_line(aes(Year, .fitted)) + geom_point(aes(Year, 
                                                                       y)) + geom_ribbon(aes(x = Year, ymin = .lwr, ymax = .upr), 
                                                                                         linetype = 2, alpha = 0.2, fill = "blue") + facet_wrap(~Station) + 
  xlab("") + ylab("SWE (demeaned)")
p





cnts<-d  %>% select(Year,.fitted,Station) %>% pivot_wider(values_from=.fitted,names_from   = Station) %>% arrange(Year) %>% select(-Year)

exp(cnts*att_Z$`scaled:scale`+att_Z$`scaled:center`) %>% rowSums() %>% `names<-` (new.df$Year %>% unique %>% sort)


#residuals
fit <- fit.ar1
par(mfrow = c(5, 5), mar = c(2, 2, 1, 1))
apply(MARSSresiduals(fit, type = "tt1")$model.residuals, 
      1, acf, na.action = na.pass)

#-----------------------------------------------
### correlation only
ns <- length(unique(swe.feb$Station))
B <- "zero"
Q <- "unconstrained"
R <- diag(0.01, ns)
U <- "zero"
A <- "zero"
x0 <- "zero"
mod.list.corr = list(B = B, Q = Q, R = R, U = U, x0 = x0, A = A, 
                     tinitx = 0)

fit.corr <- MARSS(t(test.ts_z), model = mod.list.corr)

fit <- fit.corr
d <- fitted(fit, type = "ytT", interval = "prediction")
d$Year <- d$t + 2022-max(d$t)
d$Station <- d$.rownames
p <- ggplot(data = d) + geom_line(aes(Year, .fitted)) + geom_point(aes(Year, 
                                                                       y)) + geom_ribbon(aes(x = Year, ymin = .lwr, ymax = .upr), 
                                                                                         linetype = 2, alpha = 0.2, fill = "blue") + facet_wrap(~Station) + 
  xlab("") + ylab("SWE (demeaned)")
p

cnts<-d  %>% select(Year,.fitted,Station) %>% pivot_wider(values_from=.fitted,names_from   = Station) %>% arrange(Year) %>% select(-Year)

exp(cnts*att_Z$`scaled:scale`+att_Z$`scaled:center`) %>% rowSums() %>% `names<-` (new.df$Year %>% unique %>% sort)



#-----------------------------------------------
### DFA

n_fac<-3

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



fit.dfa <- MARSS(t(test.ts_z), model = mod.list.dfa, control = list(maxit = 1000),
                 inits = list(A = matrix(0, ns, 1)))

fit.dfa$AICc

fit <- fit.dfa
d <- fitted(fit, type = "ytT", interval = "prediction") %>% mutate(vals=ifelse(is.na(y),.fitted,y),interpolated=is.na(y))
d$Year <- d$t+ 2022-max(d$t)
d$Station <- d$.rownames
p <- ggplot(data = d) + geom_line(aes(Year, vals)) + geom_point(aes(Year, 
                                                                       vals,col=interpolated))  +

  facet_wrap(~Station) + 
  xlab("") + ylab("SWE (demeaned)")+ggplot2::scale_color_manual(values=c("black","red"))
p

cnts<-d  %>% select(Year,vals,Station) %>% pivot_wider(values_from=vals,names_from   = Station) %>% arrange(Year) %>% select(-Year)

exp(cnts*att_Z$`scaled:scale`+att_Z$`scaled:center`) %>% rowSums() %>% `names<-` (new.df$Year %>% unique %>% sort) 


#fit MARSS model to log escapement


npop<-ncol(test.ts_z)
#trend
U_DPS<-matrix(DPS_vec,ncol=1)
U=list("equal","zero",U_DPS=U_DPS)#,unequal,,U_DPS=U_DPS"equal","zero"
#autocorrellation
B_DPS<-matrix(list(0),npop,npop);diag(B_DPS)<-DPS_vec
B=list("diagonal and equal","identity",B_DPS=B_DPS)#,"diagonal and unequal","zero"


#process error
Q_DPS_diag<-Q_DPS_cov<-B_DPS
for ( i in 1:npop){
  for(j in 1:npop){
    if(i==j)next
    if(DPS_vec[i]==DPS_vec[j]){
      Q_DPS_cov[i,j]<-paste0(DPS_vec[i],"_cov")
    }else{
      next
    }
  }
}

test<-array(Q_DPS_cov,c(npop,npop,nrow(test.ts_z)))
test[,,1]<-diag(npop)
Q=list("diagonal and equal", "equalvarcov",Q_DPS_diag=Q_DPS_diag)#, " "diagonal and unequal","equalvarcov",
#Observation error
R=c("diagonal and unequal","diagonal and equal")
  # diag(ncol(test.ts_z))*.001
  #c("diagonal and equal","diagonal and unequal")


# 
# install.packages("MARSS", repos = c("https://atsa-es.r-universe.dev", "https://cloud.r-project.org"))

cntl.list <- list( maxit = 2000)

out_list<-list()
model.data <- data.frame(stringsAsFactors = FALSE)
# fit models & store results
for (b in 1:length(B)) {
  for (q in 1:length(Q)) {
    for(u in 1:length(U)){
      for(r in 1:length(R)){
      # if(!(q=="zero" & b!="identity")){
        model <- list(R = R[[r]], B=B[[b]], Q=Q[[q]], U = U[[u]])
        kemz <- MARSS::MARSS(t(test.ts_z),
                             model = model)
        model.data <- rbind(
          model.data,
          data.frame(
            R=ifelse(length(R[[r]])>1,names(R)[r],R[[r]]),
            B = ifelse(length(B[[b]])>1,names(B)[b],B[[b]]),
            Q = ifelse(length(Q[[q]])>1,names(Q)[q],Q[[q]]),
            U= ifelse(length(U[[u]])>1,names(U)[u],U[[u]]),
            logLik = kemz$logLik,
            K = kemz$num.params,
            AICc = kemz$AICc,
            stringsAsFactors = FALSE,
            preds=exp(kemz$ytT*att_Z$`scaled:scale`+att_Z$`scaled:center`) %>% colSums() %>% tail(1)
          )
        )
        assign(paste("kemz", b,q,u,r,sep = "."), kemz)
        out_list[[paste("kemz", b,q,u,r, sep = ".")]]<-kemz
      } # end b loop
    } # end q loop
  } # end of U loop
}
# }

model.data %>% arrange(AICc) %>% dplyr::mutate(delta_AICc=AICc-min(AICc),.after=AICc) %>% View

model.data %>% arrange(AICc) %>% dplyr::mutate(delta_AICc=AICc-min(AICc),.after=AICc,preds=round(preds)) %>% write_csv("model.data.csv")

#--------------------------------------------------------------

#trend
U_DPS<-matrix(DPS_vec,ncol=1)
U=list(U_DPS=U_DPS)#,unequal,,U_DPS=U_DPS,"zero"
#autocorrellation
B_DPS<-matrix(list(0),npop,npop);diag(B_DPS)<-DPS_vec
B=list("diagonal and equal")#,"diagonal and unequal","zero"


#process error
Q_DPS_diag<-Q_DPS_cov<-B_DPS
for ( i in 1:npop){
  for(j in 1:npop){
    if(i==j)next
    if(DPS_vec[i]==DPS_vec[j]){
      Q_DPS_cov[i,j]<-paste0(DPS_vec[i],"_cov")
    }else{
      next
    }
  }
}

test<-array(Q_DPS_cov,c(npop,npop,nrow(test.ts_z)))
test[,,1]<-diag(npop)
Q=list( "diagonal and equal")#, "diagonal and unequal" "diagonal and unequal","equalvarcov",
#Observation error
R=c("diagonal and equal")



for (b in 1:length(B)) {
  for (q in 1:length(Q)) {
    for(u in 1:length(U)){
      for(r in 1:length(R)){
        # if(!(q=="zero" & b!="identity")){  
        model <- list(R = R[[r]], B=B[[b]], Q=Q[[q]], U = U[[u]])
        kemz <- MARSS::MARSS(t(test.ts_z),
                             model = model)
        model.data <- rbind(
          model.data,
          data.frame(
            R=ifelse(length(R[[r]])>1,names(R)[r],R[[r]]),
            B = ifelse(length(B[[b]])>1,names(B)[b],B[[b]]),
            Q = ifelse(length(Q[[q]])>1,names(Q)[q],Q[[q]]),
            U= ifelse(length(U[[u]])>1,names(U)[u],U[[u]]),
            logLik = kemz$logLik,
            K = kemz$num.params,
            AICc = kemz$AICc,
            stringsAsFactors = FALSE,
            preds=exp(kemz$ytT*att_Z$`scaled:scale`+att_Z$`scaled:center`) %>% colSums() %>% tail(1)
          )
        )
        assign(paste("kemz", b,q,u,r,sep = "."), kemz)
        out_list[[paste("kemz", b,q,u,r, sep = ".")]]<-kemz
      } # end b loop
    } # end q loop
  } # end of U loop
}





library(mice)

dimnames(test.ts_z)[[2]]<-NULL
imputed_Data <- mice(test.ts_z, m=5, seed = 300)

#compute estimates by year
exp(complete(imputed_Data,1)*att_Z$`scaled:scale`+att_Z$`scaled:center`) %>% rowSums() %>% `names<-` (new.df$Year %>% unique %>% sort)

exp(complete(imputed_Data,2)*att_Z$`scaled:scale`+att_Z$`scaled:center`) %>% rowSums() %>% `names<-` (new.df$Year %>% unique %>% sort)

exp(complete(imputed_Data,3)*att_Z$`scaled:scale`+att_Z$`scaled:center`) %>% rowSums() %>% `names<-` (new.df$Year %>% unique %>% sort)

library(Amelia)
dim(test.ts_z)
test.ts_z<-cbind(test.ts_z,year=1:nrow(test.ts_z))

test<-amelia(x=test.ts_z,empri = 75,ts=24,poly=1)

#compute estimates by year
exp(test$imputations$imp1[,-24]*att_Z$`scaled:scale`+att_Z$`scaled:center`) %>% rowSums() %>% `names<-` (new.df$Year %>% unique %>% sort)



#back calculated
## latent states
mod_states<-exp(fit1$states*att_Z$`scaled:scale`+att_Z$`scaled:center`)
## observations with imputed values where data missing (using this for these values for the run reconstruction)
mod_obs<- exp(fit1$ytT*att_Z$`scaled:scale`+att_Z$`scaled:center`) %>% colSums()





#what Mark did in 2022


library(MARSS)
model=list(
  Q="equalvarcov",
  # R="diagonal and equal", #default, but getting error when explicity specifying it
  # R="diagonal and unequal",
  B="diagonal and unequal",
  U="unequal")
fit1=MARSS(t(test.ts_z),	model=model,control = list(maxit=2500))

# AIC(fit1,fit2,fit3,fit4,fit5)

#back calculated
## latent states
mod_states<-exp(fit1$states*att_Z$`scaled:scale`+att_Z$`scaled:center`)
## observations with imputed values where data missing (using this for these values for the run reconstruction)
mod_obs<- exp(fit1$ytT*att_Z$`scaled:scale`+att_Z$`scaled:center`)

## one-year-ahead forecast
mod_forecast<-(MARSS::forecast.marssMLE(fit1,h=1))$pred %>% filter(t==23) %>% pull(estimate) %>% `*`(att_Z$`scaled:scale`) %>% `+`(att_Z$`scaled:center`) %>% exp %>% sum()

#mainstem harvest
mainstem_harvest<-mainstem.morts %>% group_by(Year) %>% summarise(morts=sum(Fishing_Mortality)) 

#Columbia River return(ish) reconstruction = tributary escapement +mainstem harvest

##total tributary escapement
trib_escape<-tibble(
  Year=seq(from=min(new.df$Year),to=max(new.df$Year),by=1),
  runsize_obs=colSums((mod_obs))
                    )

##CR return
CR_return<-trib_escape %>% 
  left_join(mainstem_harvest) %>% 
  mutate(runsize_obs=runsize_obs+morts) %>% 
  select(-morts)

write_csv(CR_return,file="Winter_steelhead_Columbia.csv")

# out<-list(fit1=fit1,att_Z=att_Z)
# save(out,file=("out.rds"))

# plot(colSums(exp(mod_obs)))
# plot(colSums(exp(mod_states)))



salm<-fpcDamCounts::fpc_runsum("BON","2000-01-01","2023-12-31","salmon")

sthd<-salm %>% mutate(year=lubridate::year(CountDate),month=lubridate::month(CountDate),year=ifelse(month>5,year+1,year)) %>% filter(month%in%c(11:12,1:3)) %>% group_by(year) %>% summarize(n=sum(AllSteelhead),unclipped=sum(UnClpSteelhead)) 


print(sthd,n=Inf)
sthd %>% filter(between(year,2014,2023)) %>% mutate(rank(n),rank(unclipped))


sthd<-salm %>% mutate(year=lubridate::year(CountDate),month=lubridate::month(CountDate)) %>% filter(between(month,4,6)) %>% group_by(year) %>% summarize(n=sum(AllSteelhead),unclipped=sum(UnClpSteelhead))%>% filter(between(year,2014,2023)) %>% mutate(rank(n),rank(unclipped))



sthd<-salm %>% mutate(year=lubridate::year(CountDate),month=lubridate::month(CountDate),mday=lubridate::mday(CountDate)) %>% filter(month==7|(month==6&mday>=16)) %>% group_by(year) %>% summarize(n=sum(AdultChinook),jack=sum(JackChinook))%>% filter(between(year,2014,2023)) %>% mutate(rank(n),rank(jack))


salm %>% mutate(year=lubridate::year(CountDate),month=lubridate::month(CountDate),mday=lubridate::mday(CountDate)) %>% group_by(year) %>% summarize(n=sum(Sockeye))%>% filter(between(year,2014,2023)) %>% mutate(rank(n))


fpcDamCounts::fpc_runsum("RIS","2022-01-01","2023-12-31","salmon")%>% mutate(year=lubridate::year(CountDate),month=lubridate::month(CountDate),mday=lubridate::mday(CountDate)) %>% group_by(year) %>% summarize(n=sum(Sockeye))%>% filter(between(year,2014,2023)) %>% mutate(rank(n))
