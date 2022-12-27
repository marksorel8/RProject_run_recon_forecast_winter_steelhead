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
##estimates of main stem columbia commercial and sport harvest and post-release mortality on theaggregate run
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
                        !grepl("pHOS",escapement_methodology) ) %>% #looking for multiple entries per year x populations
  group_by(year,population_name,sub_population_name) %>%
  mutate(n=n()) %>%
  #deal with some cases where more than one entry for a year x population
  filter(!(n>1&escapement_methodology%in%c("NOSA TSAEJ","Trap Count (Natural-Origin)")),
          !(population_name%in%c("North Fork Toutle Winter Steelhead","Elochoman-Skamokawa Winter Steelhead","Mill-Abernathy-Germany Creeks Winter Steelhead","Grays-Chinook Winter Steelhead")&
              (is.na(sub_population_name)|sub_population_name=="")),#using green and trap counts indvidually
         !(population_name=="Upper Gorge (Columbia) Winter Steelhead"&(is.na(sub_population_name)|sub_population_name=="")),#using Wind river, which is the same as upper Gorge
         
         !(population_name=="Klickitat Summer and Winter Steelhead"&sub_population_name!="Klickitat Winter Steelhead"
         )) %>% 
  mutate(
         Population=ifelse(sub_population_name=="",population_name,sub_population_name),
         Population=sub(" Winter Steelhead","",Population))  %>% 
  #drop missing values
  filter(!is.na(abundance_qty)) %>% 
  #add 2022 Kalama (Steve Grey pers. correspondence)
  bind_rows(
    filter(.,population_name == "Kalama Winter Steelhead"&year==2021) %>% mutate(year="2022",abundance_qty="881")
  ) %>% 
  distinct() %>% #get rid of any duplicates
    ungroup %>% 
  # columns needed for analysis: year, population, escapement
select(Year=year,Population_Name=Population,Escapement=abundance_qty) %>% 
  mutate(across(c(Year,Escapement),as.integer))
  

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
  mutate(Escapement=as.numeric(Escapement))
rm(nosa_df)

## populations to exclude from analysis (some because upstream of Wilamtter Falls, which we are using counts from, and Youngs Bay probably because of short time series length)
pop.exclude<-list("Calapooia River", "Cedar Creek", "Molalla River", "North Santiam River", "South Santiam River", "Youngs Bay")

# Combined WA and OR escapements
all.esc.df<-bind_rows(wa.esc.df2, or.esc.df,willamette.upper.esc) %>% arrange(Population_Name, Year) %>% filter(!Population_Name %in% pop.exclude) %>% 
  group_by(Population_Name) %>%
  rename(Population_Escapement=Escapement) %>% 
  arrange(Population_Name, Year) %>% 
  transform(Population_Escapement = as.numeric(Population_Escapement))


#add tributary harvest rates (table of values originating from god knows where)
new.df<-all.esc.df %>% left_join(trib.mort.rates) %>% 
  filter(Year>=2001) %>% #years with more complete data
#expand escapement to account for tributary harvest
  mutate(Population_Escapement=Population_Escapement/(1-TribMortRate))



#pivot wider with column for each population for MARSS modeling
wide_df.ts<-new.df %>% 
  dplyr::select(Year,Population_Name,Population_Escapement) %>% pivot_wider(names_from = Population_Name,values_from = Population_Escapement) %>% arrange(Year) %>% 
  #drop Year column and log for input into MARSS
  select(-1) %>% log

#look at correlations between population escapement time series
cor.ts<-cor(wide_df.ts,use="pairwise.complete.obs")


# Z-score escapements
test.ts_z<-scale(wide_df.ts)
att_Z<-attributes(test.ts_z)#save means and SDs for back-transform

#fitt MARSS model to log escapement
## I explored some different specifications and the below one had most support based on AICc
library(MARSS)
model=list(
  Q="equalvarcov",
  # R="diagonal and equal", #default, but getting error when explicity specifying it
  # R="diagonal and unequal",
  U="unequal")
fit1=MARSS(t(test.ts_z),	model=model,control = list(maxit=2500))

# AIC(fit1,fit2,fit3,fit4,fit5)

#back calculated
## latent states
mod_states<-exp(fit1$states*att_Z$`scaled:scale`+att_Z$`scaled:center`)
## observations with imputed values where data missing (using this for these values for the run reconstruction)
mod_obs<- exp(fit1$ytT*att_Z$`scaled:scale`+att_Z$`scaled:center`)

## one-year-ahead forecast
mod_forecast<-(MARSS::forecast.marssMLE(fit1,h=1))$pred %>% filter(t==23) %>% pull(estimate) %>% `*`(att_Z$`scaled:scale`) %>% `+`(att_Z$`scaled:center`) %>% exp

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

