#function to read data from various sources. returns a list with several data objects


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

get_data_fun<-function(data_start_year = 0,#exclude years before this
                       data_end_year=2023#exclude years after this
                       ){
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
         !(population_name%in%c("Elochoman-Skamokawa Winter Steelhead","Grays-Chinook Winter Steelhead","Lower Cowlitz Winter Steelhead","Mill-Abernathy-Germany Creeks Winter Steelhead","Tilton Winter Steelhead","Upper Cowlitz and Cispus Winter Steelhead") &data_type!="TSAEJ")|
           #after 2021, mill, abernath and Germany were entered seperately and the total was not entered
           population_name=="Mill-Abernathy-Germany Creeks Winter Steelhead"&year>2021,
         
         # Using Total Escapement instead of Trap Count
         !(population_name=="Upper Gorge (Columbia) Winter Steelhead"&escapement_methodology!="Total Escapement"),
         
         #
         !(population_name=="Klickitat Summer and Winter Steelhead"&sub_population_name!="Klickitat Winter Steelhead")
         
         
  ) %>%
  #get rid of any duplicate rows
  distinct() %>% 
  
  #looking for multiple entries per year x populations
  group_by(year,population_name) %>%
  mutate(n=n()) %>% arrange(population_name,year) %>% 
  arrange(desc(n)) %>% 
  #sum mill, abernathy and Germany
  mutate(sub_population_name=ifelse(population_name=="Mill-Abernathy-Germany Creeks Winter Steelhead"&year>2021,"",sub_population_name)) %>% 
  group_by(population_name,sub_population_name,year) %>% 
  summarize(abundance_qty=sum(as.numeric(abundance_qty))) %>% 
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
all.esc.df<-bind_rows(wa.esc.df2, or.esc.df,willamette.upper.esc %>% mutate(source="Willamette_falls")) %>% 
  ##add 2023 and lower george data from spreadsheet provided by steve Gray
  bind_rows(readxl::read_xlsx("data/Copy of 2023_Wild Winter STHD_All_Sorel.xlsx",sheet=3)) %>% 
 arrange(Population_Name, Year) %>% filter(!Population_Name %in% pop.exclude) %>% 
  group_by(Population_Name) %>%
  rename(Population_Escapement=Escapement) %>% 
  arrange(Population_Name, Year) %>% 
  transform(Population_Escapement = as.numeric(Population_Escapement))


#add tributary harvest rates (table of values originating from god knows where)


new.df<-all.esc.df %>% left_join(trib.mort.rates) %>% 
  filter(between(Year,data_start_year,data_end_year)) %>% 
  #expand escapement to account for tributary harvest
  mutate(Population_Escapement=Population_Escapement/(1-TribMortRate)) %>% 
  mutate(Population_Escapement=ifelse(Population_Escapement==0,1,Population_Escapement))
# %>% filter(Population_Name!="Lower Gorge (Columbia)")


data_summary<-new.df  %>% group_by(Population_Name) %>% 
  summarize(min_year=min(Year),
            max_year=max(Year),
            missing_years=sum(is.na(Population_Escapement)),
            n_years=n(),
            mean_esc=round(mean(Population_Escapement,na.rm=T),2),
            multiples= n_years!=length(unique(Year))
            ) %>% 
  ungroup()

if(sum(data_summary$multiples)>=1){
  print("ALERT: there is more than one count per year for at least one population")
}


#plot data time series by population
ts_plot<-ggplot(new.df,aes(x=Year,y=Population_Escapement))+geom_line()+geom_point()+facet_wrap(~Population_Name,scales="free_y")



return(list(
  new.df=new.df,   #long tibble with returns
  data_summary=data_summary, #some summary stats form data
  ts_plot=ts_plot, # plot of abundance by population

  mainstem.morts=mainstem.morts #mainstem harvest
))
}
