# download escapement data from sASI and StreamNet (Coordinated Assessment database)
# SASI https://data.wa.gov/Natural-Resources-Environment/WDFW-Salmonid-Stock-Inventory-Population-Escapemen/fgyz-n3uk
# StreamNet https://www.streamnet.org/data/coordinated-assessments/
# StreamNet documentation https://www.streamnet.org/streamnet-rest-api-documentation/

# StreamNet
# sn.apikey <- "D661AF5F-543F-40F9-878E-42D9EBF2E7A8" # Tim's API key for streamnet 
# sn.connect <- GET(paste("https://api.streamnet.org/api/v1/ca/tables.json?XApiKey=", sn.apikey, sep=""))
# api.connect<-content(sn.connect)

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
extra.esc<-read_xls(path="data/Input_winter_steelhead.xls", sheet = "Escapement") %>% as_tibble()
mainstem.morts<-read_xls(path="data/Input_winter_steelhead.xls", sheet = "FishingMortality_Mainstem") %>% as_tibble()
trib.mort.rates<-read_xls(path="data/Input_winter_steelhead.xls", sheet = "HarvestMortRate_Tributaries") %>% as_tibble()
runsizes_old<-read_xls(path="data/Input_winter_steelhead.xls", sheet = "Runsizes_old_method") %>% as_tibble()

# Populations from SASI
population.list<-c(#"Big White Salmon River Winter Steelhead", # no data available for Big White Salmon
  "Coweeman Winter Steelhead",
  "East Fork Lewis Winter Steelhead",
  "Elochoman/Skamokawa Winter Steelhead",
  "Grays/Chinook Winter Steelhead", 
  "Kalama Winter Steelhead",
  "Klickitat Summer and Winter Steelhead",
  "Lower Cowlitz Winter Steelhead",
  #"Lower Gorge (Columbia) Winter Steelhead", # no data available for Lower Cowlitz
  "Mill/Abernathy/Germany Creeks Winter Steelhead",
  #"North Fork Lewis Winter Steelhead", # no data available for North Fork Lewis
  "North Fork Toutle Winter Steelhead", 
  "Salmon Creek Winter Steelhead", #no data available for Salmon Creek
  "South Fork Toutle Winter Steelhead",
  "Tilton Winter Steelhead",
  "Upper Cowlitz and Cispus Winter Steelhead", 
  "Upper Gorge (Columbia) Winter Steelhead", 
  "Washougal Winter Steelhead")

# Load escapement data from SASI
wa.esc <- read.socrata("https://data.wa.gov/resource/fgyz-n3uk.json") %>% as_tibble() %>%  dplyr::filter(species %in% "Steelhead", production_type != "Hatchery")
wa.esc$stock_number<-as.numeric(wa.esc$stock_number); wa.esc$year<-as.numeric(wa.esc$year); wa.esc$abundance_qty<-as.numeric(wa.esc$abundance_qty)

# Lower Columbia DPS/ESU
cedar.ck.esc<-filter(extra.esc, Population_Name == "Cedar Creek")
coweeman.esc<-filter(wa.esc, population_name == "Coweeman Winter Steelhead", calculation_type == "Expanded", escapement_methodology == "Total Natural Spawners"); coweeman.esc$population_name<-"Coweeman River"; coweeman.esc$esu<-"Lower Columbia"
cowlitz.esc<-filter(wa.esc, population_name == "Upper Cowlitz and Cispus Winter Steelhead", calculation_type == "Unexpanded"); cowlitz.esc$population_name<-"Upper Cowlitz & Cispus"; cowlitz.esc$esu<-"Lower Columbia"
green.river.esc<-filter(wa.esc, population_name == "North Fork Toutle Winter Steelhead", sub_population_name == "Green River (NF Toutle) Winter Steelhead", calculation_type == "Expanded"); green.river.esc$population_name<-"Green River"; green.river.esc$esu<-"Lower Columbia"
kalama.esc<-filter(wa.esc, population_name == "Kalama Winter Steelhead", calculation_type == "Expanded", escapement_methodology == "Total Natural Escapement"); kalama.esc$population_name<-"Kalama River"; kalama.esc$esu<-"Lower Columbia"
lewis.ef.esc<-filter(wa.esc, population_name == "East Fork Lewis Winter Steelhead", calculation_type == "Expanded", data_type == "Spawner Fish"); lewis.ef.esc$population_name<-"Lewis River - E. Fork"; lewis.ef.esc$esu<-"Lower Columbia"
tilton.esc<-filter(wa.esc, population_name == "Tilton Winter Steelhead", calculation_type == "Unexpanded"); tilton.esc$population_name<-"Tilton River"; tilton.esc$esu<-"Lower Columbia"
toutle.nf.esc<-filter(wa.esc, population_name == "North Fork Toutle Winter Steelhead", sub_population_name == "North Fork Toutle River Winter Steelhead", calculation_type == "Unexpanded"); toutle.nf.esc$population_name<-"Toutle River - N. Fork"; toutle.nf.esc$esu<-"Lower Columbia"
toutle.sf.esc<-filter(wa.esc, population_name == "South Fork Toutle Winter Steelhead", calculation_type == "Expanded", escapement_methodology == "Total Natural Spawners"); toutle.sf.esc$population_name<-"Toutle River - S. Fork"; toutle.sf.esc$esu<-"Lower Columbia"
washougal.esc<-filter(wa.esc, population_name == "Washougal Winter Steelhead", calculation_type == "Expanded", escapement_methodology == "Total Natural Spawners"); washougal.esc$population_name<-"Washougal River"; washougal.esc$esu<-"Lower Columbia"
wind.esc<-filter(wa.esc, population_name == "Upper Gorge (Columbia) Winter Steelhead", data_series_num == "3", calculation_type == "Expanded"); wind.esc$population_name<-"Wind River"; wind.esc$esu<-"Lower Columbia"

# Mid Columbia DPS/ESU
klickitat.esc<-filter(wa.esc, population_name == "Klickitat Summer and Winter Steelhead", sub_population_name == "Klickitat Winter Steelhead", calculation_type == "Unexpanded"); klickitat.esc$population_name<-"Klickitat River"; klickitat.esc$esu<-"Mid Columbia"

# Southwest Washington DPS/ESU
abernathy.ck.esc<-filter(wa.esc, population_name == "Mill/Abernathy/Germany Creeks Winter Steelhead", sub_population_name == "Abernathy Creek Winter Steelhead", calculation_type == "Expanded"); abernathy.ck.esc$population_name<-"Abernathy Creek"; abernathy.ck.esc$esu<-"Southwest Washington"
elochoman.esc<-filter(wa.esc, population_name == "Elochoman/Skamokawa Winter Steelhead", sub_population_name == "Elochoman River Winter Steelhead", calculation_type == "Expanded"); elochoman.esc$population_name<-"Elochoman River"; elochoman.esc$esu<-"Southwest Washington"
germany.ck.esc<-filter(wa.esc, population_name == "Mill/Abernathy/Germany Creeks Winter Steelhead", sub_population_name == "Germany Creek Winter Steelhead", calculation_type == "Expanded"); germany.ck.esc$population_name<-"Germany Creek"; germany.ck.esc$esu<-"Southwest Washington"
grays.esc <-filter(wa.esc, population_name == "Grays/Chinook Winter Steelhead", calculation_type == "Expanded"); grays.esc$population_name<-"Grays River"; grays.esc$esu<-"Southwest Washington"
mill.ck.esc<-filter(wa.esc, population_name == "Mill/Abernathy/Germany Creeks Winter Steelhead", sub_population_name == "Mill Creek Winter Steelhead", calculation_type == "Expanded"); mill.ck.esc$population_name<-"Mill Creek"; mill.ck.esc$esu<-"Southwest Washington"
#salmon.cr.esc<-filter(wa.esc, population_name == "Salmon Creek Winter Steelhead"); salmon.cr.esc$population_name<-"Salmon Creek"; salmon.cr.esc$esu<-"Southwest Washington"
skamokawa.esc <-filter(wa.esc, population_name == "Elochoman/Skamokawa Winter Steelhead", sub_population_name == "Skamokawa Creek Winter Steelhead"); skamokawa.esc$population_name<-"Skamokawa River"; skamokawa.esc$esu<-"Southwest Washington"

# All escapements from SASI, not including Cedar Creek
wa.escapements<-rbind(abernathy.ck.esc, coweeman.esc, cowlitz.esc, elochoman.esc, germany.ck.esc, grays.esc, green.river.esc, kalama.esc, 
                      klickitat.esc, lewis.ef.esc, mill.ck.esc, skamokawa.esc, tilton.esc, toutle.nf.esc, toutle.sf.esc, washougal.esc, wind.esc)  
wa.esc.df<-data.frame(Year=wa.escapements$year, Population_Name=wa.escapements$population_name, DPS_ESU=wa.escapements$esu, State="WA", Escapement=wa.escapements$abundance_qty) %>% as_tibble()
# Add Cedar Creek
wa.esc.df<-rbind(wa.esc.df, cedar.ck.esc[,-6:-7])

# OR Escapements
coord.assessements<-read_xls(path="data/Escapements_CoordinatedAssessement_db_2022.xls", sheet = "NOSA") %>% as_tibble()
# Lower Columbia DPS/ESU  
clackamas.esc<-filter(coord.assessements, LOCATIONNAME == "Clackamas River"); clackamas.esc$population_name<-"Clackamas River"; clackamas.esc$esu<-"Lower Columbia"
hood.esc<-filter(coord.assessements, LOCATIONNAME == "Hood River"); hood.esc$population_name<-"Hood River"; hood.esc$esu<-"Lower Columbia"
sandy.esc<-filter(coord.assessements, LOCATIONNAME == "Sandy River"); sandy.esc$population_name<-"Sandy River"; sandy.esc$esu<-"Lower Columbia"
scappoose.esc<-filter(coord.assessements, LOCATIONNAME == "Scappoose {non-TRT Population}"); scappoose.esc$population_name<-"Scappoose"; scappoose.esc$esu<-"Lower Columbia"
# Mid Columbia DPS/ESU
fifteen.mile.esc<-filter(coord.assessements, LOCATIONNAME == "Fifteenmile Creek"); fifteen.mile.esc$population_name<-"Fifteen Mile Creek"; fifteen.mile.esc$esu<-"Mid Columbia"
# Southwest Washington DPS/ESU
big.creek.esc<-filter(coord.assessements, LOCATIONNAME == "Big Creek {non-TRT Population}"); big.creek.esc$population_name<-"Big Creek"; big.creek.esc$esu<-"Southwest Washington"
clatskanie.esc<-filter(coord.assessements, LOCATIONNAME == "Clatskanie River {non-TRT Population}"); clatskanie.esc$population_name<-"Clatskanie River"; clatskanie.esc$esu<-"Southwest Washington"
youngs.bay.esc<-filter(coord.assessements, LOCATIONNAME == "Youngs Bay {non-TRT Population}"); youngs.bay.esc$population_name<-"Youngs Bay"; youngs.bay.esc$esu<-"Southwest Washington"
# Willamette DPS/ESU
willamette.upper.esc<-filter(extra.esc, Population_Name == "Upper Willamette")[,-6]
calapooia.esc<-filter(coord.assessements, LOCATIONNAME == "Calapooia River"); calapooia.esc$population_name<-"Calapooia River"; calapooia.esc$esu<-"Willamette"
molalla.esc<-filter(coord.assessements, LOCATIONNAME == "Molalla River"); molalla.esc$population_name<-"Mollala River"; molalla.esc$esu<-"Willamette"
north.santiam.esc<-filter(coord.assessements, LOCATIONNAME == "North Santiam River"); north.santiam.esc$population_name<-"North Santiam River"; north.santiam.esc$esu<-"Willamette"
south.santiam.esc<-filter(coord.assessements, LOCATIONNAME == "South Santiam River"); south.santiam.esc$population_name<-"South Santiam River"; south.santiam.esc$esu<-"Willamette"

or.escapements<-rbind(big.creek.esc, clackamas.esc, clatskanie.esc, fifteen.mile.esc, hood.esc, sandy.esc, scappoose.esc, 
                      youngs.bay.esc, calapooia.esc, molalla.esc, north.santiam.esc, south.santiam.esc)
or.esc.df<-data.frame(Year=or.escapements$SPAWNINGYEAR, Population_Name=or.escapements$population_name, 
                      DPS_ESU=or.escapements$esu, State="OR", Escapement=or.escapements$NOSAIJ)
or.esc.df<-rbind(or.esc.df, willamette.upper.esc[,-6])

# Combined WA and OR escapements grouped by DPS/ESU, join to trib harvest mort rates, and calculate trib terminal run sizes
# Set up forecasts by Population_Name
end.year<-2022
pop.exclude<-list("Calapooia River", "Cedar Creek", "Mollala River", "North Santiam River", "South Santiam River", "Youngs Bay")
all.esc.df<-rbind(wa.esc.df, or.esc.df) %>% arrange(Population_Name, Year) %>% filter(!Population_Name %in% pop.exclude) %>% 
  group_by(Population_Name, DPS_ESU, State) %>%
  rename(Population_Escapement=Escapement) %>% 
  add_column(Population_Escapement_Source = "Observed", 
             Population_Escapement_Predicted_Low = NA, Population_Escapement_Predicted_High = NA) %>% 
  complete(Year = seq(from=2019, to=end.year, by=1)) %>%  
  arrange(Population_Name, Year) %>% 
  transform(Population_Escapement = as.numeric(Population_Escapement))

new.df<-all.esc.df

for (i in 1 : 25){ #length(unique(all.esc.df$Population_Escapement))
  # set up forecasting dataframe for each population
  pred.df<-filter(all.esc.df, Population_Name == unique(all.esc.df$Population_Name)[i])
  #pred.df<-filter(all.esc.df, Population_Name == "Klickitat River")
  
  ar.auto<-auto.arima(y=as.ts(pred.df[,"Population_Escapement"]))
  #ar.model<-Arima(y=as.ts(pred.df[,"Population_Escapement"], #seasonal=list(order=c(3,1,0), period),
   #                       start=min(pred.df$Year), end=max(pred.df$Year)-5, frequency = 1), 
    #              order=c(1,0,0), 
     #             lambda="auto", #seasonal=list(order=c(0,1,0), period), order=c(1,0,0),
      #            include.mean = F, 
       #           include.drift = F)
  #ar.model2<-Arima(y=as.ts(pred.df[,"Population_Escapement"],include.mean = F, include.drift = F,
                          #start=min(pred.df$Year), end=max(pred.df$Year), frequency = 1))
                    
  
  ar.forecast<-forecast(ar.auto, h=length(which(is.na(pred.df$Population_Escapement) == TRUE)), bootstrap = F); ar.forecast #, level=95, 
  fill.na<-which(is.na(pred.df$Population_Escapement))
  pred.df$Population_Escapement[fill.na]<-round(ar.forecast$mean, 0)
  pred.df$Population_Escapement_Predicted_Low[fill.na]<-round(as.numeric(ar.forecast$low[,2]),0)
  pred.df$Population_Escapement_Predicted_High[fill.na]<-round(as.numeric(ar.forecast$upper[,2]),0)
  pred.df$Population_Escapement_Source[fill.na]<-"Predicted"
  
  new.df[which(new.df$Population_Name == pred.df$Population_Name),]$Population_Escapement <- pred.df$Population_Escapement
  new.df$Population_Escapement_Predicted_Low<-replace(x=new.df$Population_Escapement_Predicted_Low, values=pred.df$Population_Escapement_Predicted_Low)
  new.df$Population_Escapement_Predicted_High<-replace(x=new.df$Population_Escapement_Predicted_High, values=pred.df$Population_Escapement_Predicted_High)
  new.df[which(new.df$Population_Name == pred.df$Population_Name),]$Population_Escapement_Source <- pred.df$Population_Escapement_Source
}
#print(new.df, n=50)
new.df$Population_Escapement<-as.numeric(new.df$Population_Escapement)
#print(filter(new.df, Population_Escapement_Source == 'Predicted'), n=60)
new.df[which(is.na(new.df$Population_Escapement)),]

# Join to trib harvest mortality rates
new.df <- new.df %>% left_join(y=trib.mort.rates, by=c("Population_Name")) %>% rename(Population_TribMortRate=TribMortRate) %>% 
  rename(DPS_ESU=DPS_ESU.x)
new.df<-new.df[,-which(names(new.df) == 'DPS_ESU.y')]
new.df$Population_TerminalRunSize<-round(new.df$Population_Escapement * (1 + new.df$Population_TribMortRate), 0)

# Summarize by DPS/ESU of populations included in run recon and forecasting
esc.by.esu <- group_by(new.df, Year, DPS_ESU) %>% 
  summarise(DPS_ESU_Escapement=sum(Population_Escapement, na.rm=T), DPS_ESU_TerminalRunSize=sum(Population_TerminalRunSize, na.rm=T))

esc.by.esu %>% filter(Year %in% 2020:2022)
  
# Summarize total
mainstem.m<-mainstem.morts %>% group_by(Year) %>% summarize(Mainstem_Mortality=round(sum(Fishing_Mortality)))
total.esc<-esc.by.esu %>% group_by(Year) %>% 
  summarise(Total_Escapement=sum(DPS_ESU_Escapement, na.rm=T), Total_TerminalRunSize=sum(DPS_ESU_TerminalRunSize)) %>% 
  left_join(mainstem.m, by="Year")
total.esc$Total_Runsize<-total.esc$Total_TerminalRunSize + total.esc$Mainstem_Mortality


# Plot all available escapements for each DPS_ESU
# Lower Columbia
ggplot(filter(new.df, DPS_ESU == "Lower Columbia", Population_Escapement_Source == "Observed", Year >= 2001), aes(Year, Population_Escapement)) + 
  geom_line() + facet_wrap(~Population_Name, scales="free") + ggtitle("Lower Columbia DPS/ESU") + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_point(data=filter(new.df, DPS_ESU == "Lower Columbia", Population_Escapement_Source == "Predicted", Year >= 2018), color = 'red') +
  scale_x_continuous(limits = c(2001, end.year)) + theme(axis.text.x = element_text(angle = 65, hjust=1))
# Mid Columbia
ggplot(filter(new.df, DPS_ESU == "Mid Columbia", Population_Escapement_Source == "Observed", Year >= 2001), aes(Year, Population_Escapement)) + 
  geom_line() + facet_wrap(~Population_Name, scales="free") + ggtitle("Mid Columbia DPS/ESU") + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_point(data=filter(new.df, DPS_ESU == "Mid Columbia", Population_Escapement_Source == "Predicted", Year >= 2018), color = 'red') +
  scale_x_continuous(limits = c(2001, end.year)) + theme(axis.text.x = element_text(angle = 65, hjust=1))
# Southwest Washington
ggplot(filter(new.df, DPS_ESU == "Southwest Washington", Population_Escapement_Source == "Observed", Year >= 2001), aes(Year, Population_Escapement)) + 
  geom_line() + facet_wrap(~Population_Name, scales="free") + ggtitle("Southwest Washington DPS/ESU") + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_point(data=filter(new.df, DPS_ESU == "Southwest Washington", Population_Escapement_Source == "Predicted", Year >= 2018), color = 'red') +
  scale_x_continuous(limits = c(2001, end.year)) + theme(axis.text.x = element_text(angle = 65, hjust=1))
# Willamette
ggplot(filter(new.df, DPS_ESU == "Willamette", Population_Escapement_Source == "Observed", Year >= 2001), aes(Year, Population_Escapement)) + 
  geom_line() + facet_wrap(~Population_Name, scales="free") + ggtitle("Willamette DPS/ESU") + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_point(data=filter(new.df, DPS_ESU == "Willamette", Population_Escapement_Source == "Predicted", Year >= 2018), color = 'red') +
  scale_x_continuous(limits = c(2001, end.year)) + theme(axis.text.x = element_text(angle = 65, hjust=1))

# Plot by ESU totals
ggplot(filter(esc.by.esu, Year %in% 2001:(end.year-1)), aes(Year, DPS_ESU_TerminalRunSize)) + 
  geom_line() + facet_wrap(~DPS_ESU, scales="free") + ggtitle("Terminal Runsize by DPS/ESU") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(data=filter(esc.by.esu, Year == end.year), color = 'red')

# Plot total runsize plus forecast for total terminal runsize
ggplot(filter(total.esc, Year %in% 2001:2022), aes(Year, Total_Runsize)) + 
  geom_line(color="black") + ggtitle("Total Runsize") + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_point(data=filter(total.esc, Year == end.year), aes(Year, Total_TerminalRunSize), color = 'red') +
  #geom_line(data=runsizes_old, aes(Year, RunSize_Old), color = 'blue') + 
  #geom_line(data=filter(total.esc2, Year %in% 2001:2019), aes(Year, Total_Runsize), color = 'orange') + 
  scale_y_continuous(limits = c(5000, 45000))

new.df2<-rbind(wa.esc.df, or.esc.df) %>% arrange(Population_Name, Year) %>% #filter(!Population_Name %in% pop.exclude) %>% 
  group_by(Population_Name, DPS_ESU, State) %>%
  rename(Population_Escapement=Escapement) %>% 
  add_column(Population_Escapement_Source = "Observed", 
             Population_Escapement_Predicted_Low = NA, Population_Escapement_Predicted_High = NA) %>% 
  complete(Year = seq(from=2019, to=end.year, by=1)) %>%  
  arrange(Population_Name, Year) %>% 
  transform(Population_Escapement = as.numeric(Population_Escapement))

new.df2 <- new.df2 %>% left_join(y=trib.mort.rates, by=c("Population_Name")) %>% rename(Population_TribMortRate=TribMortRate) %>% 
  rename(DPS_ESU=DPS_ESU.x)
new.df2<-new.df2[,-which(names(new.df2) == 'DPS_ESU.y')]
new.df2$Population_TerminalRunSize<-round(new.df2$Population_Escapement * (1 + new.df2$Population_TribMortRate), 0)

esc.by.esu2 <- group_by(new.df2, Year, DPS_ESU) %>% 
  summarise(DPS_ESU_Escapement=sum(Population_Escapement, na.rm=T), DPS_ESU_TerminalRunSize=sum(Population_TerminalRunSize, na.rm=T)) 

mainstem.m<-mainstem.morts %>% group_by(Year) %>% summarize(Mainstem_Mortality=round(sum(Fishing_Mortality)))
total.esc2<-esc.by.esu2 %>% group_by(Year) %>% 
  summarise(Total_Escapement=sum(DPS_ESU_Escapement, na.rm=T), Total_TerminalRunSize=sum(DPS_ESU_TerminalRunSize)) %>% 
  left_join(mainstem.m, by="Year")
total.esc2$Total_Runsize<-total.esc2$Total_TerminalRunSize + total.esc2$Mainstem_Mortality
            
            
                                             
                                
# WA CRC data
# CRC data maintained by Eric Kraig
# Also found here: S:\FP\Science\CRC Program\Multi-Year Estimates

wa.crc.sthd<-read.csv(file="data/Statewide Steelhead Data 1961-2019.csv", h=T) %>% as_tibble() %>% 
  dplyr::filter(Year >= 2000, Month %in% c(11, 12, 1, 2, 3, 4), Subregion == c("CRLOW", "CRMID"), Run == 'W', Mark == "W")
wa.crc.sthd$Catch<-as.numeric(wa.crc.sthd$Catch)
rename(wa.crc.sthd, Population_Name = System)  

abernathy.crc<-filter(wa.crc.sthd, System == 'Abernathy Cr.'); abernathy.crc$Population_Name<-"Abernathy Creek"
cowlitz.crc<-filter(wa.crc.sthd, System == 'Cowlitz R.'); cowlitz.crc$Population_Name<-"Cowlitz River"
grays.crc<-filter(wa.crc.sthd, System == 'Grays R.'); grays.crc$Population_Name<-"Grays River"
washougal.crc<-filter(wa.crc.sthd, System == 'Washougal R.'); washougal.crc$Population_Name<-"Washougal River"
coal.crc<-filter(wa.crc.sthd, System == 'Coal cr.'); coal.crc$Population_Name<-"Coal Creek"
kalama.crc<-filter(wa.crc.sthd, System == 'Kalama R.'); kalama.crc$Population_Name<-"Kalama River"
lewis.crc<-filter(wa.crc.sthd, System == 'Lewis  R.'); lewis.crc$Population_Name<-"Lewis River - EF"
salmon.cr.crc<-filter(wa.crc.sthd, System == 'Salmon Cr.'); salmon.cr.crc$Population_Name<-"Salmon Creek"  
germany.cr.crc<-filter(wa.crc.sthd, System == 'Germany Cr.'); germany.cr.crc$Population_Name<-"Germany Creek"
hamilton.cr.crc<-filter(wa.crc.sthd, System == 'Hamilton Cr.'); hamilton.cr.crc$Population_Name<-"Hamilton Creek"  
elochoman.crc<-filter(wa.crc.sthd, System == 'Elochoman R.'); elochoman.crc$Population_Name<-"Elochoman River"
white.salmon.crc<-filter(wa.crc.sthd, System == 'White Salmon R.'); white.salmon.crc$Population_Name<-"White Salmon River"
klickitat.crc<-filter(wa.crc.sthd, System == 'Klickitat R.'); klickitat.crc$Population_Name<-"Klickitat River"

harv<-rbind(abernathy.crc, cowlitz.crc, grays.crc, washougal.crc, coal.crc, kalama.crc, lewis.crc, 
                  salmon.cr.crc, germany.cr.crc, hamilton.cr.crc, elochoman.crc, white.salmon.crc, klickitat.crc) %>% 
  group_by(Year, Population_Name) %>% summarize(catch=sum(Catch))

harvest.df<-data.frame(Year=2000:year(Sys.time()), Population_Name=NA, Catch=NA)


Year<-2000:year(Sys.time())

harv.df<-data.frame(Year=2000:2020, Location=NA, Harvest=NA)

