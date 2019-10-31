library(tidyverse)


#setwd("C:/Users/MAGNUS/OneDrive/A) salmo salar master/DATA/Gjenfangst PIT villsmolt Dale/Master-villsmolt-Dale-MagnusHulbak")

#load elfishing and wolf data for wild smolts from Dale

elfiske_wolf<-read.table("DATA/Dale_elfiske_sandflaten_wolftrap_csv.csv", sep=";", dec=",", header = TRUE, na.strings = NA)
elfiske_wolf
#------------------------------------------------------------------------------------------------

#Changing the date format from e.g 15.04.2019 as in excel, to 15-04-19.
#dates$V1 <- format(as.Date(dates$V1), "%d.%m.%Y")

#elfiske_wolf$FANGST_DATO <- format(as.Date(elfiske_wolf$FANGST_DATO, "%d.%m.%Y")
elfiske_wolf$FANGST_DATO<- format(as.Date(elfiske_wolf$FANGST_DATO, "%d.%m.%Y"))
elfiske_wolf

#Koder for å gjøre fangstdato om til d.m.y og ikke y.m.d som ikke funket helt.
#dates$V1 <- strptime(as.character(dates$V1), "%d.%m.%Y")
#dates$V1 <- format(dates$V1, "%d.%m.%Y")

#elfiske_wolf$FANGST_DATO<-strptime(as.character(elfiske_wolf$FANGST_DATO,"%Y-%m-%d"))
#elfiske_wolf$FANGST_DATO<-format(elfiske_wolf$FANGST_DATO,"%d.%m.%Y")


#lager nye kolonner kalt dato og dato_uts. Kolonnen dato består av FANGST_dato(dato elfisket eller fanget i wolf-felle) omgjort til antall dager etter 1.januar, mens kolonne dato_uts er utsettsdato(Dato sluppet ut, enten på sandflaten eller nedenfor wolf)

elfiske_wolf$Dato=as.numeric(format(as.Date(elfiske_wolf$FANGST_DATO,"%Y-%m-%d"),"%j"))

elfiske_wolf$Dato_uts=as.numeric(format(as.Date(elfiske_wolf$UTSETT_DATO,"%d.%m.%Y"),"%j"))

elfiske_wolf


#-------------------------------------------------------------------------------------------------
#Selecting columns fangststed and lengde. Filter for Dale_Sandflaten in Fangssted, where all relevant elfishing data of wild smolts is from, excluding recaptures. Finding mean length and standard deviation.


length_elfished_wildsmolt <- elfiske_wolf %>%
  
  select(FANGST_STED, LENGDE, GJENFANGST) %>%
  
  filter(FANGST_STED == "Dale_Sandflaten") %>%
  
  filter(GJENFANGST != "Ja")

length_elfished_wildsmolt

# mean length and standard deviation

length_elfished_wildsmolt %>%
  summarise(mean_length = mean(LENGDE),
            sd_length = sd(LENGDE))

#-------------------------------------------------------------------------------------------------
#Creating a new dataframe with only wolf-trap data

dat_w=(elfiske_wolf[elfiske_wolf$FANGST_STED=="Dale_wolf",])

#creating a new dataframe with only elfished data from Sandflaten

dat_s=(elfiske_wolf[elfiske_wolf$FANGST_STED=="Dale_Sandflaten",])

#Number of smolts caught in wolf trap per date. Grouping by date, and counting number of rows per date. 1 row=1 smolt. The first observation, 21.04.2019, is fish caught in the trap during 17.04-21.04. From the 21.04 the wolf was checked each day.

smolts_caught_in_wolf_with_time <- dat_w %>%
  group_by(Dato) %>%
  summarise(count = n()) 
 
smolts_caught_in_wolf_with_time 


mod1<-dat_w %>% 
  group_by(Dato) %>%
  summarise(count = n()) %>%
  glm(Dato~count, family="poisson")

#plotting number of smolts caught in wolf trap against time. Link this plot up with temperature and water discharge data from Dale?

dat_w %>%  
  group_by(Dato) %>%
  summarise(count = n()) %>% 
  ggplot(aes(Dato, count))+
  geom_point()+
  labs(title = "Number of wild smolts caught in wolf trap against time", x="Day of the year", y="N smolts in wolf trap")




#-----------------------------------------------------------------------------------------------------

 # I want to find out if a sample of elfished wild smolts migrate out of the river continously throughout may and june, or if they migrate directly after capture. Is the sample representative of the population in terms of migration? 

#Check the time difference between capture and release on Dale_sandflaten, and recapture in the wolf trap.

#smolts found dead in the wolf trap have been registered as "død" in Utsettsted in Dale_elfiske_sandflaten etc excel file.


#---------------------------------------------------------------------------------------------------------




# sorterer datasettet med wolf data hvor man fjerner radene(fiskene) hvor PIT ID er NA, og individene som ble funnet døde.  
dw=dat_w %>%
  
  filter(!is.na(PIT_ID)) %>%
  
  filter(UTSETT_STED!="D>d") %>%
  
  select(Dato,PIT_ID,LENGDE,VEKT,GJENFANGST)

# sorterer datasettet med elfiskedata fra sandflaten(oppstrøms wolf) der jeg fjerner radene(fiskene) hvor PIT ID er NA, og individene som ble funnet døde.  


ds=dat_s %>%
  
  filter(!is.na(PIT_ID)) %>%
  
  filter(UTSETT_STED!="D>d") %>%
  
  select(Dato,PIT_ID,LENGDE,VEKT,GJENFANGST)

#Slår sammen datasettene med funksjonen left_join basert på PIT-id. Med andre ord sorterer jeg slik at jeg kun ser på villsmolt som har blitt elfisket OG fanget i wolf-fellen. Ergo kan jeg se på forskjell i elfisketidspunkt og utvandringstidspunkt(dato fanget i wolf).

newd=left_join(dw,ds,by="PIT_ID")

#plotter fangstdato i felle mot elfiskedato

newd %>%
  
  filter(!is.na(Dato.y)) %>%
  
  ggplot(aes(y = Dato.x, x = Dato.y)) +
  
  geom_point() +
  
  geom_smooth(method = "lm") +
  labs(y = "Day captured in wolf trap", x = "Day PIT-tagged") +
  labs(title = "Timing of outwards migration for different groups of PIT-tagged wild smolt")




newd %>%
  ggplot(aes(y = Dato.x, x = LENGDE.x)) + geom_point() +
  geom_smooth(method = "lm")






#----------------------------------------------------------------------------------------------------

#Importing dataset with gill samples and measured ATPase activity in wild smolt caught in Dale wolf trap

gilldata <- read.table("DATA/Gjelleprøver av vill- og settesmolt Dale vår-19.csv", sep=";", dec=",", header = TRUE, na.strings = NA)


ATPase_wild <- gilldata %>% 
  select(Dato,opphav,Lengde.mm.,PIT.merke,nmol.ADP.mg.protein.hour) %>% 
  filter(opphav=="villsmolt") %>% 
  group_by(Dato) %>% 
  ggplot(aes(Dato, nmol.ADP.mg.protein.hour))+ 
  geom_boxplot()+
  labs(title="ATPase gill activity in wild smolts at various timepoints")+
  labs(y="nmol ADP/mg protein/hour", x="Date of gill samples")
ATPase_wild  

Lenght_ATPase<-gilldata %>% 
  filter(opphav=="villsmolt") %>% 
  ggplot(aes(Lengde.mm.,nmol.ADP.mg.protein.hour))+
  geom_point()+
  geom_smooth(method="lm")
Lenght_ATPase






  
