setwd("C:/Users/MAGNUS/OneDrive/Dokumenter/Biologi Bachelor/A) salmo salar master/DATA")
library(tidyverse)
Data_villsmolt_Dale.df<-read.table("REEDITED-GJENFANGST-ALLOFTHEM.csv", sep=";", dec=",", package="vegan")

#Lage tibble/tabell av data
Data_villsmolt_Dale.df<-as_tibble(Data_villsmolt_Dale.df)
Data_villsmolt_Dale.df

#select
Data_villsmolt_Dale.df %>% 
  select(V2,V4) %>% 
  filter(V2="Dale_sandflaten",V4="Elfiske") %>% 

  