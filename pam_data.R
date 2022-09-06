#Pam DATA and Graphing script
library(tidyverse)
library(dplyr)
library(knitr)
library(ggplot2)
propD<-read_csv("Data/qPCR/proportionD.csv")
std_w<-read_csv("Data/qPCR/std.csv")
pam_d<-read_csv("Data/PAM_data/pam_dat.csv")


#converting pam_d into long-way
pam_sym<-pam_d %>%
  pivot_longer(cols =starts_with("aoi"),
               names_to = c("AOI"),
               values_to = c("FvFm"))%>%
  mutate( species = str_sub(FragID, 1, 1),
          colony = str_sub(FragID, 1, 2))%>%
  full_join(propD)

ggplot(pam_sym, aes (x = temp, y = FvFm, color = propD))+geom_point()


turbC<-pam_sym %>% 
    filter(species =="T") %>%
    group_by(colony, propD<.5)
turbC

ggplot(turbC, aes (x = temp, y = FvFm, color = propD))+geom_point()

turbC%>% 
  case_when(propD<.5 = FALSE ~ )

pt<-pam_sym %>%
  filter(species == "T")
ggplot(pam_sym, aes (x = temp, y = FvFm, color = propD))+geom_point()
