#Pam DATA and Graphing script
library(tidyverse)
library(knitr)
propD<-read_csv("Data/qPCR/proportionD.csv")
std_w<-read_csv("Data/qPCR/std.csv")
pam_d<-read_csv("Data/PAM_data/pam_dat.csv")


#converting pam_d into long-way
pam_sym<-pam_d %>%
  pivot_longer(cols =starts_with("aoi"),
               names_to = c("AOI"),
               values_to = c("Fv/Fm"))%>%
  mutate( species = str_sub(FragID, 1, 1),
          colony = str_sub(FragID, 1, 2))%>%
  full_join(propD)