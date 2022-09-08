#Pam DATA and Graphing script
library(tidyverse)
library(dplyr)
library(knitr)
library(ggplot2)
library(ggthemes)
library(drc)
library(broom)
propD<-read_csv("Data/qPCR/proportionD.csv")
pam_d<-read_csv("Data/PAM_data/pam_dat.csv")

#Removing NA's from growth data, T2-06A,T2-13A etc.
df<-df%>%drop_na() 

#converting pam_d into long-way
pam_sym<-pam_d %>%
  pivot_longer(cols =starts_with("aoi"),
               names_to = c("AOI"),
               values_to = c("FvFm"))%>%
  filter(temp != 32, temp != 30)%>%
  mutate( species = str_sub(FragID, 1, 1),
          colony = str_sub(FragID, 1, 2))%>%
  full_join(propD) %>%
  mutate(dom = case_when(propD<.5 ~ 'C', propD>.5 ~ 'D'))%>%
  drop_na()
#Adding placeholder data @ tmp 38, all FvFm= 0

pam_sym<-expand_grid(colony = distinct(pam_sym,colony)[[1]],
            dom = distinct(pam_sym,dom)[[1]],
            FragID = c("A","B","C"),
            temp = 38,
            FvFm = 0)%>%
  full_join(pam_sym)

ggplot(pam_sym, aes (x = temp, y = FvFm, color = dom))+
  geom_point() + 
  facet_wrap(~colony+dom)

# Define function to fit 3-parameter LL model to data and return NULL if fitting error
ll3 <- function(data) {
  drm(FvFm ~ temp, data = data, 
      fct = LL.3(names = c("hill", "max", "ED50")),
      upperl = c(100, 0.7, 40),
      lowerl = c(20, 0.3, 30))}
tryll3 <- possibly(ll3, otherwise = NULL)
# Fit model to each coral, get parameters, fitted values, and residuals
#nested data, all columns except for grouping variables, fit response curve for every group combo of colony symbionts
initmods <- pam_sym %>%
  nest(data = c(FragID, species, AOI, temp, propD, FvFm)) %>%
# Fit the model to each coral, map is using nested data and applies a function previously defined
  
  mutate(ll3 = map(data, tryll3)) %>%
  # Get model parameters and fitted values/residuals
  mutate(pars = map(ll3, tidy),
         pred = map2(ll3, data, ~augment(.x, drop_na(.y, FvFm))))

# Extract ed50 parameter values from model fits
ed50 <- initmods %>% 
  dplyr::select(colony,dom,pars) %>%
  unnest(pars) %>%
  filter(term == "ED50")
# Collect raw data, fitted values, and diagnostics
vals <- initmods %>%
  dplyr::select(colony,dom,pred) %>%
  unnest(pred) %>%
  full_join(ed50) %>%
  full_join(pam_sym) %>%
  rename(ed50 = estimate) %>%
  mutate(problem = "none")

#higher Cooks distance, more influence that point is having on the fit of the regression
f1<- ggplot(vals,aes(x = temp, y = FvFm)) +
  geom_point()+
  facet_grid(colony ~ dom)+
  geom_line(aes(y = .fitted))+
  geom_vline(lty = 2, aes(xintercept = ed50))+
  geom_text(aes(x= ed50, y = 0.1, label = round(ed50,2)),nudge_x = -1.4)+
  ggtitle("ED50 of T. reniformis and P.damicornis")+
  xlab("Temperature (C)") +ylab("Fv/Fm")+
  theme_stata()+
  labs("")
#plyr::left_join(df1, df2[c("columny", "name")], by = c("columnx" = "columny"))
#Plotting ED50 vs growthp%>%

#Adds parts of PAM_sym data to df dataframe that has PAM data, AOI
df<-df%>%
  dplyr::select(FragID,colony,perc_change2)%>%
  full_join(pam_sym)

#Final dataframe grabing ED50 and combining it so FvFm, symbiont type, 
#ED50, and per_chg are all in the same dataframe
f2<-vals%>%
  dplyr::select(FragID,colony,ed50,species,AOI)%>%
  full_join(df)%>%
  drop_na()#Gets rid of dummy data for temp (38) Unsure if this is okay, but the A,B,C were throwing off perc/chg

#Plots all points based on symbiont type C or D as a function of percentchg and FvFm  
ggplot(f2,aes(x =perc_change2 , y = FvFm, color = dom)) +
  geom_point()

#Plots points based on symbiont type and ED50 from vals, better looking
ggplot(f2,aes(x=ed50, y =perc_change2, color = dom, shape = species))+
  geom_point(size = 3)+
  xlab("ED50")+
  ylab("Percent Change")+
  labs(shape = "Species", color = "Symbiont Type")+
  scale_shape_discrete(name  ="Species",
                       breaks=c("P", "T"),
                       labels=c(expression(italic("P.damicornis")),expression(italic("T.reniformis"))))+
  scale_color_discrete(name = "Symbiont Type",
                       breaks = c("C","D"),
                       labels = c("Cladocopium spp.", "Durisdinium spp."))



ggplot(f2,aes(x=ed50, y =perc_change2, color = species))+
  geom_point()


