library(tidyverse)
propD<-read_csv("Data/qPCR/proportionD.csv")

bw_mastersheet <- read_csv("Data/Buoyant_Weight/bw_mastersheet.csv") %>%
  mutate(species = str_sub(FragID, 1, 1),
         colony = str_sub(FragID, 1, 2)) %>%
          full_join(propD)
#full_join(symdat)
  

df <- bw_mastersheet %>%
  group_by(FragID,colony, species, propD) %>%
  summarise(perc_change = (weight[TimePoint == 3] -weight[TimePoint == 1])/(weight[TimePoint == 1])*100)
  #summarise(perc_change = (weight[TimePoint == 2] -weight[TimePoint == 1])/(weight[TimePoint == 1])*100)
#group_By<- tidyverse function that uses column names (no spaces, lower case etc), to make certain cgoups 
df %>%
  filter(species == "P") %>%
  ggplot(aes(x=propD, y=perc_change, color = colony))+geom_point()#for P1 & P2
df %>%
  filter(species == "T") %>%
  ggplot(aes(x=propD, y=perc_change, color = colony))+geom_point()

mod<-lm(perc_change ~ propD*colony, data = filter(df, species == "T"))
anova(mod)
