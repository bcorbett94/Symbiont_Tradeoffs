library(tidyverse)
symdat<-read_csv("Data/Buoyant_Weight/symdat.csv")

bw_mastersheet <- read_csv("Data/Buoyant_Weight/bw_mastersheet.csv") %>%
  mutate(species = str_sub(FragID, 1, 1),
         colony = str_sub(FragID, 1, 2)) %>%
  full_join(symdat)


df <- bw_mastersheet %>%
  group_by(FragID, sym,colony, species) %>%
  summarise(perc_change = (weight[TimePoint == 3] -weight[TimePoint == 1])/(weight[TimePoint == 1])*100)
#group_By<- tidyverse function that uses column names (no spaces, lower case etc), to make certain cgoups 
df %>%
  filter(species == "T") %>%
  ggplot(aes(x=colony, y=perc_change, color = sym))+geom_point()


df
mod<-lm(perc_change ~ sym+colony, data = filter(df, species == "T"))
anova(mod)
