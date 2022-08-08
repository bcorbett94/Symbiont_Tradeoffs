library(tidyverse)
symdat<-read_csv("Data/Buoyant_Weight/symdat.csv")

bw_mastersheet <- read_csv("Data/Buoyant_Weight/bw_mastersheet.csv") %>%
  mutate(species = str_sub(FragID, 1, 1)) %>%
  full_join(symdat)


df <- bw_mastersheet %>%
  group_by(FragID, sym, species) %>%
  summarise(perc_change = (weight[TimePoint == 2] -weight[TimePoint == 1])/(weight[TimePoint == 1])*100)
#group_By<- tidyverse function that uses column names (no spaces, lower case etc), to make certain cgoups 
df %>%
  filter(species == "P") %>%
  ggplot(aes(x=sym, y=perc_change))+geom_point()


#