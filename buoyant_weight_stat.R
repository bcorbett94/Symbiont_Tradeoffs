library(tidyverse)
propD<-read_csv("Data/qPCR/proportionD.csv")

# Import buoyant weight data
bw_mastersheet <- read_csv("Data/Buoyant_Weight/bw_mastersheet.csv") %>%
  mutate(species = str_sub(FragID, 1, 1),
         colony = str_sub(FragID, 1, 2)) %>%
  full_join(propD)
  
# Calculate percent growth
df <- bw_mastersheet %>%
  group_by(FragID,colony, species, propD) %>%
  summarise(perc_change = (weight[TimePoint == 3] -weight[TimePoint == 1])/(weight[TimePoint == 1])*100)
  #summarise(perc_change = (weight[TimePoint == 2] -weight[TimePoint == 1])/(weight[TimePoint == 1])*100)
#group_By<- tidyverse function that uses column names (no spaces, lower case etc), to make certain cgoups 


#############
# Plot weight over time
fig <- ggplot(bw_mastersheet, aes(x = TimePoint, y = log(weight), 
                                  group = FragID, color = colony)) +
  geom_point() +
  geom_line() +
  facet_wrap(~species)
# Bryce: there is a row with NA for species. Track down why this is there and remove it upstream.
fig %+% filter(bw_mastersheet, !is.na(species))

# There are quite a few frags that decrease in weight between time points
# and we need to QC these. Did they break? Did they have partial mortality?




#############
# Plot Pocillopora growth
df %>%
  filter(species == "P") %>%
  ggplot(aes(x=propD, y=perc_change, color = colony))+geom_point()#for P1 & P2

# Analyze Pocillopora
pmod <- lm(perc_change ~ propD*colony, data = filter(df, species == "P"))
anova(pmod)



#############
# Plot Turbinaria growth

df %>%
  filter(species == "T") %>%
  ggplot(aes(x=propD, y=perc_change, color = colony))+geom_point()

mod<-lm(perc_change ~ propD*colony, data = filter(df, species == "T"))
anova(mod)


#############

