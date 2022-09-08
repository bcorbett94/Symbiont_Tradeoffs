 library(tidyverse)
library(ggplot2)
library(knitr)
propD<-read_csv("Data/qPCR/proportionD.csv")
std_w<-read_csv("Data/qPCR/std.csv")
pam_d<-read_csv("Data/PAM_data/pam_dat.csv")

#convert pam_d into long fo

#offset data
offset<-std_w %>%
  rowwise() %>%
  mutate(offset = mean(c(`perc std 1`,`perc std 2`)))

# Import buoyant weight data
bw_mastersheet <- read_csv("Data/Buoyant_Weight/bw_mastersheet.csv") %>%
  full_join(offset)%>% 
  mutate(species = str_sub(FragID, 1, 1),
         colony = str_sub(FragID, 1, 2),
         pedw = case_when(pedestal == 1 ~ 2.308/offset,
                          pedestal == 2 ~ 2.860/offset)) %>%
  mutate(adj_w = weight/offset,
         final= adj_w-pedw)%>%
  full_join(propD)


#adding column using offset values to adjust weight

# count frags of each colony that are C or D dominated
bw_mastersheet %>%
  distinct(FragID, colony, propD) %>%
  group_by(colony, propD>.5)%>%
  summarise(n = n())

# Create function to convert buoyant weight to dry weight based on temperature and skeletal density
bw.dw <- function(bw, temp, CoralDens) {
  StdAir <- 39.092
  StdFW <- 0.0047263 * temp + 21.474
  StdSW <- 0.0052731 * temp + 21.003
  StdDens <- 1/(1-(StdFW/StdAir))
  SWDens <- StdDens*(1-(StdSW/StdAir))
  CoralWeight <- bw/(1-(SWDens/CoralDens))
  return(CoralWeight)
}

# Remove pedestal weight, and convert coral buoyant weight to dry weight
bw_mastersheet <- bw_mastersheet %>%
  mutate(coraldens = case_when(species == "T" ~ 2.1,                # REPLACE 2.1 WITH TURBINARIA SKELETAL DENSITY
                              species == "P" ~ 2.1),               # REPLACE 2.1 WITH POCILLOPORA SKELETAL DENSITY
         weight_coral = pmap_dbl(.l = list(final, as.numeric(temp), coraldens), ~bw.dw(..1, ..2, ..3)))

#############
# Plot weight over time
fig <- ggplot(bw_mastersheet, aes(x = date, y = weight_coral, 
                                  group = FragID, color = colony)) +
  geom_point() +
  geom_line() +
  facet_wrap(~species)
# Bryce: there is a row with NA for species. Track down why this is there and remove it upstream.
fig %>% filter(bw_mastersheet, !is.na(species))

# There are quite a few frags that decrease in weight between time points
# and we need to QC these. Did they break? Did they have partial mortality?
lost_weight <- bw_mastersheet %>%
  group_by(FragID) %>%
  filter(weight_coral[TimePoint == 4] < weight_coral[TimePoint == 2])

#############
# Calculate percent growth
df <- bw_mastersheet %>%
  group_by(FragID,colony, species, propD) %>%
  #summarise(perc_change1 = (weight_coral[TimePoint == 4] - weight_coral[TimePoint == 1])/(weight_coral[TimePoint == 1])*100)
  summarise(perc_change2 = (weight_coral[TimePoint == 3] - weight_coral[TimePoint == 1])/(weight_coral[TimePoint == 1])*100)
  
#group_By<- tidyverse function that uses column names (no spaces, lower case etc), to make certain cgoups 


#############
# Plot Pocillopora growth, change perc_change# to run multiple anovas
df %>%
 filter(species == "P") %>%
  ggplot(aes(x=propD, y=perc_change2, color = colony))+geom_point()#from
  
# Analyze Pocillopora
pmod <- 
  lm(perc_change2 ~ propD*colony, data = filter(df, species == "P"))
anova(pmod)

#############
# Plot Turbinaria growth

df %>%
  filter(species == "T") %>%
  ggplot(aes(x=propD, y=perc_change2, color = colony))+geom_point()#from

mod <- lm(perc_change2 ~ propD*colony, data = filter(df, species == "T"))
anova(mod)


