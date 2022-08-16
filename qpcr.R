library(steponeR)
library(tidyverse)
library(dplyr)
plates <- list.files(path = "Data/qPCR", pattern = ".txt", full.names = TRUE)
plates
#pl
# Read in data and calculate target ratios
df <- steponeR(files = plates, 
               delim = "\t", 
               target.ratios = c("C.D"), 
               fluor.norm = list(C = 2.234, D = 1),
               copy.number = list(C = 20, D = 3),
               ploidy = list(C = 1, D = 1),
               extract = list(C = 0.813, D = 0.813))
qpcr <- df$result

# Calculate proportion D for each sample
qpcr<-qpcr %>%
  mutate(propD = case_when(is.na(D.CT.mean) & !is.na(C.CT.mean) ~ 0,
                                        !is.na(D.CT.mean) & is.na(C.CT.mean) ~ 1,
                                        !is.na(C.D) ~ 1/((C.D)+1))) 

# Filter out duplicates (run on muiltiple plates)
dupesamp <- qpcr %>%
  count(Sample.Name) %>%
  filter(n > 1)
dupesamp

dupes<- qpcr %>%
  filter(Sample.Name %in% dupesamp$Sample.Name)


nondupes <- qpcr %>%
  filter(!Sample.Name %in% dupes$Sample.Name)

# Look at the duplicated samples to see which runs are best
dupes %>% arrange(Sample.Name)

# across the board, samples run on the 7.22 plate worked better than the 7.02 plate, so pick those...

# pick the best rows from the duplicated samples
best <- dupes %>%
  filter(File.Name == "bc_symtrad_7.22.txt")

# Merge best runs of dupes back with all the nondupes
qpcr_good <- bind_rows(nondupes, best)



qpcr_good %>%
  select(FragID = Sample.Name,propD) %>%
  filter(!FragID == "positive",!FragID == "negative") %>% 
  write_csv("Data/qPCR/proportionD.csv") 


#mutate(prop = 1/((C.D)+1))
#%>% head()
view(qpcr
  )
#add_column()

#fluornorm-> flouroscent normalization, has to do with the difference in fluoroscent dyes VIC & FAM
# took 2.234 cycles more for C to amplify vs D for 1,
#copy number: for actin gene, genome has more than one actin, 
#extract -> extract efficiency, erxtract from 100 cells probably getting 80 percent of it 
#get proportion of D, 1/((C+D)
#qpcr %>%
 # mutate(prop = 1/((C.D)+1))
  #add_column()
  
View(qpcr)
