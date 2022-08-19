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


qpcr<-separate(qpcr, col = Sample.Name, into = c('Sample.Name','Dilution'), sep = ':', remove = TRUE, convert = FALSE, extra = "warn", fill = "warn")
qpcr <- as_tibble(qpcr)

#Filtering out Samples that didn't work

qpcr_good<-qpcr %>% 
  filter (!(C.reps == 0 & D.reps ==0)) %>%
  filter(!(C.reps <= 1 & D.reps <= 1)) %>%
  filter(!Sample.Name== "positive",!Sample.Name == "negative")

bad<-anti_join(qpcr,qpcr_good)
bad
# Filter out duplicates (run on muiltiple plates)

dupesamp <- qpcr_good %>%
  count(Sample.Name) %>%
  filter(n>=2)
dupesamp


dupes<- qpcr_good %>%
  filter(Sample.Name %in% dupesamp$Sample.Name)
dupes
nondupes <- qpcr_good %>%
  filter(!Sample.Name %in% dupes$Sample.Name)

# Look at the duplicated samples to see which runs are best
dupes %>% arrange(Sample.Name)
# across the board, samples run on the 7.22 plate worked better than the 7.02 plate, so pick those...

# pick the best rows from the duplicated samples, Turbinaria only run, output nine observations for plate run on 8.16
best <- dupes %>%
  filter(File.Name != "bc_symtrad1_7.02.txt")%>%
  filter(File.Name != "bc_symtrad2_7.02.txt") %>%
  filter(Dilution!=100 | is.na(Dilution))%>%
  slice(-c(11,15))
best

# Merge best runs of dupes back with all the nondupes
qpcr_good <- bind_rows(nondupes, best)

#qpcr_good<-slice(qpcr_good,-c(62,69,31,43,58))
qpcr_good %>%
  select(FragID = Sample.Name,propD) %>%
  write_csv("Data/qPCR/proportionD.csv") 

#mutate(prop = 1/((C.D)+1))
#%>% head()
view(qpcr_good)
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
