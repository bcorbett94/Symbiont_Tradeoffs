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

qpcr %>%
  group_by(
    D.CT.mean, C.CT.mean
  )
  

  is.na(qpcr$D.CT.mean)

  qpcr<-mutate (proportion1 = if_else(is.na(D.CT.mean) == TRUE), 0 , 1)

  

#mutate(prop = 1/((C.D)+1))
#%>% head()
view(qpcr
  )
#add_column()
# View data
is.na_replace_0 <- data$x_num                               # Duplicate first column
is.na_replace_0[is.na(is.na_replace_0)] <- 0                # Replace by 0

#fluornorm-> flouroscent normalization, has to do with the difference in fluoroscent dyes VIC & FAM
# took 2.234 cycles more for C to amplify vs D for 1,
#copy number: for actin gene, genome has more than one actin, 
#extract -> extract efficiency, erxtract from 100 cells probably getting 80 percent of it 
#get proportion of D, 1/((C+D)
#qpcr %>%
 # mutate(prop = 1/((C.D)+1))
  #add_column()
  
View(qpcr)
ap