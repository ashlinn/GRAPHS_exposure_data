
# final bw exposure-response analysis (started April 2018)

# look at: BW_analysis.R (from Jan 2017)

prenatalCO <- readRDS("/Users/quinnak/Dropbox/BW EDD/time_weighted_co_summary_prenatal_validity1_Aug31.rds")

# 1332 rows



# bw data:
bw <- readRDS("/Users/quinnak/Dropbox/BW EDD/BW_data_complete_Aug2017.rds") # I think this is the final BW data, this file from Aug 2017, generated from "time_weighted_exposure.R" 

alldata <- merge(bw, prenatalCO, by = "mstudyid", all.x = TRUE)

# remove rows with pregnancy duration over 301 days (43 weeks)
alldata <- filter(alldata, preg_duration <301) #1213 rows

# remove wealth index and add new asset index from Dan C
alldata <- select(alldata, -c(wealthindex, wealthindex2))

asset <- read.csv("/Users/quinnak/Dropbox/BW ER Analysis/April 2018/Asset_Index.csv", stringsAsFactors = FALSE)
alldata <- merge(alldata, asset, by = "mstudyid", all.x = TRUE)

# 4 NA for asset index

# add malaria

hist(alldata$bweight_grams)
