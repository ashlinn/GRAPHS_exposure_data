#### INTERPOLATE EXPOSURE DATA OVER PREGNANCY #############

# load packages
require(haven)
require(readxl)
require(plyr)
require(dplyr)
require(lubridate)
require(PerformanceAnalytics)


# load data

# bw data
bw4 <- readRDS("/Users/quinnak/BW EDD/BW_data_complete_Aug2017.rds")

# co data
co <- readRDS("/Users/quinnak/Dropbox/BW ER Analysis/Birthweight ER analysis/FINAL_CO_parameters_withvalidation_2016Jun14.rds")

############
# previous data
bw <- read_dta("/Users/quinnak/BW EDD/BW_data_cleaned_abstract_stata12.dta")
edd <- read_excel("/Users/quinnak/BW EDD/GRAPHS_FINALIZED_EDD_confirmation_by_Blair_May_19_2017.xlsx")
edd <- select(edd, -X__1, -X__2)
edd$finaledd <- as.numeric(edd$finaledd)
edd$finaledd <- as.Date('1900-01-01')+edd$finaledd-2

# rationale for trimesters (BJW):
# length of pregnancy: 40 weeks (280 days)
# 14 weeks is universally used as end of first trimester by obstetricians
# 3rd tri: 28 and under is considered miscarriage by who still even though viability here is around 24 weeks, so I vote we use 28 weeks for this study 
edd$preg_start <- edd$finaledd - days(280) # 40 weeks
edd$tri2_begin <- edd$preg_start + weeks(14) # 14 weeks
edd$tri3_begin <- edd$preg_start + weeks(28) # 28 weeks

# subsetting key variables in bw

#Identity & cluster: mscreenid, mstudyid, intervention, community

# Infant outcome: bweight_grams, LBW, PTB, SGA, B1ALIVE1 (live birth), HCIRC, BLENGTH, BSEX1, NUMBABY (mult births)

# dates: DATDELIV2 (birth date), DATBWT2 (date weighed), DATDIFF

# Materinal covariates: AGE, MEDLEV, [asset index], BMI, MUAC_MOTHER, CALCPARITY,  SMOKECUR, BP (hist of hypertension), DIABETES, ANAEMIA2, HIV, SYPTEST (syphilis test), NUMANCVISIT, SALARY (categorical for occupation)

# Malaria? 

# 
#The variables that should be included regardless of their association w/ exposure includes maternal age, BMI, maternal MUAC, parity, and sex of infant

# Another significant category of determinants of birth outcomes is maternal history (htn, diabetes, hiv, anaemia).  That said, when we looked at these variables before, the self reported prevalence was fairly low.  We may be able to look at these variables in relation to exposure and in relation to birth weight and consider not including in models.

# SES including wealth as well as education and occupation—and this is where the real confounding may lie. This category could also include adequacy of antenatal care. 

# Finally, there are certain things like malaria and syphilis that may be common here and these should be included.  (Syphilis only if there are not too many missing).  If we have the placental slides for malaria, we may not need to include IPTp and ITN use. 

# merge datasets
names(bw) <- tolower(names(bw))
bw2 <- filter(bw, enrolled == 1) # get rid of unenrolled
bw2 <- merge(bw2, select(edd, mstudyid, finaledd, preg_start, tri2_begin, tri3_begin), by = "mstudyid")
bw2 <- select(bw2, -trialedd) # get rid of older "trialedd"
saveRDS(bw2, file = "BW_data_finalized_edd_Aug2017.rds")

# add asset index -----


# select relevant variables
bw3 <- select(bw2, mstudyid, cstudyid, cluster, community, datevisit_enrol, bweight_grams, lbw, ptb, sga, b1alive1, hcirc, blength, bsex1, numbaby, datdeliv2, datebwt2, age, medlev, bmi, muac_mother, calcparity, smokecur, bp, diabetes, anaemia2, hiv, syptest, numancvist, salary, wealthindex, wealthindex2, finaledd, preg_start, tri2_begin, tri3_begin)

# select non-NA bweights
bw3 <- filter(bw3, b1alive1 == 1) # 1303 live births
bw3 <- bw3[!is.na(bw3$bweight_grams),] # 1293 with bweight
nrow(bw3[which(as.numeric(bw3$datebwt2 - bw3$datdeliv2)<0 | as.numeric(bw3$datebwt2 - bw3$datdeliv2) > 1),]) # 50

bw4 <- bw3[!(as.numeric(bw3$datebwt2 - bw3$datdeliv2)<0 | as.numeric(bw3$datebwt2 - bw3$datdeliv2) > 2),] # 1252 with bweight measured within first 2 days

saveRDS(bw4, file = "BW_data_complete_Aug2017.rds")

fishy <- bw3[as.numeric(bw3$datdeliv2 - bw3$preg_start) %in% boxplot.stats(as.numeric(bw3$datdeliv2 - bw3$preg_start), coef = 3)$out, c("mstudyid", "preg_start", "finaledd", "datdeliv2")]
fishy$preg_duration <- as.numeric(fishy$datdeliv2 - fishy$preg_start)
write.csv(fishy, file = "fishy_duration.csv")

##### CO -------

# merge CO data with trimester data
bw3$mstudyid[!bw3$mstudyid %in% unique(co$mstudyid)] # none
unique(co$mstudyid[!co$mstudyid %in% edd$mstudyid]) #2
co1 <- merge(co, edd[, c("mstudyid", "finaledd", "preg_start", "tri2_begin", "tri3_begin")], by = "mstudyid")

# select only mother's CO
co2 <- co1[is.na(co1$cstudyid),]
# generate: 1st trimester exposure; 3rd trimester exposure, exposure over pregnancy
# important variables from co: mstudyid, session, lastdate, co_mean_corr, co_day1_mean_corr, co_mean_first48_corr, co_mean_first72_corr, co_hours, visually_valid, visual_notes, overall_valid, finaledd, preg_start, tri2_begin, tri3_begin

co2 <- select(co2, mstudyid, session, lastdate, co_mean_corr, co_day1_mean_corr, co_mean_first48_corr, co_mean_first72_corr, co_hours, visually_valid, visual_notes, overall_valid, finaledd, preg_start, tri2_begin, tri3_begin)

# get rid of CO with duration < 48h or visually invalid trace
co2 <- co2[co2$co_hours >=48 & co2$visually_valid <3,]

# add in delivery date
co2 <- merge(co2, bw2[, c("mstudyid", "datdeliv2")], by = "mstudyid")
co2 <- filter(co2, !is.na(datdeliv2)) # delete those with no delivery date

saveRDS(co2, file = paste0("prenatal_co_valid_1or2_", format(Sys.Date(), format = "%b%d"), ".rds"))

# filter only those with validity "1"
co3 <- filter(co2, visually_valid == 1)
saveRDS(co3, file = paste0("prenatal_co_valid_only1_", format(Sys.Date(), format = "%b%d"), ".rds"))

# for each woman:
# define pregnancy as 280 days: preg_start to finaledd
# generate a co value for each day
# create trimester averages [keeping in mind baby's actual birth date]
# create overall exposure value [keeping in mind baby's actual birth date]
#trimester cutoffs in days: 1st tri 1- 97; 2nd 98 - 195; 3rd 196 - birth date
max(co2$datdeliv2 - co2$preg_start) # 406 days [!]

##### INTERPOLATION AND PLOTTING -----






#### TYPE 2 -------
# USING LINEAR INTERPOLATION BACK FROM SECOND POINT

for (j in c("co_mean_corr", "co_day1_mean_corr", "co_mean_first48_corr")) {
  
  exposurematrix <- as.data.frame(matrix(nrow = length(unique(co2$mstudyid)), ncol = 317)) #315 = 45 weeks
  names(exposurematrix) <- c(paste0("day", 1:315), "mstudyid", "preg_duration")
  studyids <- unique(co2$mstudyid)
  
  
  for (i in 1:length(studyids)) {
    tryCatch({
      codata <- filter(co2, mstudyid == studyids[i])
      codata <- filter(codata, as.Date(lastdate) < datdeliv2) # including only prenatal measurements. But what about session 5, relevant to the end of pregnancy?
      codata$preg_day <- as.numeric(as.Date(codata$lastdate) - codata$preg_start)
      codata <- codata[order(codata$preg_day),]
      measurement_days <- sort(codata$preg_day)
      exposurematrix[i,316] <- studyids[i]
      exposurematrix[i, 317] <- as.numeric(codata$datdeliv2 - codata$preg_start)[1]
      print(studyids[i])
      print(length(measurement_days))
      exposurematrix[i, 1:measurement_days[1]] <- mean(codata[codata$preg_day == measurement_days[1], j]) #constant interp back from first measurement
      exposurematrix[i, min(315,max(measurement_days)):min(315,exposurematrix$preg_duration[i])] <- mean(codata[codata$preg_day == max(measurement_days), j]) #constant interp forward from last pre-delivery measurement to end of pregnancy (max of 315 days)
      
      if (length(measurement_days) >1) {
        ### need to fix this for ones with only one measurement day
        # exposurematrix[i, (measurement_days[1]+1):measurement_days[2]] <- mean(codata[codata$preg_day == measurement_days[2],j]) # constant interp back from 2nd measurement
        
        middle <- data.frame(date = measurement_days[1]:max(measurement_days), co = NA)
        for (g in 1:length(measurement_days)) {
          middle$co[middle$date == measurement_days[g]] <- mean(codata[codata$preg_day == measurement_days[g],j], na.rm = TRUE)
        }
        middle$co_interp <- approx(middle$date, middle$co, n = nrow(middle))$y
        middle <- filter(middle, date <=315)
        
        exposurematrix[i, (measurement_days[1]):min(315,measurement_days[nrow(codata)])] <- middle$co_interp # linear interp between 2nd and last measurement
      }
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  
  
  
  # plot
  pdf(file = paste0("preg_co_interp2_",j, format(Sys.Date(), format = "%Y%b%d"), ".pdf"), height = 10, width = 10)
  par(mfrow = c(3,3))
  for (i in 1:nrow(exposurematrix)) { #1:nrow(exposurematrix)
    xax <- 1:(ncol(exposurematrix) - 2)
    yax <- exposurematrix[i, 1:(ncol(exposurematrix)-2)]
    
    # make empty plot
    plot(0, type="n", xlab="days", ylab=j ,main = exposurematrix$mstudyid[i], xlim=c(1, 315), ylim=c(0, ifelse(sum(!is.na(exposurematrix[i, 1:315])) >0, max(3,ceiling(max(exposurematrix[i, 1:315],na.rm = TRUE))), 3))) 
    # add lines for interpolated CO
    lines(xax,yax, pch = 16, col = "red",  ylab = "CO", xlab = "",  cex.main = 0.95) # interpolated CO
    
    # add trimester and delivery points
    abline(v = exposurematrix[i,"preg_duration"])
    abline(v = 98, lty = "dotted") # 1st tri
    abline(v = 196, lty = "dotted") # 2nd tri
    
    # add points for measured CO
    codata <- filter(co2, mstudyid == exposurematrix$mstudyid[i])
    codata <- filter(co2, mstudyid == studyids[i])
    codata <- filter(codata, as.Date(lastdate) < datdeliv2) # including only prenatal measurements. But what about session 5, relevant to the end of pregnancy?
    codata$preg_day <- as.numeric(as.Date(codata$lastdate) - codata$preg_start)
    
    points(codata$preg_day, codata[,j])
    
  }
  dev.off()
  
  # add trimester averages
  exposurematrix$tri1_mean <- rowMeans(exposurematrix[,1:97])
  exposurematrix$tri2_mean <- rowMeans(exposurematrix[,98:195])
  exposurematrix$tri3_mean <- rowMeans(exposurematrix[,196:315], na.rm = TRUE)
  exposurematrix$preg_mean <- rowMeans(exposurematrix[,1:315], na.rm = TRUE)
  
  
  # save
  saveRDS(exposurematrix, file = paste0("time_weighted_co_exposure2_", j, format(Sys.Date(), format = "%b%d"), ".rds"))
}


#### TYPE 3 -------
# USING LINEAR INTERPOLATION BACK FROM SECOND POINT AND THRU POSTNATAL

for (j in c("co_mean_first48_corr")) {
  
  exposurematrix <- as.data.frame(matrix(nrow = length(unique(co2$mstudyid)), ncol = 317)) #315 = 45 weeks
  names(exposurematrix) <- c(paste0("day", 1:315), "mstudyid", "preg_duration")
  studyids <- unique(co2$mstudyid)
  
  
  for (i in 1:length(studyids)) {
    tryCatch({
      codata <- filter(co2, mstudyid == studyids[i])
  #    codata <- filter(codata, as.Date(lastdate) < datdeliv2) # including only prenatal measurements. But what about session 5, relevant to the end of pregnancy?
      codata$preg_day <- as.numeric(as.Date(codata$lastdate) - codata$preg_start)
      codata <- codata[order(codata$preg_day),]
      measurement_days <- sort(codata$preg_day)
      exposurematrix[i,316] <- studyids[i]
      exposurematrix[i, 317] <- as.numeric(codata$datdeliv2 - codata$preg_start)[1]
      print(studyids[i])
      print(length(measurement_days))
      exposurematrix[i, 1:measurement_days[1]] <- mean(codata[codata$preg_day == measurement_days[1], j]) #constant interp back from first measurement
     exposurematrix[i, min(315,max(measurement_days)):min(315,exposurematrix$preg_duration[i])] <- mean(codata[codata$preg_day == max(measurement_days), j]) #constant interp forward from last pre-delivery measurement to end of pregnancy (max of 315 days)
      
      if (length(measurement_days) >1) {
       
        # exposurematrix[i, (measurement_days[1]+1):measurement_days[2]] <- mean(codata[codata$preg_day == measurement_days[2],j]) # constant interp back from 2nd measurement
        
        middle <- data.frame(date = measurement_days[1]:max(measurement_days), co = NA)
        for (g in 1:length(measurement_days)) {
          middle$co[middle$date == measurement_days[g]] <- mean(codata[codata$preg_day == measurement_days[g],j], na.rm = TRUE)
        }
        middle$co_interp <- approx(middle$date, middle$co, n = nrow(middle))$y
        middle <- filter(middle, date <=315)
        
        exposurematrix[i, (measurement_days[1]):min(315,measurement_days[nrow(codata)])] <- middle$co_interp # linear interp between 2nd and last measurement
      }
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  
  
  
  # plot
  pdf(file = paste0("preg_co_interp3_",j, format(Sys.Date(), format = "%Y%b%d"), ".pdf"), height = 10, width = 10)
  par(mfrow = c(3,3))
  for (i in 1:nrow(exposurematrix)) { #1:nrow(exposurematrix)
    xax <- 1:(ncol(exposurematrix) - 2)
    yax <- exposurematrix[i, 1:(ncol(exposurematrix)-2)]
    
    # make empty plot
    plot(0, type="n", xlab="days", ylab=j ,main = exposurematrix$mstudyid[i], xlim=c(1, 315), ylim=c(0, ifelse(sum(!is.na(exposurematrix[i, 1:315])) >0, max(3,ceiling(max(exposurematrix[i, 1:315],na.rm = TRUE))), 3))) 
    # add lines for interpolated CO
    lines(xax,yax, pch = 16, col = "red",  ylab = "CO", xlab = "",  cex.main = 0.95) # interpolated CO
    
    # add trimester and delivery points
    abline(v = exposurematrix[i,"preg_duration"])
    abline(v = 98, lty = "dotted") # 1st tri
    abline(v = 196, lty = "dotted") # 2nd tri
    
    # add points for measured CO
    codata <- filter(co2, mstudyid == exposurematrix$mstudyid[i])
    codata <- filter(co2, mstudyid == studyids[i])
  #  codata <- filter(codata, as.Date(lastdate) < datdeliv2) # including only prenatal measurements. But what about session 5, relevant to the end of pregnancy?
    codata$preg_day <- as.numeric(as.Date(codata$lastdate) - codata$preg_start)
    
    points(codata$preg_day, codata[,j])
    
  }
  dev.off()
  
  # add trimester averages
  exposurematrix$tri1_mean <- rowMeans(exposurematrix[,1:97])
  exposurematrix$tri2_mean <- rowMeans(exposurematrix[,98:195])
  exposurematrix$tri3_mean <- rowMeans(exposurematrix[,196:315], na.rm = TRUE)
  exposurematrix$preg_mean <- rowMeans(exposurematrix[,1:315], na.rm = TRUE)
  
  
  # save
  saveRDS(exposurematrix, file = paste0("time_weighted_co_exposure3_", j, format(Sys.Date(), format = "%b%d"), ".rds"))
}


# merging trimester exposure from different datasets ----
expall_1 <- readRDS("/Users/quinnak/time_weighted_co_exposure1_co_mean_corrAug25.rds")
exp24_1 <- readRDS("/Users/quinnak/time_weighted_co_exposure1_co_day1_mean_corrAug25.rds")
exp48_1 <- readRDS("/Users/quinnak/time_weighted_co_exposure1_co_mean_first48_corrAug25.rds")

expall_1$exposure_days <- rowSums(!is.na(expall_1[,1:315]))
names(expall_1)[318:321] <- paste0(names(expall_1)[318:321], rep("_co_mean_corr", 3))
names(exp24_1)[318:321] <- paste0(names(exp24_1)[318:321], rep("_co_day1_mean_corr", 3))
names(exp48_1)[318:321] <- paste0(names(exp48_1)[318:321], rep("_co_mean_first48_corr", 3))

master_exp1 <- merge(expall_1[,c(316:317,322, 318:321)], exp24_1[, c(316, 318:321)], by = "mstudyid")
master_exp1 <- merge(master_exp1, exp48_1[, c(316, 318:321)], by = "mstudyid")

saveRDS(master_exp1, file = paste0("time_weighted_co_summary1_", format(Sys.Date(), format = "%b%d"), ".rds"))


expall_2 <- readRDS("/Users/quinnak/time_weighted_co_exposure2_co_mean_corrAug25.rds")
exp24_2 <- readRDS("/Users/quinnak/time_weighted_co_exposure2_co_day1_mean_corrAug25.rds")
exp48_2 <- readRDS("/Users/quinnak/time_weighted_co_exposure2_co_mean_first48_corrAug25.rds")

expall_2$exposure_days <- rowSums(!is.na(expall_2[,1:315]))
names(expall_2)[318:321] <- paste0(names(expall_2)[318:321], rep("_co_mean_corr", 3))
names(exp24_2)[318:321] <- paste0(names(exp24_2)[318:321], rep("_co_day1_mean_corr", 3))
names(exp48_2)[318:321] <- paste0(names(exp48_2)[318:321], rep("_co_mean_first48_corr", 3))

master_exp2 <- merge(expall_2[,c(316:317,322, 318:321)], exp24_2[, c(316, 318:321)], by = "mstudyid")
master_exp2 <- merge(master_exp2, exp48_2[, c(316, 318:321)], by = "mstudyid")

saveRDS(master_exp2, file = paste0("time_weighted_co_summary2_", format(Sys.Date(), format = "%b%d"), ".rds"))



# check correlations
library(PerformanceAnalytics)
chart.Correlation(log(master_exp1[,c("preg_mean_co_mean_corr", "preg_mean_co_day1_mean_corr", "preg_mean_co_mean_first48_corr")]), method = "spearman", histogram = TRUE)
chart.Correlation(log(master_exp1[,c("tri1_mean_co_mean_corr", "tri1_mean_co_day1_mean_corr", "tri1_mean_co_mean_first48_corr")]), method = "spearman", histogram = TRUE)
chart.Correlation(log(master_exp1[,c("tri2_mean_co_mean_corr", "tri2_mean_co_day1_mean_corr", "tri2_mean_co_mean_first48_corr")]), method = "spearman", histogram = TRUE)
chart.Correlation(log(master_exp1[,c("tri3_mean_co_mean_corr", "tri3_mean_co_day1_mean_corr", "tri3_mean_co_mean_first48_corr")]), method = "spearman", histogram = TRUE)

chart.Correlation(log(master_exp2[,c("preg_mean_co_mean_corr", "preg_mean_co_day1_mean_corr", "preg_mean_co_mean_first48_corr")]), method = "spearman", histogram = TRUE)
chart.Correlation(log(master_exp2[,c("tri1_mean_co_mean_corr", "tri1_mean_co_day1_mean_corr", "tri1_mean_co_mean_first48_corr")]), method = "spearman", histogram = TRUE)
chart.Correlation(log(master_exp2[,c("tri2_mean_co_mean_corr", "tri2_mean_co_day1_mean_corr", "tri2_mean_co_mean_first48_corr")]), method = "spearman", histogram = TRUE)
chart.Correlation(log(master_exp2[,c("tri3_mean_co_mean_corr", "tri3_mean_co_day1_mean_corr", "tri3_mean_co_mean_first48_corr")]), method = "spearman", histogram = TRUE)


preg_mean <- cbind(master_exp1[,c("preg_mean_co_mean_corr", "preg_mean_co_day1_mean_corr", "preg_mean_co_mean_first48_corr")], master_exp2[,c("preg_mean_co_mean_corr", "preg_mean_co_day1_mean_corr", "preg_mean_co_mean_first48_corr")])
names(preg_mean)[1:3] <- paste0("type1_pregmean_",c("overall", "first24", "first48"))
names(preg_mean)[4:6] <- paste0("type2_pregmean_",c("overall", "first24", "first48"))
chart.Correlation(log(preg_mean), method = "spearman")

tri1 <- cbind(master_exp1[,c("tri1_mean_co_mean_corr", "tri1_mean_co_day1_mean_corr", "tri1_mean_co_mean_first48_corr")], master_exp2[,c("tri1_mean_co_mean_corr", "tri1_mean_co_day1_mean_corr", "tri1_mean_co_mean_first48_corr")])
names(tri1)[1:3] <- paste0("type1_tri1_",c("overall", "first24", "first48"))
names(tri1)[4:6] <- paste0("type2_tri1_",c("overall", "first24", "first48"))
chart.Correlation(log(tri1), method = "spearman")

tri2 <- cbind(master_exp1[,c("tri2_mean_co_mean_corr", "tri2_mean_co_day1_mean_corr", "tri2_mean_co_mean_first48_corr")], master_exp2[,c("tri2_mean_co_mean_corr", "tri2_mean_co_day1_mean_corr", "tri2_mean_co_mean_first48_corr")])
names(tri2)[1:3] <- paste0("type1_tri2_",c("overall", "first24", "first48"))
names(tri2)[4:6] <- paste0("type2_tri2_",c("overall", "first24", "first48"))
chart.Correlation(log(tri2), method = "spearman")

tri3 <- cbind(master_exp1[,c("tri3_mean_co_mean_corr", "tri3_mean_co_day1_mean_corr", "tri3_mean_co_mean_first48_corr")], master_exp2[,c("tri3_mean_co_mean_corr", "tri3_mean_co_day1_mean_corr", "tri3_mean_co_mean_first48_corr")])
names(tri3)[1:3] <- paste0("type1_tri3_",c("overall", "first24", "first48"))
names(tri3)[4:6] <- paste0("type2_tri3_",c("overall", "first24", "first48"))
chart.Correlation(log(tri3), method = "spearman")

# Correlations between Type 1 and Type 2 are 0.99 for overall and tri3; 1.00 for tri1; 0.94-0.96 for tri2 (tri2 is where it matters most)

boxplot(log(tri2), las = 2, cex.axis = 0.5, ylab = "log(co)", main = "Tri2")
grid()

# type 2 (linear interpolation) leads to slightly higher CO values in tri2
mean(tri2$type2_tri2_overall - tri2$type1_tri2_overall, na.rm = TRUE) # 0.067
mean(tri2$type2_tri2_first24 - tri2$type1_tri2_first24, na.rm = TRUE) # 0.088
mean(tri2$type2_tri2_first48 - tri2$type1_tri2_first48, na.rm = TRUE) # 0.069


# type 3 vs type 2
exp48_3 <- readRDS("/Users/quinnak/time_weighted_co_exposure3_co_mean_first48_corrAug29.rds")
exp48_3$exposure_days <- rowSums(!is.na(exp48_3[,1:315]))
names(exp48_3)[318:321] <- paste0(names(exp48_3)[318:321], rep("_co_mean_first48_corr", 3))

identical(exp48_2$mstudyid, exp48_3$mstudyid)

all48 <- cbind(exp48_2[, 316:321], exp48_3[, 318:322])
names(all48)[3:10] <- c("tri1_48_type2", "tri2_48_type2", "tri3_48_type2", "preg_mean_48_type2","tri1_48_type3", "tri2_48_type3", "tri3_48_type3", "preg_mean_48_type3" )

saveRDS(all48, paste0("time_weighted_co_summary3_", format(Sys.Date(), format = "%b%d"), ".rds"))
chart.Correlation(log(all48[, 3:10]), method = "spearman")

# Decision: Type 2 interpolation; 48 hrs.

## Adding weekly averages ------

  exposurematrix48 <- readRDS("/Users/quinnak/BW EDD/time_weighted_co_exposure2_co_mean_first48_corrAug25.rds")

weeklyavg <- as.data.frame(matrix(nrow = nrow(exposurematrix48),ncol = 44))
names(weeklyavg) <- paste0("week", 1:44)

for(i in 1:44) {
    weeklyavg[,i] <- round(rowSums(exposurematrix48[, ((i*7)+1):((i*7)+7)])/7, digits = 3)
}

exposurematrix48 <- cbind(exposurematrix48, weeklyavg)

time_weighted_co_summary_prenatal <- exposurematrix48[,316:ncol(exposurematrix48)]
saveRDS(time_weighted_co_summary_prenatal, file = paste0("time_weighted_co_summary_prenatal_", format(Sys.Date(), format = "%b%d"), ".rds"))


# Looking into outliers-----
testids<- filter(co3, co_mean_first48_corr > boxplot.stats(co2$co_mean_first48_corr, coef = 3.5)$stats[5])

testco <- co[co$mstudyid %in% testids$mstudyid,]
testco <- filter(testco, overall_valid == 3) # these are the ones whose validity is 3 because the cf confidence is low. Is it an issue of the correction?
testco <- arrange(testco, mstudyid)
testco <- filter(testco, is.na(cstudyid))
testco[, c("mstudyid", "session", "co_mean_first48", "co_mean_first48_corr", "overall_valid")]
# the corrected values don't look outrageous

# probably just subset the visual validity = 1

# filter only those with validity "1"
co3 <- filter(co2, visually_valid == 1)
saveRDS(co3, file = paste0("prenatal_co_valid_only1_", format(Sys.Date(), format = "%b%d"), ".rds"))

# Interpolation using only those with validity 1: Type 2, 48 hour -----

for (j in c("co_mean_first48_corr")) {
  
  exposurematrix <- as.data.frame(matrix(nrow = length(unique(co3$mstudyid)), ncol = 317)) #315 = 45 weeks
  names(exposurematrix) <- c(paste0("day", 1:315), "mstudyid", "preg_duration")
  studyids <- unique(co3$mstudyid)
  
  
  for (i in 1:length(studyids)) {
    tryCatch({
      codata <- filter(co3, mstudyid == studyids[i])
      codata <- filter(codata, as.Date(lastdate) < datdeliv2) # including only prenatal measurements. But what about session 5, relevant to the end of pregnancy?
      codata$preg_day <- as.numeric(as.Date(codata$lastdate) - codata$preg_start)
      codata <- codata[order(codata$preg_day),]
      measurement_days <- sort(codata$preg_day)
      exposurematrix[i,316] <- studyids[i]
      exposurematrix[i, 317] <- as.numeric(codata$datdeliv2 - codata$preg_start)[1]
      print(studyids[i])
      print(length(measurement_days))
      exposurematrix[i, 1:measurement_days[1]] <- mean(codata[codata$preg_day == measurement_days[1], j]) #constant interp back from first measurement
      exposurematrix[i, min(315,max(measurement_days)):min(315,exposurematrix$preg_duration[i])] <- mean(codata[codata$preg_day == max(measurement_days), j]) #constant interp forward from last pre-delivery measurement to end of pregnancy (max of 315 days)
      
      if (length(measurement_days) >1) {
        
        middle <- data.frame(date = measurement_days[1]:max(measurement_days), co = NA)
        for (g in 1:length(measurement_days)) {
          middle$co[middle$date == measurement_days[g]] <- mean(codata[codata$preg_day == measurement_days[g],j], na.rm = TRUE)
        }
        middle$co_interp <- approx(middle$date, middle$co, n = nrow(middle))$y
        middle <- filter(middle, date <=315)
        
        exposurematrix[i, (measurement_days[1]):min(315,measurement_days[nrow(codata)])] <- middle$co_interp # linear interp between 2nd and last measurement
      }
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  
  
  
  # plot
  pdf(file = paste0("preg_co_interp2_validity1_",j, format(Sys.Date(), format = "%Y%b%d"), ".pdf"), height = 10, width = 10)
  par(mfrow = c(3,3))
  for (i in 1:nrow(exposurematrix)) { #1:nrow(exposurematrix)
    xax <- 1:(ncol(exposurematrix) - 2)
    yax <- exposurematrix[i, 1:(ncol(exposurematrix)-2)]
    
    # make empty plot
    plot(0, type="n", xlab="days", ylab=j ,main = exposurematrix$mstudyid[i], xlim=c(1, 315), ylim=c(0, ifelse(sum(!is.na(exposurematrix[i, 1:315])) >0, max(3,ceiling(max(exposurematrix[i, 1:315],na.rm = TRUE))), 3))) 
    # add lines for interpolated CO
    lines(xax,yax, pch = 16, col = "red",  ylab = "CO", xlab = "",  cex.main = 0.95) # interpolated CO
    
    # add trimester and delivery points
    abline(v = exposurematrix[i,"preg_duration"])
    abline(v = 98, lty = "dotted") # 1st tri
    abline(v = 196, lty = "dotted") # 2nd tri
    
    # add points for measured CO
    codata <- filter(co3, mstudyid == exposurematrix$mstudyid[i])
    codata <- filter(co3, mstudyid == studyids[i])
    codata <- filter(codata, as.Date(lastdate) < datdeliv2) 
    codata$preg_day <- as.numeric(as.Date(codata$lastdate) - codata$preg_start)
    
    points(codata$preg_day, codata[,j])
    
  }
  dev.off()
  
  # add trimester averages
  exposurematrix$tri1_mean <- rowMeans(exposurematrix[,1:97])
  exposurematrix$tri2_mean <- rowMeans(exposurematrix[,98:195])
  exposurematrix$tri3_mean <- rowMeans(exposurematrix[,196:315], na.rm = TRUE)
  exposurematrix$preg_mean <- rowMeans(exposurematrix[,1:315], na.rm = TRUE)
  
  
  # save
  saveRDS(exposurematrix, file = paste0("time_weighted_co_exposure2_validity1_", j, format(Sys.Date(), format = "%b%d"), ".rds"))
}

# adding weekly averages -----
weeklyavg <- as.data.frame(matrix(nrow = nrow(exposurematrix),ncol = 44))
names(weeklyavg) <- paste0("week", 1:44)

for(i in 1:44) {
  weeklyavg[,i] <- round(rowSums(exposurematrix[, ((i*7)+1):((i*7)+7)])/7, digits = 3)
}

exposurematrix <- cbind(exposurematrix, weeklyavg)

time_weighted_co_summary_prenatal_validity1 <- exposurematrix[,316:ncol(exposurematrix)]
saveRDS(time_weighted_co_summary_prenatal_validity1, file = paste0("time_weighted_co_summary_prenatal_validity1_", format(Sys.Date(), format = "%b%d"), ".rds"))


summary1_2 <- time_weighted_co_summary_prenatal
summary1 <- time_weighted_co_summary_prenatal_validity1

overall <- merge(summary1_2, summary1, by = "mstudyid")
boxplot(overall$preg_mean.x - overall$preg_mean.y)
boxplot(overall$tri3_mean.x - overall$tri3_mean.y)
boxplot(overall[, c("preg_mean.x", "preg_mean.y")])
boxplot(overall[, c("tri1_mean.x", "tri1_mean.y")])
boxplot(overall[, c("tri2_mean.x", "tri2_mean.y")])
