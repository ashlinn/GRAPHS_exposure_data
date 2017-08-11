#### INTERPOLATE EXPOSURE DATA OVER PREGNANCY #############

# load packages
require(plyr)
require(dplyr)
require(ggplot2)
require(lubridate)
require(scales)
library(lattice)
library(reshape2)

# load data
co <- readRDS("/Users/ashlinn/Documents/Birthweight ER analysis/FINAL_CO_parameters_withvalidation_2016Jun14.rds")

temp_bw <- readRDS("") # NOTE this is temporary, not the updated file.


bw_new$birthdate <- ymd(bw_new$datdeliv2, tz = "UTC")

# need to separate out CO prior to birth date, by mstudyid
str(co$lastdate)
str(bw_new$datdeliv2)
bw_new$birthdate <- ymd(bw_new$datdeliv2, tz = "UTC")

co_new <- data.frame()
allids <- unique(bw_new$mstudyid)
for(i in 1:length(allids)) {
  co_byid <- co[co$mstudyid == allids[i],]
  birthdate <- bw_new$birthdate[bw_new$mstudyid == allids[i]]
  co_byid <- co_byid[co_byid$lastdate <= birthdate,]
  co_new <- rbind(co_new, co_byid)
}

length(unique(co_new$mstudyid)) # 1302, should be 1303
bw_new$mstudyid[!bw_new$mstudyid %in% unique(co_new$mstudyid)] # BM1607M, had no co sessions during pregnancy?
unique(co_new$mstudyid[!unique(co_new$mstudyid) %in% unique(bw_new$mstudyid)])

# remove BM1607M
bw_new <- bw_new[!bw_new$mstudyid == "BM1607M",]

range(table(co_new$mstudyid)) # 1 to 7 lascar sessions

# exclude invalid CO sessions
co_new<- filter(co_new, visually_valid == 1 | visually_valid == 2,  co_hours >=18) # might want to do sens w only 1 later

# from lascar calibration file ---- 
# NEW - if calib hasn't been done in 8 months, tail of data coral
# load file of monthly CFs
cfs <- readRDS("/Users/ashlinn/Dropbox/Ghana_exposure_data_SHARED (1)/CO_calibration_files/Calibration Factors/Datasets/calib_factors_bymonth_2016Apr29.rds")

# add new columns for the confidence - one for each month
cf_new <- cfs
cf_new[,46:88] <- NA # this range will need to be adjusted: double the number of months in the dataframe
conf_names <- paste0(names(cf_new[,3:45]), "_conf")
for (i in 46:88) { colnames(cf_new)[i] <- conf_names[i-45] }


# remove any all-NA rows
row.all.na <- apply(cf_new[,3:ncol(cf_new)], 1, function(x) {all(is.na(x))})
cf_new <- cf_new[!row.all.na,]

row.all.na2 <- apply(cfs[,3:ncol(cfs)], 1, function(x) {all(is.na(x))})
cfs <- cfs[!row.all.na2,]

# make and save plot of interpolated CFs ("Calib_factors_bymonth_interp_..."), add interpolated CFs to cf_new

pdf(file = paste0("Calib_factors_bymonth_interp_", format(Sys.Date(), format = "%Y%b%d"), ".pdf"), height = 10, width = 10)
par(mfrow = c(3,3))
for (i in 1:nrow(cfs)) {
  xax <- 1:(ncol(cfs) - 2)
  yax <- cfs[i, 3:ncol(cfs)]
  
  #### assigning initial values as the initial measured value (unless there are no measured values)
  yax[1] <- ifelse(!is.null(cfs[i,which(!is.na(cfs[i,3:ncol(cfs)]))[1]+2]), cfs[i,which(!is.na(cfs[i,3:ncol(cfs)]))[1]+2], yax[1]) 
  
  
  plot(xax,yax, pch = 16, col = "red",ylim = c(0,2), main = paste0(cfs$SN[i], " \n", cfs$lascar[i]), ylab = "Calibration Factor", xlab = "", xaxt = "n", cex.main = 0.95)
  
  #### interpolate
  values <- which(!is.na(yax))
  whichzero <- which(as.numeric(yax) < 0.2)[1] #  a "zero" CF is defined as < 0.2
  interp <- approx(xax, yax, n = length(xax), xout = xax, rule = 2) # linear interpolation (rule = 2 sets constant interpolation outside the measured range)
  interp2 <- approx(xax, yax, xout = xax, method = "constant", rule = 2) # constant interpolation
  if (!is.na(whichzero) & whichzero !=1) interp_complete <- append(interp$y[1: max(values[values< whichzero])], interp2$y[(max(values[values < whichzero])+1):length(interp2$y)]) # linearly interpolate until last measured value before a "zero", then use constant interpolation
  if (is.na(whichzero)) interp_complete <- interp$y # if no "zero", linearly interpolate across the whole range (constant interpolation after last measured point)
  if (whichzero == 1 & !is.na(whichzero)) interp_complete <- interp2$y
  
  # set colors according to CF confidence
  allpoints <- as.data.frame(interp_complete)
  allpoints$colors <- NA
  
  # generally set colors within the "good" range as green and those outside as coral
  allpoints$colors<- ifelse(allpoints$interp_complete > 1.2 | allpoints$interp_complete < 0.6, "coral", "lightgreen")
  # apply coral color and lo confidence to values after a measured value and before a zero
  if (!is.na(whichzero) & whichzero !=1) allpoints$colors[(max(values[values < whichzero])+1):whichzero] <- "coral"
  
  # apply coral color and lo confidence to entire set of data if there are no 2 adjacent valid measured CFs (including the virtual one as a measured value)
  measured <- as.numeric(yax[!is.na(yax)])
  v <- NULL
  for (j in 1:length(measured) - 1) {
    v <- append(v, measured[j] >= 0.6 & measured[j+1] >= 0.6 & measured[j] <=1.2 & measured[j+1] <=1.2)
  }
  if(sum(v) == 0) allpoints$colors <- "coral"
  
  # apply grey color and no confidence to any points with CF < 0.2
  allpoints$colors <- ifelse(allpoints$interp_complete < 0.2, "grey", allpoints$colors)
  
  # apply coral color and lo confidence to tail of data if it has been more than 8 months since lascar last calibrated and previous calibration was green
  # last caliberation
  last <- nrow(allpoints) - which(!is.na(yax[ncol(yax):1]))[1] + 1 # counting backwards 8 months from last column
  if ((nrow(allpoints) - last > 8) & (allpoints[last,2] == "lightgreen")) { 
    allpoints$colors[nrow(allpoints):(nrow(allpoints) - (nrow(allpoints) - last - 8))] <- "coral" }
  
  # plot points
  points(xax, allpoints$interp_complete, pch = 16, col = allpoints$colors)
  
  # map colors to confidence levels
  allpoints$conf <- mapvalues(allpoints$colors, from = c("lightgreen", "coral","grey"), to = c("hi", "lo", "none"), warn_missing = FALSE)
  
  ### add back in the actual measured monthly averages in black
  points(xax[2:length(xax)], yax[2:length(yax)], pch = 16, col = "black") # plots all non-NA values of yax
  
  ### make the first (virtual) point red and add lines at 0.6 and 1.2
  points(xax[1], yax[1], pch = 16, col = "red")
  abline(h=0.6, lty = "dashed", col = "blue")
  abline(h = 1.2, lty = "dashed", col = "blue")
  grid()
  ### add x axis and legend
  xlabels <-names(cfs)[3:ncol(cfs)]
  axis(side = 1, at = xax, labels = paste(substr(xlabels, 1, 3),substr(xlabels, (nchar(xlabels) - 4), nchar(xlabels))), las = 2, cex.axis = 0.5) ###
  
  legend("topleft", c("virtual", "measured", "hi", "lo", "none"), xpd = TRUE, horiz = FALSE, inset = c(0,0), bty = "n", pch = 16, col = c("red", "black", "lightgreen", "coral", "grey"), cex = 0.8, x.intersp = 0.3)
  
  ### add interpolated values to cf_new
  cf_new[i, 3:45] <- round(allpoints$interp_complete, digits = 3)
  cf_new[i, 46:88] <- allpoints$conf
}
dev.off() # end plot and interpolation


######### save the interpolated CF factors: "calib_factors_bymonth_interp_..." #########
saveRDS(cf_new, file = paste0("calib_factors_bymonth_interp_", format(Sys.Date(), format = "%Y%b%d"), ".rds"))
