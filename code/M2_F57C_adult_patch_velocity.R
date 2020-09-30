## typha adult patch velocity

library(tidyverse)
library(tidyr)
library(sm)
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)
library(data.table)


load("input_data/typha_velocity.RData")

png("Figures/Final_curves/Typha_velocity_depth.png", width = 700, height = 600)

ggplot(data = velocity, mapping = aes(x = vel_m_s, y = occurrence))+
  geom_point(alpha = 0.2, size = 3)+
  labs(x = "Velocity (m/s)", y = "Probability")+
  geom_smooth(method = "glm", method.args = list(family = "binomial"))+
  theme(text = element_text(size=25), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))

dev.off()

summary(vel_ptch_mdl <- glm(occurrence ~ vel_m_s, data = velocity, family = "binomial"))
confint(vel_ptch_mdl)
# save(vel_ptch_mdl, file = "models/vel_ptch_mdl.rda")


## upload hydraulic data

## soft bottom reaches

F57C <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/input_data/HecRas/hydraulic_ts_F57C.csv")
# LA8 <- read.csv("input_data/HecRas/hydraulic_ts_LA8.csv")
# LA11 <- read.csv("input_data/HecRas/hydraulic_ts_LA11.csv")
# LA20 <- read.csv("input_data/HecRas/hydraulic_ts_LA20_2.csv")


## go through script one at a time

hydraul <- F57C[,-1]

## select columns

hyd_vel <- hydraul[,c(1:3,4,8, 12)]
colnames(hyd_vel) <-c("DateTime", "node", "Q", "vel_ft_LOB", "vel_ft_MC", "vel_ft_ROB")

# nas <- which(complete.cases(hyd_dep) == FALSE)
## select column

hyd_vel <- hyd_vel %>%
  mutate(vel_m_LOB = (vel_ft_LOB*0.3048),
         vel_m_MC = (vel_ft_MC*0.3048),
         vel_m_ROB = (vel_ft_ROB*0.3048)) %>%
  select(-contains("ft")) %>%
  mutate(date_num = seq(1,length(DateTime), 1))

# ## melt channel position data
hyd_vel<-reshape2::melt(hyd_vel, id=c("DateTime","Q", "node", "date_num"))
hyd_vel <- hyd_vel %>% rename(vel_m_s = value)
head(hyd_vel)

## workflow
## get probabilities for depth at each hourly time step
## get thresholds i.e. 25, 50, 75%

summary(vel_ptch_mdl)

new_data <- hyd_vel %>%
  mutate(prob_fit = predict(vel_ptch_mdl, newdata = hyd_vel, type="response")) #%>%
  

head(new_data)
range(new_data$prob_fit)

save(new_data, file="output_data/M2_F57C_typha_adult_patch_velocity_discharge_probability_time_series_red_columns.RData")

# format probability time series ------------------------------------------

## look at data using lubridate etc

names(new_data)
## format date time
## format date time
new_data$DateTime<-as.POSIXct(new_data$DateTime,
                              format = "%Y-%m-%d %H:%M",
                              tz = "GMT")

## create year, month, day and hour columns

new_data <- new_data %>%
  mutate(month = month(DateTime))%>%
  mutate(year = year(DateTime))%>%
  mutate(day = day(DateTime))%>%
  mutate(hour = hour(DateTime))


head(new_data)


save(new_data, file="output_data/M2_F57C_typha_adult_patch_velocity_discharge_probs_2010_2017_TS.RData")

# probability as a function of discharge -----------------------------------


load( file="output_data/M2_F57C_typha_adult_patch_velocity_discharge_probs_2010_2017_TS.RData")
head(new_data)

## plot
range(new_data$Q) ## 0.00 998.845 
range(new_data$prob_fit) ## 1.689185e-06 8.413046e-01

peak <- new_data %>%
  group_by(variable) %>%
  filter(prob_fit == max(prob_fit)) #%>%


peakQM <- filter(peak, variable=="vel_m_MC")
peakQM  <- max(peakQM$Q)
peakQM ## 26.22926

peakQL <- filter(peak, variable=="vel_m_LOB")
peakQL  <- max(peakQL$Q) ## 
peakQL ## 26.22926

peakQR <- filter(peak, variable=="vel_m_ROB")
peakQR  <- max(peakQR$Q) ## 
peakQR ## 26.22926

## filter data by cross section position

new_dataM <- filter(new_data, variable == "vel_m_MC")
new_dataL <- filter(new_data, variable == "vel_m_LOB")
new_dataR <- filter(new_data, variable == "vel_m_ROB")

## Main channel curves

MC_curve <- spline(new_dataM$Q, new_dataM$prob_fit,
                   xmin = min(new_dataM$Q), xmax = max(new_dataM$Q), ties = mean)

MC_curve_lower <- spline(new_dataM$Q, new_dataM$prob_fit,
                         xmin = min(new_dataM$Q), xmax = peakQM, ties = mean)
MC_curve_upper <- spline(new_dataM$Q, new_dataM$prob_fit,
                         xmin = peakQM, xmax = max(new_dataM$Q), ties = mean)

## main channel values
newx1a <- approx(x = MC_curve_lower$y, y = MC_curve_lower$x, xout = 0.25)$y
newx1a <- NA
# newx1a <- min(MC_curve_lower$x)

newx1b <- approx(x = MC_curve_upper$y, y = MC_curve_upper$x, xout = 0.25)$y
newx1b 
# newx1b <- max(MC_curve_upper$x)

newx2a <- approx(x = MC_curve_lower$y, y = MC_curve_lower$x, xout = 0.50)$y
newx2a <- NA

newx2b <- approx(x = MC_curve_upper$y, y = MC_curve_upper$x, xout = 0.50)$y
newx2b

newx3a <- approx(x = MC_curve_lower$y, y = MC_curve_lower$x, xout = 0.75)$y
newx3a <- NA

newx3b <- approx(x = MC_curve_upper$y, y = MC_curve_upper$x, xout = 0.75)$y
newx3b

## LOB curves

LOB_curve <- spline(new_dataL$Q, new_dataL$prob_fit,
                    xmin = min(new_dataL$Q), xmax = max(new_dataL$Q), ties = mean)

LOB_curve_lower <- spline(new_dataL$Q, new_dataL$prob_fit,
                          xmin = min(new_dataL$Q), xmax = peakQL, ties = mean)
LOB_curve_upper <- spline(new_dataL$Q, new_dataL$prob_fit,
                          xmin = peakQL, xmax = max(new_dataL$Q), ties = mean)


newx1aL <- approx(x = LOB_curve_lower$y, y = LOB_curve_lower$x, xout = 0.25)$y
newx1aL <- NA

newx1bL <- approx(x = LOB_curve_upper$y, y = LOB_curve_upper$x, xout = 0.25)$y
newx1bL
# newx1bL <- max(LOB_curve_upper$x)

newx2aL <- approx(x = LOB_curve_lower$y, y = LOB_curve_lower$x, xout = 0.50)$y
newx2aL <- NA

newx2bL <- approx(x = LOB_curve_upper$y, y = LOB_curve_upper$x, xout = 0.50)$y
newx2bL

newx3aL <- approx(x = LOB_curve_lower$y, y = LOB_curve_lower$x, xout = 0.75)$y
newx3aL <- NA

newx3bL <- approx(x = LOB_curve_upper$y, y = LOB_curve_upper$x, xout = 0.75)$y
newx3bL

## ROB curves

ROB_curve <- spline(new_dataR$Q, new_dataR$prob_fit,
                    xmin = min(new_dataR$Q), xmax = max(new_dataR$Q), ties = mean)

ROB_curve_lower <- spline(new_dataR$Q, new_dataR$prob_fit,
                          xmin = min(new_dataR$Q), xmax = peakQR, ties = mean)
ROB_curve_upper <- spline(new_dataR$Q, new_dataR$prob_fit,
                          xmin = peakQR, xmax = max(new_dataR$Q), ties = mean)

## main channel values
newx1aR <- approx(x = ROB_curve_lower$y, y = ROB_curve_lower$x, xout = 0.25)$y
newx1aR <- NA
# newx1aR <- min(ROB_curve_lower$x)

newx1bR <- approx(x = ROB_curve_upper$y, y = ROB_curve_upper$x, xout = 0.25)$y
newx1bR
# newx1bR <- max(ROB_curve_upper$x)

newx2aR <- approx(x = ROB_curve_lower$y, y = ROB_curve_lower$x, xout = 0.50)$y
newx2aR <- NA

newx2bR <- approx(x = ROB_curve_upper$y, y = ROB_curve_upper$x, xout = 0.50)$y
newx2bR 

newx3aR <- approx(x = ROB_curve_lower$y, y = ROB_curve_lower$x, xout = 0.75)$y
newx3aR <- NA

newx3bR <- approx(x = ROB_curve_upper$y, y = ROB_curve_upper$x, xout = 0.75)$y
newx3bR

## df for Q limits

limits <- as.data.frame(matrix(ncol=3, nrow=6)) %>%
  rename(LOB = V1, MC = V2, ROB = V3) 
rownames(limits)<-c("Low_Prob_Lower", "Low_Prob_Upper", "Med_Prob_Lower",
                    "Med_Prob_Upper", "High_Prob_Lower", "High_Prob_Upper")

limits$LOB <- c(newx1aL, newx1bL, newx2aL, newx2bL, newx3aL, newx3bL)
limits$MC <- c(newx1a, newx1b, newx2a, newx2b, newx3a, newx3b)
limits$ROB <- c(newx1aR, newx1bR, newx2aR, newx2bR, newx3aR, newx3bR)

limits

write.csv(limits, "output_data/M2_F57C_adult_patch_velocity_Q_limits.csv")

## plot with thresholds

png("Figures/Application_curves/Depth/F57C_adult_patch_velocity_prob_Q_thresholds.png", width = 500, height = 600)

labels <- c(vel_m_LOB = "Left Over Bank", vel_m_MC = "Main Channel", vel_m_ROB = "Right Over Bank")

ggplot(new_data, aes(x = Q, y=prob_fit)) +
  geom_line(aes(group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"))+
  #                       name="Cross\nSection\nPosition",
  #                       breaks=c("vel_m_LOB", "vel_m_MC", "vel_m_ROB"),
  #                         labels = c("LOB", "MC", "ROB")) +
  
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  # geom_point(data = subset(new_data, variable =="vel_m_MC"), aes(y=0.25, x=newx1a), color="green") +
  geom_point(data = subset(new_data, variable =="vel_m_MC"), aes(y=0.25, x=newx1b), color="green") +
  # geom_point(data = subset(new_data, variable =="vel_m_MC"), aes(y=0.50, x=newx2a), color="red") +
  geom_point(data = subset(new_data, variable =="vel_m_MC"), aes(y=0.50, x=newx2b), color="red") +
  # geom_point(data = subset(new_data, variable =="vel_m_MC"), aes(y=0.75, x=newx3a), color="blue") +
  # geom_point(data = subset(new_data, variable =="vel_m_MC"), aes(y=0.75, x=newx3b), color="blue") +
  # 
  # # geom_point(data = subset(new_data, variable =="vel_m_LOB"), aes(y=0.25, x=newx1aL), color="green") +
  geom_point(data = subset(new_data, variable =="vel_m_LOB"), aes(y=0.25, x=newx1bL), color="green") +
  # # geom_point(data = subset(new_data, variable =="vel_m_LOB"), aes(y=0.50, x=newx2aL), color="red") +
  # geom_point(data = subset(new_data, variable =="vel_m_LOB"), aes(y=0.50, x=newx2bL), color="red") +
  # geom_point(data = subset(new_data, variable =="vel_m_LOB"), aes(y=0.75, x=newx3aL), color="blue") +
  # geom_point(data = subset(new_data, variable =="vel_m_LOB"), aes(y=0.75, x=newx3bL), color="blue") +
  # 
  # # geom_point(data = subset(new_data, variable =="vel_m_ROB"), aes(y=0.25, x=newx1aR), color="green") +
  geom_point(data = subset(new_data, variable =="vel_m_ROB"), aes(y=0.25, x=newx1bR), color="green") +
  # geom_point(data = subset(new_data, variable =="vel_m_ROB"), aes(y=0.50, x=newx2aR), color="red") +
  geom_point(data = subset(new_data, variable =="vel_m_ROB"), aes(y=0.50, x=newx2bR), color="red") +
  # geom_point(data = subset(new_data, variable =="vel_m_ROB"), aes(y=0.75, x=newx3aR), color="blue") +
  geom_point(data = subset(new_data, variable =="vel_m_ROB"), aes(y=0.75, x=newx3bR), color="blue") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "F57C: Adult Patch/Velocity: Probability ~ Q",
       y = "Probability",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()

# discharge time series plots with probability lines ----------------------

## make dataframe for all years 

# create year_month column       
new_dataMx <- new_dataM %>% unite(month_year, year:month, sep="-", remove=F) 
head(new_dataMx)

# create year_month column       
new_dataLx <- new_dataL %>% unite(month_year, year:month, sep="-", remove=F) 
head(new_dataLx)

# create year_month column       
new_dataRx <- new_dataR %>% unite(month_year, year:month, sep="-", remove=F) 
head(new_dataRx)

head(new_dataMx)


## make dataframe for all years 

## define seasons/critical period - check and change these!!
non_critical <- c(1:3, 10:12) 
critical <- c(4:9) 

new_dataMx <- new_dataMx %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )

new_dataLx <- new_dataLx %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )

new_dataRx <- new_dataRx %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )


limits
## produces percentage of time for each year and season within year for each threshold

time_statsm <- new_dataMx %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Low = sum(Q <= newx1b)/length(DateTime)*100) %>%
  dplyr::mutate(Medium = sum(Q <= newx2b)/length(DateTime)*100) %>%
  dplyr::mutate(High = sum(Q <= newx3b)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(Low.Seasonal = sum(Q <= newx1b)/length(DateTime)*100) %>%
  dplyr::mutate(Medium.Seasonal = sum(Q <= newx2b)/length(DateTime)*100) %>%
  dplyr::mutate(High.Seasonal = sum(Q <= newx3b)/length(DateTime)*100) %>%
  distinct(year, Low , Medium , High , Low.Seasonal, Medium.Seasonal, High.Seasonal) %>%
  mutate(position="MC")

time_statsm 

time_statsl <- new_dataLx %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Low = sum(Q <= newx1bL)/length(DateTime)*100) %>%
  dplyr::mutate(Medium = sum(Q <= newx2bL)/length(DateTime)*100) %>%
  dplyr::mutate(High = sum(Q <= newx3bL)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(Low.Seasonal = sum(Q <= newx1bL)/length(DateTime)*100) %>%
  dplyr::mutate(Medium.Seasonal = sum(Q <= newx2bL)/length(DateTime)*100) %>%
  dplyr::mutate(High.Seasonal = sum(Q <= newx3bL)/length(DateTime)*100) %>%
  distinct(year, Low , Medium , High , Low.Seasonal, Medium.Seasonal, High.Seasonal) %>%
  mutate(position="LOB")

sum(new_dataRx$Q >= newx1aR & new_dataRx$Q <= newx1bR)/length(new_dataRx$DateTime)*100

time_statsr <- new_dataRx %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Low = sum(Q <= newx1bR)/length(DateTime)*100) %>%
  dplyr::mutate(Medium = sum(Q <= newx2bR)/length(DateTime)*100) %>%
  dplyr::mutate(High = sum(Q <= newx3bR)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(Low.Seasonal = sum(Q <= newx1bR)/length(DateTime)*100) %>%
  dplyr::mutate(Medium.Seasonal = sum(Q <= newx2bR)/length(DateTime)*100) %>%
  dplyr::mutate(High.Seasonal = sum(Q <= newx3bR)/length(DateTime)*100) %>%
  distinct(year, Low , Medium , High , Low.Seasonal, Medium.Seasonal, High.Seasonal) %>%
  mutate(position="ROB")

time_statsr


time_stats <- rbind(time_statsm, time_statsl, time_statsr)

## melt
melt_time<-reshape2::melt(time_stats, id=c("year","season", "position"))
melt_time <- rename(melt_time, Probability_Threshold = variable)
head(melt_time)
unique(melt_time$Probability_Threshold)
write.csv(melt_time, "output_data/M2_F57C_typha_adult_patch_velocity_time_stats.csv")

## subset annual stats
ann_stats <- unique(melt_time$Probability_Threshold)[1:3]
melt_time_ann <- melt_time %>% filter(Probability_Threshold %in% ann_stats ) %>%
  select(-season) %>% distinct()

head(melt_time_ann)
unique(melt_time_ann$Probability_Threshold)

## subset seasonal stats
seas_stats <- unique(melt_time$Probability_Threshold)[4:6]
melt_time_seas <- filter(melt_time, Probability_Threshold %in% seas_stats )
head(melt_time_seas)
melt_time_seas


## plot for annual stats - need probs in order

png("Figures/Application_curves/Depth/F57C_typha_adult_patch_depth_perc_time_above_threshold_annual.png", width = 500, height = 600)

ggplot(melt_time_ann, aes(x = year, y=value)) +
  geom_line(aes( group =c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "F57C: Time within discharge limit in relation to Depth (Annual)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)

dev.off()

melt_time_noncrit <- filter(melt_time_seas, season == "non_critical")

png("Figures/Application_curves/Depth/F57C_typha_adult_patch_depth_perc_time_above_threshold_non_critcal.png", width = 500, height = 600)

ggplot(melt_time_noncrit, aes(x = year, y=value)) +
  geom_line(aes( group = c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low.Seasonal", "Medium.Seasonal", "High.Seasonal"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "F57C: Time within discharge limit in relation to Depth (Non_critical)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)

dev.off()
## plot for summer stats - need probs in order

melt_time_crit <- filter(melt_time_seas, season == "critical")

png("Figures/Application_curves/Depth/F57C_typha_adult_patch_depth_perc_time_above_threshold_critcal.png", width = 500, height = 600)

ggplot(melt_time_crit, aes(x = year, y=value)) +
  geom_line(aes( group = c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low.Seasonal", "Medium.Seasonal", "High.Seasonal"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "F57C: Time within discharge limit in relation to Depth (Critical)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)

dev.off()

# Number of days above discharge ------------------------------------------
# need number of days discharge is above the limits outlined above - counted per month
## packages

library(tidyverse)
library(tidyr)
library(sm)
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)
library(data.table)


load( file="output_data/M1_F57C_typha_adult_patch_velocity_discharge_probs_2010_2017_TS.RData")
head(new_data)

## define thresholds again
# range(new_data$Q) ## 0.00 998.845 

## smooth spline the curve to get exact value of discharge at a given probability
spl <- smooth.spline(new_data$prob_fit ~ new_data$Q)

## find peak of prob v Q

peak <- filter(new_data, prob_fit == max(prob_fit)) #%>%
peakQ <- select(peak, Q)
peakQ  <- peakQ[1,1]
peakQ ##  883.39

## function for each probability
newymid <- 0.5
newxmid <- try(uniroot(function(x) predict(spl, x, deriv = 0)$y - newymid,
                       interval = c(min(new_data$Q), peakQ))$root, silent=T)

newxmid <- 0
# all columns based on different probabilities
## count number events within each threshold with a running total - max total is the number of consequative 
# events (hours) per day. if else statements to consider the thresholds newx1a/b etc
## order by datetime

new_data <- arrange(new_data, date_num)

new_data <- new_data %>%
  group_by(month, day, year, ID01 = data.table::rleid(Q < newxmid)) %>%
  mutate(Low = if_else(Q < newxmid, row_number(), 0L)) %>%
  ungroup() %>%
  group_by(month, day, year, ID02 = data.table::rleid(Q >= newxmid)) %>%
  mutate(High = if_else(Q >= newxmid, row_number(), 0L))


head(new_data)
names(new_data)

## melt data frame so that each probability column are all in one row 
## select only columns needed - Q, month, year, day all IDs and probs
# names(new_data)

new_datax <- select(new_data, c(Q, month, year, day, ID01, Low, ID02,  High) )# all probs
names(new_datax)

## melt
melt_data<-reshape2::melt(new_datax, id=c("ID01", "ID02", "day", "month", "year", "Q"))
melt_data <- rename(melt_data, Probability.of.Occurrence = variable, 
                    consec_hours = value)


## groups data by year, month and ID & threshold
## counts the number of days in each month probability is within the depth of each threshold - days are not necessarily conseq
## each threshold separately

## count how many full days i.e. 24 hours
total_days01 <- melt_data %>% 
  filter(Probability.of.Occurrence == "Low") %>% 
  group_by(ID01, day, month, year) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_low = ifelse(n_hours >= 24, 1, 0)) # %>%
# total_days01
## count the number of days in each month
total_days_per_month01 <- total_days01 %>%
  group_by(month, year) %>%
  summarise(days_per_month_low = sum(n_days_low))


total_days02 <- melt_data %>% 
  filter(Probability.of.Occurrence== "High") %>% 
  group_by(ID02, day, month, year) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_high = ifelse(n_hours >= 24, 1, 0)) # %>%

total_days_per_month02 <- total_days02 %>%
  group_by(month, year) %>%
  summarise(days_per_month_high = sum(n_days_high))

total_days_per_month02

## combine all thresholds
total_days <- cbind( total_days_per_month01,total_days_per_month02[,3])

# create year_month column       
total_days <- ungroup(total_days) %>%
  unite(month_year, year:month, sep="-", remove=F) 

## convert month year to date format
total_days$month_year <-  zoo::as.yearmon(total_days$month_year)
total_days

## change names of columns
total_days <- rename(total_days, Low = days_per_month_low, High = days_per_month_high)

## define seasons
winter <- c(1,2,3,4,11,12) ## winter months
summer <- c(5:10) ## summer months

total_days <- total_days %>%
  mutate(season = ifelse(month %in% winter, "winter", "summer") )

# ## melt data

melt_days<-reshape2::melt(total_days, id=c("month_year", "year", "month", "season"))
melt_days <- rename(melt_days, Probability.of.Occurrence = variable,
                    n_days = value)

head(melt_days)

##  plot - number of days 

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability.of.Occurrence, color = Probability.of.Occurrence)) +
  scale_color_manual(breaks = c("Low",  "High"),
                     values=c( "red", "blue")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%b %Y")) +
  # facet_wrap(~year, scales="free_x", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Number of days within discharge limit in relation to Velocity",
       y = "Number of days per Month",
       x = "Month") #+ theme_bw(base_size = 15)

## number of days separated per year

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability.of.Occurrence, color = Probability.of.Occurrence)) +
  scale_color_manual(breaks = c("Low",  "High"),
                     values=c(  "red", "blue")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%b %Y")) +
  facet_wrap(~year, scales="free_x", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Number of days within discharge limit in relation to Velocity",
       y = "Number of days per Month",
       x = "Month") #+ theme_bw(base_size = 15)

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability.of.Occurrence, color = Probability.of.Occurrence)) +
  scale_color_manual(breaks = c("Low", "High"),
                     values=c(  "red", "blue")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%b %Y")) +
  facet_wrap(~season, scales="free_x", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Number of days within discharge limit in relation to Velocity",
       y = "Number of days per Month",
       x = "Month") #+ theme_bw(base_size = 15)

## not sure if correct, check and compare with depth. might just be the variation in days and hours in days


