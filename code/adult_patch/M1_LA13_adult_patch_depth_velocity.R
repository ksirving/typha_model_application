## typha adult patch depth

library(tidyverse)
library(tidyr)
library(sm)
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)
library(data.table)


load("input_data/typha_depth.RData")

### model

# head(depth)
# mean(na.omit(depth$depth_cm))

summary(dep_ptch_mdl <- lm(occurrence ~ depth_cm + I(depth_cm^2), data = depth))
# save(dep_ptch_mdl, file = "dep_ptch_mdl.rda")


# Hydraulic data ----------------------------------------------------------

## upload hydraulic data

# F34D <- read.csv("input_data/HecRas/hydraulic_ts_F34D.csv")
# F37B_High <- read.csv("input_data/HecRas/hydraulic_ts_F37B_High.csv")
# F45B <- read.csv("input_data/HecRas/hydraulic_ts_F45B.csv")
# F319 <- read.csv("input_data/HecRas/hydraulic_ts_F319.csv")
LA13 <- read.csv("input_data/HecRas/hydraulic_ts_LA13.csv")
# LA1 <- read.csv("input_data/HecRas/hydraulic_ts_LA1.csv")

## go through script one at a time

hydraul <- LA13[,-1]
names(hydraul)
head(hydraul)
## select columns

## change some names
hydraul <- hydraul %>%
  rename(DateTime = Q_ts.datetime, node = Gage, Q = Flow)

## change names and transform ft to cm
hyd_dep <- hydraul %>%
  select(c(DateTime, Q, node, Avg..Vel...ft.s..LOB, Hydr..Depth..ft..LOB,Avg..Vel...ft.s..MC, Hydr..Depth..ft..MC, 
           Avg..Vel...ft.s..ROB, Hydr..Depth..ft..ROB)) %>%
  rename(vel_ft_LOB = Avg..Vel...ft.s..LOB, depth_ft_LOB = Hydr..Depth..ft..LOB, vel_ft_MC = Avg..Vel...ft.s..MC,
         depth_ft_MC = Hydr..Depth..ft..MC, vel_ft_ROB = Avg..Vel...ft.s..ROB, depth_ft_ROB = Hydr..Depth..ft..ROB) %>%
  mutate(depth_cm_LOB = (depth_ft_LOB*0.3048)*100,
         depth_cm_MC = (depth_ft_MC*0.3048)*100,
         depth_cm_ROB = (depth_ft_ROB*0.3048)*100) %>%
  mutate(vel_m_LOB = (vel_ft_LOB*0.3048),
         vel_m_MC = (vel_ft_MC*0.3048),
         vel_m_ROB = (vel_ft_ROB*0.3048)) %>%
  select(-contains("ft")) %>%
  mutate(date_num = seq(1,length(DateTime), 1))

head(hyd_dep)

## create year, month, day and hour columns

hyd_dep <- hyd_dep %>%
  mutate(month = month(DateTime))%>%
  mutate(year = year(DateTime))%>%
  mutate(day = day(DateTime))%>%
  mutate(hour = hour(DateTime)) %>%
  mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))


hyd_vel <- hyd_dep %>%
  select(-contains("depth"))
head(hyd_vel)

hyd_dep <- hyd_dep %>%
  select(-contains("vel"))
head(hyd_dep)
# ## melt channel position data

hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q", "node", "date_num", "month", "day", "water_year","year", "hour"))
hyd_dep <- hyd_dep %>% rename(depth_cm = value)
hyd_dep <- filter(hyd_dep, variable == "depth_cm_MC")
hyd_vel<-reshape2::melt(hyd_vel, id=c("DateTime","Q", "node", "date_num", "month", "day", "water_year","year", "hour"))
hyd_vel <- hyd_vel %>% rename(vel_m_s = value)
hyd_vel <- filter(hyd_vel, variable == "vel_m_MC")

# depth -------------------------------------------------------------------

## filter data by cross section position

hyd_depM <- filter(hyd_dep, variable == "depth_cm_MC")
# hyd_depL <- filter(hyd_dep, variable == "depth_cm_LOB")
# hyd_depR <- filter(hyd_dep, variable == "depth_cm_ROB")

## predict values

new_data <- hyd_depM %>%
  mutate(prob_fit = predict(dep_ptch_mdl, newdata = hyd_depM, type="response")) %>%
  mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) ## predicts negative percentages - cut off at 0 for quick fix

# new_dataL <- hyd_depL %>%
#   mutate(prob_fit = predict(dep_ptch_mdl, newdata = hyd_depL, type="response")) %>%
#   mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) ## predicts negative percentages - cut off at 0 for quick fix
# 
# new_dataR <- hyd_depR %>%
#   mutate(prob_fit = predict(dep_ptch_mdl, newdata = hyd_depR, type="response")) %>%
#   mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) ## predicts negative percentages - cut off at 0 for quick fix



save(new_data, file="output_data/M1_LA13_typha_adult_patch_depth_discharge_probability_time_series_red_columns.RData")

# format probability time series ------------------------------------------

## look at data using lubridate etc

names(new_data)
## format date time
## format date time
new_data$DateTime<-as.POSIXct(new_data$DateTime,
                              format = "%Y-%m-%d %H:%M",
                              tz = "GMT")

## create year, month, day and hour columns and add water year

new_data <- new_data %>%
  mutate(month = month(DateTime)) %>%
  mutate(year = year(DateTime)) %>%
  mutate(day = day(DateTime)) %>%
  mutate(hour = hour(DateTime)) %>%
  mutate(water_year = ifelse(month == 10 | month == 11 | month == 12, year, year-1))


save(new_data, file="output_data/M1_LA13_typha_adult_patch_depth_discharge_probs_2010_2017_TS.RData")

# probability as a function of discharge -----------------------------------


load( file="output_data/M1_LA13_typha_adult_patch_depth_discharge_probs_2010_2017_TS.RData")
head(new_data)

## plot
range(new_data$Q) ## 26.22926 41750.16797 
range(new_data$prob_fit) ## 0.00000 0.8923723

peak <- new_data %>%
  group_by(variable) %>%
  filter(prob_fit == max(prob_fit)) #%>%


peakQM <- filter(peak, variable=="depth_cm_MC")
peakQM  <- max(peakQM$Q)
# 

## filter data by cross section position

new_dataM <- filter(new_data, variable == "depth_cm_MC")

## Main channel curves

load(file="root_interpolation_function.Rdata")


newx1a <- RootLinearInterpolant(new_dataM$Q, new_dataM$prob_fit, 0.25)
newx1a

if(length(newx1a) > 4) {
  newx1a <- c(newx1a[1], newx1a[length(newx1a)])
} else {
  newx1a <- newx1a
}
newx1a
newx2a  <- RootLinearInterpolant(new_dataM$Q, new_dataM$prob_fit, 0.50)

if(length(newx2a) > 4) {
  newx2a <- c(newx2a[1], newx2a[length(newx2a)])
} else {
  newx2a <- newx2a
}


newx3a <- RootLinearInterpolant(new_dataM$Q, new_dataM$prob_fit, 0.75)
newx3a

if(min(new_data$prob_fit)>0.75) {
  newx3a <- min(new_data$Q)
} else {
  newx3a <- newx3a
}

if(length(newx3a) > 4) {
  newx3a <- c(newx3a[1], newx3a[length(newx3a)])
} else {
  newx3a <- newx3a
}

newx3a

## MAKE DF OF Q LIMITS

limits <- as.data.frame(matrix(ncol=3, nrow=12)) %>%
  rename(LOB = V1, MC = V2, ROB = V3) 
rownames(limits)<-c("Low_Prob_1", "Low_Prob_2", "Low_Prob_3", "Low_Prob_4",
                    "Med_Prob_1", "Med_Prob_2", "Med_Prob_3", "Med_Prob_4",
                    "High_Prob_1", "High_Prob_2", "High_Prob_3", "High_Prob_4")


limits$MC <- c(newx1a[1], newx1a[2],newx1a[3], newx1a[4],
               newx2a[1], newx2a[2],newx2a[3], newx2a[4], 
               newx3a[1], newx3a[2],newx3a[3],newx3a[4])


limits


write.csv(limits, "output_data/M1_LA13_adult_patch_Depth_Q_limits.csv")

## plot with thresholds

png("Figures/Application_curves/Depth/LA13_adult_patch_depth_prob_Q_thresholds.png", width = 500, height = 600)
labels <- c(depth_cm_LOB = "Left Over Bank", depth_cm_MC = "Main Channel", depth_cm_ROB = "Right Over Bank")

ggplot(new_data, aes(x = Q, y=prob_fit)) +
  geom_line(aes(group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"))+
  #                       name="Cross\nSection\nPosition",
  #                       breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"),
  #                         labels = c("LOB", "MC", "ROB")) +
  
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.25, x=newx1a[1]), color="green") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.25, x=newx1a[2]), color="green") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.25, x=newx1a[3]), color="green") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.25, x=newx1a[4]), color="green") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.50, x=newx2a[1]), color="red") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.50, x=newx2a[2]), color="red") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.50, x=newx2a[3]), color="red") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.50, x=newx2a[4]), color="red") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.75, x=newx3a[1]), color="blue") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.75, x=newx3a[2]), color="blue") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.75, x=newx3a[3]), color="blue") +
  geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=0.75, x=newx3a[4]), color="blue") +
  
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "LA13: Adult Patch/Depth: Probability ~ Q",
       y = "Probability",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()



# dataframe for stats -----------------------------------------------------

## make dataframe for all years 


# create year_month column       
new_dataMx <- new_dataM %>% unite(month_year, year:month, sep="-", remove=F) 
head(new_dataMx)

## make dataframe for all years 

## define seasons/critical period - 
non_critical <- c(1:3, 10:12) 
critical <- c(4:9) 

new_dataMx <- new_dataMx %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )

# time stats - mid channel ------------------------------------------------

### define expression for low threshold 
## Main channel curves

load(file="expression_Q_limit_function.RData")

low_threshM <- expression_Q(newx1a, peakQM) 
low_threshM <-as.expression(do.call("substitute", list(low_threshM[[1]], list(limit = as.name("newx1a")))))

med_threshM <- expression_Q(newx2a, peakQM)
med_threshM <-as.expression(do.call("substitute", list(med_threshM[[1]], list(limit = as.name("newx2a")))))

high_threshM <- expression_Q(newx3a, peakQM)
high_threshM <-as.expression(do.call("substitute", list(high_threshM[[1]], list(limit = as.name("newx3a")))))

low_threshM
med_threshM
high_threshM

###### calculate amount of time

time_statsm <- new_dataMx %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Low = sum(eval(low_threshM))/length(DateTime)*100) %>%
  dplyr::mutate(Medium = sum(eval(med_threshM))/length(DateTime)*100) %>%
  dplyr::mutate(High = sum(eval(high_threshM))/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(Low.Seasonal = sum(eval(low_threshM))/length(DateTime)*100) %>%
  dplyr::mutate(Medium.Seasonal = sum(eval(med_threshM))/length(DateTime)*100) %>%
  dplyr::mutate(High.Seasonal = sum(eval(high_threshM))/length(DateTime)*100) %>%
  distinct(year, Low , Medium , High , Low.Seasonal, Medium.Seasonal, High.Seasonal) %>%
  mutate(position="MC")


# Combine and evaluate overall stats --------------------------------------

time_stats <- time_statsm

## melt
melt_time<-reshape2::melt(time_stats, id=c("year","season", "position"))
melt_time <- rename(melt_time, Probability_Threshold = variable)
head(melt_time)
unique(melt_time$Probability_Threshold)
write.csv(melt_time, "output_data/M1_LA13_typha_adult_patch_depth_time_stats.csv")

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

png("Figures/Application_curves/Depth/LA13_typha_adult_patch_depth_perc_time_above_threshold_annual.png", width = 500, height = 600)

ggplot(melt_time_ann, aes(x = year, y=value)) +
  geom_line(aes( group =c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA13: Time within discharge limit in relation to Depth (Annual)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)

dev.off()

melt_time_noncrit <- filter(melt_time_seas, season == "non_critical")

png("Figures/Application_curves/Depth/LA13_typha_adult_patch_depth_perc_time_above_threshold_non_critcal.png", width = 500, height = 600)

ggplot(melt_time_noncrit, aes(x = year, y=value)) +
  geom_line(aes( group = c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low.Seasonal", "Medium.Seasonal", "High.Seasonal"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA13: Time within discharge limit in relation to Depth (Non_critical)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)

dev.off()
## plot for summer stats - need probs in order

melt_time_crit <- filter(melt_time_seas, season == "critical")

png("Figures/Application_curves/Depth/LA13_typha_adult_patch_depth_perc_time_above_threshold_critcal.png", width = 500, height = 600)

ggplot(melt_time_crit, aes(x = year, y=value)) +
  geom_line(aes( group = c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low.Seasonal", "Medium.Seasonal", "High.Seasonal"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA13: Time within discharge limit in relation to Depth (Critical)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)

dev.off()

# Number of days above discharge ------------------------------------------
# need number of days discharge is above the limits outlined above - counted per month
## packages
new_dataM <- new_dataM %>%
  ungroup() %>%
  group_by(month, day, year, ID01 = data.table::rleid(eval(low_threshM))) %>%
  mutate(Low = if_else(eval(low_threshM), row_number(), 0L)) %>%
  ungroup() %>%
  group_by(month, day, year, ID02 = data.table::rleid(eval(med_threshM))) %>%
  mutate(Medium = if_else(eval(med_threshM), row_number(), 0L)) %>%
  ungroup() %>%
  group_by(month, day, year, ID03 = data.table::rleid(eval(high_threshM))) %>%
  mutate(High = if_else(eval(high_threshM), row_number(), 0L))

new_dataM <- mutate(new_dataM, position="MC")


## melt data frame so that each probability column are all in one row 
## select only columns needed - Q, month, year, day all IDs and probs
# names(new_data)

new_datax <- select(new_dataM, c(Q, month, year, day, ID01, Low, ID02, Medium, ID03, High, position, DateTime) )# all probs


## melt
melt_data<-reshape2::melt(new_datax, id=c("ID01", "ID02", "ID03", "day", "month", "year", "Q", "position"))
melt_data <- rename(melt_data, Probability_Threshold = variable, 
                    consec_hours = value)

melt_data
## groups data by year, month and ID & threshold
## counts the number of days in each month probability is within the depth of each threshold - days are not necessarily conseq
## each threshold separately

## count how many full days i.e. 24 hours
total_days01 <- melt_data %>% 
  filter(Probability_Threshold == "Low") %>% 
  group_by(ID01, day, month, year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_low = ifelse(n_hours >= 24, 1, 0)) # %>%
total_days01
## count the number of days in each month
total_days_per_month01 <- total_days01 %>%
  group_by(month, year, position) %>%
  summarise(days_per_month_low = sum(n_days_low))

total_days_per_month01

total_days02 <- melt_data %>% 
  filter(Probability_Threshold == "Medium") %>% 
  group_by(ID02, day, month, year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_medium = ifelse(n_hours >= 24, 1, 0)) # %>%

total_days_per_month02 <- total_days02 %>%
  group_by(month, year, position) %>%
  summarise(days_per_month_medium = sum(n_days_medium))

# total_days_per_month02

total_days03 <- melt_data %>% 
  filter(Probability_Threshold == "High") %>% 
  group_by(ID03, day, month, year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_high = ifelse(n_hours >= 24, 1, 0)) # %>%

total_days_per_month03 <- total_days03 %>%
  group_by(month, year, position) %>%
  summarise(days_per_month_high = sum(n_days_high))

total_days_per_month03

## combine all thresholds
total_days <- cbind( total_days_per_month01,total_days_per_month02[,4], total_days_per_month03[,4])
head(total_days)

write.csv(total_days, "output_data/M1_LA13_typha_adult_patch_depth_total_days.csv")

# # create year_month column       
total_days <- ungroup(total_days) %>%
  unite(month_year, year:month, sep="-", remove=F)


## convert month year to date format
library(zoo)
total_days$month_year <-  zoo::as.yearmon(total_days$month_year)
total_days$month_year <- as.Date(total_days$month_year)

## change names of columns
total_days <- rename(total_days, Low = days_per_month_low, Medium = days_per_month_medium, High = days_per_month_high)

# total_hours <- rename(total_hours, Low = n_days_low, Medium = n_days_medium, High = n_days_high)

## define seasons/critical period
# non_critical <- c(6:8) 
# critical <- c(9:12, 1:5) 


total_days <- total_days %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )


# ## melt data

melt_days<-reshape2::melt(total_days, id=c("month_year", "year", "month", "season", "position"))
melt_days <- rename(melt_days, Probability_Threshold = variable,
                    n_days = value)

head(melt_days)
# filter(melt_days, season=="non_critical" & position == "ROB")

## save df
write.csv(melt_days, "output_data/M1_LA13_typha_adult_patch_depth_total_days_long.csv")



## plot all ts
png("Figures/Application_curves/Depth/LA13_typha_adult_patch_depth_lob_rob_mc_no_days_within_Q.png", width = 500, height = 600)

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_color_manual(name="Probability Threshold",breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_x_date(breaks=pretty_breaks(), labels = date_format("%b %Y")) +
  scale_y_continuous(limits=c(0,31)) +
  # scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%b %Y")) +
  facet_wrap(~position, nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA13: Number of days within discharge limit in relation to Depth",
       y = "Number of days per Month",
       x = "Year") #+ theme_bw(base_size = 15)
dev.off()


## plot by year
png("Figures/Application_curves/Depth/LA13_typha_adult_patch_depth_lob_rob_mc_no_days_within_Q_by_year.png", width = 500, height = 600)

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_color_manual(name="Probability Threshold", breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) +
  scale_x_date(breaks=pretty_breaks(),labels = date_format("%b")) +
  scale_y_continuous(limits=c(0,31)) +
  # scale_x_continuous(breaks=as.numeric(month_year), labels=format(month_year,"%b")) +
  facet_wrap(~year+position, scale="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA13: Number of days within discharge limit in relation to Depth: Mid Channel",
       y = "Number of days per Month",
       x = "Month") #+ theme_bw(base_size = 15)
dev.off()

## plot by season/critical period
png("Figures/Application_curves/Depth/LA13_typha_adult_patch_depth_lob_rob_mc_no_days_within_Q_by_season.png", width = 500, height = 600)

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_color_manual(name="Probability Threshold",breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) +
  scale_x_date(breaks=pretty_breaks(),labels = date_format("%Y")) +
  scale_y_continuous(limits=c(0,31)) +
  # scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%Y")) +
  facet_wrap(~season +position, scales="free", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA13: Number of days within discharge limit in relation to Depth",
       y = "Number of days per Month",
       x = "Year") #+ theme_bw(base_size = 15)

dev.off()



# Velocity ----------------------------------------------------------------
## filter data by cross section position

hyd_velM <- filter(hyd_vel, variable == "vel_m_MC")


## predict values
load(file="models/vel_ptch_mdl.Rda")

new_data <- hyd_velM %>%
  mutate(prob_fit = predict(vel_ptch_mdl, newdata = hyd_velM, type="response")) %>%
  mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) ## predicts negative percentages - cut off at 0 for quick fix



save(new_data, file="output_data/M1_LA13_typha_adult_patch_velocity_discharge_probability_time_series_red_columns.RData")

# format probability time series ------------------------------------------

## look at data using lubridate etc
# probability as a function of discharge -----------------------------------

## plot
range(new_data$Q) ## 0.00 998.845 
range(new_data$prob_fit) ## 1.689185e-06 8.413046e-01

peak <- new_data %>%
  group_by(variable) %>%
  filter(prob_fit == max(prob_fit)) #%>%


peakQM <- filter(peak, variable=="vel_m_MC")
peakQM  <- max(peakQM$Q)
peakQM ## 26.22926


## filter data by cross section position

new_dataM <- filter(new_data, variable == "vel_m_MC")


## Main channel curves

load(file="root_interpolation_function.Rdata")


newx1a <- RootLinearInterpolant(new_dataM$Q, new_dataM$prob_fit, 0.25)
newx1a

if(length(newx1a) > 4) {
  newx1a <- c(newx1a[1], newx1a[length(newx1a)])
} else {
  newx1a <- newx1a
}
newx1a
newx2a  <- RootLinearInterpolant(new_dataM$Q, new_dataM$prob_fit, 0.50)

if(length(newx2a) > 4) {
  newx2a <- c(newx2a[1], newx2a[length(newx2a)])
} else {
  newx2a <- newx2a
}


newx3a <- RootLinearInterpolant(new_dataM$Q, new_dataM$prob_fit, 0.75)
newx3a

if(min(new_data$prob_fit)>0.75) {
  newx3a <- min(new_data$Q)
} else {
  newx3a <- newx3a
}

if(length(newx3a) > 4) {
  newx3a <- c(newx3a[1], newx3a[length(newx3a)])
} else {
  newx3a <- newx3a
}

newx3a

## MAKE DF OF Q LIMITS

limits <- as.data.frame(matrix(ncol=3, nrow=12)) %>%
  rename(LOB = V1, MC = V2, ROB = V3) 
rownames(limits)<-c("Low_Prob_1", "Low_Prob_2", "Low_Prob_3", "Low_Prob_4",
                    "Med_Prob_1", "Med_Prob_2", "Med_Prob_3", "Med_Prob_4",
                    "High_Prob_1", "High_Prob_2", "High_Prob_3", "High_Prob_4")


limits$MC <- c(newx1a[1], newx1a[2],newx1a[3], newx1a[4],
               newx2a[1], newx2a[2],newx2a[3], newx2a[4], 
               newx3a[1], newx3a[2],newx3a[3],newx3a[4])


limits


write.csv(limits, "output_data/M1_LA13_adult_patch_velocity_Q_limits.csv")

## plot with thresholds

png("Figures/Application_curves/Depth/LA13_adult_patch_velocity_prob_Q_thresholds.png", width = 500, height = 600)

labels <- c(vel_m_LOB = "Left Over Bank", vel_m_MC = "Main Channel", vel_m_ROB = "Right Over Bank")

ggplot(new_data, aes(x = Q, y=prob_fit)) +
  geom_line(aes(group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"))+
  #                       name="Cross\nSection\nPosition",
  #                       breaks=c("vel_m_LOB", "vel_m_MC", "vel_m_ROB"),
  #                         labels = c("LOB", "MC", "ROB")) +
  
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  geom_point(data = subset(new_data, variable =="vel_m_MC"), aes(y=0.25, x=newx1a[1]), color="green") +
  geom_point(data = subset(new_data, variable =="vel_m_MC"), aes(y=0.25, x=newx1a[2]), color="green") +
  geom_point(data = subset(new_data, variable =="vel_m_MC"), aes(y=0.25, x=newx1a[3]), color="green") +
  geom_point(data = subset(new_data, variable =="vel_m_MC"), aes(y=0.25, x=newx1a[4]), color="green") +
  geom_point(data = subset(new_data, variable =="vel_m_MC"), aes(y=0.50, x=newx2a[1]), color="red") +
  geom_point(data = subset(new_data, variable =="vel_m_MC"), aes(y=0.50, x=newx2a[2]), color="red") +
  geom_point(data = subset(new_data, variable =="vel_m_MC"), aes(y=0.50, x=newx2a[3]), color="red") +
  geom_point(data = subset(new_data, variable =="vel_m_MC"), aes(y=0.50, x=newx2a[4]), color="red") +
  geom_point(data = subset(new_data, variable =="vel_m_MC"), aes(y=0.75, x=newx3a[1]), color="blue") +
  geom_point(data = subset(new_data, variable =="vel_m_MC"), aes(y=0.75, x=newx3a[2]), color="blue") +
  geom_point(data = subset(new_data, variable =="vel_m_MC"), aes(y=0.75, x=newx3a[3]), color="blue") +
  geom_point(data = subset(new_data, variable =="vel_m_MC"), aes(y=0.75, x=newx3a[4]), color="blue") +
  
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "LA13: Adult Patch/Velocity: Probability ~ Q",
       y = "Probability",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()

# discharge time series plots with probability lines ----------------------

## make dataframe for all years 

# create year_month column       
new_dataMx <- new_dataM %>% unite(month_year, year:month, sep="-", remove=F) 
head(new_dataMx)



head(new_dataMx)


## make dataframe for all years 

## define seasons/critical period - check and change these!!
non_critical <- c(1:3, 10:12) 
critical <- c(4:9) 

new_dataMx <- new_dataMx %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )


limits
## produces percentage of time for each year and season within year for each threshold
# time stats - mid channel ------------------------------------------------

# time stats - mid channel ------------------------------------------------

### define expression for low threshold 
## Main channel curves
low_threshM <- expression_Q(newx1a, peakQM) 
low_threshM <-as.expression(do.call("substitute", list(low_threshM[[1]], list(limit = as.name("newx1a")))))

med_threshM <- expression_Q(newx2a, peakQM)
med_threshM <-as.expression(do.call("substitute", list(med_threshM[[1]], list(limit = as.name("newx2a")))))

high_threshM <- expression_Q(newx3a, peakQM)
high_threshM <-as.expression(do.call("substitute", list(high_threshM[[1]], list(limit = as.name("newx3a")))))

low_threshM
med_threshM
high_threshM

###### calculate amount of time

time_statsm <- new_dataMx %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Low = sum(eval(low_threshM))/length(DateTime)*100) %>%
  dplyr::mutate(Medium = sum(eval(med_threshM))/length(DateTime)*100) %>%
  dplyr::mutate(High = sum(eval(high_threshM))/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(Low.Seasonal = sum(eval(low_threshM))/length(DateTime)*100) %>%
  dplyr::mutate(Medium.Seasonal = sum(eval(med_threshM))/length(DateTime)*100) %>%
  dplyr::mutate(High.Seasonal = sum(eval(high_threshM))/length(DateTime)*100) %>%
  distinct(year, Low , Medium , High , Low.Seasonal, Medium.Seasonal, High.Seasonal) %>%
  mutate(position="MC")





time_stats <- time_statsm

## melt
melt_time<-reshape2::melt(time_stats, id=c("year","season", "position"))
melt_time <- rename(melt_time, Probability_Threshold = variable)
head(melt_time)
unique(melt_time$Probability_Threshold)
write.csv(melt_time, "output_data/M1_LA13_typha_adult_patch_velocity_time_stats.csv")

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

png("Figures/Application_curves/Depth/LA13_typha_adult_patch_depth_perc_time_above_threshold_annual.png", width = 500, height = 600)

ggplot(melt_time_ann, aes(x = year, y=value)) +
  geom_line(aes( group =c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA13: Time within discharge limit in relation to Depth (Annual)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)

dev.off()

melt_time_noncrit <- filter(melt_time_seas, season == "non_critical")

png("Figures/Application_curves/Depth/LA13_typha_adult_patch_depth_perc_time_above_threshold_non_critcal.png", width = 500, height = 600)

ggplot(melt_time_noncrit, aes(x = year, y=value)) +
  geom_line(aes( group = c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low.Seasonal", "Medium.Seasonal", "High.Seasonal"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA13: Time within discharge limit in relation to Depth (Non_critical)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)

dev.off()
## plot for summer stats - need probs in order

melt_time_crit <- filter(melt_time_seas, season == "critical")

png("Figures/Application_curves/Depth/LA13_typha_adult_patch_depth_perc_time_above_threshold_critcal.png", width = 500, height = 600)

ggplot(melt_time_crit, aes(x = year, y=value)) +
  geom_line(aes( group = c(), color = Probability_Threshold)) +
  scale_color_manual(name = "Probability Threshold", breaks = c("Low.Seasonal", "Medium.Seasonal", "High.Seasonal"),
                     values=c( "green", "red", "blue"),
                     labels = c("Low", "Medium", "High")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  facet_wrap(~position, scales="free_x", nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA13: Time within discharge limit in relation to Depth (Critical)",
       y = "Time (%)",
       x = "Year") #+ theme_bw(base_size = 15)

dev.off()

# Number of days above discharge ------------------------------------------
# need number of days discharge is above the limits outlined above - counted per month
## packages

new_dataM <- new_dataM %>%
  ungroup() %>%
  group_by(month, day, year, ID01 = data.table::rleid(eval(low_threshM))) %>%
  mutate(Low = if_else(eval(low_threshM), row_number(), 0L)) %>%
  ungroup() %>%
  group_by(month, day, year, ID02 = data.table::rleid(eval(med_threshM))) %>%
  mutate(Medium = if_else(eval(med_threshM), row_number(), 0L)) %>%
  ungroup() %>%
  group_by(month, day, year, ID03 = data.table::rleid(eval(high_threshM))) %>%
  mutate(High = if_else(eval(high_threshM), row_number(), 0L))

new_dataM <- mutate(new_dataM, position="MC")


new_datax <- select(new_dataM, c(Q, month, year, day, ID01, Low, ID02, Medium, ID03, High, position, DateTime) )# all probs


## melt
melt_data<-reshape2::melt(new_datax, id=c("ID01", "ID02", "ID03", "day", "month", "year", "Q", "position"))
melt_data <- rename(melt_data, Probability_Threshold = variable, 
                    consec_hours = value)

melt_data
## groups data by year, month and ID & threshold
## counts the number of days in each month probability is within the depth of each threshold - days are not necessarily conseq
## each threshold separately

## count how many full days i.e. 24 hours
total_days01 <- melt_data %>% 
  filter(Probability_Threshold == "Low") %>% 
  group_by(ID01, day, month, year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_low = ifelse(n_hours >= 24, 1, 0)) # %>%
total_days01
## count the number of days in each month
total_days_per_month01 <- total_days01 %>%
  group_by(month, year, position) %>%
  summarise(days_per_month_low = sum(n_days_low))

total_days_per_month01

total_days02 <- melt_data %>% 
  filter(Probability_Threshold == "Medium") %>% 
  group_by(ID02, day, month, year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_medium = ifelse(n_hours >= 24, 1, 0)) # %>%

total_days_per_month02 <- total_days02 %>%
  group_by(month, year, position) %>%
  summarise(days_per_month_medium = sum(n_days_medium))

# total_days_per_month02

total_days03 <- melt_data %>% 
  filter(Probability_Threshold == "High") %>% 
  group_by(ID03, day, month, year, position) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_high = ifelse(n_hours >= 24, 1, 0)) # %>%

total_days_per_month03 <- total_days03 %>%
  group_by(month, year, position) %>%
  summarise(days_per_month_high = sum(n_days_high))

total_days_per_month03

## combine all thresholds
total_days <- cbind( total_days_per_month01,total_days_per_month02[,4], total_days_per_month03[,4])
head(total_days)

write.csv(total_days, "output_data/M1_LA13_typha_adult_patch_velocity_total_days.csv")


# # create year_month column       
total_days <- ungroup(total_days) %>%
  unite(month_year, year:month, sep="-", remove=F)


## convert month year to date format
library(zoo)
total_days$month_year <-  zoo::as.yearmon(total_days$month_year)
total_days$month_year <- as.Date(total_days$month_year)

## change names of columns
total_days <- rename(total_days, Low = days_per_month_low, Medium = days_per_month_medium, High = days_per_month_high)

# total_hours <- rename(total_hours, Low = n_days_low, Medium = n_days_medium, High = n_days_high)

## define seasons/critical period
# non_critical <- c(6:8) 
# critical <- c(9:12, 1:5) 


total_days <- total_days %>%
  mutate(season = ifelse(month %in% non_critical, "non_critical", "critical") )


# ## melt data

melt_days<-reshape2::melt(total_days, id=c("month_year", "year", "month", "season", "position"))
melt_days <- rename(melt_days, Probability_Threshold = variable,
                    n_days = value)

head(melt_days)
## save df
write.csv(melt_days, "output_data/M1_LA13_typha_adult_patch_velocity_total_days_long.csv")



## plot all ts
png("Figures/Application_curves/Depth/LA13_typha_adult_patch_velocityh_lob_rob_mc_no_days_within_Q.png", width = 500, height = 600)

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_color_manual(name="Probability Threshold",breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_x_date(breaks=pretty_breaks(), labels = date_format("%b %Y")) +
  scale_y_continuous(limits=c(0,31)) +
  # scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%b %Y")) +
  facet_wrap(~position, nrow=3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA13: Number of days within discharge limit in relation to Depth",
       y = "Number of days per Month",
       x = "Year") #+ theme_bw(base_size = 15)
dev.off()


## plot by year
png("Figures/Application_curves/Depth/LA13_typha_adult_patch_velocity_lob_rob_mc_no_days_within_Q_by_year.png", width = 500, height = 600)

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_color_manual(name="Probability Threshold", breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) +
  scale_x_date(breaks=pretty_breaks(),labels = date_format("%b")) +
  scale_y_continuous(limits=c(0,31)) +
  # scale_x_continuous(breaks=as.numeric(month_year), labels=format(month_year,"%b")) +
  facet_wrap(~year+position, scale="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA13: Number of days within discharge limit in relation to Depth: Mid Channel",
       y = "Number of days per Month",
       x = "Month") #+ theme_bw(base_size = 15)
dev.off()

## plot by season/critical period
png("Figures/Application_curves/Depth/LA13_typha_adult_patch_velocity_lob_rob_mc_no_days_within_Q_by_season.png", width = 500, height = 600)

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Probability_Threshold, color = Probability_Threshold)) +
  scale_color_manual(name="Probability Threshold",breaks = c("Low", "Medium", "High"),
                     values=c( "green", "red", "blue")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1)) +
  scale_x_date(breaks=pretty_breaks(),labels = date_format("%Y")) +
  scale_y_continuous(limits=c(0,31)) +
  # scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%Y")) +
  facet_wrap(~season +position, scales="free", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "LA13: Number of days within discharge limit in relation to Depth",
       y = "Number of days per Month",
       x = "Year") #+ theme_bw(base_size = 15)

dev.off()


