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
#patch occurrence

png("Figures/Final_curves/Typha_adult_depth.png", width = 700, height = 600)

ggplot(data = depth, mapping = aes(x = depth_cm, y = occurrence))+
  geom_point(alpha = 0.2, size = 3)+
  labs(x = "Depth (cm)", y = "Probability of Occurrence")+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
  theme(text = element_text(size=25), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))

dev.off()

# head(depth)
# mean(na.omit(depth$depth_cm))

summary(dep_ptch_mdl <- lm(occurrence ~ depth_cm + I(depth_cm^2), data = depth))
# save(dep_ptch_mdl, file = "dep_ptch_mdl.rda")


# Hydraulic data ----------------------------------------------------------



## soft bottom reaches

F57C <- read.csv("/Users/katieirving/Documents/git/flow_eco_mech/input_data/HecRas/hydraulic_ts_F57C.csv")
# LA8 <- read.csv("input_data/HecRas/hydraulic_ts_LA8.csv")
# LA11 <- read.csv("input_data/HecRas/hydraulic_ts_LA11.csv")
# LA20 <- read.csv("input_data/HecRas/hydraulic_ts_LA20_2.csv")


## go through script one at a time

hydraul <- F57C[,-1]
names(hydraul)
head(hydraul)
## select columns

hyd_dep <- hydraul[,c(1:3,5,9,13)]
colnames(hyd_dep) <-c("DateTime", "node", "Q", "depth_ft_LOB", "depth_ft_MC", "depth_ft_ROB")

# nas <- which(complete.cases(hyd_dep) == FALSE)
# hyd_dep[nas,]

## convert unit from feet to meters

hyd_dep <- hyd_dep %>%
  mutate(depth_cm_LOB = (depth_ft_LOB*0.3048)*100,
         depth_cm_MC = (depth_ft_MC*0.3048)*100,
         depth_cm_ROB = (depth_ft_ROB*0.3048)*100) %>%
  select(-contains("ft")) %>%
  mutate(date_num = seq(1,length(DateTime), 1))


## workflow
## get probabilities for depth at each hourly time step
## get thresholds i.e. 25, 50, 75%

# ## melt channel position data
hyd_dep<-reshape2::melt(hyd_dep, id=c("DateTime","Q", "node", "date_num"))
hyd_dep <- hyd_dep %>% rename(depth_cm = value)
# head(hyd_dep)
# summary(dep_ptch_mdl)

new_data <- hyd_dep %>%
  mutate(prob_fit = predict(dep_ptch_mdl, newdata = hyd_dep, type="response")) %>%
  mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) ## predicts negative percentages - cut off at 0 for quick fix

# range(new_data$prob_fit)
# head(new_data)


save(new_data, file="output_data/M1_F57C_typha_adult_patch_depth_discharge_probability_time_series_red_columns.RData")

# format probability time series ------------------------------------------

## look at data using lubridate etc

names(new_data)
## format date time
new_data$DateTime<-as.POSIXct(new_data$DateTime,
                              format = "%Y-%m-%d %H:%M",
                              tz = "America/Los_Angeles")

## create year, month, day and hour columns

new_data <- new_data %>%
  mutate(month = month(DateTime))%>%
  mutate(year = year(DateTime))%>%
  mutate(day = day(DateTime))%>%
  mutate(hour = hour(DateTime))


head(new_data)

save(new_data, file="output_data/M1_F57C_typha_adult_patch_depth_discharge_probs_2010_2017_TS.RData")

# probability as a function of discharge -----------------------------------


load( file="output_data/M1_F57C_typha_adult_patch_depth_discharge_probs_2010_2017_TS.RData")
head(new_data)

## plot
range(new_data$Q) ## 26.22926 41750.16797 
range(new_data$prob_fit) ## 0.00000 0.8923723

peak <- new_data %>%
  group_by(variable) %>%
  filter(prob_fit == max(prob_fit)) #%>%


peakQM <- filter(peak, variable=="depth_cm_MC")
peakQM  <- max(peakQM$Q)
peakQM ## 1537.531

peakQL <- filter(peak, variable=="depth_cm_LOB")
peakQL  <- max(peakQL$Q) ## 
peakQL ## 1952.538

peakQR <- filter(peak, variable=="depth_cm_ROB")
peakQR  <- max(peakQR$Q) ## 
peakQR ## 5950.665

## filter data by cross section position

new_dataM <- filter(new_data, variable == "depth_cm_MC")
new_dataL <- filter(new_data, variable == "depth_cm_LOB")
new_dataR <- filter(new_data, variable == "depth_cm_ROB")

## Main channel curves

MC_curve <- spline(new_dataM$Q, new_dataM$prob_fit,
                   xmin = min(new_dataM$Q), xmax = max(new_dataM$Q), ties = mean)

MC_curve_lower <- spline(new_dataM$Q, new_dataM$prob_fit,
                         xmin = min(new_dataM$Q), xmax = peakQM, ties = mean)
MC_curve_upper <- spline(new_dataM$Q, new_dataM$prob_fit,
                         xmin = peakQM, xmax = max(new_dataM$Q), ties = mean)

## main channel values
newx1a <- approx(x = MC_curve_lower$y, y = MC_curve_lower$x, xout = 25)$y
newx1a
# newx1a <- min(MC_curve_lower$x)

newx1b <- approx(x = MC_curve_upper$y, y = MC_curve_upper$x, xout = 25)$y
newx1b 
# newx1b <- max(MC_curve_upper$x)

newx2a <- approx(x = MC_curve_lower$y, y = MC_curve_lower$x, xout = 50)$y
newx2a

newx2b <- approx(x = MC_curve_upper$y, y = MC_curve_upper$x, xout = 50)$y
newx2b

newx3a <- approx(x = MC_curve_lower$y, y = MC_curve_lower$x, xout = 75)$y
newx3a

newx3b <- approx(x = MC_curve_upper$y, y = MC_curve_upper$x, xout = 75)$y
newx3b

## LOB curves

LOB_curve <- spline(new_dataL$Q, new_dataL$prob_fit,
                    xmin = min(new_dataL$Q), xmax = max(new_dataL$Q), ties = mean)

LOB_curve_lower <- spline(new_dataL$Q, new_dataL$prob_fit,
                          xmin = min(new_dataL$Q), xmax = peakQL, ties = mean)
LOB_curve_upper <- spline(new_dataL$Q, new_dataL$prob_fit,
                          xmin = peakQL, xmax = max(new_dataL$Q), ties = mean)

### problem here as 2 peaks!!! fix later!!!!!
newx1aL <- approx(x = LOB_curve_lower$y, y = LOB_curve_lower$x, xout = 25)$y
newx1aL
newx1aL <- approx(x = LOB_curve$y, y = LOB_curve$x, xout = 25)$y
newx1aL
newx1aL <- min(LOB_curve_lower$x)

newx1bL <- approx(x = LOB_curve_upper$y, y = LOB_curve_upper$x, xout = 25)$y
newx1bL
# newx1bL <- max(LOB_curve_upper$x)

newx2aL <- approx(x = LOB_curve_lower$y, y = LOB_curve_lower$x, xout = 50)$y
newx2aL <- min(LOB_curve_lower$x)

newx2bL <- approx(x = LOB_curve_upper$y, y = LOB_curve_upper$x, xout = 50)$y
newx2bL

newx3aL <- approx(x = LOB_curve_lower$y, y = LOB_curve_lower$x, xout = 75)$y
newx3aL <- NA

newx3bL <- approx(x = LOB_curve_upper$y, y = LOB_curve_upper$x, xout = 75)$y
newx3bL

## ROB curves

ROB_curve <- spline(new_dataR$Q, new_dataR$prob_fit,
                    xmin = min(new_dataR$Q), xmax = max(new_dataR$Q), ties = mean)

ROB_curve_lower <- spline(new_dataR$Q, new_dataR$prob_fit,
                          xmin = min(new_dataR$Q), xmax = peakQR, ties = mean)
ROB_curve_upper <- spline(new_dataR$Q, new_dataR$prob_fit,
                          xmin = peakQR, xmax = max(new_dataR$Q), ties = mean)

## main channel values
newx1aR <- approx(x = ROB_curve_lower$y, y = ROB_curve_lower$x, xout = 25)$y
newx1aR
# newx1aR <- min(ROB_curve_lower$x)

newx1bR <- approx(x = ROB_curve_upper$y, y = ROB_curve_upper$x, xout = 25)$y
newx1bR
# newx1bR <- max(ROB_curve_upper$x)

newx2aR <- approx(x = ROB_curve_lower$y, y = ROB_curve_lower$x, xout = 50)$y
newx2aR

newx2bR <- approx(x = ROB_curve_upper$y, y = ROB_curve_upper$x, xout = 50)$y
newx2bR

newx3aR <- approx(x = ROB_curve_lower$y, y = ROB_curve_lower$x, xout = 75)$y
newx3aR

newx3bR <- approx(x = ROB_curve_upper$y, y = ROB_curve_upper$x, xout = 75)$y
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

write.csv(limits, "output_data/M1_F57C_adult_patch_Depth_Q_limits.csv")

## plot with thresholds

png("Figures/Application_curves/Depth/F57C_adult_depth_prob_Q_thresholds.png", width = 500, height = 600)
labels <- c(depth_cm_LOB = "Left Over Bank", depth_cm_MC = "Main Channel", depth_cm_ROB = "Right Over Bank")

ggplot(new_data, aes(x = Q, y=prob_fit)) +
  geom_line(aes(group = variable, lty = variable)) +
  scale_linetype_manual(values= c("dotted", "solid", "dashed"))+
  #                       name="Cross\nSection\nPosition",
  #                       breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"),
  #                         labels = c("LOB", "MC", "ROB")) +
  
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  # # geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=25, x=newx1a), color="green") +
  # geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=25, x=newx1b), color="green") +
  # # geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=50, x=newx2a), color="red") +
  # geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=50, x=newx2b), color="red") +
  # # geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=75, x=newx3a), color="blue") +
  # # geom_point(data = subset(new_data, variable =="depth_cm_MC"), aes(y=75, x=newx3b), color="blue") +
  # 
  # # geom_point(data = subset(new_data, variable =="depth_cm_LOB"), aes(y=25, x=newx1aL), color="green") +
  # geom_point(data = subset(new_data, variable =="depth_cm_LOB"), aes(y=25, x=newx1bL), color="green") +
  # geom_point(data = subset(new_data, variable =="depth_cm_LOB"), aes(y=50, x=newx2aL), color="red") +
  # geom_point(data = subset(new_data, variable =="depth_cm_LOB"), aes(y=50, x=newx2bL), color="red") +
  # # geom_point(data = subset(new_data, variable =="depth_cm_LOB"), aes(y=75, x=newx3aL), color="blue") +
  # # geom_point(data = subset(new_data, variable =="depth_cm_LOB"), aes(y=75, x=newx3bL), color="blue") +
  # 
  # # geom_point(data = subset(new_data, variable =="depth_cm_ROB"), aes(y=25, x=newx1aR), color="green") +
  # geom_point(data = subset(new_data, variable =="depth_cm_ROB"), aes(y=25, x=newx1bR), color="green") +
  # # geom_point(data = subset(new_data, variable =="depth_cm_ROB"), aes(y=50, x=newx2aR), color="red") +
  # geom_point(data = subset(new_data, variable =="depth_cm_ROB"), aes(y=50, x=newx2bR), color="red") +
  # # geom_point(data = subset(new_data, variable =="depth_cm_ROB"), aes(y=75, x=newx3aR), color="blue") +
  # geom_point(data = subset(new_data, variable =="depth_cm_ROB"), aes(y=75, x=newx3bR), color="blue") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "F57C: Seedling/Depth: Probability ~ Q",
       y = "Probability",
       x = "Q (cfs)") #+ theme_bw(base_size = 15)

dev.off()


# discharge time series plots with probability lines ----------------------

##  plot time series of discharge - 0.2 prob line

ggplot(new_datax) +
  geom_line(aes(x =DateTime, y=Q)) +
  # theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(new_datax$month_year), labels=format(new_datax$month_year,"%b %Y")) +
  geom_hline(yintercept=newxmid, linetype="dashed", color="red")+
  facet_wrap(~year, scales="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Discharge over time",
       y = "Discharge",
       x = "Time") #+ theme_bw(base_size = 15)

##  plot time series of discharge - subset to one year
new_datax_2016 <- filter(new_datax, year==2016)

ggplot(new_datax_2016) +
  geom_line(aes(x =DateTime, y=Q)) +
  # theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(new_datax$month_year), labels=format(new_datax$month_year,"%b %Y")) +
  geom_hline(yintercept=newxmid, linetype="dashed", color="red")+
  # geom_hline(yintercept=newx1a, linetype="dashed", color="green")+
  # geom_hline(yintercept=newx3a, linetype="dashed", color="blue")+
  # geom_hline(yintercept=newx3b, linetype="dashed", color="blue")+
  # facet_wrap(~month, scales="free_x", nrow=4) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Discharge over time",
       y = "Discharge",
       x = "Time") #+ theme_bw(base_size = 15)

# dataframe for stats -----------------------------------------------------

## make dataframe for all years 

head(new_datax)
names(new_datax)

## define seasons
winter <- c(1,2,3,4,11,12) ## winter months
summer <- c(5:10) ## summer months

new_datax <- new_datax %>%
  mutate(season = ifelse(month %in% winter, "winter", "summer") )

## produces percentage of time for each year and season within year for each threshold

time_stats <- new_datax %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(High = sum(Q >= newxmid)/length(DateTime)*100) %>%
  dplyr::mutate(Low = sum(Q < newxmid)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(High.Seasonal = sum(Q >= newxmid)/length(DateTime)*100) %>%
  dplyr::mutate(Low.Seasonal = sum(Q < newxmid)/length(DateTime)*100) %>%
  distinct(year, Low , High, Low.Seasonal, High.Seasonal)


time_stats

## melt
melt_time<-reshape2::melt(time_stats, id=c("year","season"))
melt_time <- rename(melt_time, Probability.of.Occurrence = variable)

## subset annual stats
ann_stats <- unique(melt_time$Probability.of.Occurrence)[1:2]
melt_time_ann <- melt_time %>% filter(Probability.of.Occurrence %in% ann_stats ) %>%
  select(-season) %>% distinct()

# head(melt_time_ann)
unique(melt_time_ann$Probability.of.Occurrence)

## subset seasonal stats
seas_stats <- unique(melt_time$Probability.of.Occurrence)[3:4]
melt_time_seas <- filter(melt_time, Probability.of.Occurrence %in% seas_stats )
head(melt_time_seas)
melt_time_seas


## plot for annual stats - need probs in order
ggplot(melt_time_ann, aes(x = year, y=value)) +
  geom_line(aes( group = Probability.of.Occurrence, color = Probability.of.Occurrence)) +
  scale_color_manual(breaks = c("Low", "High"),
                     values=c( "red", "blue")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  # facet_wrap(~year, scales="free_x", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Time within discharge limit in relation to Depth (Annual)",
       y = "Percentage of time",
       x = "Year") #+ theme_bw(base_size = 15)

## plot for winter stats - need probs in order

melt_time_winter <- filter(melt_time_seas, season == "winter")
unique(melt_time_winter$Probability.of.Mortality)

ggplot(melt_time_winter, aes(x = year, y=value)) +
  geom_line(aes( group = c(), color = Probability.of.Occurrence)) +
  scale_color_manual(breaks = c("Low.Seasonal", "High.Seasonal"),
                     values=c( "red", "blue")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  # facet_wrap(~year, scales="free_x", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Time within discharge limit in relation to Depth (Winter)",
       y = "Percentage of time",
       x = "Year") #+ theme_bw(base_size = 15)

## plot for summer stats - need probs in order

melt_time_summer <- filter(melt_time_seas, season == "summer")

ggplot(melt_time_summer, aes(x = year, y=value)) +
  geom_line(aes( group = c(), color = Probability.of.Occurrence)) +
  scale_color_manual(breaks = c("Low.Seasonal", "High.Seasonal"),
                     values=c( "red", "blue")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  # scale_x_continuous(breaks=as.numeric(total_days$month_year), labels=format(total_days$month_year,"%b %Y")) +
  # facet_wrap(~year, scales="free_x", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Time within discharge limit in relation to Depth (Summer)",
       y = "Percentage of time",
       x = "Year") #+ theme_bw(base_size = 15)

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


load( file="output_data/M1_F57C_typha_adult_patch_depth_discharge_probs_2010_2017_TS.RData")
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
  mutate(n_days_low = ifelse(n_hours >= 23, 1, 0)) # %>%
# total_days01
## count the number of days in each month
total_days_per_month01 <- total_days01 %>%
  group_by(month, year) %>%
  summarise(days_per_month_low = sum(n_days_low))


total_days02 <- melt_data %>% 
  filter(Probability.of.Occurrence== "High") %>% 
  group_by(ID02, day, month, year) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_high = ifelse(n_hours >= 23, 1, 0)) # %>%

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
  labs(title = "Number of days within discharge limit in relation to Depth",
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
  labs(title = "Number of days within discharge limit in relation to Depth",
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
  labs(title = "Number of days within discharge limit in relation to Depth",
       y = "Number of days per Month",
       x = "Month") #+ theme_bw(base_size = 15)




