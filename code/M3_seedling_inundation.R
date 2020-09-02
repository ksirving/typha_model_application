## typha adult patch velocity

library(tidyverse)
library(lubridate)


load("input_data/typha_depth.RData")

ggplot(data = depth[!is.na(depth$seedling_survial_perc),], mapping = aes(x = depth_cm, y = seedling_survial_perc))+
  geom_point()+
  labs(x = "Depth (cm)", y = "Seedling survival (%)")+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
  theme(text = element_text(size=20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))


head(depth)
summary(dep_sdlng_mdl <- lm(seedling_survial_perc~depth_cm + I(depth_cm^2), data = depth))
save(dep_sdlng_mdl, file = "models/dep_sdlng_mdl.rda")

## upload hydarlic data

hydraul <- read.csv("input_data/demo_ts_F57C.csv")
## select columns
hyd_dep <- hydraul[,c(1:3,9)]
colnames(hyd_dep)[4] <-"depth_ft"

## convert unit from feet to meters
hyd_dep$depth_cm <- (hyd_dep$depth_ft*0.3048)*100
head(hyd_dep)
range(hyd_dep$depth_cm)

## plot depth v Q rating curve
plot(hyd_dep$Q, hyd_dep$depth_cm,  main = "F57C: Depth ~ Q", xlab="Q (cfs)", ylab="Depth (cm)")

## predict on node data using model
names(hyd_dep)
names(depth)
head(depth)


hyd_dep$date_num <- seq(1,length(hyd_dep$DateTime), 1)
plot(hyd_dep$date_num, hyd_dep$depth_cm, type="n")
lines(hyd_dep$date_num, hyd_dep$depth_cm)

## workflow
## get probabilities for depth at each hourly time step
## get thresholds i.e. 25, 50, 75%

head(hyd_dep)
summary(dep_sdlng_mdl)

new_data <- hyd_dep %>%
  select(c(DateTime, Q, depth_cm, date_num)) %>%
  mutate(prob_fit = predict(dep_sdlng_mdl, newdata = hyd_dep, type="response")) %>%
  mutate(prob_fit = ifelse(prob_fit<=0, 0, prob_fit)) ## predicts negative percentages - cut off at 0 for quick fix

range(new_data$prob_fit)
head(new_data)

save(new_data, file="output_data/M3_F57C_typha_seedling_depth_discharge_probability_time_series_red_columns.RData")

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

save(new_data, file="output_data/M3_F57C_typha_seedling_depth_discharge_probs_2010_2017_TS.RData")

# probability as a function of discharge -----------------------------------


load( file="output_data/M3_F57C_typha_seedling_depth_discharge_probs_2010_2017_TS.RData")
head(new_data)

## plot
range(new_data$Q) ## 0.00 998.845 
range(new_data$prob_fit) ## 10.4718 100
## smooth spline the curve to get exact value of discharge at a given probability
spl <- smooth.spline(new_data$prob_fit ~ new_data$Q)

## find peak of prob v Q

peak <- filter(new_data, prob_fit == max(prob_fit)) #%>%
peakQ <- select(peak, Q)
peakQ  <- peakQ[1,1]
peakQ ## 2.21

## function for each probability
min(new_data$Q)
newymid <- 50
newxmid <- try(uniroot(function(x) predict(spl, x, deriv = 0)$y - newymid,
                       interval = c(min(new_data$Q), max(new_data$Q)))$root, silent=T)
newxmid ## on descent of curve

plot(new_data$Q, new_data$prob_fit, type="n", main = "Seedling/Depth: Probability according to Q", xlab="Q (cfs)", ylab="Seedling Survival (%)")
lines(spl, col="black")
points(newxmid, newymid, col="red", pch=19) # 50%


plot(new_data$depth_cm, new_data$prob_fit, main = "Seedling/Depth: Probability according to Depth (cm)", xlab="Depth (cm)", ylab="Probability of Occurrence")


### plot discharge over time

# create year_month column       
new_datax <- new_data %>% unite(month_year, year:month, sep="-", remove=F) 
head(new_datax)

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
  dplyr::mutate(High = sum(Q <= newxmid)/length(DateTime)*100) %>%
  dplyr::mutate(Low = sum(Q > newxmid)/length(DateTime)*100) %>%
  ungroup() %>%
  dplyr::group_by(year, season) %>%
  dplyr::mutate(High.Seasonal = sum(Q <= newxmid)/length(DateTime)*100) %>%
  dplyr::mutate(Low.Seasonal = sum(Q > newxmid)/length(DateTime)*100) %>%
  distinct(year, Low , High, Low.Seasonal, High.Seasonal)


time_stats

## melt
melt_time<-reshape2::melt(time_stats, id=c("year","season"))
melt_time <- rename(melt_time, Seedling.Survival = variable)

## subset annual stats
ann_stats <- unique(melt_time$Seedling.Survival)[1:2]
melt_time_ann <- melt_time %>% filter(Seedling.Survival %in% ann_stats ) %>%
  select(-season) %>% distinct()

# head(melt_time_ann)
unique(melt_time_ann$Seedling.Survival)

## subset seasonal stats
seas_stats <- unique(melt_time$Seedling.Survival)[3:4]
melt_time_seas <- filter(melt_time, Seedling.Survival %in% seas_stats )
head(melt_time_seas)
melt_time_seas


## plot for annual stats - need probs in order
ggplot(melt_time_ann, aes(x = year, y=value)) +
  geom_line(aes( group = Seedling.Survival, color = Seedling.Survival)) +
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
# unique(melt_time_winter$Probability.of.Mortality)

ggplot(melt_time_winter, aes(x = year, y=value)) +
  geom_line(aes( group = c(), color = Seedling.Survival)) +
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
  geom_line(aes( group = c(), color = Seedling.Survival)) +
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


load( file="output_data/M1_F57C_typha_seedline_depth_discharge_probs_2010_2017_TS.RData")
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
                       interval = c(min(new_data$Q), max(new_data$Q)))$root, silent=T)


# all columns based on different probabilities
## count number events within each threshold with a running total - max total is the number of consequative 
# events (hours) per day. if else statements to consider the thresholds newx1a/b etc
## order by datetime

new_data <- arrange(new_data, date_num)

new_data <- new_data %>%
  group_by(month, day, year, ID01 = data.table::rleid(Q > newxmid)) %>%
  mutate(Low = if_else(Q > newxmid, row_number(), 0L)) %>%
  ungroup() %>%
  group_by(month, day, year, ID02 = data.table::rleid(Q <= newxmid)) %>%
  mutate(High = if_else(Q <= newxmid, row_number(), 0L))


head(new_data)
names(new_data)

## melt data frame so that each probability column are all in one row 
## select only columns needed - Q, month, year, day all IDs and probs
# names(new_data)

new_datax <- select(new_data, c(Q, month, year, day, ID01, Low, ID02,  High) )# all probs
names(new_datax)

## melt
melt_data<-reshape2::melt(new_datax, id=c("ID01", "ID02", "day", "month", "year", "Q"))
melt_data <- rename(melt_data, Seedling.Survival = variable, 
                    consec_hours = value)


## groups data by year, month and ID & threshold
## counts the number of days in each month probability is within the depth of each threshold - days are not necessarily conseq
## each threshold separately

## count how many full days i.e. 24 hours
total_days01 <- melt_data %>% 
  filter(Seedling.Survival == "Low") %>% 
  group_by(ID01, day, month, year) %>%
  summarise(n_hours = max(consec_hours))  %>%
  mutate(n_days_low = ifelse(n_hours >= 24, 1, 0)) # %>%
# total_days01
## count the number of days in each month
total_days_per_month01 <- total_days01 %>%
  group_by(month, year) %>%
  summarise(days_per_month_low = sum(n_days_low))


total_days02 <- melt_data %>% 
  filter(Seedling.Survival== "High") %>% 
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
melt_days <- rename(melt_days, Seedling.Survival = variable,
                    n_days = value)

head(melt_days)

##  plot - number of days 

ggplot(melt_days, aes(x =month_year, y=n_days)) +
  geom_line(aes( group = Seedling.Survival, color = Seedling.Survival)) +
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
  geom_line(aes( group = Seedling.Survival, color = Seedling.Survival)) +
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
  geom_line(aes( group = Seedling.Survival, color = Seedling.Survival)) +
  scale_color_manual(breaks = c("Low", "High"),
                     values=c(  "red", "blue")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  scale_x_continuous(breaks=as.numeric(melt_days$month_year), labels=format(melt_days$month_year,"%b %Y")) +
  facet_wrap(~season, scales="free_x", nrow=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Number of days within discharge limit in relation to Depth",
       y = "Number of days per Month",
       x = "Month") #+ theme_bw(base_size = 15)




