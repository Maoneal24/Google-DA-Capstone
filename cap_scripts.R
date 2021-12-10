library(tidyverse)
library(lubridate)
library(ggmap)
library(RColorBrewer)
library(data.table)

##import csvs
mydir <- "csvfolder"
myfiles <- list.files(path=mydir,pattern="*.csv", full.names=TRUE)

##create raw dataframe
dat_csv_raw <- map_dfr(myfiles, read_csv, col_types = cols("start_station_id" = col_character(),"end_station_id" = col_character()))

##create clean (working) dataframe while removing the WATSON TESTING rides
dat_csv_clean <- distinct(filter(dat_csv_raw, is.na(start_station_name) | start_station_name != "WATSON TESTING - DIVVY"))

##remove unwanted columns
dat_csv_clean <- dat_csv_clean[, !colnames(dat_csv_clean) %in% c("start_station_name","start_station_id","end_station_name","end_station_id")]

##create a ride_length column by calculating the difference between start and end times
dat_csv_clean$ride_length <- dat_csv_clean$ended_at - dat_csv_clean$started_at

##filter out the rides <= 0 second long
dat_csv_clean <- filter(dat_csv_clean, ride_length > 0)

##create a weekday column
dat_csv_clean$weekday <- weekdays(dat_csv_clean$started_at)

##create a month column
dat_csv_clean$month <- months(dat_csv_clean$started_at)

##create a time column
dat_csv_clean$time <- format(dat_csv_clean$started_at,format = "%H:%M:%S")

##create a pie chart comparing member and casual ride counts
casual_count <- as.integer(count(filter(dat_csv_clean, member_casual == "casual")))
member_count <- as.integer(count(filter(dat_csv_clean, member_casual == "member")))

df_pi <- data.frame(
  group = c("member", "casual"),
  value = c(member_count, casual_count))

df_pi <- df_pi %>%
  arrange(desc(group)) %>%
  mutate(prop=value/sum(df_pi$value)*100) %>%
  mutate(ypos=cumsum(prop)-0.5*prop)

pie <- ggplot(data=df_pi, aes(x="", y=prop, fill=group))

pie + geom_bar(stat='identity', width=1, color="black") +
  coord_polar("y", start=0) +
  theme_void() + #removes background, grid, and numeric labels
  geom_text(aes(y=ypos, label=paste(group,"\n",round(prop,digits=2),"%"),size=6)) +
  scale_fill_brewer(palette="Set2") +
  labs(title="All Rides by User Type") +
  theme(plot.title=element_text(size=14,face="bold.italic", hjust=0.5,vjust=0),
        legend.position="none")

##Create graphs for rideable_type
ggplot(dat_csv_clean,aes(x=rideable_type)) +
  geom_bar() +
  scale_x_discrete(name="bicycle types", labels=c("Classic", "Docked", "Electric")) +
  labs(title="count of rides by type of bicycle") +
  facet_wrap(~member_casual)

## rideable pie - start by creating the counts to fill the pie chart dataframe
#member counts
classic_count_m <- as.integer(count(filter(dat_csv_clean, member_casual == "member" & rideable_type == "classic_bike")))
docked_count_m <- as.integer(count(filter(dat_csv_clean, member_casual == "member" & rideable_type == "docked_bike")))
electric_count_m <- as.integer(count(filter(dat_csv_clean, member_casual == "member" & rideable_type == "electric_bike")))

#casual counts
classic_count_c <- as.integer(count(filter(dat_csv_clean, member_casual == "casual" & rideable_type == "classic_bike")))
docked_count_c <- as.integer(count(filter(dat_csv_clean, member_casual == "casual" & rideable_type == "docked_bike")))
electric_count_c <- as.integer(count(filter(dat_csv_clean, member_casual == "casual" & rideable_type == "electric_bike")))

#dataframe for rideable pie
df_ride_pie <- data.frame(
  group = c("member", "member", "member", "casual", "casual", "casual"),
  type = c("classic", "docked", "electric", "classic", "docked", "electric"),
  value = c(classic_count_m, docked_count_m,electric_count_m,classic_count_c, docked_count_c,electric_count_c)
)

df_ride_pie <- df_ride_pie %>%
  arrange(desc(type)) %>%
  mutate(prop=(ifelse(group=="member",value/member_count*100,value/casual_count*100)))

dt_ride_pie <- data.table(df_ride_pie)

dt_ride_pie[,ypos:=cumsum(prop)-0.5*prop, by=group]

#plot rideable_type pie chart
ggplot(data=dt_ride_pie, aes(x="", y=prop, fill=type)) +
  geom_col(position="stack", width=1, color="black") +
  geom_text(aes(label=paste(round(prop,digits=1),"%", sep=""), x=1.1),
            position=position_stack(vjust=0.5)) +
  theme_void() +
  labs(title="Proportions of Bike Type by User Type") +
  theme(plot.title=element_text(size=14,
                                face="bold.italic", 
                                hjust=0.5,
                                margin = margin(10,0,10,0)),
        legend.title=element_blank(),
        legend.position = "top",
        strip.text=element_text(size=10,
                                face="bold",
                                vjust=0)) +
  coord_polar("y", start=0) +
  facet_wrap(~group, strip.position = "bottom")

##create histogram counting rides by the day of the week
ggplot(dat_csv_clean, aes(x=weekday, group_by=member_casual, fill=member_casual)) +
  geom_histogram(position="dodge", bins=7, stat="count", color="black") +
  labs(title="Count of rides by day of the week") +
  scale_x_discrete(name="Weekday",
                   limits=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"),
                   labels=c("Sun","Mon","Tues","Wed","Thurs","Fri","Sat")) +
  scale_y_continuous(name="Rides", breaks=seq(100000,500000,100000), labels=c("100K","200K","300K","400K","500K")) +
  scale_fill_brewer(palette="Set2") +
  theme(legend.position = c(0.5,0.96),
        legend.background = element_rect(fill="transparent"),
        legend.title=element_blank(),
        legend.direction="horizontal")

##create histogram of ride counts by month
ggplot(dat_csv_clean, aes(x=month, group_by=member_casual, fill=member_casual)) +
  geom_histogram(position="dodge", bins=12, color="black", stat="count") +
  labs(title="Count of rides by Month") +
  scale_fill_brewer(palette="Set2") +
  scale_x_discrete(name="Month", limits=c("January","February","March","April","May","June","July","August","September","October","November","December"),
                   labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  scale_y_continuous(name="Rides", breaks=c(100000,200000,300000,400000), labels=c("100K","200K","300K","400K")) +
  theme(legend.position=c(0.15,0.75),
        legend.background = element_rect(fill="transparent"),
        legend.title=element_blank())

##create histogram of ride counts by time of day
ggplot(dat_csv_clean, aes(x=format(started_at, format="%H"), group_by=member_casual, fill=member_casual)) +
  geom_histogram(position="dodge", bins=24, color="black", stat="count") +
  labs(title="Count of rides by hour of the day") +
  scale_fill_brewer(palette="Set2") +
  scale_y_continuous(name="Rides", breaks=c(100000,200000,300000), labels=c("100K","200K","300K")) +
  scale_x_discrete(name="Starting Hour (24hr format)") +
  theme(legend.position=c(0.25,0.75),
        legend.background = element_rect(fill="transparent"),
        legend.title=element_blank())

##Scatter plot illustrating ride length
#commented out because it takes 10-15 minutes to produce and I source this file often during troubleshooting
# ggplot(dat_csv_clean, aes(x=started_at, y=ride_length, color=member_casual)) +
#   geom_point() +
#   labs(x="Ride Start", y="length in seconds", title="ride durations") +
#   scale_fill_brewer(palette="Set2")

##Histogram showing count of ride_length by member_casual
ggplot(dat_csv_clean, aes(x=ride_length/60, group_by=member_casual, fill=member_casual)) +
  geom_histogram(position="dodge", binwidth=30, color="black") +
  labs(title="Rides by length and user type") +
  scale_x_continuous(name="Ride length in minutes",breaks=seq(30,300,30),labels=c("30","60","90","120","150","180","210","240","270","300"),limits=c(0,300)) +
  scale_y_continuous(name="rides",breaks=c(0,500000,1000000,1500000,2000000), labels=c("0","0.5M","1M","1.5M","2M")) +
  scale_fill_brewer(palette="Set2") +
  theme(legend.position=c(0.75,0.75),
        legend.background = element_rect(fill="transparent"),
        legend.title=element_blank())

##Box plots of ride_length by member_casual. 
timeplot_data <- dat_csv_clean[colnames(dat_csv_clean) %in% c("ride_length","member_casual")]

ggplot(timeplot_data, aes(x=member_casual, y=ride_length/60)) +
  geom_boxplot() +
  scale_y_continuous(name="Ride length in minutes") +
  theme(axis.title.x=element_blank()) +
  labs(title = "All rides")

ggplot(timeplot_data, aes(x=member_casual, y=ride_length/60)) +
  geom_boxplot(outlier.shape=NA) +
  scale_y_continuous(name="Ride length in minutes",
                     limits=c(0,60)) +
  theme(axis.title.x=element_blank()) +
  labs(title = "All rides, outliers hidden")

##heatmap showing ride locations
##commented out because it demands a significant amount of memory to perform, I am able to manually set my working memory to an acceptable level with the next line (memory.limit(size=56000). If you have a lot of RAM or you use a cloud computing service then this shouldn't be an issue. Ensure that you have enough memory to do this before doing so.
##memory.limit(size=56000) ##DO NOT RUN this line unless you know you need it and it is safe to do so with your machine
# chic_area = c(left=-87.7, bottom=41.85, right=-87.55, top=41.975)
# 
# ride_map <- get_stamenmap(bbox=chic_area, zoom=14, maptype="toner-lite")
# 
# ride_map <- ggmap(ride_map, extent="device",legend="none")
# 
# ride_map <- ride_map + stat_density2d(data=dat_csv_clean, aes(x=start_lng, y=start_lat, fill=..level..,alpha=..level..), geom="polygon") +
#   scale_fill_gradientn(colors=rev(brewer.pal(7, "Spectral"))) +
#   guides(size="none", alpha= "none") +
#   ggtitle("Trip start density") +
#   theme_bw() +
#   facet_wrap(~member_casual)
# 
# print(ride_map)

