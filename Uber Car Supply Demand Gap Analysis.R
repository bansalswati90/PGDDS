#Loading required libraries
library(stringr)
library(ggplot2)
library(scales)

#Loading the Uber dataset
raw.uber <-  read.csv("Uber Request Data.csv", header = T, stringsAsFactors = FALSE)
uber <- raw.uber

#Structure of Uber
str(uber)

#Data Cleaning

#Checking whether the requests are all unique
length(unique(uber$Request.id))
#6745 , which indicates all requests are unique

sum(duplicated(uber$Request.id))
#No duplicates

#Checking NA/missing values
sum(is.na(uber))
#6564
#This NA values corresponds to Drop timestamp when the cab has been "Cancelled" which is 1264 and
#when "No Cars Available", the driver id and Drop timestamp is NA (2650+2650) , therefore total is 6564.
#We need not remove this NA value as it is part of Requests which are not completed.

#Checking blank values
sapply(uber,function(x) length(which(x=="")))
#No blank values

#summary of uber
summary(uber)
#No uppercase , lowercase problem

#Dates are present in different formats, need to bring dates in standard format of "YYYY-mm-dd HH:MM:SS" format

#First to bring in same format we append 00 for seconds for rows where / is present
uber$Request.timestamp <-
  ifelse(
    str_detect(uber$Request.timestamp, "\\/"),
    paste(uber$Request.timestamp, ":00", sep = ""),
    uber$Request.timestamp)

uber$Drop.timestamp <-
  ifelse(
    str_detect(uber$Drop.timestamp, "\\/"),
    paste(uber$Drop.timestamp, ":00" , sep = ""),
    uber$Drop.timestamp)


#Replacing all / to - , so as to bring all dates in same format
uber$Request.timestamp <- str_replace_all(uber$Request.timestamp, "\\/", "-")
uber$Drop.timestamp <- str_replace_all(uber$Drop.timestamp, "\\/", "-")

#Now using as.POSIXct to bring all dates in standard format
uber$Request.timestamp <- as.POSIXct(uber$Request.timestamp, format = "%d-%m-%Y %H:%M:%S")
uber$Drop.timestamp <- as.POSIXct(uber$Drop.timestamp, format = "%d-%m-%Y %H:%M:%S")

#Deriving new variables: Extracting date
uber$Request.date <- as.Date(uber$Request.timestamp)

#Deriving column for Request hour
uber$Request.hour <- format(uber$Request.timestamp, "%H")

#Deriving new variables: Extracting date
uber$Drop.date<-as.Date(uber$Drop.timestamp)

#Deriving column for Drop hour
uber$Drop.hour <- format(uber$Drop.timestamp, "%H")
 
#The data consists of requests for five days in the month of July 2016 only, hence not deriving columns for those variable

#Deriving total time taken by the trip
uber$Time_taken <- difftime(uber$Drop.timestamp, uber$Request.timestamp)

#Converting to numeric
uber$Request.hour <- as.numeric(uber$Request.hour)
uber$Drop.hour <- as.numeric(uber$Drop.hour)
uber$Time_taken <- round(as.numeric(uber$Time_taken), 2)

#To make the analysis effective, request time has been divided in the 5 time slots as per our assumptions
#Identifying the time slots
Uber_time_slot <- function(x) {
  ifelse (x <= 4,
          "Early Morning",
          ifelse(
            x >= 5 &
              x <= 9,
            "Morning Rush",
            ifelse (
              x >= 10 &
                x <= 16,
              "Afternoon",
              ifelse (x >= 17 & x <= 21, "Evening Rush", "Late Night")
            )
          ))
}

#Creating Time slots
uber$Time.slot <- sapply(uber$Request.hour,Uber_time_slot)

#Converting it into factor with levels defined as per time slots
uber$Time.slot <- factor(uber$Time.slot , levels=c("Early Morning","Morning Rush","Afternoon","Evening Rush","Late Night"))

#To find the ride status is completed or not we create a derived column
uber$Status.Completed <- ifelse(uber$Status=="Trip Completed" ,"Completed","Not Completed")

#writing the clean data for visualisation in Tableau
write.csv(uber,"uber_test.csv",row.names = F)

####Plotting Graphs####

#Using ggplot for plotting graphs , geom_text to add text to the graphs, labs to add labels for x axis ,y axis , title of graph
#ggtitle to add title and subtitle to the plot , guide to add appropriate labels to legends, theme_bw() to add a black and white background theme

#Count of airport and city pickups
#Univariate analysis: using histogram to plot frequency Pickup Point on x axis and frequency of Requests on Y axis
plot_airport_city <- ggplot(uber,aes(x=Pickup.point , fill = Pickup.point)) + 
                            geom_histogram(stat="count", bins = length(unique(uber$Pickup.point))) + 
                            geom_text(stat='count',aes(label=..count..),vjust=-0.25) + 
                            theme_bw() + 
                            labs(x="Pickup Points",y="Frequency of Requests") +
                            ggtitle("Pickup Point Wise - Frequency of Request" , subtitle = "Histogram for Airport and City Pickup" )
plot_airport_city

table(uber$Pickup.point)
#Airport requests : 3238
#City requests : 3507

####1.Visually identify the most pressing problems for Uber####

#Uber Trip Statuswise Frequency

#Univariate analysis:For showing Frequency , using histogram having status on x-axis and Frequency of Requests on y axis
plot_statuswise_frequency <- ggplot(uber, aes(x = Status ,fill=Status)) + 
                                    geom_histogram(stat="count", bins = length(unique(uber$Status))) + 
                                    ggtitle("Status Wise - Frequency of Request" , subtitle = "Histogram for Trip Status") + 
                                    labs(x = "Status", y ="Frequency of Requests") + 
                                    geom_text(stat = 'count', aes(label = ..count..), vjust=-0.5) + 
                                    theme_bw()
plot_statuswise_frequency

table(uber$Status)

#This shows out of 6745 requests made for five days in the month of July 2016 has 
#1264 Cancellations
#2650 No Cars Available
#2831 Trip Completed
#This shows only 41.97% of total Requests are completed

#Identify the most problematic types of requests (city to airport / airport to city etc.)

#Segmented Univariate Analysis:Using Stacked bar-chart , where each bar represents Status and y axis shows Frequency of Requests
#Different proportions of bars represent the pickup point (airport or city).
plot_statuspickup_count <- ggplot(uber, aes(x = Status, fill = Pickup.point)) + 
                                  geom_bar() + 
                                  ggtitle("Statuswise-Pickup Point - Frequency of Request" , subtitle = "Request fulfillment for City and Airport Pickup") + 
                                  labs(x = "Status", y ="Number of Requests") + 
                                  geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust = 0.5)) + 
                                  guides(fill=guide_legend("Pickup Point")) +
                                  theme_bw()
plot_statuspickup_count

#This shows for the city to airport requests : Cancellation is a problem 
#For airport to city : No Cars Available is a problem.


#Requests per hour wise for different Status

#Segmented Univariate Analysis:Using Stacked bar-chart , where each bar represents Request Hour and y axis shows Frequency of Requests
#Different proportions of bars represent the pickup point (airport or city).

plot_overall_requests_perhour <- ggplot(uber,aes(x=Request.hour , fill=Pickup.point)) + 
                                        geom_bar(aes(y = ..count..)) + 
                                        geom_text(stat = 'count', aes(y = ..count.. , label = ..count..),position = position_stack(vjust = 0.5)) +
                                        guides(fill=guide_legend("Pickup Point")) +                                     
                                        theme_bw() +
                                        labs(x="Request Hour",y="Count of Requests") + 
                                        ggtitle("Requests per hour Pickup pointwise", subtitle = "Hourly ditribution of requests for the airport and city pickup")
plot_overall_requests_perhour
#The above plot depicts the following:
#The number of pickup requests are higher in the city during morning hours.
#The number of pickup requests are higher at the airport during evening hours.


####Requests per time slots

#Segmented Univariate Analysis:Using Stacked bar-chart , where each bar represents Time slots and y axis shows Frequency of Requests
#Different proportions of bars represent the pickup point (airport or city).

plot_requests_pertimeslots_pickupwise <- ggplot(uber,aes(x=Time.slot , fill=Pickup.point)) + 
                                            geom_bar(aes(y = ..count..)) + 
                                            geom_text(stat = 'count', aes(y = ..count.. , label = ..count..),position = position_stack(vjust = 0.5)) +
                                            guides(fill=guide_legend("Pickup Point")) +                                     
                                            theme_bw() +
                                            labs(x="Time Slot",y="Count of Requests") + 
                                            ggtitle("Requests per slot Pickup pointwise", subtitle = "Time slot wise ditribution of requests for the airport and city pickup")
plot_requests_pertimeslots_pickupwise

#The above plot depicts the following:
#The number of pickup requests are higher in the city during Morning Rush slot.
#The number of pickup requests are higher at the airport during Eevening Rush slot.


#Segmented Univariate Analysis:Using grouped bar-chart , where each bar represents Time Slots and y axis shows Frequency of Requests
#Different bars of grouped bars represent the Status(Cancelled, No Cars Available, Trip Completed).

plot_requests_pertimeslots_statuswise <- ggplot(uber,aes(x=Time.slot , fill=Status)) + 
                                            geom_bar(aes(y=..count..),position = "dodge") + 
                                            geom_text(stat = 'count', aes(y=..count.. , label = ..count..),  position = position_dodge(width = 1) , vjust = -0.25) +
                                            theme_bw() +
                                            labs(x="Request slots",y="Number of requests") + 
                                            ggtitle("Requests per time slots", subtitle = "Early Morning :  00:00Hr To  04:59Hr\nMorning Rush : 05:00Hr To  09:59Hr\nAfternoon : 10:00Hr To 16:59Hr\nEvening Rush : 17:00Hr  To 20:59Hr\nLate Night : 21:00Hr To 23:59Hr")
plot_requests_pertimeslots_statuswise

#This indicates that during the Morning Rush hour (from 5am to 9am) the number of cancellations are high
#During the Evening Rush time slot i.e. from 5pm to 9pm the number of No cars available are higher

####Plots for Cancellations####

#Subsetting the data to show only cancellations
cancellations_overall <- subset(uber, uber$Status == "Cancelled")

#Cancellations per time slots for pickups

#Segmented Univariate Analysis:Using Stacked bar-chart , where each bar represents Time slot and y axis shows Frequency of Requests Cancelled
#Different proportions of bars represent the pickup point (airport or city).
plot_cancellations_pertimeslots <- ggplot(cancellations_overall,aes(x=Time.slot,fill=Pickup.point)) + 
                                          geom_bar() + 
                                          geom_text(stat = 'count', aes(label = ..count..),position = position_stack(vjust=0.5)) +
                                          guides(fill=guide_legend("Pickup Point")) +
                                          theme_bw() + 
                                          labs(x="Time slots",y="Number of Requests Cancelled",title="Cancellations per time slots for pickups")

plot_cancellations_pertimeslots

####Plots for No Cabs Available####

#Subsetting the data to show only No Cars Available
Nocabs_overall <- subset(uber, uber$Status == "No Cars Available")

#Requests per time slots
#Segmented Univariate Analysis:Using Stacked bar-chart , where each bar represents Time Slot and y axis shows Frequency of Requests for No cars Available
#Different proportions of bars represent the pickup point (airport or city).

plot_nocabs_pertimeslots <- ggplot(Nocabs_overall,aes(x=Time.slot,fill=Pickup.point)) + 
                                  geom_bar() + 
                                  geom_text(stat = 'count', aes(label = ..count..),position = position_stack(vjust=0.5)) +
                                  guides(fill=guide_legend("Pickup Point")) +
                                  theme_bw() + 
                                  labs(x="Time slots",y="Number of Requests for No Cars Available",title="No Cars Available per time slots for pickups")

plot_nocabs_pertimeslots


#From the above plots we clearly see that:
#For city pickup during the Morning Rush time slot that is from 5am to 9am the number of cancellations are the highest 
#For airport pickup during the Evening Rush time slot i.e. from 5pm to 9pm the incidents of No cars available are the highest. 

####2. Find out the gap between supply and demand and show the same using plots ####

#Find the time slots when the highest gap exists

#Supply is the number of trips completed and Demand is the number of trip requests made for pickups
#Therefore Supply Demand Gap = (Total number of Requests made) - (Total Trip completed)
#So, considering only the unfulfilled requests that is Cancelled and No cars Available, which is when status.completed is Not Completed
uber_gap <- subset(uber, Status.Completed=="Not Completed")

#Plotting graphs for supply demand gap

#Segmented Univariate Analysis:For time series data using line chart, x axis represents the hour at which the request is made, y axis represents the Number of requests
#Demand which is the requests made is shown in blue colour, and supply ( number of requests completed) is shown in green colour , The supply demand gap is shown in red colour)
plot_overall_supply_demand_gap <- ggplot(uber,aes(x=Request.hour)) + 
                                          geom_line (stat = 'count',colour="blue4")+
                                          geom_line (stat = 'count', aes(colour=Status.Completed))+
                                          scale_colour_manual(values=c("green","red3"),name = "Status")+
                                          geom_text(aes(x = 8, y = 450, label = "Demand")) +
                                          geom_text(aes(x = 8, y = 290, label = "Gap")) +
                                          geom_text(aes(x = 8, y = 190, label = "Supply")) +
                                          theme_bw() + 
                                          labs(x="Request hours",y="Number of Requests") +
                                          ggtitle("Supply Demand Gap" , subtitle = "Demand = Total Requests\nSupply = Total Trips Completed\nGap = Total Requests Cancelled + No Cars Available")
plot_overall_supply_demand_gap

#Find the time slots when the highest gap exists
#Plotting using uber_gap subset
#Univariate Analysis:Using bar-chart , where each bar represents Time slot and y axis shows Number of Requests Not Completed
plot_supply_demand_gap <- ggplot(uber_gap,aes(x=Time.slot , fill = Time.slot)) + 
                                geom_bar() + 
                                geom_text(stat = 'count', aes(label = ..count..), vjust=-0.25) +
                                theme_bw() + 
                                labs(x="Time slots",y="Number of Requests Not Completed",title="Supply Demand Gap for Time Slots")
plot_supply_demand_gap

#Answer: Above plot clearly shows that the highest supply demand gap exists for :
#Evening Rush slot (from 5pm to 9pm) Total gap during Evening Rush slot is 1558
#Morning Rush slot (from 5am to 9am) Total gap during Morning Rush slot is 1249


#Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots

#Segmented Univariate Analysis:Using Grouped bar-chart , where each bar represents Time slot and y axis shows Number of Requests Not Completed
#Individual bars of grouped bars represent the pickup point (airport or city).
plot_supply_demand_pickup_gap <- ggplot(uber_gap,aes(x=Time.slot , fill=Pickup.point)) + 
                                        geom_bar(aes(y=..count..),position = "dodge") + 
                                        geom_text(stat = 'count', aes(y=..count.. , label = ..count..),  position = position_dodge(width = 1) , vjust = -0.25) +
                                        guides(fill=guide_legend("Pickup Point")) + 
                                        theme_bw() + 
                                        labs(x="Time slots",y="Number of Requests Not Completed") + 
                                        ggtitle("Supply Demand Gap", subtitle = "Time Slots wise - Requests Not Completed  for Airport and City pickup")
plot_supply_demand_pickup_gap

#Answer: Above plot clearly shows that the highest supply demand gap for pickup exists for:
#Airport to city : Evening Rush slot (from 5pm to 9pm) :  1427
#City to Airport : Morning Rush slot (from 5am to 9am) :  1205

####3. What do you think is the reason for this issue for the supply-demand gap? Write the answer in less than 100 words. You may accompany the write-up with plot(s).####

####Answer: 

#Segmented Univariate Analysis:Using Stacked bar-chart , where each bar represents Time slot and y axis shows Percentage Requests Cancelled
#Different proportions of bars represent the pickup point (airport or city).

plot_pickup_cancellation_percentage <- ggplot(cancellations_overall,aes(x=Time.slot,fill=Pickup.point)) + 
                                              geom_bar(aes(y = ((..count..)/sum(..count..)))) + 
                                              geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))),stat = "count",position = position_stack(vjust=0.5) , vjust = 0.25) +
                                              scale_y_continuous(labels=percent) +
                                              theme_bw() + 
                                              labs(x="Time slots",y="Percentage Requests Cancelled") + 
                                              ggtitle("Percentage Cancellations", subtitle = "Percentage Request Cancellation per time slots for city and airport pickups")
plot_pickup_cancellation_percentage

#Segmented Univariate Analysis:Using Stacked bar-chart , where each bar represents Time slot and y axis shows Percentage Requests for No Cars Available
#Different proportions of bars represent the pickup point (airport or city).
plot_pickup_nocars_percentage <- ggplot(Nocabs_overall,aes(x=Time.slot,fill=Pickup.point)) + 
                                        geom_bar(aes(y = ((..count..)/sum(..count..)))) + 
                                        geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))),stat = "count",position = position_stack(vjust=0.5) , vjust = 0.25) +
                                        scale_y_continuous(labels=percent) +
                                        theme_bw() + 
                                        labs(x="Time slots",y="Percentage Requests for No Cars Available") + 
                                        ggtitle("Percentage No Cars Available Requests" , subtitle = "Percentage Requests for 'No Cars Available per time slots for airport and city pickups")
plot_pickup_nocars_percentage

#A.For City pickups, supply demand mismatch is the highest during morning rush hours.
plot_pickup_cancellation_percentage 
#clearly indicates that 64.9% out of total cancellations for the city pickup happen during Morning Rush hour (from 5am to 9am) while during other time slots these percentage is not more that 10%.
#During this time slot, the cancellations are contributing most to the shortage. This suggests the unwillingness of the drivers to go to airport during morning hours. The possible reason may be
#1. Long distance of the airport from the city pickup area
#2. Long waiting hours at the airport for the return trip, since the the airport pickup at Morning_Rush and Afternoon slots are not very high
#3. Traffic congestion 
max(uber$Time_taken,na.rm = T)
#Maximum time taken for completed trip is 83 Minutes
min(uber$Time_taken,na.rm = T)
#Minimum time taken for completed trip is 20.78 Minutes
mean(uber$Time_taken,na.rm=T)
#52.41378 Minutes
#This shows that the average time taken for completed trip is around 52 Minutes, which implies only short distance trips/or trips which takes around 1 hour are completed and may be long distance ones are cancelled

#B.For Airport pickups, supply demand mismatch is the highest during evening rush hours.
#During this time slot, the non availability  of cars is the main factor for the shortage.
plot_pickup_nocars_percentage 
#clearly indicates that 49.8% out of total unavailability of cars, for airport pickup incidents happen most during Evening Rush hour (from 5pm to 9pm) while during other time slots these percentage is not more that 15%.
#The possible reason for cars not being available at the airport during evening rush hours may be
#1. From city not many cars reach the airport at the Evening Rush slot
#2. by the later hours of the evening rush time slot the drivers may be off duty or may have completed their targets for the day

#### 4. Recommend some ways to resolve the supply-demand gap.####
# My recommendation to Uber is:
#1. In-order to solve "No Cabs Availability" issue, analyze from where the requests are high and make sure the cabs  are available at that location depending upon the rush times. 
#2. Give discount offers to customers during non-peak time for airport drops so that the cars availability problem is overcome during rush time
#3. In order to avoid frequent "Cancellations" , check the possible shortest route prior to trip start to avoid traffic and update the traffic location on real time basis to get the shortest and fastest route available , so the overall trip time reduces.
#4. Give certain incentives to drivers so that they are encouraged to take the  trips more often during the rush hours, thereby reducing the number of cancellations and No Cars Available. 
#5. Drivers should be provided the option to work in rotational shifts , so that the cabs are made available round the clock.
#6. Penalize the driver / customer in case of request cancellation without appropriate reasons.
