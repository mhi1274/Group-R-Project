#R group prject code for my part 
# the ticketing rate for instate males versus females
# imported james's csv file containing all instate information 
instatenh <- read.csv("Instate.csv")
 
#next selceted coluemns needed for question: "driver_gender", "violation", and "stop_outcome" 
   nh_male_female<- select(instatenh, driver_gender,violation,stop_outcome)
   
#filtered for any offense containg speeedind and exclude missing values (used "grep")
h_male_female_speed <- nh_male_female[grep("Speeding",nh_male_female$violation),]
nh_male_female_speedinvert <- 
temp <-nh_male_female_speed[grep("Summons",nh_male_female_speed$stop_outcome, invert = TRUE),]

#A new table was created called nh_male_female_inverted to contain the new values 
temp <- filter(nh_male_female_speedinvert,driver_gender=="M" |driver_gender=="F" )
male_v_female <- temp

#bar grpah displaying wanring vs tickets in percent, labeled s1. 
#required "scales" packages 
s1 <- ggplot(male_v_female, aes(x=stop_outcome,fill = stop_outcome))+geom_bar(aes(y = (..count..)/sum(..count..)))+scale_y_continuous(labels = percent)
s1 <- s1 + ylab("Percent")
s1 <- s1 + xlab("Outcome of Stop")
s1 <- s1 +labs(fill = "Outcome")
s1 <- s1 + ggtitle("% of Stop Outcome of Entire Male/Female Population")
s1 <- s1 + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25)

#filtered to habe only males in table and femlaes in another 
male <- filter(nh_male_female_speedinvert,driver_gender=="M")
female <- filter(nh_male_female_speedinvert,driver_gender=="F" )

#male graph 
bm <- ggplot(male, aes(x=stop_outcome,fill = stop_outcome))+geom_bar(aes(y = (..count..)/sum(..count..)))+scale_y_continuous(labels = percent)
bm <- bm + ylab("Percent")
bm <- bm + xlab("Outcome of Stop")
bm <- bm + ggtitle("% Of Male Outcome Result")
bm <- bm + labs(fill = "Outcome")
bm <- bm + scale_fill_manual(values = c("red", "blue"))
bm <- bm + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/su
#female graph
bf <- ggplot(female, aes(x=stop_outcome,fill = stop_outcome))+geom_bar(aes(y = (..count..)/sum(..count..)))+scale_y_continuous(labels = percent)
bf <- bf+ ylab("Percent")
bf <- bf + labs(fill = "Outcome")
bf <- bf + xlab("Outcome of stop")
bf <- bf + xlab("Outcome of stop")
bf <- bf + scale_fill_manual(values = c("red", "blue"))
bf <- bf + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25)

#mitch code

```install.packages("dplyr")

library(dplyr)
library(Hmisc)
library(reshape2)
library(scales)
instate_cops <- read.csv("Instate.csv")


instate_cops_describe <- describe(instate_cops)
#this provides a breakdown of all columns

#counts the total number of each violation
instate_number_of_each_violation <- instate_cops %>%
  group_by(violation) %>%
  summarise(crime_count=n()) %>%
  arrange(desc(crime_count))

#Average rate for all traffic stops
instate_average_age_traffic_stop <- instate_cops%>%
  summarise(mean(driver_age, na.rm = T))

#Average age of all stops for speeding
instate_average_age_speeding_stop <- instate_cops%>%
  filter(violation == 'Speeding')%>%
  summarise(mean(driver_age, na.rm = T))

#average age of all stops that resulted in a ticket
instate_average_age_ticket <- instate_cops%>%
  filter(violation == 'Speeding')%>%
  filter(stop_outcome == "Ticket")%>%
  summarise(mean(driver_age, na.rm = T))

#Average rate of all stops that resulted in a warning
instate_average_age_warning <- instate_cops%>%
  filter(violation == 'Speeding')%>%
  filter(stop_outcome == "Warning")%>%
  summarise(mean(driver_age, na.rm = T))


#groups together those who are 38 and younger and also were pulled over for just speeding
instate_speeding_below_age_38 <- (instate_cops)%>%
  filter(violation == "Speeding")%>%
  filter(driver_age <= 38)%>%
  arrange(driver_age)


#groups together those who are 39 and older and also were pulled over for just speeding
instate_speeding_above_age_38 <- instate_cops%>%
  filter(violation == "Speeding")%>%
  filter(driver_age >38)%>%
  arrange(driver_age)

#total number of speeding tickets
instate_number_of_tickets_total <- instate_cops%>%
  filter(violation == "Speeding")%>%
  filter(stop_outcome == "Ticket")%>%
  count(violation)

#total number of warnings
instate_number_of_warnings_total <- instate_cops%>%
  filter(violation == "Speeding")%>%
  filter(stop_outcome == "Warning")%>%
  count(violation)```

part 2
```#total number of tickets given to those who were younger than 39
instate_number_of_tickets_38_lower <- instate_speeding_below_age_38%>%
  filter(stop_outcome == "Ticket")%>%
  count(stop_outcome)

#toatal number of tickets given to those who were 39 and older
instate_number_of_tickets_38_higher <- instate_speeding_above_age_38%>%
  filter(stop_outcome == "Ticket")%>%
  count(stop_outcome)  

#average age of those who are 39 and older
instate_average_age_above_38 <- instate_speeding_above_age_38%>%
  summarise(mean(driver_age))

#average age of those who are 38 and younger
instate_average_age_below_38 <- instate_speeding_below_age_38%>%
  summarise(mean(driver_age))

#average age of those who received tickets in the age group 38 and lower
instate_average_age_of_tickets_38_lower <- instate_speeding_below_age_38%>%
  filter(stop_outcome == "Ticket")%>%
  summarise(mean(driver_age))


#average age of those who received tickets in the age group 39 and higher
instate_average_age_of_tickets_38_upper <- instate_speeding_above_age_38%>%
  filter(stop_outcome == "Ticket")%>%
  summarise(mean(driver_age))

#average age of those who received warnings in the age group 39 and higher
instate_average_age_of_warning_38_upper <- instate_speeding_above_age_38%>%
  filter(stop_outcome == "Warning")%>%
  summarise(mean(driver_age))

#average age of those who received warnings in the age group 38 and lower
instate_average_age_of_warning_38_lower <- instate_speeding_below_age_38%>%
  filter(stop_outcome == "Warning")%>%
  summarise(mean(driver_age))



#Here we assign the 6 different discrete variables with the six continuous variables.

averageaggespeeding1 <- c("1. T 15-39", "1. W 15-39", "2. T All", "2. W All", "3. T 39+", "3. W 39+")
averageaggespeeding1
speedingnumbers <- c( 25.5, 26.6, 33.8, 39.8, 51.1, 53.3)
speedingnumbers
average_age_dataframe <- data.frame(averageaggespeeding1,speedingnumbers)
average_age_dataframe


# Plot the 6X6 graph
bar <- ggplot(average_age_dataframe, aes(averageaggespeeding1, speedingnumbers)) +
  geom_col(col=c("red"), fill =c("blue"))
bar <- bar + ylab("Age of Driver")
bar <- bar + xlab("Outcome of Stop")
bar <- bar + ggtitle("Average Age of Tickets and Warnings")

bar```


