library(readxl)
data <- read_excel("C:/Users/gaurav/Downloads/nigerian_transport_and_logistics_vehicle_telemetry 1.xlsx")
View(data)

head(data)
str(data)
summary(data)

#Data Cleaning
colSums(is.na(data))
data <- unique(data)
data$timestamp <- as.POSIXct(data$timestamp)

#Exploratory Data Analysis
mean(data$speed_kmh)
median(data$speed_kmh)
max(data$speed_kmh)
mean(data$fuel_rate_lph)

#Analyze Vehicle Types
table(data$vehicle_type)
library(ggplot2)

ggplot(data,aes(vehicle_type))+
  geom_bar()

#Fuel Efficiency Analysis
aggregate(fuel_rate_lph ~ vehicle_type,
          data=data,
          mean)
ggplot(data,
       aes(vehicle_type,
           fuel_rate_lph))+
  stat_summary(fun=mean,
               geom="bar")

#Urban vs Rural Analysis
aggregate(speed_kmh ~ road_class,
          data=data,
          mean)
aggregate(fuel_rate_lph ~ road_class,
          data=data,
          mean)

#Harsh Braking Analysis
table(data$harsh_brake)
table(data$vehicle_type,
      data$harsh_brake)
ggplot(data,
       aes(vehicle_type,
           fill=harsh_brake))+
  geom_bar()
#Correlation Analysis
cor(data[,c("speed_kmh",
            "rpm",
            "fuel_rate_lph",
            "engine_temp_c")])
library(corrplot)

corrplot(cor(data[,c("speed_kmh",
                     "rpm",
                     "fuel_rate_lph",
                     "engine_temp_c")]))

#Fuel Consumption Prediction
model <- lm(fuel_rate_lph ~ speed_kmh +
              rpm +
              throttle_pct +
              engine_temp_c,
            data=data)

summary(model)
#Dashboard Visualizations
ggplot(data,
       aes(speed_kmh,
           fuel_rate_lph))+
  geom_point(alpha=0.3)+
  geom_smooth(method="lm")
