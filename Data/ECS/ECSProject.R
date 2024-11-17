# Load necessary libraries
library(ggplot2)

# Load data
data <- read.csv(file =  here(".Data/ECS/SydneyTmax.csv")) # Replace with your data file
data$date <- as.Date(data$date)
data$year <- format(data$date, "%Y")

# Calculate yearly average
yearly_avg <- aggregate(max_temp ~ year, data = data, FUN = mean)

# Linear regression model
model <- lm(max_temp ~ year, data = yearly_avg)
summary(model)

# Plot the trend
ggplot(yearly_avg, aes(x = as.numeric(year), y = max_temp)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Yearly Average Maximum Temperature with Trendline",
       x = "Year", y = "Temperature (°C)") +
  theme_minimal()