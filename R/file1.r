# Load required libraries
library(ggplot2)
library(dplyr)
library(janitor)

# Read the dataset
data <- read.csv("C:\\Users\\acer\\Downloads\\road.csv")

# Clean column names to make them easier to work with
data <- data %>% clean_names()

# Check the cleaned column names
print(colnames(data))

# Remove the "Total" row for state-wise analysis
data_states <- data[data$state_ut != "Total", ]
total_row <- data[data$state_ut == "Total", ]

# Create the top 10 accidents chart with cleaned names
top_accidents_2014 <- data_states %>%
  arrange(desc(fine_total_acc_2014)) %>%
  head(10)

# 3. Pie chart of fatalities distribution among top 5 states
top_fatalities <- data_states %>%
  arrange(desc(Fine_Persons_Killed_2014)) %>%
  head(5)

# Include total for comparison
fatalities_pie_data <- data.frame(
  State = c("Total", top_fatalities$State_UT),
  Deaths = c(total_row$Fine_Persons_Killed_2014, top_fatalities$Fine_Persons_Killed_2014)
)

p3 <- ggplot(fatalities_pie_data, aes(x = "", y = Deaths, fill = State)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Fatalities in Fine Weather (2014)") +
  theme_void() +
  theme(legend.position = "right") +
  scale_fill_brewer(type = "qual", palette = "Set3")

print(p3)

# 4. Weather condition distribution pie chart
p4 <- ggplot(weather_comparison, aes(x = "", y = Accidents_2014, fill = Weather)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Accidents by Weather Condition (2014)") +
  theme_void() +
  theme(legend.position = "right") +
  scale_fill_brewer(type = "qual", palette = "Set2")

print(p4)

# 5. Scatter plot: Accidents vs Injuries in fine weather
p5 <- ggplot(data_states, aes(x = Fine_Total_Acc_2014, y = Fine_Persons_Injured_2014)) +
  geom_point(alpha = 0.7, size = 3, color = "darkblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Relationship between Accidents and Injuries in Fine Weather (2014)",
       x = "Total Accidents", y = "Persons Injured") +
  theme_minimal() +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)

print(p5)

# 6. Scatter plot: Accidents vs Fatalities
p6 <- ggplot(data_states, aes(x = Fine_Total_Acc_2014, y = Fine_Persons_Killed_2014)) +
  geom_point(alpha = 0.7, size = 3, color = "darkred") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Relationship between Accidents and Fatalities in Fine Weather (2014)",
       x = "Total Accidents", y = "Persons Killed") +
  theme_minimal() +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)

print(p6)


# 7. Box plot of accidents across different weather conditions
# Reshape data for box plot
weather_data <- data_states %>%
  select(State_UT, Fine_Total_Acc_2014, Mist_fog_Total_Acc_2014, 
         Cloudy_Total_Acc_2014, Light_rain_Total_Acc_2014, Heavy_rain_Total_Acc_2014) %>%
  tidyr::gather(key = "Weather_Condition", value = "Accidents", -State_UT) %>%
  mutate(Weather_Condition = gsub("_Total_Acc_2014", "", Weather_Condition),
         Weather_Condition = gsub("_", "/", Weather_Condition))

p7 <- ggplot(weather_data, aes(x = Weather_Condition, y = Accidents)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  labs(title = "Distribution of Accidents by Weather Condition (2014)",
       x = "Weather Condition", y = "Number of Accidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = comma)

print(p7)

# 8. Box plot of fatality rates by weather condition
fatality_data <- data_states %>%
  select(State_UT, Fine_Persons_Killed_2014, Mist_fog_Persons_Killed_2014, 
         Cloudy_Persons_Killed_2014, Light_rain_Persons_Killed_2014, Heavy_rain_Persons_Killed_2014) %>%
  tidyr::gather(key = "Weather_Condition", value = "Fatalities", -State_UT) %>%
  mutate(Weather_Condition = gsub("_Persons_Killed_2014", "", Weather_Condition),
         Weather_Condition = gsub("_", "/", Weather_Condition))

p8 <- ggplot(fatality_data, aes(x = Weather_Condition, y = Fine - Persons Killed - 2014 )) +
  geom_boxplot(fill = "salmon", alpha = 0.7) +
  labs(title = "Distribution of Fatalities by Weather Condition (2014)",
       x = "Weather Condition", y = "Fine - Persons Killed - 2014") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = comma)

print(p8)
# 9. Heatmap of accidents by state and weather condition
library(reshape2)

heatmap_data <- data_states %>%
  select(State_UT, Fine_Total_Acc_2014, Mist_fog_Total_Acc_2014, 
         Cloudy_Total_Acc_2014, Light_rain_Total_Acc_2014, Heavy_rain_Total_Acc_2014) %>%
  column_to_rownames("State_UT")

colnames(heatmap_data) <- c("Fine", "Mist/Fog", "Cloudy", "Light Rain", "Heavy Rain")

heatmap_melted <- melt(as.matrix(heatmap_data))

p9 <- ggplot(heatmap_melted, aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red", name = "Accidents") +
  labs(title = "Heatmap of Accidents by State and Weather Condition (2014)",
       x = "Weather Condition", y = "State/UT") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p9)

# 10. Multi-panel comparison plot
library(gridExtra)

# Create a combined visualization
grid.arrange(p1, p5, p7, p3, ncol = 2, nrow = 2,
             top = "Road Accident Analysis Dashboard - 2014")

# 11. Time series comparison (2014 vs 2016)
comparison_data <- data_states %>%
  select(State_UT, Fine_Total_Acc_2014, Fine_Clear_Total_Accidents_2016) %>%
  filter(!is.na(Fine_Clear_Total_Accidents_2016)) %>%
  arrange(desc(Fine_Total_Acc_2014)) %>%
  head(10) %>%
  tidyr::gather(key = "Year", value = "Accidents", -State_UT) %>%
  mutate(Year = ifelse(grepl("2014", Year), "2014", "2016"))

p11 <- ggplot(comparison_data, aes(x = State_UT, y = Accidents, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Fine Weather Accidents: 2014 vs 2016",
       x = "State/UT", y = "Number of Accidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("2014" = "steelblue", "2016" = "orange"))

print(p11)

