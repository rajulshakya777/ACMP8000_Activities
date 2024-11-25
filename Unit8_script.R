# Load necessary libraries
library(dplyr)
library(readr)
#install.packages("tidyverse")
library(tidyverse)

# Load the dataset
migration_data <- read_csv("./Desktop/IT\ Innovation/Assignments/Doc/Activity/Data\ analysis/5/incoming_and_outgoing_migrants_counts.csv")

# Step 6 : Data summary
data <- migration_data_cleaned

#library(tidyverse)

# Load the dataset
# Assuming the dataset is saved as "migrants.csv" in the working directory
#data <- read_csv("incoming_and_outgoing_migrants_counts.csv")

# Step 1: Filter and Aggregate Data
# Summarize total incoming and outgoing counts by year
yearly_trends <- data %>%
  group_by(Year, Direction) %>%
  summarise(Total_Count = sum(Count, na.rm = TRUE)) %>%
  pivot_wider(names_from = Direction, values_from = Total_Count)

# Replace NA with 0 if any missing values appear
yearly_trends[is.na(yearly_trends)] <- 0

# Step 2: Create Line Plot for Trends
ggplot(yearly_trends, aes(x = Year)) +
  geom_line(aes(y = Incoming, color = "Incoming"), size = 1.2) +
  geom_line(aes(y = Outgoing, color = "Outgoing"), size = 1.2) +
  labs(title = "Migration Trends Over Time",
       x = "Year", y = "Number of Migrants",
       color = "Direction") +
  scale_color_manual(values = c("Incoming" = "blue", "Outgoing" = "red")) +
  theme_minimal()

# Step 3: Regional Analysis
# Summarize total migration counts by region
regional_summary <- data %>%
  group_by(Region, Direction) %>%
  summarise(Total_Count = sum(Count, na.rm = TRUE)) %>%
  pivot_wider(names_from = Direction, values_from = Total_Count)

# Replace NA with 0
regional_summary[is.na(regional_summary)] <- 0

# Bar Plot for Regional Incoming Migration
ggplot(regional_summary, aes(x = reorder(Region, -Incoming), y = Incoming)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Total Incoming Migration by Region",
       x = "Region", y = "Number of Migrants") +
  theme_minimal()

# Step 4: Outlier Analysis
# Boxplot for Incoming Migration by Region
ggplot(data, aes(x = Region, y = Count, fill = Direction)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Variation in Migration by Region",
       x = "Region", y = "Number of Migrants",
       fill = "Direction") +
  theme_minimal()

