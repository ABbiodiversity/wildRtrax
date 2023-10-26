# Load necessary libraries
library(tidyverse)

# Step 2: Data Preparation
# Read your data into a dataframe (assuming your data has columns 'Location', 'Date', 'Signal', and 'Noise')
data <- read.csv("your_data.csv")

# Step 3: Calculate SNR
data <- data %>%
  mutate(SNR_dB = 10 * log10(Signal / Noise))

# Step 4: Group by Location
grouped_data <- data %>%
  group_by(Location)

# Step 5: Calibration
midnight_calibrations <- grouped_data %>%
  filter(is_midnight_recording) %>%
  summarize(avg_SNR_dB = mean(SNR_dB))

# Step 6: SNR Trend Analysis
location_trends <- grouped_data %>%
  mutate(Time = as.Date(Date) - min(as.Date(Date))) %>%
  lm(SNR_dB ~ Time, data = .)

# Step 7: Prioritize Locations
location_trends %>%
  summarize(Location, SNR_Trend_Coefficient = coef(location_trends)[2]) %>%
  arrange(desc(SNR_Trend_Coefficient))
