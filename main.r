# ==========================================================
# Project: BUS336 Hackathon
# Purpose: Data cleaning, exploration, and preparation for Tableau
# Author: Steven Duong, Darian Sidhu, Brady Van Unen
# Date: Oct 29, 2025
# ==========================================================

# ---------------------------------------------------------------------------
# Bioretention Condition Assessment (BCA) Analysis - 2022 vs 2024
# ---------------------------------------------------------------------------
# Purpose:
# This code analyzes the condition of bioretention sites over time (2022 to 2024)
# to understand how stewardship programs (No Stewardship, Seeding Stewardship, Green Streets)
# affect site performance. Goal is to identify trends, changes in condition scores,
# and which specific features (like Vegetation, Soil, Inlet/Outlet) contribute to
# improvements or declines in site condition.
#
# Workflow Overview:
# 1. Load necessary libraries for data manipulation, visualization, and regression.
# 2. Import the 2022 and 2024 BCA Excel datasets and inspect them.
# 3. Clean the datasets:
#    - Remove demolished/missing sites.
#    - Convert stewardship to a categorical variable.
#    - Convert relavent columns to numeric.
# 4. Align 2022 and 2024 data by common GRI ID to allow year-to-year comparison.
# 5. Combine datasets into one for analysis and create a year and stewardship column for 2022 data.
# 6.1 Exploratory Data Analysis (EDA):
#    - Correlation analysis between numeric variables in 2024.
#    - Linear regression to check the effect of stewardship on condition scores in 2024 (Cross-sectional regression)
#    - Compute the change in condition score (2024 - 2022) and classify sites as Improved, Worsened, or No Change.
#    - Estimate future condition scores for 2026 by extrapolating the observed changes from 2022 to 2024.
#      -This is a simple, informal forecast and not a formal predictive model.
#    - Summarize change metrics (average, median) by stewardship type.
# 6.2 Feature-level analysis:
#    - Calculate numeric feature changes between 2022 and 2024 for each site.
#    - Pivot longer to tidy format and compute average/median changes by stewardship type.
#    - Identify which features contributed most to improvements or declines in condition scores.
# 7. Export cleaned datasets and summary tables for Tableau visualization and further analysis.
#
# Expected Insights:
# - Determine whether stewardship programs improve overall condition scores.
# - Identify which features of the sites (e.g., Vegetation Score, Soil Score, Planting Bed, CDA Score)
#   drive changes in site performance.
# - Quantify differences between stewardship types and highlight areas for management focus.
# ---------------------------------------------------------------------------

# ---------------------------
# 1. Load libraries
# ---------------------------
library(tidyverse)
library(lubridate)
library(ggplot2)
library(GGally)
library(caTools)
library(MASS) # If using select make sure to specify from dplyr by using dplyr::select()
library(readxl)
library(dplyr)


# ---------------------------
# 2. Load Data
# ---------------------------
# Use excel_sheets("2024 Bioretention Condition Assessment_City Studio Data.xlsx") to see existing sheets
# Seperate sheets into different data frames (currently as a tibble)

BCA_2024 <- read_excel("2024 Bioretention Condition Assessment_City Studio Data.xlsx", sheet = "24CSTable")
BCA_2022 <- read_excel("2024 Bioretention Condition Assessment_City Studio Data.xlsx", sheet = "22CSTable")


# ---------------------------
# 3. Inspect data
# ---------------------------
glimpse(BCA_2024)
summary(BCA_2024)
head(BCA_2024)

glimpse(BCA_2022)
summary(BCA_2022)
head(BCA_2022)

# ---------------------------
# 4. Clean Data
# ---------------------------

# 2024 Data
BCA_2024 <- BCA_2024 %>%
  filter(`Missing or demolished` != "yes") %>% # Removing demolished sites
  mutate(
    Stewardship = ifelse(is.na(Stewardship), "No Stewardship", Stewardship),
    Stewardship = factor(Stewardship, levels = c("No Stewardship", "Seeding Stewardship", "Green Streets"))
  )                                                              # Converting NAs to "No Stewardship," so they are still useable
                                                                 # Converting them into a factor (Categorical variable).
                                                                 # Opens up room to even compare between stewardship types

# Removing any other NAs in the data set, done after to prevent all the NAs in the Stewardship column disappearing
BCA_2024 <- na.omit(BCA_2024)

# 2022 Data
BCA_2022 <- na.omit(BCA_2022)                                      # Doesn't contain "Stewardship" column, so it is done here

BCA_2022 <- BCA_2022 %>%
  filter(`Missing or demolished` != "yes") %>%                   # Remove demolished sites
  dplyr::select(-`Condition assessment done in 2024?`) %>%       # Remove this column, did not seem useful
  mutate(
    `Pre-Treatment Score` = as.numeric(`Pre-Treatment Score`),   # Converting these two columns into numeric
    `Outlet Score` = as.numeric(`Outlet Score`)                  # because they were characters before (Observing from str(BCA_2022))
  )

# ---------------------------
# 5. Feature engineering
# ---------------------------
# Extracting all sites with similar GRI IDs to keep it strictly between them
common_sites <- intersect(BCA_2022$`GRI ID`, BCA_2024$`GRI ID`)

# 2022 Cleaned Dataset
BCA_2022_aligned <- BCA_2022 %>%
  filter(`GRI ID` %in% common_sites) %>%                         # Filter all GRI IDs that are in common with BCA_2024
  filter(`Site Name` != "Yukon St @ W 64th Ave SE") %>%          # There was a duplicate GRI ID so removed the site that was similar
  mutate(Year = 2022, Stewardship = "No Stewardship") %>%        # Added a Year column and a Stewardship column to the 2022 data
  rename_with(~paste0(., "_2022"), -`GRI ID`)                    # Renames all the columns with _2022 at the end to distinguish

# 2024 Cleaned Dataset
BCA_2024_aligned <- BCA_2024 %>%
  filter(`GRI ID` %in% common_sites) %>%
  mutate(Year = 2024) %>%
  rename_with(~paste0(., "_2024"), -`GRI ID`)


# Combined dataset which are distinguished by _202# at the end of each feature
BCA_combined <- full_join(BCA_2024_aligned, BCA_2022_aligned, by = "GRI ID")

# ---------------------------
# 6.1 EDA - Exploratory Data Analysis
# ---------------------------

# Correlation Analysis
numeric_2024 <- BCA_2024 %>%
  dplyr::select(where(is.numeric))

GGally::ggpairs(numeric_2024)

# Test how Stewardship affect the condition score between (None, Green Streets, and Seedling Stewardship)
lm_result <- lm(`Condition Score` ~ Stewardship, data = BCA_2024)
summary(lm_result)                                               # Multiple R-squared is low because other factors were not added into model

# Subtracts the difference for each site, calculate the change rate of each site, and the predicted score for each site.
BCA_forecast <- BCA_combined %>%
  mutate(
    score_change = `Condition Score_2024` - `Condition Score_2022`,
    annual_change_rate = score_change / 2,
    predicted_2026 = `Total Score_2024` + (2 * annual_change_rate),
    change_direction = case_when(                                 # This is just an if/else statement assigning a meaning in a
      score_change < 0  ~ "Improved",                             # new column beside each observation. Easier to interpret each
      score_change > 0  ~ "Worsened",                             # observation score_change. Refer to Note below.
      score_change == 0 ~ "No Change"
    )
  )

score_changelm <- lm(score_change ~ Stewardship_2024, data = BCA_forecast)         # Regression analysis on change
summary(score_changelm)


# Provides summary information on the three stewardship types.
BCA_summary <- BCA_forecast %>%
  group_by(Stewardship_2024) %>%
  summarise(
    avg_change = mean(score_change),
    median_change = median(score_change),
    n = n(),
    avg_score_2022 = mean(`Condition Score_2022`),
    avg_score_2024 = mean(`Condition Score_2024`)
  )

# Distribution of change in condition score between stewardship types
BCA_distribution <- BCA_forecast %>%
  group_by(Stewardship_2024, change_direction) %>%
  summarise(n = n()) %>%
  mutate(percent = n / sum(n) * 100)

# Note: When a score is 1 = Good, 5 = Bad
# So when reading the betas as negative it means the score is going lower, which is a good thing.
# Or when reading negative changes, means score is going towards 1 = Good.

# ---------------------------
# 6.2 EDA - Exploratory Data Analysis on numerical features
# ---------------------------

# Select numeric columns from 2022 and 2024
numeric_cols_2022 <- BCA_combined %>% 
  dplyr::select(ends_with("_2022")) %>% 
  dplyr::select(where(is.numeric))

numeric_cols_2024 <- BCA_combined %>% 
  dplyr::select(ends_with("_2024")) %>% 
  dplyr::select(where(is.numeric))

# Compute changes for all features that are numeric
feature_changes <- numeric_cols_2024 - numeric_cols_2022
feature_changes <- feature_changes %>% 
  mutate(                                                        # Need to add in the GRI ID and stewardship labels
    GRI_ID = BCA_combined$`GRI ID`,                              # to group them together and seperate in table.
    Stewardship = BCA_combined$Stewardship_2024
  )

Feature_summary <- feature_changes %>%               
  pivot_longer(
    cols = -c(GRI_ID, Stewardship, Year_2024),                   # Pivot without these columns
    names_to = "Feature",                                        # Pivot longer to tidy the data and make it easier to interpret
    values_to = "Change"
  ) %>%
  group_by(Stewardship, Feature) %>%                             # Here, we are grouping by the two main features, since we
  mutate(
    Feature = str_remove(Feature, "_2024$")                      # Removing _2024 to not be confused
  ) %>%
  summarise(                                                     # did not group by GRI_ID, it will not show up in the table.
    avg_change = mean(Change, na.rm = TRUE),                      # Then calculating the Average change between the two years
    median_change = median(Change, na.rm = TRUE),                 # (2024-2022). Refer to Note on number signs.
    n = n()
  ) %>%
  arrange(desc(Feature))

# Note: When a score is 1 = Good, 5 = Bad
# So when reading the betas as negative it means the score is going lower, which is a good thing.
# Or when reading negative changes, means score is going towards 1 = Good.

# ---------------------------
# 7. Export cleaned data file for Tableau
# ---------------------------
write.csv(BCA_combined, "BCA_combined_clean.csv", row.names = FALSE)
write.csv(BCA_forecast, "BCA_forecast_clean.csv", row.names = FALSE)
write.csv(BCA_summary, "BCA_summary_clean.csv", row.names = FALSE)
write.csv(BCA_distribution, "BCA_distribution_clean.csv", row.names = FALSE)
write.csv(Feature_summary, "Feature_summary_clean.csv", row.names = FALSE)
