# ==========================================================
# Project: BUS336 Hackathon
# Purpose: Data cleaning, exploration, and preparation for Tableau
# Author: Steven Duong, Darian Sidhu, Brady Van Unen
# Date: Oct 28, 2025
# ==========================================================

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

BCA2024 <- read_excel("2024 Bioretention Condition Assessment_City Studio Data.xlsx", sheet = "24CSTable")
BCA2022 <- read_excel("2024 Bioretention Condition Assessment_City Studio Data.xlsx", sheet = "22CSTable")


# ---------------------------
# 3. Inspect data
# ---------------------------
glimpse(BCA2024)
summary(BCA2024)
head(BCA2024)

glimpse(BCA2022)
summary(BCA2022)
head(BCA2022)

# ---------------------------
# 4. Clean Data
# ---------------------------

# 2024 Data
BCA2024 <- BCA2024 %>%
  filter(`Missing or demolished` != "yes") %>% # Removing demolished sites
  mutate(
    Stewardship = ifelse(is.na(Stewardship), "No Stewardship", Stewardship),
    Stewardship = factor(Stewardship, levels = c("No Stewardship", "Seeding Stewardship", "Green Streets"))
  )                                                              # Converting NAs to "No Stewardship," so they are still useable
                                                                 # Converting them into a factor (Categorical variable).
                                                                 # Opens up room to even compare between stewardship types

# Removing any other NAs in the data set, done after to prevent all the NAs in the Stewardship column disappearing
BCA2024 <- na.omit(BCA2024)

# 2022 Data
BCA2022 <- na.omit(BCA2022)                                      # Doesn't contain "Stewardship" column, so it is done here

BCA2022 <- BCA2022 %>%
  filter(`Missing or demolished` != "yes") %>%                   # Remove demolished sites
  dplyr::select(-`Condition assessment done in 2024?`) %>%       # Remove this column, did not seem useful
  mutate(
    `Pre-Treatment Score` = as.numeric(`Pre-Treatment Score`),   # Converting these two columns into numeric
    `Outlet Score` = as.numeric(`Outlet Score`)                  # because they were characters before (Observing from str(BCA2022))
  )

# ---------------------------
# 5. Feature engineering
# ---------------------------
# Extracting all sites with similar GRI IDs to keep it strictly between them
common_sites <- intersect(BCA2022$`GRI ID`, BCA2024$`GRI ID`)

# 2022 Cleaned Dataset
BCA2022_aligned <- BCA2022 %>%
  filter(`GRI ID` %in% common_sites) %>%                         # Filter all GRI IDs that are in common with BCA2024
  filter(`Site Name` != "Yukon St @ W 64th Ave SE") %>%          # There was a duplicate GRI ID so removed the site that was similar
  mutate(Year = 2022, Stewardship = "No Stewardship") %>%        # Added a Year column and a Stewardship column to the 2022 data
  rename_with(~paste0(., "_2022"), -`GRI ID`)                    # Renames all the columns with _2022 at the end to distinguish

# 2024 Cleaned Dataset
BCA2024_aligned <- BCA2024 %>%
  filter(`GRI ID` %in% common_sites) %>%
  mutate(Year = 2024) %>%
  rename_with(~paste0(., "_2024"), -`GRI ID`)


# Combined dataset which are distinguished by _202# at the end of each feature
BCA_combined <- full_join(BCA2024_aligned, BCA2022_aligned, by = "GRI ID")

# ---------------------------
# 6. EDA - Exploratory Data Analysis
# ---------------------------



# ---------------------------
# 7. Export cleaned data file for Tableau
# ---------------------------


