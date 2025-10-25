# ==========================================================
# Project: BUS336 Hackathon
# Purpose: Data cleaning, exploration, and preparation for Tableau
# Author: Steven Duong, Darian Sidhu, Brady Van Unen
# Date: Oct 25, 2025
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



# ---------------------------
# 5. Feature engineering
# ---------------------------



# ---------------------------
# 6. EDA - Exploratory Data Analysis
# ---------------------------



# ---------------------------
# 7. Export cleaned data file for Tableau
# ---------------------------


