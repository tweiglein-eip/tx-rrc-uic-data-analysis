# File header -------------------------------------------------------------

# File Name: 02_analyze_data.R
# Created: 04 Sep 2024 by Tyler Weiglein
# Last Modified: 05 Sep 2024 by Tyler Weiglein

# Purpose: To perform the analyses mentioned in this letter 
# (https://drive.google.com/file/d/1eecdiYKP56aN8QXEaeGe97HafjIJuCae/view?usp=sharing) 
# to the U.S. Environmental Protection Agency (EPA) regarding the Texas Railroad
# Commission (RRC) Class II Underground Injection Control (UIC) program.


# Preliminaries -----------------------------------------------------------

# Clear console and environment

cat("\014") 
rm(list = ls(all.names = TRUE))

# Load package(s)

library(readxl)
library(scales)
library(tidyverse)

# Get most recent download

download_folder <- paste0("data/", list.files("data") %>% 
                            sort(decreasing = TRUE) %>% 
                            .[1])


# Read in data ------------------------------------------------------------

# Specify tables of interest

table_nums <- c("01", "07", "10")

# Read in data

for (i in seq_along(table_nums)) {
  
  load(paste0(download_folder, "/table_", table_nums[i], ".RData"))
  
}


# Create plot of SNC violations time series -------------------------------

# Create data frame for Class II well SNC violations

class_2_snc_viol <- table_10 %>% 
  left_join(table_01, by = "UIC_CNTL_NO") %>%
  filter(UIC_CLASS == "2",
         ENF_ACT_SNC_FLAG == "Y") %>%
  mutate(year = year(ymd(ENF_ACT_VIOL_DATE)))

# Create SNC violations time series

snc_timeseries <- class_2_snc_viol %>% 
  filter(year <= 2023) %>% # Filter to only include years w/ complete data
  group_by(year) %>% 
  summarize(n_snc = n())

# Specify plot parameters
  
size_title <- 16
size_text <- 14
  
# Plot SNC time series

snc_timeseries_plot <- snc_timeseries %>% 
  ggplot(aes(x = year, y = n_snc)) +
  geom_line() +
  geom_point() +
  labs(x = "Year",
       y = "Number of SNC Violations") +
  scale_y_continuous(labels = label_comma()) +
  theme_bw() +
  theme(axis.title = element_text(size = size_title, face = "bold"),
        axis.text = element_text(size = size_text))

# Save plot

ggsave("fig/Figure 1 - Number of SNC Violations by Year.png",
       snc_timeseries_plot,
       width = 8,
       height = 6,
       units = "in")


# Calculate proportion of SNC violations by type --------------------------

# Read in violation codes key

viol_code_key <- read_excel("rrc_uic_viol_code_key.xlsx")

# Calculate proportion of SNC violations by type

snc_df <- class_2_snc_viol %>% 
  left_join(viol_code_key, by = "ENF_ACT_VIOL_CODE")

snc_by_type <- snc_df %>% 
  group_by(ENF_ACT_VIOL_CODE, VIOLATION_TYPE) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  mutate(pct = round(count / sum(count) * 100, 2)) %>% 
  arrange(desc(pct))

# Redo above analysis for before 2011 and from 2011 onward

snc_by_type_pre_2011 <- snc_df %>% 
  filter(year < 2011) %>% 
  group_by(ENF_ACT_VIOL_CODE, VIOLATION_TYPE) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  mutate(pct = round(count / sum(count) * 100, 2)) %>% 
  arrange(desc(pct))

snc_by_type_2011_onward <- snc_df %>% 
  filter(year >= 2011) %>% 
  group_by(ENF_ACT_VIOL_CODE, VIOLATION_TYPE) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  mutate(pct = round(count / sum(count) * 100, 2)) %>% 
  arrange(desc(pct))


# Analyze proportion of wells that have failed one or more MITs -----------

# Create data frame of UIC control numbers with 1 or more MIT failures

mit_failure_df <- snc_df %>% 
  filter(VIOLATION_TYPE == "MIT FAILURE") %>% 
  group_by(UIC_CNTL_NO) %>% 
  summarize(mit_fail = 1) # Set boolean to indicate MIT failure

# Create table of percent of UIC control numbers with 1 or more MIT failures

mit_failure_table <- table_01 %>% 
  left_join(mit_failure_df, by = "UIC_CNTL_NO") %>% 
  mutate(UIC_INJ_CO2 = ifelse(UIC_INJ_CO2 == " ", NA, UIC_INJ_CO2)) %>% 
  filter(UIC_ACTIVATED_FLAG == "Y") %>% 
  group_by(UIC_INJ_CO2) %>% 
  summarize(n_mit_fail = sum(mit_fail, na.rm = TRUE),
            n_tot = n(),
            pct_mit_fail = round(n_mit_fail / n_tot * 100, 2)) %>% 
  arrange(desc(UIC_INJ_CO2)) %>% 
  mutate(UIC_INJ_CO2 = if_else(is.na(UIC_INJ_CO2), "Unknown",
                               if_else(UIC_INJ_CO2 == "N", "No", "Yes"))) %>% 
  rename(`Authorized to Inject CO2?` = UIC_INJ_CO2,
         `Active UIC Control Numbers with One or More MIT Failures` = n_mit_fail,
         `Total Active UIC Control Numbers` = n_tot,
         `Percent of Active UIC Control Numbers with 1 or More MIT Failures` = pct_mit_fail)

# Save to CSV

write_excel_csv(mit_failure_table,
                "table/Table 1 - Pct of UIC Cntrl No with 1 or More MIT Failures by CO2 Inj Approval.csv")


# Analyze MIT failures year-by-year ---------------------------------------

# Get year of first MIT test

first_test <- table_07 %>% 
  arrange(UIC_H5_TEST_DATE) %>% 
  filter(!duplicated(UIC_CNTL_NO)) %>% 
  mutate(year_first_test = str_sub(UIC_H5_TEST_DATE, 1, 4) %>% 
           as.numeric()) %>% 
  select(UIC_CNTL_NO, year_first_test)

# Extract W-3 (plugging record) year and join tables

table_01_mod <- table_01 %>% 
  mutate(UIC_INJ_CO2 = ifelse(UIC_INJ_CO2 == " ", "NA", UIC_INJ_CO2),
         year_w3 = str_sub(UIC_W3_DATE, 1, 4) %>% 
           as.numeric(),
         year_w3 = if_else(year_w3 == 0, # For wells that haven't been plugged, set plug year to 9999
                           9999,
                           year_w3)) %>% 
  left_join(first_test, by = "UIC_CNTL_NO")

# Analyze percent of wells w/ MIT failure by year

mit_timeseries <- snc_df %>% 
  filter(UIC_ACTIVATED_FLAG == "Y",
         VIOLATION_TYPE == "MIT FAILURE") %>% 
  mutate(UIC_INJ_CO2 = ifelse(UIC_INJ_CO2 == " ", "NA", UIC_INJ_CO2)) %>% 
  group_by(UIC_CNTL_NO, year, UIC_INJ_CO2) %>% 
  summarize(mit_fail_count = n()) %>% # Number of MIT failures per UIC cntl no. and year
  mutate(mit_fail_bool = if_else(mit_fail_count > 0, 1, 0)) %>% 
  group_by(year, UIC_INJ_CO2) %>% 
  summarize(n_mit_fail = sum(mit_fail_bool)) # Number of MIT failures per year by CO2 authorization
  
# Calculate number of "active" wells (i.e., wells subject to MIT) for
# each year in MIT violations time series

active_uic_cntl_no <- mit_timeseries %>% 
  select(-n_mit_fail) %>% 
  mutate(n_active_uic = NA)

for (i in seq_along(active_uic_cntl_no$year)) {
  
  active_uic_cntl_no$n_active_uic[i] <- table_01_mod %>% 
    filter(UIC_ACTIVATED_FLAG == "Y") %>% 
    filter(UIC_INJ_CO2 == active_uic_cntl_no$UIC_INJ_CO2[i],
           year_first_test <= active_uic_cntl_no$year[i],
           year_w3 >= active_uic_cntl_no$year[i]) %>% 
    pull() %>% 
    length()
  
}

# Join w/ MIT failure time series and calculate percentage of wells

mit_timeseries <- mit_timeseries %>% 
  left_join(active_uic_cntl_no, by = c("year", "UIC_INJ_CO2")) %>% 
  filter(year <= 2023) %>% 
  mutate(pct_mit_fail = n_mit_fail / n_active_uic * 100,
         UIC_INJ_CO2 = if_else(UIC_INJ_CO2 == "NA", NA, UIC_INJ_CO2))

# Plot MIT failure time series

breaks_val <- c("Y", "N", NA)
labels_val <- c("Yes", "No", "Unknown")

mit_timeseries_plot <- mit_timeseries %>% 
  ggplot(aes(x = year, y = pct_mit_fail, color = UIC_INJ_CO2, fill = UIC_INJ_CO2)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x = "Year",
       y = "Percent Active UIC Cntl. No. w/ MIT Failure (%)",
       color = expression("Authorized to Inj. CO"[2]*"?"),
       fill = expression("Authorized to Inj. CO"[2]*"?")) +
  scale_color_discrete(breaks = breaks_val,
                       labels = labels_val) +
  scale_fill_discrete(breaks = breaks_val,
                      labels = labels_val) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.2, 0.8),
        legend.background = element_rect(color = "black", linewidth = 0.25),
        legend.title = element_text(size = size_title, face = "bold"),
        legend.text = element_text(size = size_text),
        axis.title = element_text(size = size_title, face = "bold"),
        axis.text = element_text(size = size_text))

# Save plot

ggsave("fig/Figure 2 - Pct UIC Cntl Numbers w MIT Failure by Year and CO2 Authorization.png",
       mit_timeseries_plot,
       width = 8,
       height = 6,
       units = "in")
