library(tidyverse)
library(writexl)

write_path <- "../output/tables"
latest_period <- "_22_25"

# BMI category by school year by 3 year period
file_name <- "bmi_category_by_school_year_by_3_year_period"
ncmp_summarise(ncmp_data_3_years_combined,
               .by_count = c(BMI_Category, School_Year, Year),
               .by_denom = c(School_Year, Year),
               .include_oo = T,
               .round_5 = T) |> 
  rename(Period = Year,
         value = pct) |> 
  janitor::clean_names() |> 
  write_xlsx(path = paste0(write_path, file_name, ".xlsx"),
             col_names = T,
             format_headers = F)


# By deprivation ----------------------------------------------------------

# BMI category by school year by IDACI 2025 Quintile by 3 year period
file_name <- paste0("bmi_category_by_school_year_by_deprivation_by_3_year_period")
ncmp_summarise(ncmp_data_3_years_combined,
               .by_count = c(BMI_Category, School_Year, Year, IDACI_2025_Quintile),
               .by_denom = c(School_Year, Year, IDACI_2025_Quintile),
               .include_oo = T,
               .round_5 = T) |> 
  rename(Period = Year,
         value = pct) |> 
  janitor::clean_names() |> 
  write_xlsx(path = paste0(write_path, file_name, ".xlsx"),
             col_names = T,
             format_headers = F)

# BMI category by school year by gender by IDACI 2025 Quintile by 3 year period
file_name <- paste0("bmi_category_by_school_year_by_deprivation_by_gender_by_3_year_period")
ncmp_summarise(ncmp_data_3_years_combined,
               .by_count = c(BMI_Category, School_Year, Year, IDACI_2025_Quintile, Gender),
               .by_denom = c(School_Year, Year, IDACI_2025_Quintile, Gender),
               .include_oo = T,
               .round_5 = T) |> 
  rename(Period = Year,
         value = pct) |> 
  janitor::clean_names() |> 
  write_xlsx(path = paste0(write_path, file_name, ".xlsx"),
             col_names = T,
             format_headers = F)

# By ethnicity ------------------------------------------------------------

# BMI category by school year by ethnicity by 3 year period
file_name <- paste0("bmi_category_by_school_year_by_ethnicity_by_3_year_period")
ncmp_summarise(ncmp_data_3_years_combined,
               .by_count = c(BMI_Category, School_Year, Year, Ethnicity),
               .by_denom = c(School_Year, Year, Ethnicity),
               .include_oo = T,
               .round_5 = T) |> 
  rename(Period = Year,
         value = pct) |> 
  janitor::clean_names() |> 
  write_xlsx(path = paste0(write_path, file_name, ".xlsx"),
             col_names = T,
             format_headers = F)

# BMI category by school year by ethnicity by gender by 3 year period
file_name <- paste0("bmi_category_by_school_year_by_ethnicity_by_gender_by_3_year_period")
ncmp_summarise(ncmp_data_3_years_combined,
               .by_count = c(BMI_Category, School_Year, Year, Ethnicity, Gender),
               .by_denom = c(School_Year, Year, Ethnicity, Gender),
               .include_oo = T,
               .round_5 = T) |> 
  rename(Period = Year,
         value = pct) |> 
  janitor::clean_names() |> 
  write_xlsx(path = paste0(write_path, file_name, ".xlsx"),
             col_names = T,
             format_headers = F)

# By deprivation and ethnicity --------------------------------------------

# BMI category by school year by grouped deprivation by ethnicity by 3 year period
file_name <- paste0("bmi_category_by_school_year_by_deprivation_(grouped)_by_ethnicity_by_3_year_period")

all_bham <- ncmp_summarise(ncmp_data_3_years_combined,
                           .by_count = c(Year, School_Year, BMI_Category),
                           .by_denom = c(Year, School_Year),
                           .include_oo = T,
                           .round_5 = T) |> 
  rename(all_bham_pct = pct,
         all_bham_lower_ci = lower_ci,
         all_bham_upper_ci = upper_ci) |> 
  select(Year, School_Year, BMI_Category, all_bham_pct, all_bham_lower_ci, all_bham_upper_ci)

ncmp_data_3_years_combined |> 
  mutate(IDACI_2025_Quintile = case_when(IDACI_2025_Quintile == "1" ~ "1",
                                         IDACI_2025_Quintile == "2" ~ "2",
                                         .default = "3-5")) |> 
  ncmp_summarise(.by_count = c(BMI_Category, Year, School_Year, IDACI_2025_Quintile, Ethnicity),
                 .by_denom = c(Year, School_Year, IDACI_2025_Quintile, Ethnicity),
                 .include_oo = T,
                 .round_5 = T) |> 
  left_join(all_bham,
            by = c("Year", "School_Year", "BMI_Category")) |> 
  mutate(D = pct - all_bham_pct,
         D_lower = D - sqrt((pct - lower_ci)^2 + (all_bham_upper_ci - all_bham_pct)^2),
         D_upper = D + sqrt((pct - lower_ci)^2 + (all_bham_upper_ci - all_bham_pct)^2),
         sig_diff = case_when(D_lower < 0 & D_upper < 0 ~ "↓",
                              D_lower > 0 & D_upper > 0 ~ "↑",
                              .default = "-"),
         sig_diff_tooltip = case_when(sig_diff != "-" & pct > all_bham_pct ~ "Sig. higher than Birmingham average",
                                      sig_diff != "-" & pct < all_bham_pct ~ "Sig. lower than Birmingham average",
                                      .default = "Similar to Birmingham average")) |> 
  select(-D, -D_lower, -D_upper, -sig_diff, -all_bham_pct, -all_bham_lower_ci, -all_bham_upper_ci) |> 
  rename(Period = Year,
         Significance_Test = sig_diff_tooltip,
         value = pct) |>
  janitor::clean_names() |> 
  write_xlsx(path = paste0(write_path, file_name, ".xlsx"),
             col_names = T,
             format_headers = F)

# By geography ------------------------------------------------------------

# BMI category by school year by ward by 3 year period
file_name <- paste0("bmi_category_by_school_year_by_ward_by_3_year_period")

ncmp_summarise(ncmp_data_3_years_combined,
               .by_count = c(BMI_Category, School_Year, Year, WD18CD, WD18NM),
               .by_denom = c(School_Year, Year, WD18CD, WD18NM),
               .include_oo = T,
               .round_5 = T) |> 
  rename(Period = Year,
         value = pct) |> 
  janitor::clean_names() |> 
  write_xlsx(path = paste0(write_path, file_name, ".xlsx"),
             col_names = T,
             format_headers = F)

# BMI category by school year by ward by 3 year period
file_name <- paste0("bmi_category_by_school_year_by_msoa_by_3_year_period")
ncmp_summarise(ncmp_data_3_years_combined,
               .by_count = c(BMI_Category, School_Year, Year, MSOA21CD),
               .by_denom = c(School_Year, Year, MSOA21CD),
               .include_oo = T,
               .round_5 = T) |> 
  rename(Period = Year,
         value = pct) |> 
  janitor::clean_names() |> 
  write_xlsx(path = paste0(write_path, file_name, ".xlsx"),
             col_names = T,
             format_headers = F)

# BMI category by school year by pcon by 3 year period
file_name <- paste0("bmi_category_by_school_year_by_constituency_by_3_year_period")
ncmp_summarise(ncmp_data_3_years_combined,
               .by_count = c(BMI_Category, School_Year, Year, PCON22CD),
               .by_denom = c(School_Year, Year, PCON22CD),
               .include_oo = T,
               .round_5 = T) |> 
  rename(Period = Year,
         value = pct) |> 
  janitor::clean_names() |> 
  write_xlsx(path = paste0(write_path, file_name, ".xlsx"),
             col_names = T,
             format_headers = F)


# By school ---------------------------------------------------------------

# BMI category by school year by school by 3 year period
file_name <- paste0("bmi_category_by_school_year_by_school_by_3_year_period")

schools <- read.csv("C:/Users/bccaengs/OneDrive - Birmingham City Council/Documents/Work/Useful datasets/all_uk_schools_inc_closed_20260204.csv",
                    header = T) |> 
  janitor::clean_names() |> 
  select(urn, establishment_name, establishment_status_name)

ncmp_summarise(ncmp_data_3_years_combined,
               .by_count = c(BMI_Category, School_Year, Year, DoH_URN),
               .by_denom = c(School_Year, Year, DoH_URN),
               .include_oo = T,
               .round_5 = T) |> 
  rename(Period = Year,
         value = pct) |>
  mutate(DoH_URN = as.integer(DoH_URN)) |> 
  left_join(schools,
            by = join_by(DoH_URN == urn)) |> 
  rename(school_name = establishment_name,
         status = establishment_status_name) |>
  relocate(school_name:status) |> 
  janitor::clean_names() |> 
  rename(urn = do_h_urn) |> 
  relocate(urn,
           .before = status) |> 
  write_xlsx(path = paste0(write_path, file_name, ".xlsx"),
             col_names = T,
             format_headers = F)

# By school - top 40 ------------------------------------------------------

schools <- read.csv("C:/Users/bccaengs/OneDrive - Birmingham City Council/Documents/Work/Useful datasets/all_uk_schools_inc_closed_20260204.csv",
                    header = T) |> 
  janitor::clean_names() |> 
  select(urn, establishment_name, establishment_status_name)

# Y6 - find which schools are in the top 40 for % overweight/obese for each year of data
# then count
high_oo_schools_y6 <- ncmp_data |> 
  ncmp_summarise(.by_count = c(BMI_Category, School_Year, Year, DoH_URN),
                 .by_denom = c(School_Year, Year, DoH_URN),
                 .include_oo = T,
                 .round_5 = F) |> 
  filter(BMI_Category == "Overweight/Obese",
         School_Year == "Year 6") |> 
  mutate(DoH_URN = as.integer(DoH_URN)) |> 
  left_join(schools,
            by = join_by(DoH_URN == urn)) |> 
  rename(school_name = establishment_name,
         status = establishment_status_name) |>
  relocate(school_name:status) |> 
  filter(status == "Open") |> 
  group_by(Year) |> 
  slice_max(pct, n = 40) |> 
  ungroup() |> 
  count(school_name, DoH_URN) |> 
  arrange(desc(n))

# Y6 - calculate % oo for most recent year, including significance
# and add in number of years in top 40
oo_school_y6 <- ncmp_data |> 
  filter(Year == "2024/2025") |> 
  ncmp_summarise(.by_count = c(BMI_Category, School_Year, Year, DoH_URN),
                 .by_denom = c(School_Year, Year, DoH_URN),
                 .include_oo = T,
                 .round_5 = F) |> 
  filter(BMI_Category == "Overweight/Obese",
         School_Year == "Year 6") |> 
  mutate(DoH_URN = as.integer(DoH_URN)) |> 
  left_join(schools,
            by = join_by(DoH_URN == urn)) |> 
  rename(school_name = establishment_name,
         status = establishment_status_name) |>
  relocate(school_name:status) |> 
  filter(status == "Open") |> 
  calculate_sigdiff(.school_year = "Year 6",
                    suppression_value = 0) |> 
  slice_max(pct, n = 40) |> 
  arrange(desc(pct)) |> 
  select(Year, DoH_URN, school_name, School_Year, count, denominator, pct, sig_diff_new) |> 
  left_join(high_oo_schools_y6,
            by = c("school_name", "DoH_URN")) |> 
  rename(`School Name` = school_name,
         `School Year` = School_Year,
         `Number Overweight/Obese` = count,
         `Total Children` = denominator,
         `% Overweight/Obese` = pct,
         `Significance` = sig_diff_new,
         `Number of years in top 40 since 2014/2015` = n)

# R - find which schools are in the top 40 for % overweight/obese for each year of data
# then count
high_oo_schools_reception <- ncmp_data |> 
  ncmp_summarise(.by_count = c(BMI_Category, School_Year, Year, DoH_URN),
                 .by_denom = c(School_Year, Year, DoH_URN),
                 .include_oo = T,
                 .round_5 = F) |> 
  filter(BMI_Category == "Overweight/Obese",
         School_Year == "Reception") |> 
  mutate(DoH_URN = as.integer(DoH_URN)) |> 
  left_join(schools,
            by = join_by(DoH_URN == urn)) |> 
  rename(school_name = establishment_name,
         status = establishment_status_name) |>
  relocate(school_name:status) |> 
  filter(status == "Open") |> 
  group_by(Year) |> 
  slice_max(pct, n = 40) |> 
  ungroup() |> 
  count(school_name, DoH_URN) |> 
  arrange(desc(n))

# R - calculate % oo for most recent year, including significance
# and add in number of years in top 40
oo_school_reception <- ncmp_data |> 
  filter(Year == "2024/2025") |> 
  ncmp_summarise(.by_count = c(BMI_Category, School_Year, Year, DoH_URN),
                 .by_denom = c(School_Year, Year, DoH_URN),
                 .include_oo = T,
                 .round_5 = F) |> 
  filter(BMI_Category == "Overweight/Obese",
         School_Year == "Reception") |> 
  mutate(DoH_URN = as.integer(DoH_URN)) |> 
  left_join(schools,
            by = join_by(DoH_URN == urn)) |> 
  rename(school_name = establishment_name,
         status = establishment_status_name) |>
  relocate(school_name:status) |> 
  filter(status == "Open") |> 
  calculate_sigdiff(.school_year = "Reception",
                    suppression_value = 0) |> 
  slice_max(pct, n = 40) |> 
  arrange(desc(pct)) |> 
  select(Year, DoH_URN, school_name, School_Year, count, denominator, pct, sig_diff_new) |> 
  left_join(high_oo_schools_reception,
            by = c("school_name", "DoH_URN")) |> 
  rename(`School Name` = school_name,
         `School Year` = School_Year,
         `Number Overweight/Obese` = count,
         `Total Children` = denominator,
         `% Overweight/Obese` = pct,
         `Significance` = sig_diff_new,
         `Number of years in top 40 since 2014/2015` = n)

# write to file
file_name <- "overweight_obese_by_school_with_significance_2024-25"
write_xlsx(x = list(Reception = oo_school_reception,
                    `Year 6` = oo_school_y6),
           path = paste0(write_path, file_name, ".xlsx"),
           col_names = T,
           format_headers = F)

# BMI category change -----------------------------------------------------

# BMI category change by school year, latest period only
file_name <- paste0("bmi_category_change", latest_period)

ncmp_summarise(cohorts_combined,
               .by_count = c(BMI_Category_Clinical_r, BMI_Category_Clinical_y6),
               .by_denom = c(BMI_Category_Clinical_r),
               .include_oo = F,
               .round_5 = T) |> 
  mutate(period = periods[8],
         .before = BMI_Category_Clinical_r) |> 
  rename(value = pct) |> 
  janitor::clean_names() |> 
  write_xlsx(path = paste0(write_path, file_name, ".xlsx"),
             col_names = T,
             format_headers = F)

# BMI category change by school year by deprivation, latest period only
file_name <- paste0("bmi_category_change_by_deprivation_(grouped)", latest_period)

cohorts_combined |> 
  mutate(IDACI_2025_Quintile = case_when(IDACI_2025_Quintile == "1" ~ "1",
                                         IDACI_2025_Quintile == "2" ~ "2",
                                         IDACI_2025_Quintile == "3" ~ "3-5",
                                         IDACI_2025_Quintile == "4" ~ "3-5",
                                         IDACI_2025_Quintile == "5" ~ "3-5")) |> 
  ncmp_summarise(.by_count = c(BMI_Category_Clinical_r, BMI_Category_Clinical_y6, IDACI_2025_Quintile),
                 .by_denom = c(BMI_Category_Clinical_r, IDACI_2025_Quintile),
                 .include_oo = F,
                 .round_5 = T) |> 
  mutate(period = periods[8],
         .before = BMI_Category_Clinical_r) |> 
  rename(value = pct) |> 
  janitor::clean_names() |> 
  write_xlsx(path = paste0(write_path, file_name, ".xlsx"),
             col_names = T,
             format_headers = F)

# BMI category change by school year by gender, latest period only
file_name <- paste0("bmi_category_change_by_by_gender", latest_period)

ncmp_summarise(cohorts_combined,
               .by_count = c(BMI_Category_Clinical_r, BMI_Category_Clinical_y6, Gender_y6),
               .by_denom = c(BMI_Category_Clinical_r, Gender_y6),
               .include_oo = F,
               .round_5 = T) |> 
  mutate(period = periods[8],
         .before = BMI_Category_Clinical_r) |> 
  rename(value = pct,
         gender = Gender_y6) |> 
  janitor::clean_names() |> 
  write_xlsx(path = paste0(write_path, file_name, ".xlsx"),
             col_names = T,
             format_headers = F)


# Short stature -----------------------------------------------------------

# Short stature by school year by three year period
file_name <- paste0("short_stature_by_school_year_by_3_year_period")

ncmp_summarise(ncmp_data_3_years_combined,
               .by_count = c(Year, School_Year, short_stature),
               .by_denom = c(Year, School_Year),
               .include_oo = F,
               .round_5 = T) |> 
  filter(short_stature == TRUE) |> 
  rename(value = pct,
         period = Year) |> 
  janitor::clean_names() |> 
  write_xlsx(path = paste0(write_path, file_name, ".xlsx"),
             col_names = T,
             format_headers = F)

# Short stature by school year by deprivation by three year period
file_name <- paste0("short_stature_by_school_year_by_deprivation_by_3_year_period")

ncmp_summarise(ncmp_data_3_years_combined,
               .by_count = c(Year, School_Year, short_stature, IDACI_2025_Quintile),
               .by_denom = c(Year, School_Year, IDACI_2025_Quintile),
               .include_oo = F,
               .round_5 = T) |> 
  filter(short_stature == TRUE) |> 
  rename(value = pct,
         period = Year) |> 
  janitor::clean_names() |> 
  write_xlsx(path = paste0(write_path, file_name, ".xlsx"),
             col_names = T,
             format_headers = F)

# Short stature by school year by gender by three year period
file_name <- paste0("short_stature_by_school_year_by_gender_by_3_year_period")

ncmp_summarise(ncmp_data_3_years_combined,
               .by_count = c(Year, School_Year, short_stature, Gender),
               .by_denom = c(Year, School_Year, Gender),
               .include_oo = F,
               .round_5 = T) |> 
  filter(short_stature == TRUE) |> 
  rename(value = pct,
         period = Year) |> 
  janitor::clean_names() |> 
  write_xlsx(path = paste0(write_path, file_name, ".xlsx"),
             col_names = T,
             format_headers = F)

# Short stature by school year by ethnicity by three year period
file_name <- paste0("short_stature_by_school_year_by_ethnicity_by_3_year_period")

ncmp_summarise(ncmp_data_3_years_combined,
               .by_count = c(Year, School_Year, short_stature, Ethnicity),
               .by_denom = c(Year, School_Year, Ethnicity),
               .include_oo = F,
               .round_5 = T) |> 
  filter(short_stature == TRUE) |> 
  rename(value = pct,
         period = Year) |> 
  janitor::clean_names() |> 
  write_xlsx(path = paste0(write_path, file_name, ".xlsx"),
             col_names = T,
             format_headers = F)
