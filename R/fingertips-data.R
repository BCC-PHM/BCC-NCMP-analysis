library(tidyverse)
library(fingertipsR)

# England/WM/Core Cities obesity data (fingertips) -----------------------------------------------

# download reception indicators from Fingertips using for loop
reception_indicators <- c(90316, 90317, 92464, 90319, 20601)

# England
england_reception_fingertips <- data.frame()

for (id in reception_indicators){
  temp <- fingertips_data(IndicatorID = id, AreaTypeID = 15) |> 
    filter(Sex == "Persons") |> 
    select(Timeperiod, Count, Denominator) |> 
    distinct() |> 
    mutate(School_Year = "Reception",
           indicator = id,
           Area_Name = "England") |> 
    rename(Year = Timeperiod,
           count = Count,
           denominator = Denominator)
  england_reception_fingertips <- rbind(england_reception_fingertips, temp)
} 

# West Midlands
wm_reception_fingertips <- data.frame()

for (id in reception_indicators){
  temp <- fingertips_data(IndicatorID = id, AreaTypeID = 6) |> 
    filter(Sex == "Persons",
           AreaName == "West Midlands region (statistical)") |> 
    select(Timeperiod, Count, Denominator) |> 
    #distinct() |> 
    mutate(School_Year = "Reception",
           indicator = id,
           Area_Name = "West Midlands") |> 
    rename(Year = Timeperiod,
           count = Count,
           denominator = Denominator)
  wm_reception_fingertips <- rbind(wm_reception_fingertips, temp)
} 

# Core Cities
cc_reception_fingertips <- data.frame()

for (id in reception_indicators){
  temp <- fingertips_data(IndicatorID = id, AreaTypeID = 502) |> 
    filter(Sex == "Persons",
           AreaName %in% c("Bristol", "Liverpool", "Sheffield", "Nottingham", "Leeds", "Manchester", "Newcastle upon Tyne")) |> 
    select(Timeperiod, Count, Denominator) |> 
    #distinct() |> 
    mutate(School_Year = "Reception",
           indicator = id,
           Area_Name = "Core Cities") |> 
    rename(Year = Timeperiod,
           count = Count,
           denominator = Denominator)
  cc_reception_fingertips <- rbind(cc_reception_fingertips, temp)
} 

# summarise to get core cities average
cc_reception_fingertips <- cc_reception_fingertips |> 
  group_by(Year, School_Year, indicator, Area_Name) |> 
  summarise(count = sum(count),
            denominator = sum(denominator))

# combine reception fingertips indicators into one df
reception_fingertips <- rbind(england_reception_fingertips,
                              wm_reception_fingertips) |> 
  rbind(cc_reception_fingertips)

# Add BMI category labels using lookup
reception_indicators_lookup <- data.frame(indicator = reception_indicators,
                                          BMI_Category = c("Underweight", "Healthy Weight", "Overweight", "Obese", "Overweight/Obese"))

reception_fingertips <- left_join(reception_fingertips,
                                  reception_indicators_lookup,
                                  by = "indicator") |> 
  select(-indicator)

# repeat for y6
# download y6 indicators from Fingertips using for loop
y6_indicators <- c(90320, 90321, 92465, 90323, 20602)

# England
england_y6_fingertips <- data.frame()

for (id in y6_indicators){
  temp <- fingertips_data(IndicatorID = id, AreaTypeID = 15) |> 
    filter(Sex == "Persons") |> 
    select(Timeperiod, Count, Denominator) |> 
    distinct() |> 
    mutate(School_Year = "Year 6",
           indicator = id,
           Area_Name = "England") |> 
    rename(Year = Timeperiod,
           count = Count,
           denominator = Denominator)
  england_y6_fingertips <- rbind(england_y6_fingertips, temp)
} 

# West Mids
wm_y6_fingertips <- data.frame()

for (id in y6_indicators){
  temp <- fingertips_data(IndicatorID = id, AreaTypeID = 6) |> 
    filter(Sex == "Persons",
           AreaName == "West Midlands region (statistical)") |> 
    select(Timeperiod, Count, Denominator) |> 
    #distinct() |> 
    mutate(School_Year = "Year 6",
           indicator = id,
           Area_Name = "West Midlands") |> 
    rename(Year = Timeperiod,
           count = Count,
           denominator = Denominator)
  wm_y6_fingertips <- rbind(wm_y6_fingertips, temp)
} 

# Core Cities
cc_y6_fingertips <- data.frame()

for (id in y6_indicators){
  temp <- fingertips_data(IndicatorID = id, AreaTypeID = 502) |> 
    filter(Sex == "Persons",
           AreaName %in% c("Bristol", "Liverpool", "Sheffield", "Nottingham", "Leeds", "Manchester", "Newcastle upon Tyne")) |> 
    select(Timeperiod, Count, Denominator) |> 
    #distinct() |> 
    mutate(School_Year = "Year 6",
           indicator = id,
           Area_Name = "Core Cities") |> 
    rename(Year = Timeperiod,
           count = Count,
           denominator = Denominator)
  cc_y6_fingertips <- rbind(cc_y6_fingertips, temp)
} 

# summarise to get core cities average
cc_y6_fingertips <- cc_y6_fingertips |> 
  group_by(Year, School_Year, indicator, Area_Name) |> 
  summarise(count = sum(count),
            denominator = sum(denominator))

# combine y6 fingertips indicators into one df
y6_fingertips <- rbind(england_y6_fingertips,
                       wm_y6_fingertips) |> 
  rbind(cc_y6_fingertips)

# Add BMI category labels using lookup
y6_indicators_lookup <- data.frame(indicator = y6_indicators,
                                   BMI_Category = c("Underweight", "Healthy Weight", "Overweight", "Obese", "Overweight/Obese"))

y6_fingertips <- left_join(y6_fingertips,
                           y6_indicators_lookup,
                           by = "indicator") |> 
  select(-indicator)

# bind year 6 and reception dfs together
fingertips_data <- rbind(reception_fingertips,
                         y6_fingertips)

# years formatted differently in fingertips data so create new ref vector
years_fingertips <- sub("(20)(.*?)(20)", "\\1\\2", years)

# create df with three years of data (first three years i.e. oldest)
fingertips_data_period_1 <- fingertips_data |> 
  filter(Year == years_fingertips[1]|Year == years_fingertips[2]|Year == years_fingertips[3]) |> 
  mutate(Year = paste0(years_fingertips[1], " to ", years_fingertips[3]))

# same again but with next three year period
fingertips_data_period_2 <- fingertips_data |> 
  filter(Year == years_fingertips[2]|Year == years_fingertips[3]|Year == years_fingertips[4]) |> 
  mutate(Year = paste0(years_fingertips[2], " to ", years_fingertips[4]))

# and so on
fingertips_data_period_3 <- fingertips_data |> 
  filter(Year == years_fingertips[3]|Year == years_fingertips[4]|Year == years_fingertips[5]) |> 
  mutate(Year = paste0(years_fingertips[3], " to ", years_fingertips[5]))

fingertips_data_period_4 <- fingertips_data |> 
  filter(Year == years_fingertips[4]|Year == years_fingertips[5]|Year == years_fingertips[6]) |> 
  mutate(Year = paste0(years_fingertips[4], " to ", years_fingertips[6]))

fingertips_data_period_5 <- fingertips_data |> 
  filter(Year == years_fingertips[5]|Year == years_fingertips[6]|Year == years_fingertips[7]) |> 
  mutate(Year = paste0(years_fingertips[5], " to ", years_fingertips[7]))

fingertips_data_period_6 <- fingertips_data |> 
  filter(Year == years_fingertips[6]|Year == years_fingertips[7]|Year == years_fingertips[8]) |> 
  mutate(Year = paste0(years_fingertips[6], " to ", years_fingertips[8]))

fingertips_data_period_7 <- fingertips_data |> 
  filter(Year == years_fingertips[7]|Year == years_fingertips[8]|Year == years_fingertips[9]) |> 
  mutate(Year = paste0(years_fingertips[7], " to ", years_fingertips[9]))

fingertips_data_period_8 <- fingertips_data |> 
  filter(Year == years_fingertips[8]|Year == years_fingertips[9]|Year == years_fingertips[10]) |> 
  mutate(Year = paste0(years_fingertips[8], " to ", years_fingertips[10]))

fingertips_data_3_years_combined <- bind_rows(fingertips_data_period_1,
                                              fingertips_data_period_2,
                                              fingertips_data_period_3,
                                              fingertips_data_period_4,
                                              fingertips_data_period_5,
                                              fingertips_data_period_6,
                                              fingertips_data_period_7,
                                              fingertips_data_period_8)

# summarise by 3 year group
fingertips_data_3_years_combined <- fingertips_data_3_years_combined |> 
  group_by(Year, School_Year, BMI_Category, Area_Name) |> 
  summarise(count = sum(count),
            denominator = sum(denominator))

# calculate percentage and CIs
fingertips_data_3_years_combined <- fingertips_data_3_years_combined |> 
  group_by(Year, School_Year, BMI_Category, Area_Name) |> 
  PHEindicatormethods::phe_proportion(x = count,
                                      n = denominator,
                                      multiplier = 100) |> 
  rename(pct = value,
         lower_ci = lowercl,
         upper_ci = uppercl)

# save as .rda to save quarto having to load every time
save(fingertips_data_3_years_combined, file = "fingertips_data_3_years_combined.rda")

# Participation rates (fingertips) ----------------------------------------

# find out how many reception children were measured in most recent 3 year period according to fingertips
fingertips_reception_participants <- fingertips_data(IndicatorID = 90290, AreaTypeID = 502, AreaCode = "E08000025") |> 
  filter(Timeperiod == years_fingertips[8]|Timeperiod == years_fingertips[9]|Timeperiod == years_fingertips[10]) |> 
  summarise(count = sum(Count)) |> 
  pull(count)

# get mean reception participation rate for most recent period from fingertips
fingertips_reception_participation <- fingertips_data(IndicatorID = 90290, AreaTypeID = 502, AreaCode = "E08000025") |> 
  filter(Timeperiod == years_fingertips[8]|Timeperiod == years_fingertips[9]|Timeperiod == years_fingertips[10]) |> 
  summarise(mean_pct = mean(Value)) |> 
  pull(mean_pct)


# find out how many reception children are in the data used (for most recent period
data_reception_participants <- ncmp_data_period_8 |> 
  filter(School_Year == "Reception") |> 
  count() |> 
  pull(n)

# find out how many year 6 children were measured in most recent period according to fingertips
fingertips_y6_participants <- fingertips_data(IndicatorID = 90291, AreaTypeID = 502, AreaCode = "E08000025") |> 
  filter(Timeperiod == years_fingertips[8]|Timeperiod == years_fingertips[9]|Timeperiod == years_fingertips[10]) |> 
  summarise(count = sum(Count)) |> 
  pull(count)

# get mean year 6 participation rate for most recent period from fingertips
fingertips_y6_participation <- fingertips_data(IndicatorID = 90291, AreaTypeID = 502, AreaCode = "E08000025") |> 
  filter(Timeperiod == years_fingertips[8]|Timeperiod == years_fingertips[9]|Timeperiod == years_fingertips[10]) |> 
  summarise(mean_pct = mean(Value)) |> 
  pull(mean_pct)

# find out how many reception children are in the data used (for most recent period)
data_y6_participants <- ncmp_data_period_8 |> 
  filter(School_Year == "Year 6") |> 
  count() |> 
  pull(n)

# save values as .rda to save quarto having to load from fingertips every time
participation_info <- list(fingertips_reception_participants = fingertips_reception_participants,
                           fingertips_reception_participation = fingertips_reception_participation,
                           data_reception_participants = data_reception_participants,
                           fingertips_y6_participants = fingertips_y6_participants,
                           fingertips_y6_participation = fingertips_y6_participation,
                           data_y6_participants = data_y6_participants)

save(participation_info, file = "data/participation_info.rda")