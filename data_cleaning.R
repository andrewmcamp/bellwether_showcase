# ==================================================================================================================== |
#   PROJECT:  Teacher Retention and the Four-Day School Week in Missouri
#   PROGRAM:  Clean district calendar status obtained from MO DESE data center
#               (https://apps.dese.mo.gov/MCDS/home.aspx?categoryid=1&view=2)
#             Using other files downloaded from MO DESE data center for contextual information
#
#   AUTHOR:   Andrew M. Camp
#   CREATED:  July 1, 2024
#   STATUS:   v1.0
#
# ==================================================================================================================== |
# ---------------------------------------------------------------------------------------------------- (0) Preamble ----
# Load packages
library(tidyverse)
library(data.table)
library(educationdata)
library(showtext)
library(janitor)
library(readxl)
library(sf)

# Load fonts
font_add_google("Roboto Condensed", "roboto")
showtext_auto()

# Program options (if any)
line_pal = c("districts" = "#e41a1c", "schools" = "#984ea3", "students" = "#377eb8", "teachers" = "#4daf4a")
map_pal = c("Non-Adoptors" = "#C8C9C7", "Pre-COVID" = "#8da0cb", "During COVID" = "#66c2a5", "Post-COVID" = "#fc8d62")




# --------------------------------------------------------------------------- (1) Read in data and initial cleaning ----
# Reading in from spreadsheet and clean up the column names a bit
dt = read_excel("00_raw_data/Districts and Charters Attending a 4 day week 2010-11 to 2023-24 (2).xlsx") |>
  clean_names() |>
  as.data.table()

# Initial filtering (remove totals row)
dt = dt |>
  _[district_code != "Totals"]

# Reshaping to long
dt = dt |>
  melt(id.vars = c("district_code", "name", "k_8_district"),
       variable.name = "school_year", variable.factor = FALSE,
       value.name = "fdsw") |>
  _[, school_year := as.numeric(substr(school_year, 7, 10)) - 1] |>
  _[, fdsw := ifelse(is.na(fdsw), FALSE, TRUE)] |>
  _[, k_8_district := NULL] |>
  _[, state_leaid := case_when(nchar(district_code) == 4 ~ paste0("00", district_code),
                               nchar(district_code) == 5 ~ paste0("0", district_code),
                               nchar(district_code) == 6 ~ district_code)] |>
  _[, .(state_leaid, name, school_year, fdsw)]



# ----------------- (2) Loading all buildings/districts in Missouri and getting count of buildings by district/year ----
# Path to downloaded DESE data file
frl_file = "00_raw_data/Free and Reduced Priced Lunch Percentage by Building 2023-24 to 2009-10.xlsx"

# Empty list to hold data
frl = vector("list", length = 15)

## Using loops to read in data
# 2009-2010 through 2013-2014 observations in the excel file have data beginning on line 16
sheets = c("2009-2010", "2010-2011", "2011-2012", "2012-2013", "2013-2014")

for (s in 1:length(sheets)) {
  
  frl[[s]] = read_excel(frl_file, sheet = sheets[s], skip = 15) |>
    as.data.table() 
  
  # Removing the 7th column (not needed)
  frl[[s]] = frl[[s]] |>
    _[, 7 := NULL]
  
  # Adding in the school year
  frl[[s]] = frl[[s]] |>
    _[, school_year := as.numeric(substr(sheets[s], 1, 4))]
  
  # Naming columns (for consistency when appending later)
  setnames(frl[[s]], c("state_leaid", "district_name", "building_no", "building_name", "enrollment", "frl", "school_year"))
  
}

# 2014-2015 through 2023-2024 observations have data beginning on line 10
sheets = c("2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019", 
      "2019-2020", "2020-2021", "2021-2022", "2022-2023", "2023-2024")

for (s in 1:length(sheets)) {
  
  frl[[s+5]] = read_excel(frl_file, sheet = sheets[s], skip = 9) |>
    as.data.table() 
  
  # Removing the 7th and 8th columns (not needed)
  frl[[s+5]] = frl[[s+5]] |>
    _[, 7:8 := NULL]

  # Adding in the school year
  frl[[s+5]] = frl[[s+5]] |>
    _[, school_year := as.numeric(substr(sheets[s], 1, 4))]
  
  # Naming columns (for consistency when appending later)
  setnames(frl[[s+5]], c("state_leaid", "district_name", "building_no", "building_name", "frl", "enrollment", "school_year"))
  
}

## Combining together and some light cleaning
frl = rbindlist(frl, fill = TRUE) |>
  # Converting state leaid to a character with length 6
  _[, state_leaid := as.character(state_leaid)] |>
  _[, state_leaid := case_when(nchar(state_leaid) == 4 ~ paste0("00", state_leaid),
                               nchar(state_leaid) == 5 ~ paste0("0", state_leaid),
                               nchar(state_leaid) == 6 ~ state_leaid)] |>
  # Keeping only observations with a positive student enrollment
  _[enrollment > 0] |>
  # And only observations with a valid state_leaid
  _[nchar(state_leaid) == 6]

# Ordering rows/columns
setorder(frl, state_leaid, school_year, building_no)
setcolorder(frl, c("state_leaid", "school_year", "district_name", "building_no", "building_name", "enrollment", "frl"))

## Cleaning up
rm(frl_file, sheets, s)



# -------------------------------------------------------------------------- (3) Now getting staff counts over time ----
# Path to downloaded DESE data file
fte_file = "00_raw_data/District Faculty Information.xlsx"

# Reading in data
fte = read_excel(fte_file) |>
  clean_names() |>
  as.data.table()

# Renaming some columns
colnames(fte)[1:2] = c("school_year", "state_leaid")

# Keeping years after 2009-2010
fte = fte |>
  _[school_year >= 2009]

# Keeping just useful variables
fte = fte |>
  _[, .(state_leaid, school_year, administrator_fte, administrator_salary_average,
        teacher_fte, teacher_salary_avg_reg_term, teacher_average_years_exp, teacher_mast_degree_percent)]

# Renaming a few more variables
colnames(fte)[4] = "admin_salary_avg"
colnames(fte)[6:8] = c("teacher_salary_avg", "teacher_avg_exp", "teacher_mast_pct")



# ------------------------------------------------------------------------------------ (4) Merge all files together ----
# Initial merge
schools = merge(frl, dt, by = c("state_leaid", "school_year"), all.x = TRUE) |>
  merge(fte, by = c("state_leaid", "school_year"), all.x = TRUE)

# Cleaning up
rm(frl, dt, fte)

# Cleaning up the FDSW variable
schools = schools |>
  _[, fdsw := ifelse(is.na(fdsw), FALSE, fdsw)]

# Imposing a "once treated always treated" classification to FDSW status
schools = schools |>
  _[, first_fdsw := ifelse(fdsw == TRUE, school_year, NA_integer_)] |>
  _[, first_fdsw := min(first_fdsw, na.rm = TRUE), by = state_leaid] |>
  _[, fdsw := case_when(is.na(first_fdsw) ~ fdsw,
                        school_year >= first_fdsw & fdsw == TRUE ~ fdsw,
                        school_year >= first_fdsw & fdsw == FALSE ~ TRUE,
                        TRUE ~ fdsw)]



# --------------------------------------------------------------- (5) Building district-level data set (for graphs) ----
districts = schools |>
  _[, .(enrollment = sum(enrollment, na.rm = TRUE),
        frl = sum(frl, na.rm = TRUE),
        admin_fte = mean(administrator_fte, na.rm = TRUE),
        admin_salary_avg = mean(admin_salary_avg, na.rm = TRUE),
        teacher_fte = mean(teacher_fte, na.rm = TRUE),
        teacher_salary_avg = mean(teacher_salary_avg, na.rm = TRUE),
        teacher_avg_exp = mean(teacher_avg_exp, na.rm = TRUE),
        teacher_mast_pct = mean(teacher_mast_pct, na.rm = TRUE),
        fdsw = max(fdsw),
        first_fdsw = min(first_fdsw),
        number_of_schools = .N), 
    by = .(state_leaid, school_year)] |>
  # Replacing NaN values with NAs
  _[, admin_fte := ifelse(is.nan(admin_fte), NA, admin_fte)] |>
  _[, admin_salary_avg := ifelse(is.nan(admin_salary_avg), NA, admin_salary_avg)] |>
  _[, teacher_fte := ifelse(is.nan(teacher_fte), NA, teacher_fte)] |>
  _[, teacher_salary_avg := ifelse(is.nan(teacher_salary_avg), NA, teacher_salary_avg)] |>
  _[, teacher_avg_exp := ifelse(is.nan(teacher_avg_exp), NA, teacher_avg_exp)] |>
  _[, teacher_mast_pct := ifelse(is.nan(teacher_mast_pct), NA, teacher_mast_pct)]


# Saving out data set for later reference
saveRDS(districts, "01_clean_data/district_fdsw_status.rds")




# ------------------------------------------------------------------------------------ (6) Growth of 4DSW over time ----
# Starting with the district level data, collapsing down to FDSW == 1 or 0 by year
t = districts |>
  _[, .(number_of_districts = .N,
        number_of_schools = sum(number_of_schools),
        enrollment = sum(enrollment, na.rm = TRUE),
        teacher_fte = sum(teacher_fte,na.rm = TRUE)),
    by = .(school_year, fdsw)] |>
  # Rounding enrollment and FTE to the nearest whole number
  _[, enrollment := round(enrollment)] |>
  _[, teacher_fte := round(teacher_fte)] |>
  # Adding in a total by year for both groups 
  _[, total_districts := sum(number_of_districts), by = .(school_year)] |>
  _[, total_schools := sum(number_of_schools), by = .(school_year)] |>
  _[, total_enrollment := sum(enrollment), by = .(school_year)] |>
  _[, total_teachers := sum(teacher_fte), by = .(school_year)] |>
  # Keeping FDSW Observations
  _[fdsw == 1] |>
  # Adding in some percents
  _[, percent_districts := number_of_districts / total_districts] |>
  _[, percent_schools := number_of_schools / total_schools] |>
  _[, percent_enrollment := enrollment / total_enrollment] |>
  _[, percent_teachers := teacher_fte / total_teachers] |>
  # Renaming and dropping extra columns
  _[, number_districts := number_of_districts] |>
  _[, number_schools := number_of_schools] |>
  _[, number_enrollment := enrollment] |>
  _[, number_teachers := teacher_fte] |>
  _[, -c("total_districts", "total_schools", "total_enrollment", "total_teachers",
         "number_of_districts", "number_of_schools", "enrollment", "teacher_fte", "fdsw")] |>
  # Reshaping to long
  melt(id.vars = c("school_year"),
       measure.vars = patterns("^number_*", "^percent_*"),
       verbose = FALSE) |>
  # Manually replacing variable with what the numeric values represent
  _[, variable := case_when(variable == 1 ~ "districts",
                            variable == 2 ~ "schools",
                            variable == 3 ~ "students",
                            variable == 4 ~ "teachers")] |>
  # Adding labels for each group
  _[, label_districts := ifelse(school_year == max(school_year) & variable == "districts", 
                                paste(scales::comma(value1), "districts"), "")] |> 
  _[, label_schools := ifelse(school_year == max(school_year) & variable == "schools", 
                              paste(scales::comma(value1), "schools"), "")] |>
  _[, label_students := ifelse(school_year == max(school_year) & variable == "students", 
                               paste(scales::comma(value1), "students"), "")] |>
  _[, label_teachers := ifelse(school_year == max(school_year) & variable == "teachers", 
                               paste(scales::comma(value1), "teachers"), "")] 

|>
  # Now graphing
  ggplot(aes(x = school_year, y = value2, color = variable, group = variable)) +
  annotate("text", x = 2019.5, y = 0.235, label = "COVID-19", hjust = 0.5, vjust = 0, 
           family = "roboto", size = 3, color = "#424242") +
  geom_segment(aes(x = 2019.5, xend = 2019.5, y = 0, yend = 0.23), 
               linetype = "dashed", linewidth = 0.25, color = "#424242") +
  geom_point(show.legend = FALSE) +
  geom_line(linewidth = 1) +
  geom_text(aes(label = label_districts), hjust = 0, nudge_x = 0.1, 
            family = "roboto", fontface = "bold", size = 5, show.legend = FALSE) +
  geom_text(aes(label = label_schools), hjust = 0, nudge_x = 0.1,
            family = "roboto", fontface = "bold", size = 5, show.legend = FALSE) +
  geom_text(aes(label = label_students), hjust = 0, nudge_x = 0.1, nudge_y = -0.005, 
            family = "roboto", fontface = "bold", size = 5, show.legend = FALSE) +
  geom_text(aes(label = label_teachers), hjust = 0, nudge_x = 0.1, nudge_y = 0.005, 
            family = "roboto", fontface = "bold", size = 5, show.legend = FALSE) +
  scale_y_continuous(breaks = seq(0, 0.30, 0.05), labels = scales::percent) +
  scale_x_continuous(breaks = seq(2010, 2023, 1), labels = function(x) {paste0(x, "-", (x-1999))}) +
  scale_color_manual(values = line_pal) +
  expand_limits(x = 2024) +
  labs(caption = "Note: Adopting districts are considered always treated.") +
  theme_minimal() +
  xlab("School Year") +
  ylab("Percent of Total") +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(family = "roboto", size = 12),
        axis.title = element_text(size = 12, color = "#424242"),
        axis.text = element_text(size = 12, color = "black"))
ggsave("04_figures/001a_fdsw_growth.svg", width = 13, height = 7)



# --------------------------------------------------------------------------------------- (7) Map of FDSW districts ----
# Loading shapefile for Missouri school districts
modist = read_sf("03_resources/MO_Public_School_Districts.geojson")

# Some pre-processing of the districts data.table to have district code (ID) and adoption cohorts for fill
dt = districts |>
  filter(school_year == max(school_year)) |>
  mutate(cohort = case_when(first_fdsw <= 2019 ~ "Pre-COVID",
                            first_fdsw <= 2022 ~ "During COVID",
                            first_fdsw <= 2023 ~ "Post-COVID",
                            TRUE ~ "Non-Adoptors"),
         cohort = factor(cohort, levels = c("Non-Adoptors", "Pre-COVID", "During COVID", "Post-COVID"))) |>
  select("DIST_CODE" = state_leaid, cohort) 
  
# Merging the two
modist = modist |>
  left_join(dt)

# Mapping
modist |>
  ggplot(aes(fill = cohort)) +
  geom_sf(color = "#424242") +
  scale_fill_manual(values = map_pal) +
  labs(caption = "Note: Adopting districts are considered always treated.",
       fill = "Adoption Cohort") +
  theme_void() +
  theme(text = element_text(family = "roboto", size = 12))
ggsave("04_figures/001b_fdsw_map_2022.svg", width = 13, height = 7)



