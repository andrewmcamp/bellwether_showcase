# ------------------------------------------------------------------------------------------ (0) Preamble ----
# Loading packages
library(shiny)
library(shinyjs)
library(bslib)
library(readxl)
library(writexl)
library(janitor)
library(tidyverse)
library(fixest)

# Options
options(shiny.maxRequestSize=100*1024^2)

# Calling other scripts for components
#source("fileInputArea.R")

# ---------------------------------------------------------------- (1) Functions to actually estimate VAS ----
# Preparing the data for estimation
prep_data <- function(dta = data, focal_year = 2023) {
  
  dta = dta |>
    # Cleaning the variable names
    clean_names() |>
    # Standardizing test scores within year/grade/subject/test_name
    group_by(year, grade, subject, test_name) |>
    add_count(name = "std_cell_size") |>
    filter(std_cell_size > 29) |>
    mutate(scale_mean = mean(scale_score),
           scale_sd = sd(scale_score),
           z_score = (scale_score - scale_mean)/scale_sd) |>
    filter(abs(z_score) < 10) |>
    select(-std_cell_size, -scale_mean, -scale_sd) |>
    ungroup() |>
    # Going to 1 obs per student/year and removing retained students
    group_by(studentid, year, subject) |>
    add_count(name = "stu_year_grd_subj_count") |>
    filter(stu_year_grd_subj_count == 1) |>
    select(-stu_year_grd_subj_count) |>
    ungroup() |>
    group_by(studentid, subject) |>
    arrange(studentid, subject, desc(year), desc(grade)) |>
    mutate(retained_flag = ifelse(lead(grade) == grade, 1, 0),
           skipped_year_flag = ifelse(lead(year) != (year - 1), 1, 0)) |>
    filter(retained_flag == 0 | is.na(retained_flag))
  
  # Need to expand the data frame to include "missing" years for those who re-enter
  subject = unique(dta$subject)
  studentid = unique(dta$studentid)
  year = unique(dta$year)
  
  expander = crossing(year, subject) |>
    crossing(studentid)
  
  dta = expander |>
    left_join(dta, by = c("year", "subject", "studentid")) |>
    group_by(studentid, subject) |>
    arrange(studentid, subject, desc(year), desc(grade))
  rm(expander, subject, studentid, year)
  
  # Adding up to two lags of the prior year test scores
  dta = dta |>
    mutate(lag1_z_score = lead(z_score),
           lag2_z_score = lead(z_score, 2)) |>
    ungroup()
  
  # Keeping just the 2023 observations
  dta = dta |>
    filter(year == focal_year, !is.na(z_score))
  
  length(unique(dta$studentid))
  length(unique(dta$teacherid))
  
  # Reshaping to wide to include lags for each subject
  dta = dta |>
    select(-scale_score, -test_name, -calc_norm, -retained_flag, -skipped_year_flag) |>
    mutate(subject = tolower(subject)) |>
    pivot_wider(id_cols = c("year", "grade", "studentid", "standardized_ethnicity",
                            "frl", "mobility_flag", "schoolid"),
                names_from = subject,
                values_from = c("teacherid", "z_score", "lag1_z_score", "lag2_z_score"),
                names_glue = "{subject}_{.value}") |>
    mutate(has_ela_z = ifelse(is.na(ela_z_score), 0, 1),
           has_math_z = ifelse(is.na(math_z_score), 0, 1),
           has_ela_lag1 = ifelse(is.na(ela_lag1_z_score), 0, 1),
           has_math_lag1 = ifelse(is.na(math_lag1_z_score), 0, 1),
           has_ela_lag2 = ifelse(is.na(ela_lag2_z_score), 0, 1),
           has_math_lag2 = ifelse(is.na(math_lag2_z_score), 0, 1),
           ela_lag1_z_score = ifelse(is.na(ela_lag1_z_score), 0, ela_lag1_z_score),
           math_lag1_z_score = ifelse(is.na(math_lag1_z_score), 0, math_lag1_z_score),
           ela_lag2_z_score = ifelse(is.na(ela_lag2_z_score), 0, ela_lag2_z_score),
           math_lag2_z_score = ifelse(is.na(math_lag2_z_score), 0, math_lag2_z_score))
  
  # School-level covariates
  dta = dta |>
    group_by(year, schoolid) |>
    mutate(school_frl = mean(frl),
           school_mob = mean(mobility_flag),
           school_mth = mean(math_z_score, na.rm = TRUE),
           school_ela = mean(ela_z_score, na.rm = TRUE)) |>
    ungroup() |>
    group_by(year, math_teacherid) |>
    add_count(name = "nstu") |>
    mutate(teacher_frl = (sum(frl) - frl)/(nstu-1),
           teacher_mob = (sum(mobility_flag) - mobility_flag)/(nstu-1),
           teacher_mth = (sum(math_z_score, na.rm = TRUE) - math_z_score)/(nstu-1),
           teacher_ela = (sum(ela_z_score, na.rm = TRUE) - ela_z_score)/(nstu-1),
           teacher_has_math_lag1 = (sum(has_math_lag1, na.rm = TRUE) - has_math_lag1)/(nstu-1),
           teacher_has_math_lag2 = (sum(has_math_lag2, na.rm = TRUE) - has_math_lag2)/(nstu-1),
           teacher_has_ela_lag1 = (sum(has_ela_lag1, na.rm = TRUE) - has_ela_lag1)/(nstu-1),
           teacher_has_ela_lag2 = (sum(has_ela_lag2, na.rm = TRUE) - has_ela_lag2)/(nstu-1)) |>
    ungroup()
  
  # Returning prepared data
  return(dta)
    
}

# Actually estimating the value add scores
est_vam_models <- function(dta = data, eb = "Yes", rescale = "Yes", rs_mean = 80, rs_sd = 5) {
  
  # Estimating the models
  vam_math = feols(math_z_score ~ 
                     has_math_lag1 + has_math_lag2 +
                     has_math_lag1:math_lag1_z_score + has_math_lag2:math_lag2_z_score +
                     has_ela_lag1 + has_ela_lag2 +
                     has_ela_lag1:ela_lag1_z_score + has_ela_lag2:ela_lag2_z_score +
                     frl + mobility_flag + grade +
                     school_frl + school_mob + school_mth + school_ela + 
                     teacher_has_math_lag1 + teacher_has_math_lag2 +
                     teacher_has_ela_lag1 + teacher_has_ela_lag2, dta, 
                   warn = FALSE, notes = FALSE)
  
  vam_ela = feols(ela_z_score ~ 
                    has_math_lag1 + has_math_lag2 +
                    has_math_lag1:math_lag1_z_score + has_math_lag2:math_lag2_z_score +
                    has_ela_lag1 + has_ela_lag2 +
                    has_ela_lag1:ela_lag1_z_score + has_ela_lag2:ela_lag2_z_score +
                    frl + mobility_flag + grade +
                    school_frl + school_mob + school_mth + school_ela + 
                    teacher_has_math_lag1 + teacher_has_math_lag2 +
                    teacher_has_ela_lag1 + teacher_has_ela_lag2, dta, 
                  warn = FALSE, notes = FALSE)
  
  # Predicting student test scores
  dta = dta |>
    modelr::add_predictions(vam_math, var = "pred_math_z_score") |>
    modelr::add_predictions(vam_ela, var = "pred_ela_z_score")
  
  # Get residuals
  dta = dta |>
    mutate(math_resid = math_z_score - pred_math_z_score,
           ela_resid = ela_z_score - pred_ela_z_score)
  rm(vam_math, vam_ela)
  gc()
  
  # Regressing residuals on teacherid
  math_vas = feols(math_resid ~ i(math_teacherid), dta, warn = FALSE, notes = FALSE)
  ela_vas = feols(ela_resid ~ i(ela_teacherid), dta, warn = FALSE, notes = FALSE)
  
  # Getting teacher coefficients and standard errors
  math_tva = data.frame(
    teacherid = names(summary(math_vas)$coefficients),
    unadjusted_vas = unname(summary(math_vas)$coefficients),
    vas_se = unname(summary(math_vas)$se)) |>
    mutate(teacherid = as.numeric(substr(teacherid, 17, nchar(teacherid)))) |>
    filter(!is.na(teacherid)) |>
    ungroup()
  
  ela_tva = data.frame(
    teacherid = names(summary(ela_vas)$coefficients),
    unadjusted_vas = unname(summary(ela_vas)$coefficients),
    vas_se = unname(summary(ela_vas)$se)) |>
    mutate(teacherid = as.numeric(substr(teacherid, 16, nchar(teacherid)))) |>
    filter(!is.na(teacherid)) |>
    ungroup()
  
  ## Standardizing the value add scores to have mean 0 and SD 1
  math_tva = math_tva |>
    mutate(unadjusted_vas = (unadjusted_vas - mean(unadjusted_vas))/sd(unadjusted_vas))
  
  ela_tva = ela_tva |>
    mutate(unadjusted_vas = (unadjusted_vas - mean(unadjusted_vas))/sd(unadjusted_vas))
  
  ## If Empirical Bayes Shrinkage is specified, then shrinking estimates
  if (eb == "Yes") {
    incProgress(0.1, detail = "Applying shrinkage")
    # Using the population to get the prior and variance parameters
    prior_math = 0
    prior_ela = 0
    var_math = sd(math_tva$unadjusted_vas, na.rm = TRUE)^2
    var_ela = sd(ela_tva$unadjusted_vas, na.rm = TRUE)^2
    
    # Applying the shrinkage
    math_tva = math_tva |>
      mutate(a_j = (var_math/(var_math + vas_se^2)),
             adjusted_vas = (a_j * unadjusted_vas) + (1-a_j) * prior_math) |>
      select(-a_j)
    
    ela_tva = ela_tva |>
      mutate(a_j = (var_ela/(var_ela) + vas_se^2),
             adjusted_vas = (a_j * unadjusted_vas) + (1-a_j) * prior_ela) |>
      select(-a_j)
  }
  
  ## If re-scaling is specified, then re-scaling
  if (rescale == "Yes") {
    incProgress(0.1, detail = "Rescaling estimates")
    math_tva = math_tva |>
      mutate(rescaled_vas = (adjusted_vas * rs_sd) + rs_mean)
    
    ela_tva = ela_tva |>
      mutate(rescaled_vas = (adjusted_vas * rs_sd) + rs_mean)
  }
  
  # Getting the average student characteristics for each teacher and subject
  ela_cov = dta |>
    #filter(complete_cases_ela == 1) |>
    mutate(white = ifelse(standardized_ethnicity == "WHITE OR CAUCASIAN", 1, 0),
           urm = case_when(standardized_ethnicity == "BLACK OR AFRICAN-AMERICAN" ~ 1,
                           standardized_ethnicity == "HISPANIC OR LATINO" ~ 1,
                           standardized_ethnicity == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ 1,
                           standardized_ethnicity == "AMERICAN INDIAN OR ALASKA NATIVE" ~ 1,
                           TRUE ~ 0)) |>
    group_by(ela_teacherid) |>
    summarise(per_frl = mean(frl, na.rm = TRUE),
              per_mob = mean(mobility_flag, na.rm = TRUE),
              per_white = mean(white, na.rm = TRUE),
              per_urm = mean(urm, na.rm = TRUE)) |>
    select("teacherid" = ela_teacherid, everything())
  
  math_cov = dta |>
    #filter(complete_cases_math == 1) |>
    mutate(white = ifelse(standardized_ethnicity == "WHITE OR CAUCASIAN", 1, 0),
           urm = case_when(standardized_ethnicity == "BLACK OR AFRICAN-AMERICAN" ~ 1,
                           standardized_ethnicity == "HISPANIC OR LATINO" ~ 1,
                           standardized_ethnicity == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ 1,
                           standardized_ethnicity == "AMERICAN INDIAN OR ALASKA NATIVE" ~ 1,
                           TRUE ~ 0)) |>
    group_by(math_teacherid) |>
    summarise(per_frl = mean(frl, na.rm = TRUE),
              per_mob = mean(mobility_flag, na.rm = TRUE),
              per_white = mean(white, na.rm = TRUE),
              per_urm = mean(urm, na.rm = TRUE)) |>
    select("teacherid" = math_teacherid, everything())
  
  # Now combining the covariates with the value add scores
  math_tva = math_tva |>
    left_join(math_cov, by = "teacherid")
  
  ela_tva = ela_tva |>
    left_join(ela_cov, by = "teacherid")
  
  # Changing all variable names to upper case to match template/provided data
  math_tva = math_tva |>
    rename_all(toupper)
  
  ela_tva = ela_tva |>
    rename_all(toupper)
  
  ## Putting together and returning
  results = list("Math TVA" = math_tva, "ELA TVA" = ela_tva)
  return(results)
  
}

# Combining the two functions above
prep_and_est <- function(dta = data, focal_year = 2023, eb = TRUE, rescale = TRUE, rs_mean = 80, rs_sd = 5) {
  
  incProgress(0.25, detail = "Preparing data")
  dta = prep_data(dta, focal_year)
  
  incProgress(0.5, detail = "Estimating models")
  results = est_vam_models(dta, eb, rescale, rs_mean, rs_sd)
  
  return(results)
  
}

# ---------------------------------------------------------------------- (2) Building out page components ----
# Contents for the home/landing page
home_page <- div(
  h2("Welcome to the [XYZ] Value-Add Model Estimation App"),
  p("This app allows you to estimate value-add scores for teachers using data from [XYZ] powered schools."),
  p("To get started, click on the 'Data' tab above to upload your data. Next, select any estimation options",
    "that you want to include in your model in the `Options` tab.", 
    "Finally, click on the 'Estimate' tab to run the model and download")
)


# Making a contents for a page that allows you to upload a csv/xlsx file and then see the contents of that file
select_data_page <- div(
  h2("Select Data"),
  tags$hr(),
  # Write a short intro on what the page is for
  p("Upload a CSV or XLSX file that contains the data you would like to use for estimating value-add scores.",
    a(href="teacher value add data template.xlsx", "Click here to download a template if needed.", 
      download=NA, target="_blank"),),
  
  # File upload dialog
  fluidRow(
    #includeCSS(css_btn_area),
    style = "margin-top: 0px;",
    column(
      width = 12, offset = 0,
      # fileInputArea(
      #   "upload",
      #   label = "Select Data",
      #   buttonLabel = "Either .CSV or .XLSX files are accepted.",
      #   multiple = FALSE,
      #   accept = c(".csv", ".xlsx")
      # )
    )
  ),
)


# Making the contents for the select data page which includes a file upload dialog (CSV/XLSX)
est_options_page <- div(
  h2("Estimation Options"),
  tags$hr(),
  # Two column layout with radio buttons in each column
  fluidRow(
    column(
      width = 6,
      card(
        # Title and button for more information on re-scaling value-add scores
        fluidRow(
          column(width = 10, p("Rescale Value-Add Scores")),
          column(width = 2, actionLink("more_info_rescale", "", icon = icon("question")))
        ),
        # Radio button to re-scale value-add scores
        radioButtons("rescale", label = NULL,
                     choices = c("Yes", "No"),
                     selected = "Yes"),
        # Inputs for new mean and standard deviation of value add scores
        numericInput("new_mean", "Rescaled Mean", value = 80),
        numericInput("new_sd", "Rescaled Standard Deviation", value = 3)
      )
     ),
    column(
      width = 6,
      card(
        # Title and button for more information on re-scaling value-add scores
        fluidRow(
          column(width = 10, p("Empirical Bayes Shrinkage")),
          column(width = 2, actionLink("more_info_bayes", "", icon = icon("question")))
        ),
        # Radio button to select whether to perform Empirical Bayes Shrinkage
        radioButtons("shrinkage", label = NULL,
                     choices = c("Yes", "No"),
                     selected = "Yes")),
        card(
          # Numeric input for which year to estimate value-add scores for
          fluidRow(
            column(width = 10, p("Year to Estimate Value-Add For")),
            column(width = 2, actionLink("more_info_year", "", icon = icon("question"))),
          ),
          numericInput("yr", label = NULL, 
                       value = 2023, min = 2000, max = 2050)
        )
      ),
    )
)

# Making a page that includes a button to estimate the model, a table showing the first 10 rows, and an option to download the file
vas <- div(
  shinyjs::useShinyjs(),
  h2("Estimate Value-Add Scores"),
  tags$hr(),
  p("After you've uploaded the required data and selected options for estimation,",
    "click the button below to estimate value-add scores for the data you uploaded."),
  p("Note that this can take some time depending on the size of the data."),
  
  # Button to estimate the model
  fluidRow(column(12, downloadButton("downloadData", "Estimate Value-Add Scores", width = "100%"))),
  

  
)

# Modal for more information for re-scaling value-add scores
modal_rescale <- modalDialog(
  title = "Rescale Value-Add Scores",
  "Value-add scores returned by the [XYZ] Value-Add Model have a mean near zero and a relatively tight",
  "distribution. These units are not very intuitive for many stakeholders and, so, rescaling value-add",
  "scores to a more familiar scale can be helpful. The defaults (mean = 80 and sd = 3) will result in",
  "value-add scores that are similar to those used in letter grades (e.g., 60-100).",
  easyClose = TRUE
)

# Modal for Empircal Bayes shrinkage
modal_shrinkage <- modalDialog(
  title = "Empirical Bayes Shrinkage",
  "Empirical Bayes shrinkage is a method for improving the accuracy of estimates by borrowing information",
  "across units. In the context of value-add models, shrinkage can be used to reduce the impact of extreme",
  "value-add scores that are likely due to random error. This can help to reduce the volatility of value-add",
  "scores and make them more stable over time.",
  easyClose = TRUE
)

# Modal for year to estimate scores for
modal_select_year <- modalDialog(
  title = "Select Year",
  "Select the year for which you would like to estimate value-add scores. Note that up to two years of lagged",
  "scores are used in the model, so you may want to select a year that is at least two years after the first year",
  "that you have data for. Additionally, you cannot estimate value-add models if there are no prior years available.",
  easyClose = TRUE
)

# Modal for minimum number of students to include
modal_min_n <- modalDialog(
  title = "Minimum Number of Students",
  "Select the minimum number of students that a teacher should have assigned to them in a given school year",
  "to be included in value-add score estimation. Teachers with fewer students than this number will be excluded.",
  "The default is 10 students.",
  easyClose = TRUE
)


# Modal if no lags are available
modal_no_lags <- modalDialog(
  title = "Error: No Lags Available",
  "The year you have selected has no lags available. Check your data and the selected year.",
  easyClose = TRUE
)



# ------------------------------------------------------------------------------------ (3) Calling the UI ----
ui <- navbarPage(
  header =
    tags$head(
      tags$style(
        "body{
    min-height: 611px;
    height: auto;
    max-width: 800px;
    margin: auto;
        }"
      )
    ),
  
  # Setting theme
  theme = bs_theme(bootswatch = "lux",
                   # Setting some colors to correspond to [XYZ] K-12's website
                   primary = "#2127c3", secondary = "#000f42", success = "#125219", info = "#57bb8a"),
  
  # Title of the app
  title = "[XYZ] K-12 VAM",
  
  # Panels (pages)
  tabPanel("Home", home_page),
  tabPanel("Data", select_data_page),
  tabPanel("Options", est_options_page),
  tabPanel("Estimate", vas),
  
)


# -------------------------------------------------------------------------------- (4) Calling the Server ----

server <- function(input, output, session) {

  ## General setup
  # Disable the estimate button until the data has been uploaded
  shinyjs::disable("downloadData")
  
  ## Data upload page
  # Enabling the estimate button once the data has been uploaded
  observeEvent(input$upload$datapath, {enable("downloadData")})

  
  
  
  ## Estimation options page
  # Modal for rescaling value add scores
  observeEvent(input$more_info_rescale, {
    showModal(modal_rescale)
  })
  
  # Modal for empircal bayes shrinkage
  observeEvent(input$more_info_bayes, {
    showModal(modal_shrinkage)
  })
  
  # Modal for select year
  observeEvent(input$more_info_year, {
    showModal(modal_select_year)
  })
  
  # Modal for min number of students
  observeEvent(input$more_info_min_n, {
    showModal(modal_min_n)
  })
  
  ## Estimation page
  # Actually estimating everything :)
  vas <- reactive({
    # Requiring things...
    req(input$upload)
    
    # Doing the stuff with some progress updates
    withProgress(
      message = "Working:",
      detail = "Loading data",
      value = 0,
      {
        # Loading the data
        inFile <- input$upload
        if (is.null(inFile)) {
          return(NULL)
        }
        ext <- tools::file_ext(inFile$datapath)
        data = switch(
          ext,
          csv = read.csv(inFile$datapath, header = TRUE),
          xlsx = read_excel(inFile$datapath, sheet = 1)
        )
        
        # Now calling the rest of the functions
        results <- prep_and_est(data, input$yr, input$shrinkage, input$rescale, input$new_mean, input$new_sd)
        return(results)
      }
    )
   
  })
  
  # Download processed data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("teacher_value_added_estimates.xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(vas(), path = file)
    }
  )
  
  
  ## General close down
  # Function to stop the app  
  session$onSessionEnded(function() {
    stopApp()
  })
  
}

shinyApp(ui = ui, server = server)
