# ========================================================
# NYC Expense Budget Analysis (R Coding Sample)
# Author: Kavita Rani
# Date: 2026-04-22
# ========================================================

#-----------------INTRODUCTION----------------------
#This coding sample analyzes the New York City Expense Budget dataset using R.  
# The goal is to examine spending patterns in the FY2027 Preliminary Budget and compare them with prior Executive and Enacted budgets.

# This analysis is based on publicly available NYC Open Data.  
# The code reflects workflows developed through professional experience in fiscal policy analysis.
 
# The workflow includes data exploration, cleaning, transformation, and comparative analysis across budget stages.

# Expense Budget: (Different than Modified Analysis)
# <https://data.cityofnewyork.us/City-Government/Expense-Budget/mwzb-yiwb/about_data>



# --------------------- 1. Load Libraries ---------------------
library("tidyverse")
library("RSocrata")
library(here)
library (lubridate) # for working with date time
library(scales)   

# --------------------- 2. Setup Paths ---------------------
options(scipen = 999) 


# --------------------- 3. Custom Function ---------------------

# Function: clean_hyphen_spacing
# Purpose: Standardize spacing around hyphens in text fields
#          to avoid duplicate categories during grouping.
# Inputs:
#   df       - input dataframe
#   name_col - text column to clean
# Output:
#   dataframe with standardized hyphen spacing
clean_hyphen_spacing <- function(df, name_col) {
  df <- df %>%
    mutate(
      {{name_col}} := str_squish({{name_col}}),                    #removes all the white space
      {{name_col}} := str_replace_all({{name_col}}, "\\s*-\\s*", " - ")
    )
  return(df)
}

# --------------------- 4. Data Load ---------------------


#The dataset is sourced from NYC Open Data. Read it using Socrata

# expense_token <- "WbjwXpdsDEDzBiHlc9S5zrbzc"

# expense <- read.socrata(
#   "https://data.cityofnewyork.us/resource/mwzb-yiwb.json", 
#   app_token = expense_token)
# write.csv(expense, file = file.path(data_path, "expense.csv"))


# Load from local file 
expense_file <- read.csv("expense.csv")


# --------------------- 5. Data Exploration ---------------------

nrow(expense_file) #check number of row
str(expense_file)  # check the structure of the file
range(expense_file$publication_date)


# --------------------- 6. Data Cleaning & Preparation ---------------------

#change publication_date as Date datatype, and adding month, year and day column
expense_file <- expense_file %>%
  mutate(
    publication_date = ymd(publication_date),
    Year = year(publication_date),
    Month = month(publication_date),
    Day = day(publication_date)
  ) 

#relocating month, year and date column after publication_date
expense_file <- expense_file %>%
  relocate(Year, .after= publication_date) %>%
  relocate(Month, .after=Year) %>%
  relocate(Day, .after = Month)


#Verify,this should now show 2026-02-17
str(expense_file$publication_date)
class(expense_file$publication_date)



# --------------------- 7. Budget Selection ---------------------
#The analysis focuses on the most recent budget cycle, from the FY2026 Preliminary Budget through the FY2027 Preliminary Budget.

# extract publication_date for preliminary, exec, enacted budget
pub_date <- expense_file %>% 
  pull(publication_date) %>%    
  unique() %>%               # Remove duplicates
  sort(decreasing = TRUE)   #Extract date
top_date<- pub_date[1:6]
top_date
latest_pub_date<- top_date[1] #prelims_budget_2027
second_pub_date<- top_date[2] #enacted_budget_2026
last_pub_date<- top_date[4]   #prelims_budget_2026


recent_budget <- expense_file %>%
  filter(between(publication_date,last_pub_date,latest_pub_date)) %>% #Budget data from prelims 2026 to prelims 2027
  arrange(desc(publication_date))


# --------------------- 8. Budget Classification ---------------------

# Label records as Preliminary, Executive, or Enacted Budget based on the publication month
recent_budget <- recent_budget %>%
  mutate(
    budget_publication = case_when(
      Month == 1  ~ "Preliminary Budget",
      Month == 2 ~ "Preliminary Budget",
      Month == 4 ~ "Executive Budget",
      Month == 5 ~ "Executive Budget",
      Month == 6 ~ "Enacted Budget",
      Month == 7 ~ "Enacted Budget",
      TRUE ~ "Other"
    )
  ) %>%
  relocate(budget_publication, .before = publication_date)

unique(recent_budget$budget_publication) # check the output


# --------------------- 9. Text Cleaning ---------------------

str(recent_budget$unit_appropriation_name) #check the structure
head(recent_budget$unit_appropriation_name, 20) #check first 20 data
sum(is.na(recent_budget$unit_appropriation_name)) #check for NA

#get all the unique name for unit_appropriation_name
raw_names <- recent_budget %>%
  distinct(unit_appropriation_name) %>% arrange(unit_appropriation_name)
View(raw_names)

#check all the names with hyphen in it
hyphen_names <- raw_names %>%
  filter(str_detect(unit_appropriation_name, "-")) 
View(hyphen_names)


recent_budget <- clean_hyphen_spacing(recent_budget,unit_appropriation_name) #call function for cleaning the space

#check the names again
raw_names <- recent_budget %>%
  distinct(unit_appropriation_name) %>% arrange(unit_appropriation_name) %>%
  filter(str_detect(unit_appropriation_name, "-"))
View(raw_names)


#check agency_name 
str(recent_budget$agency_name)
head(recent_budget$agency_name, 20)
sum(is.na(recent_budget$agency_name))

#get all the unique name for unit_appropriation_name
raw_names_agency <- recent_budget %>%
  distinct(agency_name) %>% arrange(agency_name)
View(raw_names_agency)

recent_budget <- clean_hyphen_spacing(recent_budget, agency_name) #call function for cleaning the space



# --------------------- 10. Preliminary Budget 2027 Analysis ---------------------
#summarizes total planned spending and compares it with current modified and adopted budgets.

prelims_budget_2027 <- recent_budget %>%
  filter(publication_date == latest_pub_date) #displays the latest budget

summary_2027 <- prelims_budget_2027 %>%
  summarise(
    total_financial_plan    = sum(financial_plan_amount, na.rm = TRUE),
    total_current_modified  = sum(current_modified_budget_amount, na.rm = TRUE),
    total_adopted_budget    = sum(adopted_budget_amount, na.rm = TRUE),
    total_plan_change       = sum(financial_plan_amount - current_modified_budget_amount, na.rm = TRUE),
    percent_change          = total_plan_change / total_current_modified * 100
  )

summary_2027

# --------------------- 11. Agency-Level Analysis ---------------------
#Spending is analyzed across agency to identify major drivers of budget changes.
agency_summary_2027 <- prelims_budget_2027 %>%
  group_by(agency_name) %>%                         #group_by agency_name and then summarize and arrange it in descending order
  summarise(
    financial_plan   = sum(financial_plan_amount, na.rm = TRUE),
    current_modified = sum(current_modified_budget_amount, na.rm = TRUE),
    plan_change      = financial_plan - current_modified,
    percent_change   = if_else(current_modified == 0, NA_real_, 
                               plan_change / current_modified * 100),
    .groups = "drop"
  ) %>%
  arrange(desc(financial_plan))

agency_summary_2027

# --------------------- 12. Top Increases & Decreases ---------------------
#Identify top increases and decreases by expense category
top_increases_2027 <- agency_summary_2027 %>%
  arrange(desc(plan_change)) %>%
  slice_head(n = 10)

top_decreases_2027 <- agency_summary_2027 %>%
  arrange(plan_change) %>%
  slice_head(n = 10)


# --------------------- 13. Budget Comparison ---------------------

# Agency example: DCAS comparison at the unit appropriation level.
# Create one comparison table: Preliminary vs Enacted and Executive
dcas_comparison_table <- recent_budget %>%
  filter(agency_name == "DEPARTMENT OF CITYWIDE ADMIN SERVICE") %>%
  group_by(unit_appropriation_name, budget_publication) %>%
  summarise(financial_plan_amount = sum(financial_plan_amount, na.rm = TRUE), 
            .groups = "drop") %>%
  pivot_wider(
    names_from = budget_publication,
    values_from = financial_plan_amount,   #create a new row from budget_publication and fill the value from respective financial plan amount
    values_fill = 0
  ) %>%
  # compute difference and percentage change between different budget cycle 
  mutate(
    prelim_vs_enacted = `Preliminary Budget` - `Enacted Budget`,
    prelim_vs_executive = `Preliminary Budget` - `Executive Budget`,
    percent_change_prelim_vs_enacted = prelim_vs_enacted / `Enacted Budget` * 100,
    percent_change_prelim_vs_executive = prelim_vs_executive / `Executive Budget` * 100
  ) %>%
  arrange(desc(abs(prelim_vs_enacted))) %>%
  rename(
    `Enacted 2026`    = `Enacted Budget`,
    `Executive 2026`  = `Executive Budget`,
    `Preliminary 2027`= `Preliminary Budget`
  )

# --------------------- 14. Visualizations ---------------------
# Plot 2: Largest Changes (Preliminary 2027 vs Enacted 2026)
dcas_comparison_table %>%
  arrange(desc(abs(prelim_vs_enacted))) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(unit_appropriation_name, prelim_vs_enacted), y = prelim_vs_enacted)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Largest Changes in DCAS Spending (Preliminary vs Enacted)",
    x = "Unit Appropriation",
    y = "Change in Financial Plan Amount ($)"
  )

# --------------------- Conclusion ---------------------
# The analysis shows meaningful shifts in spending priorities between 
# budget stages, particularly in debt service transfers and social services.
