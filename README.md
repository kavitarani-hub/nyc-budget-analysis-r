# NYC Expense Budget Analysis (R Sample)

This repository contains a sample of my work analyzing the New York City Expense Budget using R. The analysis focuses on understanding spending patterns in the FY2027 Preliminary Budget and comparing them with prior Executive and Enacted budgets. 

## Overview

This project demonstrates an end-to-end data workflow, including:
- Data cleaning and transformation
- Standardization of text fields using modular functions
- Filtering and structuring budget data across fiscal cycles
- Aggregation and comparison of financial metrics
- Visualization of key spending changes

The workflow reflects real-world analytical processes used in fiscal policy analysis and reporting.

## Key Features

- **Multi-source data processing:** Handles NYC Open Data budget datasets with multiple publication cycles  
- **Modular function design:** Includes reusable functions (e.g., text standardization) to ensure consistency across analysis  
- **Reproducible workflow:** Structured pipeline from raw data to final outputs  
- **Analytical depth:** Combines high-level summaries with agency-level and unit-level analysis  
- **Visualization:** Uses `ggplot2` to highlight key budget changes  

## Tools & Technologies

- R (tidyverse, dplyr, ggplot2)
- RSocrata (for API-based data access)
- lubridate (date handling)
- scales (formatting)

## Example Analysis

The script includes:
- Agency-level spending summaries for the FY2027 Preliminary Budget  
- Identification of top increases and decreases across agencies  
- A detailed drill-down analysis of the Department of Citywide Administrative Services (DCAS) to examine unit-level budget changes  

## Data

- Source: NYC Open Data (Expense Budget dataset)  
- For demonstration purposes, this repository uses a simplified local dataset (`expense.csv`)  

## Note

This is a simplified and cleaned sample intended to demonstrate workflow structure, coding practices, and analytical approach. It does not include full datasets or production pipelines.

## Author

Kavita Rani  
Data Analyst | R | SQL | Tableau  
