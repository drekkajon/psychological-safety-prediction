# Predicting Workplace Psychological Safety Using Social and Demographic Indicators

## Brief Background

Psychological safety is a critical driver of team effectiveness, employee engagement, and organizational performance. Employees who feel safe to express ideas, take risks, and speak openly without fear of negative consequences are more likely to contribute to innovation and collaboration.

Despite its importance, psychological safety is often treated as a qualitative construct rather than something that can be measured and analyzed using data. This project applies data science methods to explore workplace conditions associated with psychological safety using large-scale survey data.

## Research Question

What workplace conditions are associated with management–employee relationship quality, and what does this suggest about psychological safety?

## Project Evolution

The original goal of this project was to build a predictive model using multiple workplace and demographic predictors. However, due to structural constraints in the General Social Survey (GSS)—specifically the split-ballot design—many variables were not available within the same respondent sample.

As a result, the project was adapted to focus on descriptive analysis of management–employee relationship quality (*bossemps*) as a proxy for psychological safety-related workplace conditions.

## Hypothesis

It is hypothesized that respondents will be more likely to report positive management–employee relationship quality than negative evaluations, reflecting workplace conditions that may align with psychologically safe environments.


## Prediction

The distribution of management–employee relationship quality (*bossemps*) will be skewed toward more positive categories (e.g., “Quite Good” and “Very Good”), with fewer respondents reporting negative relationship experiences.

## Data Source

- General Social Survey (GSS)  
- Multi-year extract to ensure availability of key variables  
- Restricted to employed respondents only  
- Final analytic sample: **N = 850**

## Repository Structure

├── data/ # Raw and processed datasets (if included)
├── scripts/ # R scripts for data cleaning and analysis
├── outputs/ # Graphs, tables, and results
├── reports/ # EDA and written reports
├── README.md # Project overview (this file)


## How to Run This Project

1. Download the dataset from the GSS website or use the provided extract.  
2. Place the dataset in the `/data` folder.  
3. Open the R scripts in the `/scripts` folder.  
4. Run scripts in the following order:
   - data cleaning script  
   - analysis/EDA script  
5. Outputs (tables and visualizations) will be generated in the `/outputs` folder.

## Tools & Libraries

- R  
- tidyverse (dplyr, ggplot2, etc.)  
- readxl (for data import)


## Current Status

✔ EDA completed  
✔ Data cleaned and validated  
✔ Project pivoted to descriptive analysis due to dataset constraints  
🔄 Moving into baseline modeling and feature engineering  


## Notes

This project continues to evolve as additional modeling and analysis steps are completed. Adjustments to the approach have been made to ensure methodological alignment with the available data.
