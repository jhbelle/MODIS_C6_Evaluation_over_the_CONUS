## -------------------
## Name: CaseStudy_Validation.R
## Program version: R 3.2.3
## Dependencies:
## Author: J.H. Belle
## Purpose: Carry out case study comparing model fits where y=PM2.5 (24 hour) and x=AOD for each AOD product, comparing using QAC 3 only with no filtering and correction versus QAC 1, 2, and 3 with filtering and correction
## -------------------

# Read in 24 hour PM2.5 data
G24hr <- read.csv("/home/jhbelle/EPAdata/CleanedData/G24hr.csv")


