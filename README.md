# SubSci2024
Repository for Subtidal Science 2024 student data and code

# Code files:
Sample_analysis.R - A sample analysis for anyone interested that uses some of the main packages

RLS_data_scrub.R - This script takes the macroinvert data from the online RLS database, and adds the 2022 kelp forest RLS data from Kieran and Claire, and the 2024 RLS data that's not on the website yet and combines it. The output is a clean abalone csv

Abalone_code.R - This is the beginning of a file to explore the abalone data, currently it just estimates the mean abalone density at each site and produces a csv of the site coordinates and abalone densities

Functions.R - I hate using the same long string of functions again and again so I write my own functions and store them here. mr_scrubby() is the function I'm using to clean the RLS data

# R things
I have "soft wrap" turned on, so R automatically starts a new line when my code hits the edge of my page. If you don't have this on, you'll get some super long lines (sorry). To turn it on, go Tools > Global Options > Code > check "soft wrap R source files" > Apply !

# renv
I'm using renv as a package manager, when you first open this repo renv will bootstrap itself. Run renv::restore() to use renv to install the necessary packages, and specifially the same versions I'm currently using. Hopefully this sorts out any weird version incompatibility issues!