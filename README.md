# DOC-GNSS-Processing

This repository contains R code to process the solution files that are created by Emlid Studio.
The main script is calc_GNSS_pos.R, which contains 3 R functions.

The script can be copied to a working directory on the users computer, and sourced

source("calc_GNSS_pos.R")

This will then make the functions available.

To process a *.POS file, type

procpos()

You will be prompted to select a file usings your computer's file system manager

select a *.POS.

procpos will then :

(1) Print the summary results, including the final position in NZTM and WGS84 Lat/Long, sigma values for X, Y and Z

(2) Save the summary results to a csv file with the file name of the input file appended with _summary.csv

  To disable the saving of the summary results, type procpos(NULL, writeResults = F) 
  
(3) If requested, procpos will also produce a zoomable map indicating the location of the solution

procpos can also ingest a specified *.POS file, instead of prompting the user to select.
This can be done, by setting the first argument to a character string of the file path,
eg procpos(FileName = "C:/Users/McMillanAn/OneDrive - MWLR/Projects/PRJ3820-DOC-GNSS/data/FINAL_SOLUTIONS/JVD-OCC7-P2-PPK-R10.POS")

