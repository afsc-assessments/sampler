#!/bin/bash
# Had to split up because awk couldn't open too  many files simultaneously...
#rm -f catch*.csv
awk -F , -v y1=1991 -v y2=1998 -v o=catch -f a4.awk  akfin_cat.csv 
awk -F , -v y1=1999 -v y2=2005 -v o=catch -f a4.awk  akfin_cat.csv 
awk -F , -v y1=2006 -v y2=2014 -v o=catch -f a4.awk  akfin_cat.csv 
awk -F , -v y1=2015 -v y2=2020 -v o=catch -f a4.awk  akfin_cat.csv 


###Done in R now...
