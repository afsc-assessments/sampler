#!/bin/bash
# Had to split up because awk couldn't open too  many files simultaneously...
rm -f catch*.csv
#awk -F , 'BEGIN{OFS=","} $7=="Pollock" && $4<540{print $0}' ~/OneDrive/AKFIN/AllSppCatch.csv  >akfincat.csv
awk -F, -v y1=1991 -v y2=1998 -v o=catch -f a2.awk  akfincat.csv 
awk -F, -v y1=1999 -v y2=2005 -v o=catch -f a2.awk  akfincat.csv 
awk -F, -v y1=2006 -v y2=2013 -v o=catch -f a2.awk  akfincat.csv 
awk -F, -v y1=2014 -v y2=2017 -v o=catch -f a2.awk  akfincat.csv 
awk -F, -v y1=2014 -v y2=2019 -v o=catch -f a2.awk  akfincat.csv 
awk -F, -v y1=2011 -v y2=2011 -v o=catch -f a2.awk  akfincat.csv 
