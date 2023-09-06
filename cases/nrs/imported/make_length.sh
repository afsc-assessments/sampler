#!/bin/bash
# Had to split up because awk couldn't open too  many files simultaneously...
#rm -f catch*.csv
#rm -f age*.csv
#rm -f len*.csv
#awk -F , 'BEGIN{OFS=","} $7=="Pollock" && $4<540{print $0}' ~/OneDrive/AKFIN/AllSppCatch.csv  >akfincat.csv
#awk -F, -v y1=1991 -v y2=1998 -v o=catch -f a2.awk  akfincat.csv 
#awk -F, -v y1=1999 -v y2=2005 -v o=catch -f a2.awk  akfincat.csv 
#awk -F, -v y1=2006 -v y2=2016 -v o=catch -f a2.awk  akfincat.csv 
awk -F, -v y1=1991 -v y2=1998 -v o=len -f a2.awk  norpac_length_report.csv 
awk -F, -v y1=1999 -v y2=2005 -v o=len -f a2.awk  norpac_length_report.csv 
awk -F, -v y1=2006 -v y2=2014 -v o=len -f a2.awk  norpac_length_report.csv 
awk -F, -v y1=2015 -v y2=2015 -v o=len -f a2.awk  norpac_length_report.csv 
awk -F, -v y1=2016 -v y2=2020 -v o=len -f a2.awk  norpac_length_report.csv 
#awk -F, -v y1=1991 -v y2=1998 -v o=age -f a2.awk  poll_age.csv 
#awk -F, -v y1=1999 -v y2=2005 -v o=age -f a2.awk  poll_age.csv 
#awk -F, -v y1=2006 -v y2=2015 -v o=age -f a2.awk  poll_age.csv 
#awk -F, -v y1=2016 -v y2=2016 -v o=age -f a2.awk  poll_age.csv 
#do
				#echo running year: $i
  #get_age.sh  $i
##done
