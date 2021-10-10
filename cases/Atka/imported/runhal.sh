#!/bin/bash
# Had to split up because awk couldn't open too  many files simultaneously...
 #awk -F, -v y1=1999 -v y2=2005 -v o=catch -f a2.awk  akfincat.csv 
 #awk -F, -v y1=2006 -v y2=2014 -v o=catch -f a2.awk  akfincat.csv 
 awk -F, -v y1=1999 -v y2=2005 -v o=len_hal -f a2.awk  hal_len.csv 
 awk -F, -v y1=2006 -v y2=2015 -v o=len_hal -f a2.awk  hal_len.csv 
 #awk -F, -v y1=1999 -v y2=2005 -v o=age -f a2.awk  poll_age.csv 
 #awk -F, -v y1=2006 -v y2=2014 -v o=age -f a2.awk  poll_age.csv 
#do
				#echo running year: $i
  #get_age.sh  $i
##done
