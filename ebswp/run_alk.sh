#!/bin/bash
echo $1
cut -f $1 data/runs.dat >sam.dat
sam 
#cat  lf_%year%.dat >> catch_at_len_ALL.dat
#cat  wtage%year%_str.out >> wtage_str.dat
#cat catage%year%.out >>catage.dat
#cat  wtage%year%.out >> wtage.dat