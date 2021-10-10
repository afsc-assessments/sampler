echo off 
set /a year=%1+1990
echo %year%

awk "{print $%1}" runs.dat >sam.dat
echo %1
sam 
:: pause
cat catlen_str.out >>catlen.dat
:: cat catage.out >> catage.dat
:: cat  wtage.out >> wtage.dat
:: cat catage_str.out >>catage_str.dat
:: cat  wtage_str.out >> wtage_str.dat
echo done


