echo off 
set /a year=%1+1985
echo %year%
:: call makestrata %year%

awk "{print $%1}" runs.dat >sam.dat
echo %1
sam 
:: pause
 cat catlen_str.out >>catlen.dat
:: cat  wtage.out >> wtage.dat
echo done


