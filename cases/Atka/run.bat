@echo off
del results\catage.dat
del results\wtage.dat
set /a year=%1+1989
echo %year%
awk "{print $%1}" runs.dat >sam.dat
echo %1
sam
:: pause
cat catage_str.out > results\catage%year%_str.dat
copy  wtage_str.out results\wtage%year%_str.dat
cat catage.out >> results\catage.dat
copy  wtage.out results\wtage%year%.dat
cat  wtage.out >> results\wtage.dat
:: echo done
