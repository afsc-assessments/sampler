echo off
awk -f pcod_len.awk len_pcod1.dat 

for /L %%v IN (1986,1,2007) DO  call sorthaul.bat %%v 

awk -f pcod_age.awk age_pcod1.dat >t.dat
:: move /y t.dat ageall%1.dat
:: 
:: awk -f a1.awk lenall%1.dat >t.dat
:: move /y t.dat lenall%1.dat
:: 
:: call reindex %1

