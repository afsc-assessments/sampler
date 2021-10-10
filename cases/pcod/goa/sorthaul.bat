set yr=%1
echo %yr:~2,2%
:: echo pcod_len%1.dat 
set filename=pcod_len%yr:~2,2%.dat 
c:\bin\sort  -k14 %filename% >tmp
awk "$14!=x{cnt++} {x=$14;print $1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,cnt}" tmp >%filename%
echo done with %filename%
