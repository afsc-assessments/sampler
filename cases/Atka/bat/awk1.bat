:: For age data, convert one big file to multiple year files w/ strata and hauls incremented (original Must be sorted)
:: c:\bin\sort atka_age.dat >tmp.dat
:: move tmp.dat atka_age.dat
awk -fa1.awk atka_age.dat 
:: awk "$6==%1{print $0}" length.dat >lenall19%1.dat
:: sampler
