:: : awk program to make lf samples of bogoslof data
:: awk "$6<60{yr=$6+2000}$6>60{yr=$6+1900} {yrsex=(yr $9)}{lf[yrsex]+=$10;cnt[yrsex]++} END{for(i in lf){print i,lf[i]}}" boglen.dat
:: for yfs
awk "$6<60{yr=$6+2000}$6>60{yr=$6+1900} {yrsex=(yr  %1 $9)}{lf[yrsex]+=$10;cnt[yrsex]++} END{for(i in lf){print i,lf[i]}}" lenallyfs.dat
