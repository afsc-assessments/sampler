awk "$16!=x{cnt++} {x=$16;print $1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15,cnt}" ageall%1.dat >tmp
copy tmp  ageall%1.dat  

awk "$14!=x{cnt++} {x=$14;print $1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,cnt}" lenall%1.dat >tmp
copy tmp  lenall%1.dat  
