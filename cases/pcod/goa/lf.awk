$6>10{yr=1900+$6}
$6<10{yr=2000+$6}
{x=sprintf("%1i %2i %3i %3i ", yr,$4,$1,$9);
lf[x]+=$10}
END{for (i in lf){print i,lf[i]}}
