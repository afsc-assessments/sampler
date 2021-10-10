{ind=$13 " " $10 " " $1}
$12>0{sum[ind]+=$12;sumsq[ind]+=$12*$12;cnt[ind]++}
END {for (i in sum){
  {n=cnt[i]}
  if(n>1){print i,sum[i]/n,(n*sumsq[i]-sum[i]*sum[i])/(n*(n-1)),n }
  }
}
