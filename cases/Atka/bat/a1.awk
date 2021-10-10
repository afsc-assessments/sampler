# For age data, convert one big file to multiple year files w/ strata and hauls incremented (original Must be sorted)
{area=$2}
(area<=541) {strata=1}
(area==542) {strata=2}
(area==543) {strata=3}
$1!=x {cnt++} 
{x=$1}
{print strata,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15,cnt > "age"$6".dat"}
  # // Strata tow sex length freq
