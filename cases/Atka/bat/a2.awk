{area=$2}
$6<89 {yr=2000+$6}
$6>89 {yr=1900+$6}
(area<=541) {strata=1}
(area==542) {strata=2}
(area==543) {strata=3}
  # // Strata tow sex length freq
$1!=x {towcnt++} 
{x=$1}
{print strata,towcnt,$11,$9,$10 >"length"yr".dat"}
