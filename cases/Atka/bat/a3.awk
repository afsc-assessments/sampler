# Restratifies age data for pollock (3 strata, 1=A season, 2=B 52, 3=B 51, ignore 54
# and makes read for geographical stuff...
{area=substr($2,1,2);month=$4}
(area==52&&month<6) {strata=1}
(area<=51&&month<6) {strata=1}
(area==52&&month>5) {strata=2}
(area>=50&& area<=51&&month>5) {strata=3}
area!=54{print strata,$2+0,$3+0,$4+0,$5+0,$6+0,$7+0,$8+0,$9+0,$10+0,$11+0,$12+0,$13+0,$14+0,$15+0,$16+0}
