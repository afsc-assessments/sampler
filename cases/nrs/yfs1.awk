#{area=substr($2,1,2);month=$4}
#(area==52&&month<7) {strata=1}
#(area<=51&&month<7) {strata=2}
#(area==52&&month>6) {strata=3}
#(area<=51&&month>6) {strata=4}
{print 1,$2+0,$3+0,$4+0,$5+0,$6+0,$7+0,$8+0,$9+0,$10+0,$11+0,$12+0,$13+0}
