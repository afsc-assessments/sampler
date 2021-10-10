# 
# {area=substr($2,1,2);month=$4}
# (area==52&&month<6) {strata=1}
# (area<=51&&month<6) {strata=1}
# (area==52&&month>5) {strata=2}
# (area<=51&&month>5) {strata=3}
# 
# 
# 
# stratafy to 2 trawl fisheries, a longline, and Pot fishery divided into 3 periods
# 1 - Non-pelagic trawl
# 2 - Pelagic trawl net
# 4 - Pair trawl - A trawl net (usually a large
# 5 - Shrimp trawl
# 6 - Pot
# 7 - Jig
# 8 - Longline
{area=substr($2,1,2);month=$4}
($1<6&&month<6)            {strata=1}  # Fleet 1
($1<6&&month>6&&month<9)   {strata=2}  # Fleet 2, period 2
($1<6&&month>8)            {strata=3}  # Fleet 2, period 3
($1==8&&month<6)           {strata=4}  # Fleet 3, period 1
($1==8&&month>6&&month<9)  {strata=5}  # Fleet 3, period 2
($1==8&&month>8)           {strata=6}  # Fleet 3, period 3
($1==6&&month<6)           {strata=7}  # Fleet 4, period 1
($1==6&&month>6&&month<9)  {strata=8}  # Fleet 4, period 2
($1==6&&month>8)           {strata=9}  # Fleet 4, period 3
area!=54{print strata,$2+0,$3+0,$4+0,$5+0,$6+0,$7+0,$8+0,$9+0,$10+0,$11+0,$12+0,$13+0,$14+0,$15+0,$16+0}
