:: FOR /L %%y IN (1977,1,1999) DO awk -f a518.awk ageall%%y.dat >>bogage.dat
:: FOR /L %%y IN (1977,1,1999) DO awk -f l518.awk lenall%%y.dat >>boglen.dat
awk -f l518.awk lenall2000.dat >>boglen.dat
awk -f l518.awk lenall2001.dat >>boglen.dat
awk -f l518.awk lenall2002.dat >>boglen.dat
awk -f l518.awk lenall2003.dat >>boglen.dat
awk -f l518.awk lenall2004.dat >>boglen.dat
