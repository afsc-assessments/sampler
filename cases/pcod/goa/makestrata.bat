awk -f a2.awk ageall%1.dat >t.dat
move /y t.dat ageall%1.dat

awk -f a1.awk lenall%1.dat >t.dat
move /y t.dat lenall%1.dat

call reindex %1
