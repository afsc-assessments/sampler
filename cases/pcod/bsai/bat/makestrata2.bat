awk -f a3.awk ageall%1.dat >t.dat
move /y t.dat ageall%1.dat
awk -f a4.awk lenall%1.dat >t.dat
move /y t.dat lenall%1.dat
