awk -f yfs2.awk data\ageallyfs_%1.dat >t.dat
move /y t.dat data\ageallyfs_%1.dat
awk -f yfs1.awk data\lenallyfs_%1.dat >t.dat
move /y t.dat data\lenallyfs_%1.dat
