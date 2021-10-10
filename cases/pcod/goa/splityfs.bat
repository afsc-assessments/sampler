:: awk "$6==%1{print $0}" ageallyfs.dat >data\ageallyfs_%1.dat
:: awk "$6==%1{print $0}" lenallyfs.dat >data\lenallyfs_%1.dat
awk "$6==%1{print $0}" age_yfs.dat >data\ageallyfs_%1.dat
awk "$6==%1{print $0}" len_yfs.dat >data\lenallyfs_%1.dat
