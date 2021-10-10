:: awk "$6==%1{print $0}" ageallyfs.dat >data\ageallyfs_%1.dat
:: awk "$6==%1{print $0}" lenallyfs.dat >data\lenallyfs_%1.dat
awk "$6==%1{print $0}" age_pcod1.dat >ageallpcod_%1.dat
awk "$6==%1{print $0}" len_pcod1.dat >lenallpcod_%1.dat
