awk "NF<6&&NF>2{x=$3};NF>6{print %1,x,$0}" poll%1a.out 
