echo off 
awk "{print $%1}" runs_yfs.dat >sam.dat
echo %1
sam 
echo done


