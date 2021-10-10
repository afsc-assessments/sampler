@echo off
FOR /L %%v IN (1991,1,2006) DO awk -ft1.awk ageall%%v.dat 
