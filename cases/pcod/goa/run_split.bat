
:: Split files into years
:: FOR /L %%y IN (87,1,99) DO call splitpcod %%y
call splitpcod 00
call splitpcod 01
call splitpcod 02
call splitpcod 03
call splitpcod 04
call splitpcod 05
call splitpcod 06
call splitpcod 07

:: FOR /L %%y IN (1991,1,2004) DO call makestrata %%y
