:: FOR /L %%y IN (1,1,18) DO call alkyfs %%y
:: del catage.dat
:: del  wtage.dat
del catlen.dat
FOR /L %%y IN (1,1,17) DO call alk %%y
goto end

copy bs10.dat bs_setup.dat
FOR /L %%y IN (19,1,19) DO call alk %%y
copy bs05.dat bs_setup.dat
FOR /L %%y IN (19,1,19) DO call alk %%y
copy bs01.dat bs_setup.dat
FOR /L %%y IN (19,1,19) DO call alk %%y
:: FOR /L %%y IN (1,1,23) DO call alk %%y
:: FOR /L %%y IN (24,1,27) DO call alk %%y
:end
