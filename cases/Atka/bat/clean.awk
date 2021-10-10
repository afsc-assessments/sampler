{pw=0.0000148769177516369*$13^2.826} 
{dif=$12-pw} 
dif<0{pw2=-(dif)/pw} 
dif>=0{pw2=dif/pw} pw2>.5{print $0,"junk?"} pw2<=.5{print $0,"ok"}
