DATA_SECTION
  int nrcrds
  !! nrcrds=153000;
  init_matrix data(1,nrcrds,1,116)  // sample this?
  !! cout<<     data(2)(5,116).shift(1).indexmin()<<endl;;
  !! cout<<     data(2)(5,116).shift(1).indexmax()<<endl;;
  3darray mc(1991,2007,1,9,1,112)
  4darray  c(1991,2007,1,9,1,1000,1,112)
  3darray  N(1991,2007,1,9,1,1000)
  3darray mN(1991,2007,1,9,1,4)
  3darray mb(1991,2007,1,9,1,25)
  4darray  b(1991,2007,1,9,1,1000,1,25)
  vector cl(1,112)
 !! ad_comm::change_datafile_name("binkey.dat");
  init_ivector bin(1,112)
 LOCAL_CALCS
   mc.initialize();
   for (int i=1;i<=nrcrds;i++)
   {
     int iy   = int(data(i,3));
     int ifsh = int(data(i,4));
     int isim = int(data(i,1));
     cl       = data(i)(5,116).shift(1);

     dvector bl(1,25);
     bl.initialize();
     for (int j=1;j<=112;j++)
       bl(bin(j))   += cl(j);

     b(iy,ifsh,isim)  = bl;          
     mb(iy,ifsh)   += bl;

     mc(iy,ifsh)      += cl;
      c(iy,ifsh,isim)  = cl;
   }
   for (int iy=1991;iy<=2007;iy++)
   {
     for (int j=1;j<=9;j++)
     {
       double mtmp =sum(mc(iy,j));
       if (mtmp>0)
       {
         mc(iy,j) /= mtmp;
         mb(iy,j) /= sum(mb(iy,j)) ;
         int ncnt=0;
         for (int isim=1;isim<=1000;isim++)
         {
           mtmp = sum(c(iy,j,isim));
           if (mtmp>0)
           {
             ncnt++;
             dvector p_obs = c(iy,j,isim) / mtmp;
             N(iy,j,isim) = sum(elem_prod((1.-mc(iy,j)),mc(iy,j))) / norm2(p_obs-mc(iy,j));
             mN(iy,j,1)     += N(iy,j,isim);
             mN(iy,j,2)     += 1/N(iy,j,isim);

             dvector b_obs = b(iy,j,isim) / sum(b(iy,j,isim)) ;
             double Ntmp   = sum(elem_prod((1.-mb(iy,j)),mb(iy,j))) / norm2(b_obs-mb(iy,j));
             mN(iy,j,3)     += Ntmp;
             mN(iy,j,4)     += 1/Ntmp;
             // cout <<iy<<" "<<j<<" "<<" "<<isim<< N(iy,j,isim)<<endl;
           }
         }
         cout <<iy<<" "<<j<<" "<<mN(iy,j,1)/ncnt<<" "<<1/(mN(iy,j,2)/ncnt)<<" "<<mN(iy,j,3)/ncnt<<" "<<1/(mN(iy,j,4)/ncnt)<<" "<<mc(iy,j)<<" SS2 "<<mb(iy,j)<<endl;
       }
     }
   }
    // 1 1 1991 1  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 112.421 112.421 0 112.421 112.421 224.842 112.421 899.367 112.421 899.367 1461.47 786.946 337.262 1011.79 786.946 337.262 0 1461.47 224.842 1236.63 1349.05 562.104 562.104 1798.73 449.683 1686.31 1461.47 562.104 1461.47 562.104 2360.84 899.367 449.683 786.946 674.525 112.421 449.683 337.262 112.421 0 0 112.421 449.683 0 449.683 224.842 449.683 224.842 112.421 337.262 224.842 112.421 0 0 0 224.842 562.104 337.262 0 112.421 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   exit(1);
 END_CALCS

PARAMETER_SECTION
  init_number x;
  objective_function_value obj_fun;

PROCEDURE_SECTION


REPORT_SECTION


GLOBALS_SECTION
  #include <admodel.h>
