  #include <admodel.h>
#include <admodel.h>
#include <contrib.h>

  extern "C"  {
    void ad_boundf(int i);
  }
#include <effn.htp>

model_data::model_data(int argc,char * argv[]) : ad_comm(argc,argv)
{
 nrcrds=153000;
  data.allocate(1,nrcrds,1,116,"data");
 cout<<     data(2)(5,116).shift(1).indexmin()<<endl;;
 cout<<     data(2)(5,116).shift(1).indexmax()<<endl;;
  mc.allocate(1991,2007,1,9,1,112);
  c.allocate(1991,2007,1,9,1,1000,1,112);
  N.allocate(1991,2007,1,9,1,1000);
  mN.allocate(1991,2007,1,9,1,4);
  mb.allocate(1991,2007,1,9,1,25);
  b.allocate(1991,2007,1,9,1,1000,1,25);
  cl.allocate(1,112);
 ad_comm::change_datafile_name("binkey.dat");
  bin.allocate(1,112,"bin");
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
}

model_parameters::model_parameters(int sz,int argc,char * argv[]) : 
 model_data(argc,argv) , function_minimizer(sz)
{
  initializationfunction();
  x.allocate("x");
  obj_fun.allocate("obj_fun");
  prior_function_value.allocate("prior_function_value");
  likelihood_function_value.allocate("likelihood_function_value");
}

void model_parameters::userfunction(void)
{
  obj_fun =0.0;
}

void model_parameters::report(const dvector& gradients)
{
 adstring ad_tmp=initial_params::get_reportfile_name();
  ofstream report((char*)(adprogram_name + ad_tmp));
  if (!report)
  {
    cerr << "error trying to open report file"  << adprogram_name << ".rep";
    return;
  }
}

void model_parameters::preliminary_calculations(void){
#if defined(USE_ADPVM)

  admaster_slave_variable_interface(*this);

#endif
}

model_data::~model_data()
{}

model_parameters::~model_parameters()
{}

void model_parameters::final_calcs(void){}

void model_parameters::set_runtime(void){}

#ifdef _BORLANDC_
  extern unsigned _stklen=10000U;
#endif


#ifdef __ZTC__
  extern unsigned int _stack=10000U;
#endif

  long int arrmblsize=0;

int main(int argc,char * argv[])
{
    ad_set_new_handler();
  ad_exit=&ad_boundf;
    gradient_structure::set_NO_DERIVATIVES();
    gradient_structure::set_YES_SAVE_VARIABLES_VALUES();
    if (!arrmblsize) arrmblsize=15000000;
    model_parameters mp(arrmblsize,argc,argv);
    mp.iprint=10;
    mp.preliminary_calculations();
    mp.computations(argc,argv);
    return 0;
}

extern "C"  {
  void ad_boundf(int i)
  {
    /* so we can stop here */
    exit(i);
  }
}
