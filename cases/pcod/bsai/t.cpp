#include <admodel.h>

  extern "C"  {
    void ad_boundf(int i);
  }
#include <t.htp>

model_data::model_data(int argc,char * argv[]) : ad_comm(argc,argv)
{
  nrcrds.allocate("nrcrds");
  nsims.allocate("nsims");
  l1.allocate("l1");
  l2.allocate("l2");
  styr.allocate("styr");
  endyr.allocate("endyr");
  data.allocate(1,nrcrds,1,116,"data");
  lf.allocate(styr,endyr,1,9,1,nsims,l1,l2);
  effn.allocate(styr,endyr,1,9,1,nsims);
  mlf.allocate(styr,endyr,1,9,l1,l2);
 cout << l1<<endl<<l2<<endl<<styr<<endl<<endyr<<endl;
  lf.initialize();
  mlf.initialize();
  for (int i=1;i<=nrcrds;i++)
  {
    // cout<< data(i,3)<<" "<<data(i,1)<<endl;
    int isim=data(i,1) ;
    int iyr=data(i,3);
    int igr=data(i,4);
    lf(iyr,igr,isim) = data(i)(5,116).shift(9);
    mlf(iyr,igr) += lf(iyr,igr,isim); 
  }
  for (int iyr=styr;iyr<=endyr;iyr++)
  {
    for (int igr=1;igr<=9;igr++)
    {
      double summlf =    sum(mlf(iyr,igr));
      if (summlf>0)
      {
        mlf(iyr,igr) /= sum(mlf(iyr,igr));
        // cout<<iyr<<" "<<igr<<" "<<mlf(iyr,igr)<<endl;
        for (int isim=1;isim<=nsims;isim++)
        {
          double sumlf =    sum(lf(iyr,igr,isim));
          // cout << iyr<<" "<<sumlf<<endl;
          if (sumlf >0)
          {
             lf(iyr,igr,isim) /= sumlf;
             // cout<<iyr<<endl;
             double denom = norm2(lf(iyr,igr,isim)-mlf(iyr,igr));
             if (denom>0) 
               effn(iyr,igr,isim)= (1. -mlf(iyr,igr))*mlf(iyr,igr)/denom;
          }
        // (1. -mlf(iyr,igr))*mlf(iyr,igr)/norm2(lf(iyr,igr,isim)-mlf(iyr,igr))<<endl;
        }
        if (sum(effn(iyr,igr))>0)
          cout<<iyr<<" "<<igr<<" "<<mean(effn(iyr,igr))<<" "<<1./mean(1./(.0001+effn(iyr,igr)))<<" "<<mlf(iyr,igr)<<endl;
      }
    }
  }
  exit(1);
}

model_parameters::model_parameters(int sz,int argc,char * argv[]) : 
 model_data(argc,argv) , function_minimizer(sz)
{
  initializationfunction();
  x.allocate("x");
  obj_fun.allocate("obj_fun");
}

void model_parameters::userfunction(void)
{
}

void model_parameters::report()
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
  admaster_slave_variable_interface(*this);
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
  #if defined(__GNUDOS__) || defined(DOS386) || defined(__DPMI32__)  || \
     defined(__MSVC32__)
      if (!arrmblsize) arrmblsize=150000;
  #else
      if (!arrmblsize) arrmblsize=25000;
  #endif
    model_parameters mp(arrmblsize,argc,argv);
    mp.iprint=10;
    mp.preliminary_calculations();
    mp.computations(argc,argv);
    return 0;
}

extern "C"  {
  void ad_boundf(int i)
  {
    // so we can stop here
    exit(i);
  }
}
