  #include <admodel.h>
  adstring year;
  adstring agefile;
  adstring lenfile;
  adstring outfile;
#include <admodel.h>

  extern "C"  {
    void ad_boundf(int i);
  }
#include <sam.htp>

model_data::model_data(int argc,char * argv[]) : ad_comm(argc,argv)
{
  pad_rng = new random_number_generator(1234);;
 *(ad_comm::global_datafile) >>  year; 
 *(ad_comm::global_datafile) >>  agefile; 
 *(ad_comm::global_datafile) >>  lenfile; 
  na_rcrds.allocate("na_rcrds");
  nl_rcrds.allocate("nl_rcrds");
  a1.allocate("a1");
  a2.allocate("a2");
 na=a2-a1+1;
  l1.allocate("l1");
  l2.allocate("l2");
 nl=l2-l1+1;
  nstrata.allocate("nstrata");
  strata_catch.allocate(1,nstrata,"strata_catch");
 *(ad_comm::global_datafile) >>  outfile; 
 cout <<"Writing to " <<outfile<< endl;
 cout <<"Nstrata : "<< nstrata<<endl<<strata_catch<<endl<<nl_rcrds<<endl;
 cout <<agefile<<" "<< na_rcrds<<endl<<lenfile<<endl<<nl_rcrds<<endl;// exit(1);
 ad_comm::change_datafile_name("BS_Setup.dat");
  nsims.allocate("nsims");
 if (nsims==0) nsims=1;
  sam_level.allocate("sam_level");
cout<<"Nsims: "<<nsims<<endl;
 ad_comm::change_datafile_name(agefile);
 cout<<"Reading age data..."<<endl;
  adata_in.allocate(1,na_rcrds,1,16,"adata_in");
  a_tows.allocate(1,na_rcrds);
  l_tows.allocate(1,nl_rcrds);
 a_tows=column(adata_in,16);
 natows = max(a_tows);
  na_sam_tow.allocate(1,natows);
 cout<<natows<<endl;
 na_sam_tow.initialize();
 for (i=1;i<=na_rcrds;i++) na_sam_tow(a_tows(i))++; 
 nsam_max=max(na_sam_tow);
  a_recnum.allocate(1,natows,1,nsam_max);
  a_recnum.initialize();
  int k=1;
  for (i=1;i<=natows;i++) {
    for (int j=1;j<=(na_sam_tow(i));j++) {
      a_recnum(i,j) = k;
      k++;
    }
  }
  adata.allocate(1,na_rcrds+2000,1,16);
  ldata.allocate(1,nl_rcrds+4000,1,14);
  ta_ubs.allocate(1,natows);
  a_ubs.allocate(1,na_rcrds+2000);
  l_ubs.allocate(1,nl_rcrds+4000);
 ad_comm::change_datafile_name(lenfile);
 cout<<"Reading length data..."<<endl;
  ldata_in.allocate(1,nl_rcrds,1,14,"ldata_in");
 l_tows=column(ldata_in,14);
 nltows = max(l_tows);
  nl_sam_tow.allocate(1,nltows);
 nl_sam_tow.initialize();
 for (i=1;i<=nl_rcrds;i++) nl_sam_tow(l_tows(i))++; 
 nsam_max=max(nl_sam_tow);
  tl_ubs.allocate(1,nltows);
  l_recnum.allocate(1,nltows,1,nsam_max);
 cout<<"Number of Length tows: "<<nltows<<endl;
 cout<<"Number of age tows: "<<natows<<endl;
  l_recnum.initialize();
  cout << "N sample tows "<<nl_sam_tow(l_tows(nl_rcrds)) <<" Nltows "<<nltows<<endl;
  k=1;
  for (i=1;i<=nltows;i++) {
    for (int j=1;j<=nl_sam_tow(i);j++) {
      l_recnum(i,j) = k;
      k++;
    }
  }
  l_recnum(nltows,nl_sam_tow(nltows));
  cout<<"l_recnum final: "<< l_recnum(nltows,nl_sam_tow(nltows)) <<endl;
  awt.allocate(1,nstrata,1,3);
  awtl.allocate(1,nstrata,1,3,l1,l2);
  sawtl.allocate(1,3,l1,l2);
  gawtl.allocate(l1,l2);
  cwt.allocate(1,nstrata,1,3);
  cwtl.allocate(1,nstrata,1,3,l1,l2);
  scwtl.allocate(1,3,l1,l2);
  gcwtl.allocate(l1,l2);
  awta.allocate(1,nstrata,1,3,a1,a2);
  lf.allocate(1,nstrata,1,3,l1,l2);
  nlf.allocate(1,nstrata,1,3,l1,l2);
  plf.allocate(1,nstrata,1,3,l1,l2);
  alk.allocate(1,nstrata,1,2,l1,l2,a1,a2);
  naged.allocate(1,nstrata,1,2,l1,l2);
  salk.allocate(1,2,l1,l2,a1,a2);
  galk.allocate(l1,l2,a1,a2);
  page.allocate(1,nstrata,1,3,a1,a2);
  nage.allocate(1,nstrata,1,3,a1,a2);
  plen.allocate(1,nstrata,1,3,l1,l2);
   ofstream ac_out(outfile);
   ofstream catage_out("catage.out");
   ofstream catage_out_str("catage_str.out");
   ofstream  wtage_out("wtage.out");
   ofstream  wtage_out_str("wtage_str.out");
   ofstream lf_out("LF_"+year+".dat");
   ofstream alk_out("alk_"+year+".dat");
     random_number_generator& rng= *pad_rng;
     // Set up simulation loop here...if sims=0 then NOTE there is a zeroth simulation!!!
   for (int isim=1;isim<=nsims;isim++) 
   {
     if (nsims==1) nsims=0;
     if (!nsims) {
       ta_ubs.fill_seqadd(1,1); 
       tl_ubs.fill_seqadd(1,1); 
       a_ubs.fill_seqadd(1,1); 
       l_ubs.fill_seqadd(1,1); 
     }
     else
     {
       ta_ubs.fill_randu(rng);
       tl_ubs.fill_randu(rng);
       a_ubs.fill_randu(rng);
       l_ubs.fill_randu(rng);
       ta_ubs *= (natows+1);
       tl_ubs *= (nltows+1);
       a_ubs *= (na_rcrds+1);
       l_ubs *= (nl_rcrds+1);
     }
     page.initialize();
     nage.initialize();
     plen.initialize();
     salk.initialize();
     galk.initialize();
     alk.initialize();
     naged.initialize();
     lf.initialize();
     plf.initialize();
     awt.initialize();
     awta.initialize();
     awtl.initialize();
     sawtl.initialize();
     gawtl.initialize();
     cwtl.initialize();
     cwt.initialize();
     scwtl.initialize();
     gcwtl.initialize();
     wt_tmp=0.;
     // if (!nsims)exit(1); // logic for if simulation
   //for (int i=1;i<=10000;i++) cout <<a_ubs(i)<<" "<<adata(a_ubs(i))<<endl;cout<<max(a_ubs)<<" "<<na_rcrds<<endl;exit(1);
    // Step 1,ample among tows, count total records for 
    int k=1;
    for (i=1;i<=natows*sam_level;i++)
    {
      int ii = max(1,ta_ubs(i));
      for (int j=1;j<=(na_sam_tow(ii));j++) 
      {
        // cout<<ii<<" "<<i<<" "<<j<<" "<<a_recnum(ii,j)<<endl;
        adata(k) = adata_in(a_recnum(ii,j));
        k++;
      }
    }
    int n_bsa_rcrds = k-1;
    // for (i=1;i<=n_bsa_rcrds;i++) 
    k=1;
    for (i=1;i<=nltows*sam_level;i++)
    {
      int ii = max(1,tl_ubs(i));
      for (int j=1;j<=(nl_sam_tow(ii));j++) 
      {
        // cout<<k<<" "<<ii<<" "<<i<<" "<<j<<" "<<l_recnum(ii,j)<<endl;
        ldata(k) = ldata_in(l_recnum(ii,j));
        k++;
      }
    }
    int n_bsl_rcrds = k-1;
    // for (i=1;i<=n_bsa_rcrds;i++) 
    cout <<n_bsa_rcrds<<" "<<na_rcrds<<endl;// exit(1);
    cout <<n_bsl_rcrds<<" "<<nl_rcrds<<endl;// exit(1);
    for (i=1;i<=n_bsa_rcrds;i++)
    {
      int ii = max(1,a_ubs(i));
      age   = int(adata(ii,9));
      len   = int(adata(ii,13));
      sex   = int(adata(ii,10));
      if (sex==0) sex=3;
      strat = int(adata(ii,1));
      // cout <<i<<" "<<age<<" "<<len<<" "<<sex<<" "<<strat<<endl;
      wt_tmp= adata(ii,12);
      if (strat > nstrata) {cout<<"age data strata too high "<<adata(i)<<endl<<nstrata<<endl<<strat<<" "<<wt_tmp<<" "<<i<<endl;exit(1);}
      if (len>=l1&&len<=l2) 
      {
        if (wt_tmp>0)
        {
          awt(strat,sex)     +=wt_tmp;
          awtl(strat,sex,len)+=wt_tmp;
          sawtl(sex,len)     +=wt_tmp;
          gawtl(len)         +=wt_tmp;
          cwt(strat,sex)++;
          cwtl(strat,sex,len)++;
          scwtl(sex,len)++;
          gcwtl(len)++;
        }
        if (age>=a1) 
        {
          if (age>a2) age=a2; // Plus group
          galk(len,age)++;
          if (sex==3)
          {
            salk(1,len,age)+=0.5;
            salk(2,len,age)+=0.5;
            alk(strat,1,len,age)+=0.5;
            alk(strat,2,len,age)+=0.5;
          }
          else
          {
            salk(sex,len,age)++;
            alk(strat,sex,len,age)++;
          }
        }
      }
    }
    // for (i=1;i<=nl_rcrds;i++) // Change to bootstrap N
    for (i=1;i<=n_bsl_rcrds;i++)
    {
      int ii = max(1,l_ubs(i));
      double frq   = ldata(ii,10);
      len   = ldata(ii,9);
      sex   = ldata(ii,11);
      if (sex==0) sex=3;
      strat = ldata(ii,1);
      if (strat > nstrata) {cout<<"length data strata too high "<<i<<endl<<ldata(i)<<endl;exit(1);}
      if (len>=l1&&len<=l2) 
      {
        lf(strat,sex,len)+=frq;
      }
    }
  // Done reading data, now put in arrays, add unsexed to males and females equally....
    if (!nsims)
      lf_out <<"year stratum sex length freq prop avg_wt nwt_samples nlen_samples naged"<<endl;
    for (int k=1;k<=nstrata;k++)
    {
      dmatrix wtfrq(1,2,l1,l2);
      wtfrq.initialize();
      double sumlf ;
      sumlf=0.;
      for (i=1;i<=2;i++)
      {
        cwt(k,i)  +=  cwt(k,3); // add unsexed to the males and females
        awt(k,i)  +=  awt(k,3); // add unsexed to the males and females
        cwtl(k,i) +=  cwtl(k,3);      // add unsexed to the males and females
        awtl(k,i) +=  awtl(k,3);      // add unsexed to the males and females
        lf(k,i)   +=  .5*lf(k,3);      // Add unsexed to LFreq
        nlf(k,i)   =  lf(k,i);
        sumlf     +=  sum(nlf(k,i));
        for (j=l1;j<=l2;j++)
        {
          if (cwtl(k,i,j)>0)
            awtl(k,i,j) /= cwtl(k,i,j);
          else
            if (scwtl(i,j)>0)
              awtl(k,i,j) = sawtl(i,j) / scwtl(i,j);
            else
              if (gcwtl(j)>0)
                awtl(k,i,j) = gawtl(j) / gcwtl(j);
        }
        if (cwt(k,i)>0)
          awt(k,i) /= cwt(k,i);
        wtfrq(i) = elem_prod(lf(k,i),awtl(k,i));
      }
      if (sumlf >0.)
        plf(k)  = nlf(k)/sumlf;
      double sumtmp2;
      sumtmp2=sum(wtfrq);
      if (sumtmp2 > 0)
        wtfrq /= sumtmp2;        // normalize by sex, length
      wtfrq *= strata_catch(k); // raise by catch in this stratum
      // Put back into length frequency...
      for (i=1;i<=2;i++)
      {
        for (j=l1;j<=l2;j++)
        {
          if (awtl(k,i,j)>0) 
            lf(k,i,j) = wtfrq(i,j)/awtl(k,i,j);
          else
            lf(k,i,j) = 0.;
        }
      }
      double sumtmp = sum(lf(k));
      if (sumtmp>0) 
        plen(k)/=sumtmp;
    } // loop over strata
  // Compute ALKs
    for (j=l1;j<=l2;j++)
    {
      double x1=sum(galk(j));
      if (x1>0.)
        galk(j) /= x1;
      for (i=1;i<=2;i++)
      {
        x1=sum(salk(i,j));
        if (x1>0.)
          salk(i,j) /= x1;
        else
          salk(i,j) = galk(j); // fill in holey alk
        for (int k=1;k<=nstrata;k++)
        {
          x1=sum(alk(k,i,j));
          naged(k,i,j) = x1;
          if (x1>0.)
            alk(k,i,j) /= x1;
          else
            alk(k,i,j) = salk(i,j); // fill in holey alk
          if (!nsims)
            lf_out <<year<<" "<<k<<" "<<i<<" "<<j<<" "<<lf(k,i,j)<<" "<< plf(k,i,j)<< " "<< awtl(k,i,j)<<" "<<cwtl(k,i,j)<<" "<<nlf(k,i,j)<<" "<<naged(k,i,j)<<endl;
        }
      }
    }
  //   cout <<galk<<endl<<endl<<salk<<endl<<endl<<alk<<endl;
    cout <<"this N strata: "<<nstrata<<endl;
    if (!nsims)
      ac_out <<"year type stratum sex age value "<<endl;
    dvector awt_tmp1(a1,a2);
    dmatrix awt_tmp2(1,2,a1,a2);
    dmatrix awt_tmp3(1,nstrata,a1,a2);
    dvector nat_tmp1(a1,a2);
    dmatrix nat_tmp2(1,2,a1,a2);
    dmatrix nat_tmp3(1,nstrata,a1,a2);
    nat_tmp1.initialize();
    nat_tmp2.initialize();
    nat_tmp3.initialize();
    awt_tmp1.initialize();
    awt_tmp2.initialize();
    awt_tmp3.initialize();
    for (int k=1;k<=nstrata;k++)
    {
      for (i=1;i<=2;i++)
      {
        nage(k,i) = lf(k,i)*alk(k,i);
        dvector awtmp(a1,a2);
        awtmp=elem_prod(awtl(k,i),lf(k,i))*alk(k,i);
        for (j=a1;j<=a2;j++)
        {
          if (nage(k,i,j)>0.)
            awta(k,i,j) = awtmp(j)/nage(k,i,j); 
        }
        if (!nsims){
          for (j=a1;j<=a2;j++) ac_out <<year<<" N "<<k<<" "<<i<<" "<<j<<" "<<nage(k,i,j) << endl;
          for (j=a1;j<=a2;j++) ac_out <<year<<" Biomass "<<k<<" "<<i<<" "<<j<<" "<<nage(k,i,j)*awta(k,i,j) << endl;
        }
        
        // cout <<year<<" N_Kimura "<<k<<" "<<i<<" "<<endl; cout <<"Strata catch: "<<strata_catch(k,i)*awt(k,i)<<" "<<endl; cout<< plf(k,i)<<endl; cout << alk(k,i) << endl;
        dvector kimura_tmp = strata_catch(k)*plf(k,i)*alk(k,i);
        if(awt(k,i)>0) kimura_tmp /= awt(k,i);
        if (!nsims){
          for (j=a1;j<=a2;j++) ac_out <<year<<" N_Kimura "<<k<<" "<<i<<" "<<j<<" "<<kimura_tmp(j) << endl;
          for (j=a1;j<=a2;j++) ac_out <<year<<" a "<<k<<" "<<i<<" "<<j<<" "<<awta(k,i,j) << endl;
        }
        awt_tmp1 += elem_prod(nage(k,i),awta(k,i));
        nat_tmp1 += nage(k,i);
        awt_tmp2(i) += elem_prod(nage(k,i),awta(k,i));
        awt_tmp3(k) += elem_prod(nage(k,i),awta(k,i));
        nat_tmp2(i) += nage(k,i);
        nat_tmp3(k) += nage(k,i);
      }
      double sumtmp = sum(nage(k));
      if (sumtmp>0.)
        page(k) = nage(k) / sumtmp;
      if (!nsims)
        for (i=1;i<=2;i++)
          for (j=a1;j<=a2;j++) ac_out <<year<<" p "<<k<<" "<< i<<" "<<j<<" "<<page(k,i,j) <<endl;
    }
    for (j=a1;j<=a2;j++)
    {
      if (nat_tmp1(j)>0.)
        awt_tmp1(j) /= nat_tmp1(j); 
      for (i=1;i<=2;i++)
        if (nat_tmp2(i,j)>0.)
          awt_tmp2(i,j) /= nat_tmp2(i,j); 
      for (int k=1;k<=nstrata;k++)
        if (nat_tmp3(k,j)>0.)
          awt_tmp3(k,j) /= nat_tmp3(k,j); 
    }
    if (!nsims){
      for (int k=1;k<=nstrata;k++)
      {
        for (j=a1;j<=a2;j++) ac_out <<year<<" N  "<<k<<" 99 "<<j<<" "<<nat_tmp3(k,j) << endl;
        for (j=a1;j<=a2;j++) ac_out <<year<<" a  "<<k<<" 99 "<<j<<" "<<awt_tmp3(k,j) << endl;
      }
      for (i=1;i<=2;i++)
      {
        for (j=a1;j<=a2;j++) ac_out <<year<<" N 99 "<<i<<" "<<j<<" "<<nat_tmp2(i,j) << endl;
        for (j=a1;j<=a2;j++) ac_out <<year<<" a 99 "<<i<<" "<<j<<" "<<awt_tmp2(i,j) << endl;
      }
      for (j=a1;j<=a2;j++) ac_out <<year<<" N 99 99 "<<j<<" "<<nat_tmp1(j)<< endl;
      for (j=a1;j<=a2;j++) ac_out <<year<<" a 99 99 "<<j<<" "<<awt_tmp1(j)<< endl;
    }
    // write out so data file can use directly...
    for (int k=1;k<=nstrata;k++)
    {
      catage_out_str <<isim<<" "<<sam_level<<" "<<year<<" "<<k<<" "<<nat_tmp3(k)<< endl;
      wtage_out_str  <<isim<<" "<<sam_level<<" "<<year<<" "<<k<<" "<<awt_tmp3(k)<< endl;
    }
    catage_out <<isim<<" "<<sam_level<<" "<<year<<" "<<nat_tmp1<< endl;
    wtage_out  <<isim<<" "<<sam_level<<" "<<year<<" "<<awt_tmp1<< endl;
    if (!nsims)
    for (j=l1;j<=l2;j++)
      alk_out <<year<<" 99 99 "<<galk(j)<<endl;
    if (!nsims)
    for (i=1;i<=2;i++)
      for (j=l1;j<=l2;j++)
        alk_out <<year<<" 99 "<<i<<" "<<salk(i,j)<<endl;
    if (!nsims)
    for (int k=1;k<=nstrata;k++)
      for (i=1;i<=2;i++)
        for (j=l1;j<=l2;j++)
          alk_out <<year<<" "<<k<<" "<<i<<" "<<alk(k,i,j)<<endl;
       
  cout << "Finished sim "<<isim<<" for "<<year<< endl;
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
  random_number_generator& rng= *pad_rng;
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
{
  delete pad_rng;
  pad_rng = NULL;
}

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
