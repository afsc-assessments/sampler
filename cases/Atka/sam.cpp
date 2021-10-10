  #include <admodel.h>
  adstring year;
  adstring agefile;
  adstring lenfile;
  adstring outfile;
#include <admodel.h>
#include <contrib.h>

  extern "C"  {
    void ad_boundf(int i);
  }
#include <gdbprintlib.cpp>

#include <sam.htp>

model_data::model_data(int argc,char * argv[]) : ad_comm(argc,argv)
{
  do_check=0;  
  if (argc > 1)
  {
    int on=0;
    if ( (on=option_match(argc,argv,"-io"))>-1)
    {
      do_check = 1;
    }
  }
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
 if(do_check) cout <<"Nstrata : "<< nstrata<<endl<<strata_catch<<endl<<nl_rcrds<<endl;
 if(do_check) cout <<agefile<<endl<<lenfile<<endl<<nl_rcrds<<endl;
 ad_comm::change_datafile_name("BS_Setup.dat");
  nsims.allocate("nsims");
 if (nsims==0) nsims=1;
  sam_level_1.allocate("sam_level_1");
  sam_level_2.allocate("sam_level_2");
  sam_level_3.allocate("sam_level_3");
 ad_comm::change_datafile_name(agefile);
 if(do_check) cout<<"Number of age records:  "<< na_rcrds <<endl;
 if(do_check) cout<<"Reading age data..."<<endl;
  adata_in.allocate(1,na_rcrds,1,16,"adata_in");
  a_tows.allocate(1,na_rcrds);
  l_tows.allocate(1,nl_rcrds);
 a_tows=column(adata_in,16);
 natows = max(a_tows);
  na_sam_tow.allocate(1,natows);
 if(do_check) cout<<"Number of age tows: "<<natows<<endl;
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
 /*
 */
 lenpad_bs = 9000000;
 agepad_bs = 800000;
  adata.allocate(1,na_rcrds+agepad_bs,1,16);
  ldata.allocate(1,nl_rcrds+lenpad_bs,1,5);
 if (do_check) cout <<"Number of rows from ldata "<<ldata.rowmax()<<endl;
ldata.initialize();
adata.initialize();
  ta_ubs.allocate(1,natows);
  a_ubs.allocate(1,na_rcrds+agepad_bs);
  l_ubs.allocate(1,nl_rcrds+lenpad_bs);
 ad_comm::change_datafile_name(lenfile);
 if(do_check) cout<<"Reading length data..."<<endl;
  ldata_in.allocate(1,nl_rcrds,1,5,"ldata_in");
 l_tows=column(ldata_in,2);
 nltows = max(l_tows);
  nlfrq_sam_tow.allocate(1,nltows);
  nfshl_sam_tow.allocate(1,nltows);
 nlfrq_sam_tow.initialize();
 nfshl_sam_tow.initialize();
 for (i=1;i<=nl_rcrds;i++) 
 {
   nlfrq_sam_tow(l_tows(i))++; // number of LF records for each tow
   nfshl_sam_tow(l_tows(i))+=ldata_in(i,5); // number of fish recorded for each tow
 }
 if(do_check) cout<<"Nlfrq: "<<sum(nlfrq_sam_tow)<<endl;
 if(do_check) cout<<"Nfshl: "<<sum(nfshl_sam_tow)<<endl;
 nsam_max=max(nlfrq_sam_tow);
 nsam_max=max(nlfrq_sam_tow);
  tl_ubs.allocate(1,nltows);
  l_recnum.allocate(1,nltows,1,nsam_max);
 if(do_check) cout<<"Number of Length tows: "<<nltows<<endl;
 if(do_check) cout<<"Number of age tows: "<<natows<<endl;
  l_recnum.initialize();
  if(do_check) cout << "N sample tows "<<nlfrq_sam_tow(l_tows(nl_rcrds)) <<" Nltows "<<nltows<<endl;
  k=1;
  for (i=1;i<=nltows;i++) 
  {
    for (int j=1;j<=nlfrq_sam_tow(i);j++) 
    {
      l_recnum(i,j) = k;
      k++;
    }
  }
  if(do_check) cout<< l_recnum(nltows,nlfrq_sam_tow(nltows))<<endl;
  if(do_check) cout<<"l_recnum final: "<< l_recnum(nltows,nlfrq_sam_tow(nltows)) <<endl;
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
   ofstream catage_out("catage"+year+".out");
   ofstream catage_out_str("catage"+year+"_str.out");
   ofstream  wtage_out("wtage"+year+".out");
   ofstream  wtage_out_str("wtage"+year+"_str.out");
   ofstream lf_out("LF_"+year+".dat");
   ofstream alk_out("alk_"+year+".dat");
  // 1       2       3    4   5  6    7   8     9  10  11  12  13       14     15
  // Vess  Area  Depth Month Day yy  lat long  age sex spp wt  length  specno  samplesystem
     random_number_generator& rng= *pad_rng;
     // Set up simulation loop here...if sims=0 then NOTE there is a zeroth simulation!!!
   // lf_out <<"Sim year stratum sex length freq prop " <<endl; // avg_wt nwt_samples nlen_samples naged"<<endl;
   lf_out <<"Sim year stratum sex length freq prop avg_wt nwt_samples nlen_samples naged"<<endl;
   for (int isim=1;isim<=nsims;isim++) 
   {
     if (nsims==1) nsims=0;
     if (!nsims) 
     {
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
       // l_ubs *= (nl_rcrds+1);
     }
     page.initialize();
     nage.initialize();
     plen.initialize();
     salk.initialize();
     galk.initialize();
     alk.initialize();
     naged.initialize();
     lf.initialize();
     awt.initialize();
     awta.initialize();
     awtl.initialize();
     sawtl.initialize();
     gawtl.initialize();
     cwtl.initialize();
     cwt.initialize();
     scwtl.initialize();
     gcwtl.initialize();
     plf.initialize();
     nlf.initialize();
     wt_tmp=0.;
     // if (!nsims)exit(1); // logic for if simulation
     // for (int i=1;i<=10000;i++) cout <<a_ubs(i)<<" "<<adata(a_ubs(i))<<endl;cout<<max(a_ubs)<<" "<<na_rcrds<<endl;exit(1);
    // Step 1,ample among tows, count total records for 
    int k=1;
    int ii ;
    if(do_check) cout<<"Number of age-tows "<<natows<<endl;
    for (i=1;i<=natows*sam_level_1;i++)
    {
      if (ta_ubs(i)>1)
        ii=int(ta_ubs(i));
      else
        ii=1;
      for (int j=1;j<=(na_sam_tow(ii));j++) 
      {
        // cout<<ii<<" "<<i<<" "<<j<<" "<<a_recnum(ii,j)<<endl;
        adata(k) = adata_in(a_recnum(ii,j));
        k++;
      }
    }
    // cout << adata_in<<endl;exit(1);
    int n_bsa_rcrds = k-1;
    // for (i=1;i<=n_bsa_rcrds;i++) 
    // Done age BS part
    /*
    */
    if(do_check) cout<<"Done age bootstrap part"<<endl; 
    k=1;
    int ik=1;
    for (i=1;i<=nltows*sam_level_2;i++)
    {
      if (tl_ubs(i)>1)
        ii=int(tl_ubs(i));
      else
        ii=1;
      for (int j=1;j<=nlfrq_sam_tow(ii);j++) 
      {
        // cout<<k<<" "<<ii<<" "<<i<<" "<<j<<" "<<l_recnum(ii,j)<<endl;
        for (int kk=1;kk<=ldata_in(l_recnum(ii,j),5);kk++)
        {
          // Strata tow sex length freq
          // cout<<ldata_in(l_recnum(ii,j))(1,4)<<endl;
          ldata(ik)(1,4) = ldata_in(l_recnum(ii,j))(1,4); // pick first 4 elements of vector (not freq)
          if (ldata(ik,1)<1) cout<< l_recnum(ii,j)<<" in "<<ldata_in(l_recnum(ii,j))<<endl;
          ldata(ik,5) = 1;
          ik++;
        }
        k++;
      }
    }
    if(do_check) cout<<"Done next bootstrap part"<<endl; 
    int n_bsl_rcrds = ik-1;
    if (nsims) 
      l_ubs *= n_bsl_rcrds +1;
    // for (i=1;i<=n_bsa_rcrds;i++) 
    // cout <<n_bsa_rcrds<<" "<<na_rcrds<<endl;// exit(1);
    if (do_check) cout <<"Bootstrap length records: "<< n_bsl_rcrds<<" "<<nl_rcrds<<endl;// exit(1);
    for (i=1;i<=n_bsa_rcrds;i++)
    {
      if (a_ubs(i)>1)
        ii=int(a_ubs(i));
      else
        ii=1;
      age   = int(adata(ii,9));
      len   = int(adata(ii,13));
      sex   = int(adata(ii,10));
      if (sex==0) sex=3;
      strat = int(adata(ii,1));
      // cout <<i<<" "<<age<<" "<<len<<" "<<sex<<" "<<strat<<endl;
      wt_tmp= adata(ii,12);
      if (strat > nstrata) {cout<<"age data strata too high "<<adata(i)<<endl<<nstrata<<endl<<strat<<" "<<wt_tmp<<" "<<i<<endl;exit(1);}
      if (len<l1) len=l1;
      if (len>l2) len=l2;
      // if (len>=l1&&len<=l2) 
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
    /*
    */
    for (i=1;i<=n_bsl_rcrds;i++)
    {
      if (l_ubs(i)>1)
        ii=int(l_ubs(i));
      else
        ii=1;
      double frq   = ldata(ii,5);
      len   = ldata(ii,4);
      sex   = ldata(ii,3);
      if (sex==0) sex=3;
      strat = ldata(ii,1);
      if (strat > nstrata) {cout<<"length data strata too high "<<i<<endl<<ldata(i)<<endl;exit(1);}
      if (len<l1) len=l1;
      if (len>l2) len=l2;
      // if (len>=l1 && len<=l2) // Changed to accumulate below and above l1 and l2 sizes
      if (strat<1){ cout<<ii<<" "<<ldata(ii)<<endl;exit(1);}
        lf(strat,sex,len)+=frq;
    }
  // Done reading data, now put in arrays, add unsexed to males and females equally....
    // if (!nsims)
    for (int k=1;k<=nstrata;k++)
    {
      dmatrix wtfrq(1,2,l1,l2);
      wtfrq.initialize();
      double sumlf ;
      sumlf=0.;
      for (i=1;i<=2;i++)
      {
        lf(k,i)   +=  .5*lf(k,3);      // Add unsexed to LFreq
        nlf(k,i)   =  lf(k,i);
        sumlf     +=  sum(nlf(k,i));
        cwt(k,i)  +=  cwt(k,3); // add unsexed to the males and females
        awt(k,i)  +=  awt(k,3); // add unsexed to the males and females
        cwtl(k,i) +=  cwtl(k,3);      // add unsexed to the males and females
        awtl(k,i) +=  awtl(k,3);      // add unsexed to the males and females
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
        // wtfrq(i) = lf(k,i); // For Salmon
      }
      if (sumlf >0.)
        plf(k)  = nlf(k)/sumlf;
      double sumtmp2;
      sumtmp2  = sum(wtfrq);
      if (sumtmp2 > 0)
        wtfrq /= sumtmp2;        // normalize by sex, length
      wtfrq *= strata_catch(k); // raise by catch in this stratum
      // Put back into length frequency...
      for (i=1;i<=2;i++)
      {
        for (j=l1;j<=l2;j++)
        {
          if (awtl(k,i,j)>0) 
            // lf(k,i,j) = wtfrq(i,j);
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
    /*
    */
      for (i=1;i<=2;i++)
      {
        x1=sum(salk(i,j));
        if (x1>0.)
          salk(i,j) /= x1;
        else
          salk(i,j) = galk(j); // fill in holey alk
  /*
  */
        for (int k=1;k<=nstrata;k++)
        {
          x1=sum(alk(k,i,j));
          naged(k,i,j) = x1;
          if (x1>0.)
            alk(k,i,j) /= x1;
          else
            alk(k,i,j) = salk(i,j); // fill in holey alk
  /*
  */
          if (!nsims)  // Don't print out length frequency for bootstraps...
          {
            // lf_out <<isim<<" "<< year<<" "<<k<<" "<<i<<" "<<j<<" "<<lf(k,i,j)<<" "<< plf(k,i,j)<< endl;// " "<< awtl(k,i,j)<<" "<<cwtl(k,i,j)<<" "<<nlf(k,i,j)<<" "<<naged(k,i,j)<<endl;
            lf_out <<isim<<" "<< year<<" "<<k<<" "<<i<<" "<<j<<" "<<lf(k,i,j)<<" "<< plf(k,i,j)<<" "<< awtl(k,i,j)<<" "<<cwtl(k,i,j)<<" "<<nlf(k,i,j)<<" "<<naged(k,i,j)<<endl;
          }
        }
      }
    }
  //   cout <<galk<<endl<<endl<<salk<<endl<<endl<<alk<<endl;
    // cout <<"this N strata: "<<nstrata<<endl;
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
        awtmp     = elem_prod(awtl(k,i),lf(k,i))*alk(k,i);
        for (j=a1;j<=a2;j++)
        {
          if (nage(k,i,j)>0.)
            awta(k,i,j) = awtmp(j)/nage(k,i,j); 
        }
        if (!nsims){
          for (j=a1;j<=a2;j++) ac_out <<year<<" N "      <<k<<" "<<i<<" "<<j<<" "<<nage(k,i,j)             << endl;
          for (j=a1;j<=a2;j++) ac_out <<year<<" Biomass "<<k<<" "<<i<<" "<<j<<" "<<nage(k,i,j)*awta(k,i,j) << endl;
        }
        // cout <<year<<" N_Kimura "<<k<<" "<<i<<" "<<endl; cout <<"Strata catch: "<<strata_catch(k,i)*awt(k,i)<<" "<<endl; cout<< plf(k,i)<<endl; cout << alk(k,i) << endl;
        dvector kimura_tmp = strata_catch(k)*plf(k,i)*alk(k,i);
        if(awt(k,i)>0) 
          kimura_tmp /= awt(k,i);
        if (!nsims){
          for (j=a1;j<=a2;j++) ac_out <<year<<" N_Kimura   "<<k<<" "<<i<<" "<<j<<" "<<kimura_tmp(j) << endl;
          for (j=a1;j<=a2;j++) ac_out <<year<<" avg_wt_age "<<k<<" "<<i<<" "<<j<<" "<<awta(k,i,j) << endl;
        }
        awt_tmp1    += elem_prod(nage(k,i),awta(k,i));
        nat_tmp1    += nage(k,i);
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
      catage_out_str <<isim<<" "<<
      sam_level_1<<"_"<<sam_level_2<<"_"<<sam_level_3
      <<" "<<year<<" "<<k<<" "<<nat_tmp3(k)<< endl;
      wtage_out_str  <<isim<<" "<<
      sam_level_1<<"_"<<sam_level_2<<"_"<<sam_level_3
      <<" "<<year<<" "<<k<<" "<<awt_tmp3(k)<< endl;
    }
    catage_out <<isim<<" "<<
      sam_level_1<<"_"<<sam_level_2<<"_"<<sam_level_3
    <<" "<<year<<" "<<nat_tmp1<< endl;
    wtage_out  <<isim<<" "<<
      sam_level_1<<"_"<<sam_level_2<<"_"<<sam_level_3
    <<" "<<year<<" "<<awt_tmp1<< endl;
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
       
  /*
       */
  if (isim%int(nsims/10)==0) 
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
  prior_function_value.allocate("prior_function_value");
  likelihood_function_value.allocate("likelihood_function_value");
}

void model_parameters::userfunction(void)
{
  obj_fun =0.0;
  random_number_generator& rng= *pad_rng;
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
