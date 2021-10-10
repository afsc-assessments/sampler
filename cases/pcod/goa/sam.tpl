// Sampler in admb
DATA_SECTION
 // To do: add variance estimates for wts-at-age
 //        compare different ways to convert between weights at age and numbers
  // init_int year
  !!CLASS random_number_generator rng(123);
  !! *(ad_comm::global_datafile) >>  year; 
  !! *(ad_comm::global_datafile) >>  agefile; 
  !! *(ad_comm::global_datafile) >>  lenfile; 
  init_int na_rcrds
  init_int nl_rcrds
  init_int a1
  init_int a2
  int na 

  !! na=a2-a1+1;

  init_int l1
  init_int l2
  int nl 
  !! nl=l2-l1+1;
  init_int nstrata
  init_vector strata_catch(1,nstrata)  // sample this?
  !! *(ad_comm::global_datafile) >>  outfile; 
  !! cout <<"Writing to " <<outfile<< endl;
  !! cout <<"Nstrata : "<< nstrata<<endl<<strata_catch<<endl<<nl_rcrds<<endl;
  // !! cout <<agefile<<endl<<lenfile<<endl<<nl_rcrds<<endl;exit(1);

  !! ad_comm::change_datafile_name("BS_Setup.dat");
  init_int nsims;
  !! if (nsims==0) nsims=1;
  // Sampling level relative to baseline (2 = half, 10 equal one tenth etc
  init_number sam_level; 

  !! ad_comm::change_datafile_name(agefile);
  !! cout<<"Reading age data..."<<endl;
  init_matrix adata_in(1,na_rcrds,1,16)
  int natows
  int nltows
  vector a_tows(1,na_rcrds)       // Vector of tows with age data length na_rcrds
  vector l_tows(1,nl_rcrds)       // Vector of tows with age data length na_rcrds
  // vector a_recnum(1,na_rcrds+100) // Vector of rec numbers with age data length na_rcrds+100
  !! a_tows=column(adata_in,16);
  !! natows = max(a_tows);
  ivector na_sam_tow(1,natows);
  !! cout<<"Number of age tows: "<<natows<<endl;
  !! na_sam_tow.initialize();
  !! for (i=1;i<=na_rcrds;i++) na_sam_tow(a_tows(i))++; 
  int nsam_max
  !! nsam_max=max(na_sam_tow);
  matrix a_recnum(1,natows,1,nsam_max)
 LOCAL_CALCS
  a_recnum.initialize();
  int k=1;
  for (i=1;i<=natows;i++) {
    for (int j=1;j<=(na_sam_tow(i));j++) {
      a_recnum(i,j) = k;
      k++;
    }
  }
 END_CALCS



  // !!cout<<a_tows<<endl;cout<<"END"<<endl;exit(1);
  matrix adata(1,na_rcrds+2000,1,16)
  matrix ldata(1,nl_rcrds+400000,1,14)
  vector ta_ubs(1,natows);
  vector a_ubs(1,na_rcrds+2000);
  vector l_ubs(1,nl_rcrds+400000);


  !! ad_comm::change_datafile_name(lenfile);
  !! cout<<"Reading length data..."<<endl;
  init_matrix ldata_in(1,nl_rcrds,1,14)
  // !! ldata = ldata_in;

  !! l_tows=column(ldata_in,14);
  !! nltows = max(l_tows);if (nltows==0)  nltows=1;
  vector nl_sam_tow(1,nltows);
  !! nl_sam_tow.initialize();
  !! if (nltows>1) for (i=1;i<=nl_rcrds;i++) nl_sam_tow(l_tows(i))++;  else nl_sam_tow(1)=nl_rcrds;
  !! if (nltows>1) nsam_max=max(nl_sam_tow); else nsam_max=nl_rcrds;

  vector tl_ubs(1,nltows);
  !! tl_ubs.initialize();
  matrix l_recnum(1,nltows,1,nsam_max) //matrix of lfreq record numbers for each tow 
  !! cout<<"Number of Length tows: "<<nltows<<endl;
  !! cout<<"Number of age tows: "<<natows<<endl;

 LOCAL_CALCS
  l_recnum.initialize();
  k=1;
  for (i=1;i<=nltows;i++) {
    for (int j=1;j<=(nl_sam_tow(i));j++) {
      l_recnum(i,j) = k; // Set the record number from main data set to matrix by tow and w/in tow length sample
      // cout <<i<<" "<<j<<" "<<k<<endl;
      k++;
    }
  }
  // exit(1);
 END_CALCS

  matrix    awt(1,nstrata,1,3)       // average wt by sex and strata
  3darray  awtl(1,nstrata,1,3,l1,l2) // average wt by sex and strata, and length
  matrix  sawtl(1,3,l1,l2)           // avg wt by sex and length (over all strata)
  vector  gawtl(l1,l2)               // avg wt by length (over all sex, strata)
  matrix   cwt(1,nstrata,1,3)        // count
  3darray cwtl(1,nstrata,1,3,l1,l2)
  matrix  scwtl(1,3,l1,l2)
  vector  gcwtl(l1,l2)

  3darray awta(1,nstrata,1,3,a1,a2)  // avg wt age by sex and strata
  3darray   lf(1,nstrata,1,3,l1,l2)  // length freq
  3darray  nlf(1,nstrata,1,3,l1,l2)  // number length freq
  3darray  plf(1,nstrata,1,3,l1,l2)  // Length freq proportion
  4darray  alk(1,nstrata,1,2,l1,l2,a1,a2)
  3darray naged(1,nstrata,1,2,l1,l2)
  3darray  salk(1,2,l1,l2,a1,a2)
  matrix   galk(l1,l2,a1,a2)
  3darray  page(1,nstrata,1,3,a1,a2)
  3darray  nage(1,nstrata,1,3,a1,a2)
  3darray  plen(1,nstrata,1,3,l1,l2)
  number wt_tmp
  int age
  int len
  int sex
  int strat
  int i
  int j
 LOCAL_CALCS
   ofstream ac_out(outfile);
   ofstream catage_out("catage.out");
   ofstream catage_out_str("catage_str.out");
   ofstream catlen_out_str("catlen_str.out");
   ofstream  wtage_out("wtage.out");
   ofstream  wtage_out_str("wtage_str.out");
   ofstream lf_out("LF_"+year+".dat");
   ofstream alk_out("alk_"+year+".dat");
 // 1       2       3    4   5  6    7   8     9  10  11  12  13       14     15
 // Vess  Area  Depth Month Day yy  lat long  age sex spp wt  length  specno  samplesystem
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
       cout<<"Filled tows in length samples"<<endl;
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
    cout <<"Number of length tows: "<<nltows<<endl;
    for (i=1;i<=nltows*sam_level;i++)
    {
      int ii = int(tl_ubs(i));
      // cout<<tl_ubs<<endl;exit(1);
      // cout<<k<<" "<<ii<<" "<<nl_sam_tow(ii)<<endl;
      for (int j=1;j<=int(nl_sam_tow(ii));j++) 
      {
        // cout<<k<<" "<<ii<<" "<<i<<" "<<j<<" "<<l_recnum(ii,j)<<endl;
        ldata(k) = ldata_in(l_recnum(ii,j));
        k++;
      }
    }
    int n_bsl_rcrds = k-1; 
    if (n_bsl_rcrds==0) n_bsl_rcrds = nl_rcrds;
    // for (i=1;i<=n_bsa_rcrds;i++) 
    cout <<"NBS_age_records_and_original: "<<isim <<" "<<year <<" "<< n_bsa_rcrds<<" "<<na_rcrds<<endl;// exit(1);
    cout <<"NBS_len_records_and_original: "<<isim <<" "<<year <<" "<< n_bsl_rcrds<<" "<<nl_rcrds<<endl;// exit(1);

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
      // cout<<ldata<<endl;
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
      catlen_out_str <<isim<<" "<<sam_level<<" "<<year<<" "<<k<<" "<<lf(k,1)+lf(k,2)<< endl;
      // lf_out <<year<<" "<<k<<" "<<i<<" "<<j<<" "<<lf(k,i,j)<<" "<< plf(k,i,j)<< " "<< awtl(k,i,j)<<" "<<cwtl(k,i,j)<<" "<<nlf(k,i,j)<<" "<<naged(k,i,j)<<endl;
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
 END_CALCS

PARAMETER_SECTION
  init_number x;
  objective_function_value obj_fun;

PROCEDURE_SECTION


REPORT_SECTION


GLOBALS_SECTION
  #include <admodel.h>
  adstring year;
  adstring agefile;
  adstring lenfile;
  adstring outfile;
