DATA_SECTION
 // To do: add variance estimates for wts-at-age
 //        compare different ways to convert between weights at age and numbers
  // init_int year
  int do_check;
  int do_sim;
  int TowsOnly;
	int rseed;
 LOCAL_CALCS
   TowsOnly=0;
  rseed = 123;
  do_check=0;  
  if (argc > 1)
  {
    int on=0;
    if ( (on=option_match(argc,argv,"-io"))>-1)
    {
      do_check = 1;
    }
    if ( (on=option_match(argc,argv,"-sim"))>-1)
    {
      do_sim = 1;
    }
  }
 END_CALCS
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
  !! if(do_check) cout <<"Nstrata : "<< nstrata<<endl<<strata_catch<<endl<<nl_rcrds<<endl;
  !! if(do_check) cout <<agefile<<endl<<lenfile<<endl<<nl_rcrds<<endl;

  !! ad_comm::change_datafile_name("bs_setup.dat");
  init_int nbs;
  !! if (nbs==0||do_sim) nbs=1;
  // Sampling level relative to baseline (2 = half, 10 equal one tenth etc
  init_number sam_level_age_tows; 
  init_number sam_level_ages; 
  init_number sam_level_lf_tows; 
  init_number sam_level_lfreqs; 

  !! ad_comm::change_datafile_name(agefile);
  !! if(do_check) cout<<"Sampling levels: "<<endl<<sam_level_age_tows<<endl<<sam_level_ages<<endl<<sam_level_lf_tows<<endl<<"Number of age records:  "<< na_rcrds <<endl;
  !! if(do_check) cout<<"Reading age data..."<<endl;
  init_matrix adata_in(1,na_rcrds,1,6)
  int natows
  int nltows
  vector a_tows(1,na_rcrds)       // Vector of tows with age data length na_rcrds
  vector l_tows(1,nl_rcrds)       // Vector of tows with age data length na_rcrds
  // vector a_recnum(1,na_rcrds+100) // Vector of rec numbers with age data length na_rcrds+100
  !! a_tows=column(adata_in,2);
  !! natows = max(a_tows);
  ivector na_sam_tow(1,natows);
  !! if(do_check) cout<<"Number of age tows: "<<natows<<endl;
  !! na_sam_tow.initialize();
  !! for (i=1;i<=na_rcrds;i++) na_sam_tow(a_tows(i))++; 
  int nsam_max
  !! nsam_max=max(na_sam_tow);
  matrix a_recnum(1,natows,1,nsam_max)
  int lenpad_bs;
  int agepad_bs;
 LOCAL_CALCS
  a_recnum.initialize();
  int k=1;
  for (i=1;i<=natows;i++) {
    for (int j=1;j<=(na_sam_tow(i));j++) {
      a_recnum(i,j) = k;
      k++;
    }
  }
  lenpad_bs = 9000000;
  agepad_bs = 800000;
 END_CALCS
  matrix adata(1,na_rcrds+agepad_bs,1,6)
  matrix ldata(1,nl_rcrds+lenpad_bs,1,5)
 LOCAL_CALCS
  //!!if(do_check) cout<<a_tows<<endl;cout<<"END"<<endl;
  if (do_check) cout <<"Number of rows from ldata "<<nl_rcrds<<endl;
  if (do_check) cout <<"Number of rows from adata "<<na_rcrds<<endl;
  ldata.initialize();
  adata.initialize();

  ad_comm::change_datafile_name(lenfile);
  if(do_check) cout<<"Reading length data..."<<endl;
 END_CALCS
  init_matrix ldata_in(1,nl_rcrds,1,5)
  vector ta_ubs(1,natows);
  vector a_ubs(1,na_rcrds+agepad_bs);
  vector l_ubs(1,nl_rcrds+lenpad_bs);
  // !! ldata = ldata_in;

  // Strata tow sex length freq
  !! l_tows=column(ldata_in,2);
  !! nltows = max(l_tows);
  vector nlfrq_sam_tow(1,nltows);
  vector nfshl_sam_tow(1,nltows);
 LOCAL_CALCS
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
  if(do_check) cout<<"Number of Length tows: "<<nltows<<endl;
  if(do_check) cout<<"Number of age tows: "<<natows<<endl;
 END_CALCS

  vector tl_ubs(1,nltows);
  matrix l_recnum(1,nltows,1,nsam_max)

 LOCAL_CALCS
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
  int also_write;
 LOCAL_CALCS
   ofstream ac_out(outfile);
   ofstream catage_out("results/catage"+year+".rep");
   ofstream catage_out_str("results/str_catage"+year+".rep");
   ofstream  wtage_out("results/wtage"+year+".rep");
   ofstream  wtage_out_str("results/str_wtage"+year+".rep");
   ofstream lf_out("results/LF_"+year+".rep");
   ofstream alk_out("results/alk_"+year+".rep");
   ofstream wl_out("results/wl_"+year+".rep");
  // 1       2       3    4   5  6    7   8     9  10  11  12  13       14     15
  // Vess  Area  Depth Month Day yy  lat long  age sex spp wt  length  specno  samplesystem
     random_number_generator rng(rseed);
   // Set up bootstrap loop here...if nbs=0 then NOTE there is a zeroth bootstrap!!!
   // lf_out <<"Bootstrap year stratum sex length freq prop " <<endl; // avg_wt nwt_samples nlen_samples naged"<<endl;
   lf_out <<"Bootstrap year stratum sex length freq prop avg_wt nwt_samples nlen_samples naged"<<endl;

   if (nbs==1) also_write=0; else also_write = 1;
   for (int itmp=0; itmp <= also_write;itmp++)
   {
    if (itmp==1) nbs=1; // this just writes the Estimate file in addition to doing bootstrap
   for (int ibs=1;ibs<=nbs;ibs++) 
   {
     if (nbs==1) nbs=0;
     if (!nbs) 
     {
       a_ubs.fill_seqadd(1,1); 
       ta_ubs.fill_seqadd(1,1); 

       l_ubs.fill_seqadd(1,1); 
       tl_ubs.fill_seqadd(1,1); 
     }
     else
     {
       a_ubs.fill_randu(rng);
       l_ubs.fill_randu(rng);
       ta_ubs.fill_randu(rng);
       tl_ubs.fill_randu(rng);

       ta_ubs *= (natows+1);
       tl_ubs *= (nltows+1);

       a_ubs *= (na_rcrds+1);
       // l_ubs *= (nl_rcrds+1); // This commented out because by frequency not number of records...right?

       // Ageonly reset to not sample from lengths
       // a_ubs.fill_seqadd(1,1); 
       // ta_ubs.fill_seqadd(1,1); 
       // l_ubs.fill_seqadd(1,1); 
       // tl_ubs.fill_seqadd(1,1); 
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
     // if (!nbs)exit(1); // logic for if simulation
     // for (int i=1;i<=10000;i++) cout <<a_ubs(i)<<" "<<adata(a_ubs(i))<<endl;cout<<max(a_ubs)<<" "<<na_rcrds<<endl;exit(1);

    // Step 1, sample among tows, count total records for lengths and ages
    int k=1;
    int ii ;
    if(do_check) cout<<"Number of age-tows "<<natows<<endl;
    for (i=1;i<=int(natows*sam_level_age_tows);i++)
    {
      if (ta_ubs(i)>1)
        ii=int(ta_ubs(i));
      else
        ii=1;
      for (int j=1;j<=int(sam_level_ages*na_sam_tow(ii));j++) 
      {
        // cout<<ii<<" "<<i<<" "<<j<<" "<<a_recnum(ii,j)<<endl;
        adata(k) = adata_in(a_recnum(ii,j));
        k++;
      }
    } // Loop over age tows
    // cout << adata_in<<endl;exit(1);
    int n_bsa_rcrds = k-1;
    // for (i=1;i<=n_bsa_rcrds;i++) 
    // Done age BS part
    /*
    */
    if(do_check) cout<<"Done age bootstrap part"<<endl; 
    k=1;
    int ik=1;
    for (i=1;i<=int(nltows*sam_level_lf_tows);i++)
    {
      if (tl_ubs(i)>1)
        ii=int(tl_ubs(i));
      else
        ii=1;
      for (int j=1;j<=int(sam_level_lfreqs*nlfrq_sam_tow(ii));j++) 
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
    } // Loop over length tows
    int n_bsl_rcrds = ik-1;
    if(do_check) {
						cout<<"Done length bootstrap part"<<endl; 
						cout<<"nbs "<<nbs<<endl; 
						cout<<"nltows "<<nltows<<endl; 
						cout<<"sam_level_lf_tows "<<sam_level_lf_tows<<endl; 
		}
    // AgeOnly if (nbs) l_ubs *= n_bsl_rcrds +1;
    if (nbs) l_ubs *= n_bsl_rcrds +1;
    // for (i=1;i<=n_bsa_rcrds;i++) 
    // cout <<n_bsa_rcrds<<" "<<na_rcrds<<endl;// exit(1);
    if (do_check) cout <<"Bootstrap length records: "<< n_bsl_rcrds<<" "<<nl_rcrds<<endl;// exit(1);

    for (i=1;i<=n_bsa_rcrds;i++)
    {
      if (a_ubs(i)>1) ii=int(a_ubs(i)); else ii=1;
      age   = int(adata(ii,4));
      len   = int(adata(ii,6));
      sex   = int(adata(ii,3));
      if (sex==0) sex=3;
      strat = int(adata(ii,1));
      // cout <<i<<" "<<age<<" "<<len<<" "<<sex<<" "<<strat<<endl;
      wt_tmp= adata(ii,5);
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
          if (age>a2) age=a2; // Put in plus group
          galk(len,age)++;
          if (sex==3) // Split unsexed fish into sex-spp ALK and global
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
        ii=int(l_ubs(i)); // Draw from length subsample
      else
        ii=1;
      // if (TowsOnly) ii=i;
      double frq   = ldata(ii,5);
      len          = ldata(ii,4);
      sex          = ldata(ii,3);
      if (sex==0) sex=3;
      strat        = ldata(ii,1);
      if (strat > nstrata) {cout<<"length data strata too high "<<i<<endl<<ldata(i)<<endl;exit(1);}
      if (len<l1) len=l1; // Sum into minus bin
      if (len>l2) len=l2; // Sum into plus bin
      // if (len>=l1 && len<=l2) // Changed to accumulate below and above l1 and l2 sizes
      if (strat<1){ cout<<"Strata less than 1 "<<strat<<" "<<ii<<" "<<ldata(ii)<<endl;exit(1);}
      lf(strat,sex,len)+=frq;
    }
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

          if (!nbs)  // Don't print out length frequency for bootstraps...
            lf_out <<ibs<<" "<< year<<" "<<k<<" "<<i<<" "<<j<<" "<<lf(k,i,j)<<" "<< plf(k,i,j)<<" "<< awtl(k,i,j)<<" "<<cwtl(k,i,j)<<" "<<nlf(k,i,j)<<" "<<naged(k,i,j)<<endl;
        }
      }
    }
  //   cout <<galk<<endl<<endl<<salk<<endl<<endl<<alk<<endl;
    // cout <<"this N strata: "<<nstrata<<endl;
    if (!nbs) ac_out <<"year type stratum sex age value "<<endl;
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
		/*
    awt_tmp1 = 1.;
    awt_tmp2 = 1.;
    awt_tmp3 = 1.;
		awtl     = 1.;
		awta     = 1.;
		*/
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
        if (!nbs){
          for (j=a1;j<=a2;j++) ac_out <<year<<" N "      <<k<<" "<<i<<" "<<j<<" "<<nage(k,i,j)             << endl;
          for (j=a1;j<=a2;j++) ac_out <<year<<" Biomass "<<k<<" "<<i<<" "<<j<<" "<<nage(k,i,j)*awta(k,i,j) << endl;
        }
        // cout <<year<<" N_Kimura "<<k<<" "<<i<<" "<<endl; cout <<"Strata catch: "<<strata_catch(k,i)*awt(k,i)<<" "<<endl; cout<< plf(k,i)<<endl; cout << alk(k,i) << endl;
        dvector kimura_tmp = strata_catch(k)*plf(k,i)*alk(k,i);
        if(awt(k,i)>0) 
          kimura_tmp /= awt(k,i);
        if (!nbs){
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
      if (!nbs)
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

    if (!nbs)
    {
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
      catage_out_str <<ibs<<" "<< sam_level_age_tows<<"_"<<sam_level_ages<<"_"<<sam_level_lf_tows<<"_"<<sam_level_lfreqs<<" "<<year<<" "<<k<<" "<<nat_tmp3(k)<< endl;
      wtage_out_str  <<ibs<<" "<< sam_level_age_tows<<"_"<<sam_level_ages<<"_"<<sam_level_lf_tows<<"_"<<sam_level_lfreqs<<" "<<year<<" "<<k<<" "<<awt_tmp3(k)<< endl;
    }
    catage_out       <<ibs<<" "<<sam_level_age_tows<<"_"<<sam_level_ages<<"_"<<sam_level_lf_tows<<"_"<<sam_level_lfreqs<<"  "<<year<<" "<<nat_tmp1<< endl;
    
    wtage_out        <<ibs<<" "<< sam_level_age_tows<<"_"<<sam_level_ages<<"_"<<sam_level_lf_tows<<"_"<<sam_level_lfreqs<<" "<<year<<" "<<awt_tmp1<< endl;
    wtage_out_str    <<ibs<<" "<< sam_level_age_tows<<"_"<<sam_level_ages<<"_"<<sam_level_lf_tows<<"_"<<sam_level_lfreqs<<" "<<year<<" "<<"999"<<" "<<awt_tmp1<< endl;

    if (!nbs)
    {
      for (j=l1;j<=l2;j++)
        alk_out <<year<<" 99 99 "<<galk(j)<<endl;

      for (i=1;i<=2;i++)
        for (j=l1;j<=l2;j++)
          alk_out <<year<<" 99 "<<i<<" "<<salk(i,j)<<endl;
  
      for (int k=1;k<=nstrata;k++)
        for (i=1;i<=2;i++)
          for (j=l1;j<=l2;j++)
            alk_out <<year<<" "<<k<<" "<<i<<" "<<alk(k,i,j)<<endl;
    }
       
    if (do_sim==1)
    {
      ofstream sim_age_out("sim_age_out.dat");
      ofstream sim_len_out("sim_len_out.dat");
      ofstream sim_tru_out("sim_tru_out.dat");
      dvector lwb(1,3);
      lwb(1) = 3.038;
      lwb(2) = 2.986;
      lwb(3) = 2.9954;
      dvector lwa(1,3);
      lwa(1) = 0.000004919;
      lwa(2) = 0.000006681;
      lwa(3) = 6.3611E-06;
      dvector _ages(a1,a2);
      for (int j=a1;j<=a2;j++) _ages(j) = double(j);
      dvector _awt(a1,a2);
      _awt   = 0.;
      dvector tru_caa_all(a1,a2);
      dvector tru_catch_biom(1,nstrata);
      tru_catch_biom = 0.;
      tru_caa_all = 0.;
      for (int isex=1;isex<=2;isex++)
      {
        for (int istr=1;istr<=nstrata;istr++)
        {
          tru_caa_all += nage(istr,isex);
          _awt         = lwa(isex)*pow(68*(1.-exp(-0.2*_ages)),lwb(isex) );
          // tru_catch_biom(istr) += awta(istr,isex)*nage(istr,isex);
          tru_catch_biom(istr) += _awt*nage(istr,isex);
            // awt_tmp2(i,j) /= nat_tmp2(i,j); 
          dvector tru_caa = page(istr,isex);
          int _nh,_nas,_nls; // Number of hauls, ages, and lengths
          _nh=300; _nas=2; _nls = 10;
          dvector _hauls(1,_nh);
          dvector _asamples(1,_nas);
          dvector _lsamples(1,_nls);
          dvector eps_as(1,_nas);
          dvector eps_ls(1,_nls);
          dvector sumS(a1,a2);
          dvector sumH(a1,a2);
          for (int ih = 1;ih<=_nh;ih++)
          {
            eps_as.fill_randn(rng);
            _asamples.fill_multinomial(rng,tru_caa);
            for (int j=1;j<=_nas;j++)
            {
              int _age = _asamples(j);
              double _len = 68*(1.-exp(-0.2*_age)) ;
              _len       += _len*0.08*eps_as(j);
              double _wt  = lwa(isex)*pow(10*_len,lwb(isex));
              sim_age_out << istr<<" " << ih<<" "<< isex <<" "<< _age <<" "<< _wt << " " << int(_len+.5)  <<endl;
            }
            eps_ls.fill_randn(rng);
            _lsamples.fill_multinomial(rng,tru_caa);
            for (int j=1;j<=_nls;j++)
            {
              int _age = _lsamples(j);
              double _len = int(68*(1.-exp(-0.2*_age)) );
              _len       += _len*0.08*eps_ls(j);
              sim_len_out << istr<<" " << ih<<" "<< isex <<" "<< int(_len+.5) <<" "<< 1 << endl;
            }
          }
        }
        sim_tru_out << "Avg_wt sex "<< isex<<" "<< _awt <<" "<<endl;
      }
      sim_tru_out <<    tru_caa_all    <<endl;
      sim_tru_out <<    tru_catch_biom <<endl;
    }
    if (ibs%int(nbs/10)==0) cout << "Finished bs "<<ibs<<" for "<<year<< endl;

  }
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
