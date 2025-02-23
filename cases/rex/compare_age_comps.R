#compare age comps between last assessment and the current data pull

compare_age_comps<-function(new_dat,old_dat,yrs_srv_age,maxage = max_age) {
  old_long<-as.data.frame(old_dat) %>% rename_all(function(x) gsub("X","",x)) %>% 
           pivot_longer(!year,names_to="age",values_to = "prop") %>% 
           mutate(age = as.numeric(age),assess = "old") %>%
           mutate(sex = ifelse(age<=maxage,"F","M")) %>%
           mutate(age = ifelse(age>100,as.numeric(substr(age,3,5)),age))

  
  old_males_long<-old_long %>% filter(sex == "M")
  old_females_long<-old_long %>% filter(sex == "F")

  
#remake new_long
   new_long<-new_data %>% 
    pivot_longer(!year,names_to="age",values_to = "prop") %>% 
    mutate(age = as.numeric(age),assess = "new") %>%
    mutate(sex = ifelse(age<=maxage,"F","M")) %>%
    mutate(age = ifelse(age>100,age-100,age))
   
   #might not need this
  all_long<-old_long %>% bind_rows(new_long)
  
  new_long2<-new_long %>% rename(prop_new = prop) %>% select(-c(assess))
  old_long2<-old_long %>% rename(prop_old = prop) %>% select(-c(assess))
  all_wide<-inner_join(old_long2,new_long2) %>% mutate (diffs = round(prop_new-prop_old,5))
  

  plot_diffs <-ggplot(data = all_wide) +
    geom_col(data = all_wide,aes(x = age, y = diffs), fill = "cadetblue", alpha = 0.6) +
    labs(x = "Ages", y = "Old-New Differences") + facet_wrap(vars(year))
  
  #Overlay data:
  
  lcombo <- ggplot(data = old_long) +
    geom_col(data = old_long,aes(x =  age, y =  prop), fill = "#55C667FF", alpha = 0.6) +
    geom_col(data = new_long,aes(x =  age, y =  prop), fill = "#39568CFF",alpha = 0.6) +
     labs(x = "Ages", y = "Proportion",color = "assess")
 
    lcombo<-lcombo + facet_wrap(vars(year))

theplots = list()
theplots$plot_diffs <-plot_diffs
theplots$plot_overlay <-lcombo
return(theplots)
  
}