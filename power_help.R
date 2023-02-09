# I


#########################
#POWER SIMULATIONS (scenario 3)
##########################
#The mean and SD are calculated separately for change in each of the 9 outcome components for two groups: 
#those in "vanish_1" (early-stage HCM) with the "valsartan" column equal to "A" (receiving valsartan), and those in "vanish_2" (Preclinical HCM) with the "valsartan" column equal to "P" (receiving placebo). 
#The resulting means are stored in "vanish_active_mu" and "vanish_placebo_mu", 
#while the resulting standard deviations are stored in "vanish_active_sd" and "vanish_placebo_sd".

#The calculation of mean and SD is done using the "mean" and "sd" functions, respectively, 
#with the "na.rm" argument set to "T" to remove missing values. 
#The results are concatenated using the "c" function, which combines multiple objects into a single vector.

vanish_active_mu<- c(
  vanish_1$flvedv2_dif_bsa_z[vanish_1$valsartan=="A"] %>% mean(.,na.rm=T),
  vanish_1$flvesv2_dif_bsa_z[vanish_1$valsartan=="A"] %>% mean(.,na.rm=T),
  vanish_1$flav2_dif_bsa_z[vanish_1$valsartan=="A"] %>% mean(.,na.rm=T),
  vanish_1$mlvwt2_zdif_z[vanish_1$valsartan=="A"] %>% mean(.,na.rm=T),
  vanish_1$ewvs2_zdif_z[vanish_1$valsartan=="A"] %>% mean(.,na.rm=T),
  vanish_1$swvs2_zdif_z[vanish_1$valsartan=="A"] %>% mean(.,na.rm=T),
  vanish_1$flvmass2_dif_bsa_z[vanish_1$valsartan=="A"] %>% mean(.,na.rm=T),
  vanish_1$tnt2_chg_z[vanish_1$valsartan=="A"] %>% mean(.,na.rm=T),
  vanish_1$ntbnp2_chg_z[vanish_1$valsartan=="A"] %>% mean(.,na.rm=T))

vanish_active_sd<- c(
  vanish_1$flvedv2_dif_bsa_z[vanish_1$valsartan=="A"] %>% sd(.,na.rm=T),
  vanish_1$flvesv2_dif_bsa_z[vanish_1$valsartan=="A"] %>% sd(.,na.rm=T),
  vanish_1$flav2_dif_bsa_z[vanish_1$valsartan=="A"] %>% sd(.,na.rm=T),
  vanish_1$mlvwt2_zdif_z[vanish_1$valsartan=="A"] %>% sd(.,na.rm=T),
  vanish_1$ewvs2_zdif_z[vanish_1$valsartan=="A"] %>% sd(.,na.rm=T),
  vanish_1$swvs2_zdif_z[vanish_1$valsartan=="A"] %>% sd(.,na.rm=T),
  vanish_1$flvmass2_dif_bsa_z[vanish_1$valsartan=="A"] %>% sd(.,na.rm=T),
  vanish_1$tnt2_chg_z[vanish_1$valsartan=="A"] %>% sd(.,na.rm=T),
  vanish_1$ntbnp2_chg_z[vanish_1$valsartan=="A"] %>% sd(.,na.rm=T))

vanish_placebo_mu<- c(
  vanish_2$flvedv2_dif_bsa_z[vanish_2$valsartan=="P"] %>% mean(.,na.rm=T),
  vanish_2$flvesv2_dif_bsa_z[vanish_2$valsartan=="P"] %>% mean(.,na.rm=T),
  vanish_2$flav2_dif_bsa_z[vanish_2$valsartan=="P"] %>% mean(.,na.rm=T),
  vanish_2$mlvwt2_zdif_z[vanish_2$valsartan=="P"] %>% mean(.,na.rm=T),
  vanish_2$ewvs2_zdif_z[vanish_2$valsartan=="P"] %>% mean(.,na.rm=T),
  vanish_2$swvs2_zdif_z[vanish_2$valsartan=="P"] %>% mean(.,na.rm=T),
  vanish_2$flvmass2_dif_bsa_z[vanish_2$valsartan=="P"] %>% mean(.,na.rm=T),
  vanish_2$tnt2_chg_z[vanish_2$valsartan=="P"] %>% mean(.,na.rm=T),
  vanish_2$ntbnp2_chg_z[vanish_2$valsartan=="P"] %>% mean(.,na.rm=T))

vanish_placebo_sd<- c(
  vanish_2$flvedv2_dif_bsa_z[vanish_2$valsartan=="P"] %>% sd(.,na.rm=T),
  vanish_2$flvesv2_dif_bsa_z[vanish_2$valsartan=="P"] %>% sd(.,na.rm=T),
  vanish_2$flav2_dif_bsa_z[vanish_2$valsartan=="P"] %>% sd(.,na.rm=T),
  vanish_2$mlvwt2_zdif_z[vanish_2$valsartan=="P"] %>% sd(.,na.rm=T),
  vanish_2$ewvs2_zdif_z[vanish_2$valsartan=="P"] %>% sd(.,na.rm=T),
  vanish_2$swvs2_zdif_z[vanish_2$valsartan=="P"] %>% sd(.,na.rm=T),
  vanish_2$flvmass2_dif_bsa_z[vanish_2$valsartan=="P"] %>% sd(.,na.rm=T),
  vanish_2$tnt2_chg_z[vanish_2$valsartan=="P"] %>% sd(.,na.rm=T),
  vanish_2$ntbnp2_chg_z[vanish_2$valsartan=="P"] %>% sd(.,na.rm=T))


#The code is performing a simulation for a two-sample t-test, where the outcome variable is being compared between two groups: 
#"active" and "placebo". Here the active group is simulated based on results from the early-stage HCM cohort and the placebo group is calculated from the preclinical.
#The simulation is being performed a thousand times, as indicated by the replicate(1000, {...}) function.
# In each iteration of the simulation, the following steps are performed:
#   
simulation_vanish<- 
 replicate(1000, {
   #   A data frame k is created, which will store the results of the t-test performed in each iteration of the loop.
   k <- c()
   # A vector cohort2 is created, which stores the sample size for each iteration of the loop.
   cohort2 = seq(20,500,20)
   # A loop is performed 25 times, where the index of the loop is stored in x
   for (x in 1:25) {
     # Within each iteration of the loop, two datasets are generated. 
     #The first dataset has the group label "active" and is generated using the rnorm_multi function
     # which generates multivariate normally distributed data taking correlated features into account. 
     #The mean (mu) and covariance (r) are specified. 
     #The second dataset has the group label "placebo" and is generated in a similar manner as the first dataset. 
     #Both datasets are combined into a single data frame dat using rbind.
     dat<-
       rbind(
        rnorm_multi(n= cohort2[x], 
                    varnames = c("q","w","e","r","t","y","u","i","o"),
                    mu = vanish_active_mu,
                    sd = vanish_active_sd,
                    r =cor(vanish_1 %>% select(flvedv2_dif_bsa_z,flvesv2_dif_bsa_z,flav2_dif_bsa_z,
                                               mlvwt2_zdif_z,ewvs2_zdif_z,swvs2_zdif_z,flvmass2_dif_bsa_z,
                                               tnt2_chg_z,ntbnp2_chg_z),use = "complete.obs") %>% 
                      matrix() %>% c 
                    
        ) %>% tibble() %>%
          mutate(outcome = (q+w+e+r+t+y+u+i+o)/9,
                 group = "active"),
        
        rnorm_multi(n= cohort2[x], 
                    varnames = c("q","w","e","r","t","y","u","i","o"),
                    mu = vanish_placebo_mu,
                    sd = vanish_placebo_sd,
                    r = cor(vanish_1 %>% select(flvedv2_dif_bsa_z,flvesv2_dif_bsa_z,flav2_dif_bsa_z,
                                                mlvwt2_zdif_z,ewvs2_zdif_z,swvs2_zdif_z,flvmass2_dif_bsa_z,
                                                tnt2_chg_z,ntbnp2_chg_z),use = "complete.obs") %>% 
                      matrix() %>% c
        ) %>% tibble() %>%
          # The combined data frame dat is then processed to calculate the outcome variable as 
          #the mean of the 9 normally distributed variables, and add a column x which is the index of the loop.
          mutate(outcome = (q+w+e+r+t+y+u+i+o)/9,
                 group = "placebo")) %>% 
      mutate(x= x) %>% 
       # A two-sample t-test is performed on the outcome variable between the two groups, and the p-value is extracted using the broom library and stored in a data frame k.
       
    t.test(.$outcome~.$group, data = .) %>% broom::tidy() %>% select(p.value) %>% pull() %>% 
      unlist() %>% tibble(x = x)
     # After the loop has finished, the results of all 25 iterations are combined into a single data frame k.
     
      k <- rbind(k, dat)
      
    }
   k
    })
# Finally, the results stored in k are processed to determine if the p-value is less than 0.05 for each iteration, 
# and a summary of the number of times the p-value was less than 0.05 is produced using the group_by and summarise functions.

unlist(simulation_vanish) %>% tibble(x= .) %>% 
  mutate(p = if_else(x<0.05,T,F)) %>% group_by(p) %>% summarise(n=n())
#The result from this calculation gives the estimated power from this computer simulation
