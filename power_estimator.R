library(tidyverse); library(faux)
###################################
########## MODEL 1
###################################
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
  vanish_1$flvedv2_dif_bsa_z[vanish_1$valsartan=="P"] %>% mean(.,na.rm=T),
  vanish_1$flvesv2_dif_bsa_z[vanish_1$valsartan=="P"] %>% mean(.,na.rm=T),
  vanish_1$flav2_dif_bsa_z[vanish_1$valsartan=="P"] %>% mean(.,na.rm=T),
  vanish_1$mlvwt2_zdif_z[vanish_1$valsartan=="P"] %>% mean(.,na.rm=T),
  vanish_1$ewvs2_zdif_z[vanish_1$valsartan=="P"] %>% mean(.,na.rm=T),
  vanish_1$swvs2_zdif_z[vanish_1$valsartan=="P"] %>% mean(.,na.rm=T),
  vanish_1$flvmass2_dif_bsa_z[vanish_1$valsartan=="P"] %>% mean(.,na.rm=T),
  vanish_1$tnt2_chg_z[vanish_1$valsartan=="P"] %>% mean(.,na.rm=T),
  vanish_1$ntbnp2_chg_z[vanish_1$valsartan=="P"] %>% mean(.,na.rm=T))

vanish_placebo_sd<- c(
  vanish_1$flvedv2_dif_bsa_z[vanish_1$valsartan=="P"] %>% sd(.,na.rm=T),
  vanish_1$flvesv2_dif_bsa_z[vanish_1$valsartan=="P"] %>% sd(.,na.rm=T),
  vanish_1$flav2_dif_bsa_z[vanish_1$valsartan=="P"] %>% sd(.,na.rm=T),
  vanish_1$mlvwt2_zdif_z[vanish_1$valsartan=="P"] %>% sd(.,na.rm=T),
  vanish_1$ewvs2_zdif_z[vanish_1$valsartan=="P"] %>% sd(.,na.rm=T),
  vanish_1$swvs2_zdif_z[vanish_1$valsartan=="P"] %>% sd(.,na.rm=T),
  vanish_1$flvmass2_dif_bsa_z[vanish_1$valsartan=="P"] %>% sd(.,na.rm=T),
  vanish_1$tnt2_chg_z[vanish_1$valsartan=="P"] %>% sd(.,na.rm=T),
  vanish_1$ntbnp2_chg_z[vanish_1$valsartan=="P"] %>% sd(.,na.rm=T))




k <- c()
cohort2 = seq(10,200,10)
for (vuf in 1:length(cohort2)) {
  kc <-
    replicate(1000, {
      
      dat<-
        rbind(
          rnorm_multi(n= cohort2[vuf], 
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
          
          rnorm_multi(n= cohort2[vuf], 
                      varnames = c("q","w","e","r","t","y","u","i","o"),
                      mu = vanish_placebo_mu,
                      sd = vanish_placebo_sd,
                      r = cor(vanish_1 %>% select(flvedv2_dif_bsa_z,flvesv2_dif_bsa_z,flav2_dif_bsa_z,
                                                  mlvwt2_zdif_z,ewvs2_zdif_z,swvs2_zdif_z,flvmass2_dif_bsa_z,
                                                  tnt2_chg_z,ntbnp2_chg_z),use = "complete.obs") %>% 
                        matrix() %>% c
          ) %>% tibble() %>%
            mutate(outcome = (q+w+e+r+t+y+u+i+o)/9,
                   group = "placebo")) %>% 
        mutate(x= vuf) %>% 
        
        t.test(.$outcome~.$group, data = .) %>% broom::tidy() %>% 
        select(p.value) %>% pull()
    }) %>% unlist() %>% tibble(x=.) %>% 
    mutate(outcome = if_else(x<0.05, T, F),
           n = vuf*10)
  k<- rbind(k, kc)
  model1 <- k
  
}




###################################
########## MODEL 2
###################################
vanish_active_mu<- c(
  vanish_1$flvedv2_dif_bsa_z[vanish_1$valsartan=="A"] %>% mean(.,na.rm=T)*(1-(.219-.126)),
  vanish_1$flvesv2_dif_bsa_z[vanish_1$valsartan=="A"] %>% mean(.,na.rm=T)*(1-(.204-.068)),
  vanish_1$flav2_dif_bsa_z[vanish_1$valsartan=="A"] %>% mean(.,na.rm=T)*(1-(-.00476+.0964)),
  vanish_1$mlvwt2_zdif_z[vanish_1$valsartan=="A"] %>% mean(.,na.rm=T)*(1-(.0823-.0925)),
  vanish_1$ewvs2_zdif_z[vanish_1$valsartan=="A"] %>% mean(.,na.rm=T)*(1-(-.138-.224)),
  vanish_1$swvs2_zdif_z[vanish_1$valsartan=="A"] %>% mean(.,na.rm=T)*(1-(.196-0.114)),
  vanish_1$flvmass2_dif_bsa_z[vanish_1$valsartan=="A"] %>% mean(.,na.rm=T)*(1-(.183-.0822)),
  vanish_1$tnt2_chg_z[vanish_1$valsartan=="A"] %>% mean(.,na.rm=T)*(1-(-.0278-.0307)),
  vanish_1$ntbnp2_chg_z[vanish_1$valsartan=="A"] %>% mean(.,na.rm=T)*(1-(.235-.109)))

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
  vanish_1$flvedv2_dif_bsa_z[vanish_1$valsartan=="P"] %>% mean(.,na.rm=T)*(1-(.301+.235)),
  vanish_1$flvesv2_dif_bsa_z[vanish_1$valsartan=="P"] %>% mean(.,na.rm=T)*(1-(.314+.175)),
  vanish_1$flav2_dif_bsa_z[vanish_1$valsartan=="P"] %>% mean(.,na.rm=T)*(1-(-.269-.151)),
  vanish_1$mlvwt2_zdif_z[vanish_1$valsartan=="P"] %>% mean(.,na.rm=T)*(1-(.371+.181)),
  vanish_1$ewvs2_zdif_z[vanish_1$valsartan=="P"] %>% mean(.,na.rm=T)*(1-(.153+.224)),
  vanish_1$swvs2_zdif_z[vanish_1$valsartan=="P"] %>% mean(.,na.rm=T)*(1-(-.0315+0.151)),
  vanish_1$flvmass2_dif_bsa_z[vanish_1$valsartan=="P"] %>% mean(.,na.rm=T)*(1-(-.103+.103)),
  vanish_1$tnt2_chg_z[vanish_1$valsartan=="P"] %>% mean(.,na.rm=T)*(1-(-.0618+.0137)),
  vanish_1$ntbnp2_chg_z[vanish_1$valsartan=="P"] %>% mean(.,na.rm=T)*(1-(.266+.213)))

vanish_placebo_sd<- c(
  vanish_1$flvedv2_dif_bsa_z[vanish_1$valsartan=="P"] %>% sd(.,na.rm=T),
  vanish_1$flvesv2_dif_bsa_z[vanish_1$valsartan=="P"] %>% sd(.,na.rm=T),
  vanish_1$flav2_dif_bsa_z[vanish_1$valsartan=="P"] %>% sd(.,na.rm=T),
  vanish_1$mlvwt2_zdif_z[vanish_1$valsartan=="P"] %>% sd(.,na.rm=T),
  vanish_1$ewvs2_zdif_z[vanish_1$valsartan=="P"] %>% sd(.,na.rm=T),
  vanish_1$swvs2_zdif_z[vanish_1$valsartan=="P"] %>% sd(.,na.rm=T),
  vanish_1$flvmass2_dif_bsa_z[vanish_1$valsartan=="P"] %>% sd(.,na.rm=T),
  vanish_1$tnt2_chg_z[vanish_1$valsartan=="P"] %>% sd(.,na.rm=T),
  vanish_1$ntbnp2_chg_z[vanish_1$valsartan=="P"] %>% sd(.,na.rm=T))



k <- c()
cohort2 = seq(10,200,10)
for (vuf in 1:length(cohort2)) {
  kc <-
    replicate(1000, {
      
      dat<-
        rbind(
          rnorm_multi(n= cohort2[vuf], 
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
          
          rnorm_multi(n= cohort2[vuf], 
                      varnames = c("q","w","e","r","t","y","u","i","o"),
                      mu = vanish_placebo_mu,
                      sd = vanish_placebo_sd,
                      r = cor(vanish_1 %>% select(flvedv2_dif_bsa_z,flvesv2_dif_bsa_z,flav2_dif_bsa_z,
                                                  mlvwt2_zdif_z,ewvs2_zdif_z,swvs2_zdif_z,flvmass2_dif_bsa_z,
                                                  tnt2_chg_z,ntbnp2_chg_z),use = "complete.obs") %>% 
                        matrix() %>% c
          ) %>% tibble() %>%
            mutate(outcome = (q+w+e+r+t+y+u+i+o)/9,
                   group = "placebo")) %>% 
        mutate(x= vuf) %>% 
        
        t.test(.$outcome~.$group, data = .) %>% broom::tidy() %>% select(p.value) %>% pull()
    }) %>% unlist() %>% tibble(x=.) %>% 
    mutate(outcome = if_else(x<0.05, T, F),
           n = vuf*10)
  k <- rbind(k, kc)
  model2 <- k
}

###################################
########## MODEL 3
###################################
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


k <- c()
cohort2 = seq(10,250,10)
for (vuf in 1:length(cohort2)) {
  kc <-
    replicate(1000, {
      
      dat<-
        rbind(
          rnorm_multi(n= cohort2[vuf], 
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
          
          rnorm_multi(n= cohort2[vuf], 
                      varnames = c("q","w","e","r","t","y","u","i","o"),
                      mu = vanish_placebo_mu,
                      sd = vanish_placebo_sd,
                      r = cor(vanish_1 %>% select(flvedv2_dif_bsa_z,flvesv2_dif_bsa_z,flav2_dif_bsa_z,
                                                  mlvwt2_zdif_z,ewvs2_zdif_z,swvs2_zdif_z,flvmass2_dif_bsa_z,
                                                  tnt2_chg_z,ntbnp2_chg_z),use = "complete.obs") %>% 
                        matrix() %>% c
          ) %>% tibble() %>%
            mutate(outcome = (q+w+e+r+t+y+u+i+o)/9,
                   group = "placebo")) %>% 
        mutate(x= vuf) %>% 
        
        t.test(.$outcome~.$group, data = .) %>% broom::tidy() %>% select(p.value) %>% pull()
    }) %>% unlist() %>% tibble(x=.) %>% 
    mutate(outcome = if_else(x<0.05, T, F),
           n = vuf*10)
  k <- rbind(k, kc)
  model3 <- k
  
}



models <- rbind(model1 %>% mutate(model = 1),
                model2 %>% mutate(model = 2),
                model3%>% mutate(model = 3))
models
models %>% write_csv2("powermodels.csv")

model_summaries <-
  models %>% mutate(n_=n) %>%  
  group_by(n_, outcome, model) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  filter(outcome==T) %>% 
  mutate(n=n/1000,
         par = n_*2) 
model_summaries %>%
  rbind(tibble(n_ = c(0,0,0), outcome = c(NA,NA,NA), model = c(1,2,3), n= c(0,0,0), par = c(0,0,0))) %>% 
  ggplot(aes(x=par, y = n, group = model, color = model))+
  geom_point(size = 2, show.legend = F)+
  geom_line(show.legend = F)+
  geom_hline(aes(yintercept = .8), linetype = 2)+
  scale_x_continuous(breaks = seq(0,500,50))+
  scale_y_continuous(breaks = seq(0,1,.1))+
  labs(y = "Estimated power",
       x = "Total number of patients included")+
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "gray89"),
        text = element_text(family = "Roboto", color = "black")
  )+
  #scale_fill_stepsn(colours = c("#132B43","#56B1F7"))+
  annotate("text", x= c(50,80, 125), y = c(.5,.45,.32), 
           label = c("Model 1", "Model 2", "Model 3"), 
           color = c("#132B43","#154c79", "#56B1F7"),
           angle = c(65,60, 25), family = "Roboto", size = 3)
ggsave("est_power_loess.png", dpi = 1200, height = 12, width = 16, units = "cm")

model2 %>% mutate(n_=n) %>%  
  group_by(n_, outcome) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  filter(outcome==T) %>% 
  mutate(n=n/1000) %>% 
  ggplot(aes(x=n_, y = n))+
  geom_point(size = 3, color = "red")+
  geom_line(color = "red")+
  geom_hline(aes(yintercept = .8), linetype = 2)+
  theme(panel.background = element_blank())


model3 %>% mutate(n_=n) %>%  
  group_by(n_, outcome) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  filter(outcome==T) %>% 
  mutate(n=n/1000) %>% 
  ggplot(aes(x=n_, y = n))+
  geom_point(size = 3, color = "red")+
  geom_line(color = "red")+
  geom_hline(aes(yintercept = .8), linetype = 2)+
  theme(panel.background = element_blank())
