library(tidyverse); library(scico); library(ggtext); library(ggthemes)
###################
##### FIGURE 1
##################
fig<-
  vanish_2 %>% select(composite, mlvwt2_zdif_z, 
                      ewvs2_zdif_z, swvs2_zdif_z,
                      flvedv2_dif_bsa_z, flvesv2_dif_bsa_z,
                      flvmass2_dif_bsa_z, flav2_dif_bsa_z,
                      tnt2_chg_z, ntbnp2_chg_z, valsartan,
                      comp2b,mlvwtb_z,puberty,lvef_cmrb,bmib,ageb,gender) %>%
  mutate(valsartan = fct_relevel(valsartan,"P")) %>% 
  pivot_longer(1:10) %>%
  # group_by(name) %>% 
  #group_map(~stats:::t.test.formula(value~valsartan, data = ., .keep =T)) 
  group_split(name) %>% 
  #split(.$name) %>% 
  # mutate(model = 
  map(~lm(value~valsartan+comp2b+mlvwtb_z+puberty+lvef_cmrb+bmib+ageb+gender,
          data = .)) %>% 
  #) 
  map_dfr(~broom::tidy(.x, conf.int =T)) %>% 
  filter(term=="valsartanA") %>% 
  mutate(var = c("composite", "ewvs2_zdif_z","flav2_dif_bsa_z",
                 "flvedv2_dif_bsa_z", "flvesv2_dif_bsa_z",
                 "flvmass2_dif_bsa_z", "mlvwt2_zdif_z", 
                 "ntbnp2_chg_z","swvs2_zdif_z","tnt2_chg_z"),
         var = factor(var, levels = c("composite","mlvwt2_zdif_z", 
                                      "ewvs2_zdif_z", "swvs2_zdif_z",
                                      "flvedv2_dif_bsa_z", "flvesv2_dif_bsa_z",
                                      "flvmass2_dif_bsa_z", "flav2_dif_bsa_z",
                                      "tnt2_chg_z", "ntbnp2_chg_z"),
                      labels = c("**Composite outcome**",
                                 "Max LV wall thickness", 
                                 "E` velocity", "S` velocity",
                                 "LV end-diastolic<br> volume index", "LV end-systolic<br> volume index",
                                 "LV mass index", "LA volume index",
                                 "TnT", "NT-proBNP"))) %>% 
  select(var, delta = estimate, 
         #placebo = estimate1, valsartan = estimate2, 
         p = p.value,
         .lower = conf.low, .upper = conf.high) 

adjusted_fig1<-
  rbind(
    figure1_plot_function(vanish_2, composite, "composite"),
    figure1_plot_function(vanish_2, mlvwt2_zdif_z, "mlvwt2_zdif_z"),
    figure1_plot_function(vanish_2, ewvs2_zdif_z, "ewvs2_zdif_z"),
    figure1_plot_function(vanish_2, swvs2_zdif_z, "swvs2_zdif_z"),
    figure1_plot_function(vanish_2, flvedv2_dif_bsa_z, "flvedv2_dif_bsa_z"),
    figure1_plot_function(vanish_2, flvesv2_dif_bsa_z, "flvesv2_dif_bsa_z"),
    figure1_plot_function(vanish_2, flvmass2_dif_bsa_z, "flvmass2_dif_bsa_z"),
    figure1_plot_function(vanish_2, flav2_dif_bsa_z, "flav2_dif_bsa_z"),
    figure1_plot_function(vanish_2, tnt2_chg_z, "tnt2_chg_z"),
    figure1_plot_function(vanish_2, ntbnp2_chg_z, "ntbnp2_chg_z")
  )

fig1<-
  fig %>% 
  mutate(term =  fct_rev(var)
  ) %>% 
  ggplot(aes(x=term, y=delta, ymin = .lower, ymax = .upper))+
  geom_hline(aes(yintercept = 0))+
  geom_errorbar(width = .1)+
  geom_point(color = "#227399", size = 4.5)+
  geom_segment(aes(x=10.5, xend = 10.5, y = .05, yend=1), arrow = arrow( length = unit(.02, "npc") ,type = "closed"))+
  geom_segment(aes(x=10.5, xend = 10.5, y = -.05, yend=-1), arrow = arrow( length = unit(.02, "npc") ,type = "closed"))+
  geom_segment(aes(x=0.4, xend = 0.4, y = -1.2, yend=1.2))+
  annotate("text", x=10.8, y=.5, label = "Valsartan better", color = "#964A35", size=3,
           family = "Roboto", fontface = "bold")+
  annotate("text", x=10.8, y=-.5, label = "Placebo better", color = "#327FA5", size=3,
           family = "Roboto", fontface = "bold")+
  labs(x="",
       y = "\u0394 *z*-score",
       title = "")+
  scale_y_continuous(breaks = seq(-1.2,1.2,.3))+
  theme_clean()+theme(
    plot.background = element_rect(fill = "#f5f8fa", color = "#f5f8fa"),
    plot.title = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.title.y = element_markdown(),
    axis.title.x = element_markdown(),
    axis.text.y = element_markdown(size = 9),
    axis.text.x = element_markdown(size = 10))+
  coord_flip(xlim = c(.4,11), ylim = c(-1.3,1.4), clip = "off", expand = F)

fig1_tab1<-
  adjusted_fig1 %>% 
  mutate(.est = round(.est,3),
         .lwr = round(.lwr,2),
         .upr = round(.upr,2),
         text = paste(sep = "", .est, "\n (",.lwr, " to ",.upr, ")"),
         term = factor(term, levels =c("composite","mlvwt2_zdif_z", 
                                       "ewvs2_zdif_z", "swvs2_zdif_z",
                                       "flvedv2_dif_bsa_z", "flvesv2_dif_bsa_z",
                                       "flvmass2_dif_bsa_z", "flav2_dif_bsa_z",
                                       "tnt2_chg_z", "ntbnp2_chg_z"),
                       labels = c("**Composite outcome**",
                                  "Max LV wall thickness", 
                                  "E` velocity", "S` velocity",
                                  "LV end-diastolic<br> volume index", "LV end-systolic<br> volume index",
                                  "LV mass index", "LA volume index",
                                  "TnT", "NT-proBNP") )) %>% 
  mutate(term = fct_rev(term)) %>% 
  ggplot(aes(x=term, y=group, label = text))+
  geom_text(size = 3)+
  coord_flip(expand = F,xlim = c(.4,11), clip = "off", ylim = c(.6,2.4))+
  #facet_wrap(~group)+
  theme_void()+
  theme(plot.background = element_rect(fill = "#f5f8fa", color = "#f5f8fa"),
        strip.text = element_textbox_highlight(
          size = 10, face = "bold", family = "Roboto",
          fill = "white", box.color = "white", color = "#964A35",
          halign = .5, linetype = 0, r = unit(0, "pt"), width = unit(0, "npc"),
          padding = margin(0, 0, 0, 0), margin = margin(0, 0, 0, 0),
          hi.labels = "∆Placebo", hi.family = "Roboto",
          hi.fill = "white", hi.box.col = "white", hi.col = "#327FA5"
        ))+
  annotate("text", family = "Roboto", x= c(0,10.8,10.8), y = c(1.5,1,2), 
           label = c("Positive values\n indicate improvement",
                     "∆Placebo",
                     "∆Valsartan"),fontface = "bold", 
           color = c("black", "#327FA5","#964A35"),
           size = c(3,3.5,3.5))
fig1+fig1_tab1+plot_layout(widths = c(4,2.5))
ggsave("figure1_adjusted.png", dpi = 1200, units = "cm", width = 18, height = 14)




###################
#### FIGURE 4
###################
simple_df<-
  vanish %>% 
  mutate(
    num_nab = mlvwtb_z %in% NA +ewvsb_z %in% NA+ swvsb_z %in% NA+
      flvedvb_bsa %in% NA+flvesvb_bsa %in% NA+ flavb_bsa %in% NA+
      flvmassb_bsa %in% NA+
      log_ntbnpb %in% NA+log_tntb%in% NA,
    num_na1 = mlvwt1_z %in% NA +ewvs1_z %in% NA+ swvs1_z %in% NA+
      lvedv_bsa_tte1 %in% NA+lvesv_bsa_tte1 %in% NA+lav_bsa_tte1 %in% NA+
      lvm_bsa_tte1 %in% NA+
      log_ntbnp1 %in% NA+log_tnt1%in% NA,
    num_na2 = mlvwt_z %in% NA +ewvs_z %in% NA+ swvs_z %in% NA+
      flvedv_bsa %in% NA+flvesv_bsa %in% NA+ flav_bsa %in% NA+
      flvmass_bsa %in% NA+
      log_ntbnp %in% NA+log_tnt%in% NA,
    #Z-scores at baseline as computed by me using the function above
    compb_c = (-z_score(.,x=mlvwtb_z)+
                 z_score(.,x=ewvsb_z)+
                 z_score(.,x=swvsb_z)+
                 z_score(.,x=flvedvb_bsa)+
                 z_score(.,x=flvesvb_bsa)-
                 z_score(.,x=flvmassb_bsa)-
                 z_score(.,x=flavb_bsa)-
                 z_score(.,x=log_ntbnpb)-
                 z_score(.,x= log_tntb))/(9-num_nab),
    comp1_cb = (-(mlvwt1_z-mean(mlvwtb_z,na.rm=T))/sd(mlvwtb_z,na.rm=T)+
                  (ewvs1_z-mean(ewvsb_z,na.rm=T))/sd(ewvsb_z,na.rm=T)+
                  (swvs1_z-mean(swvsb_z,na.rm=T))/sd(swvsb_z,na.rm=T)+
                  (lvedv_bsa_tte1-mean(lvedv_bsa_tteb,na.rm=T))/sd(lvedv_bsa_tteb,na.rm=T)+
                  (lvesv_bsa_tte1-mean(lvesv_bsa_tteb,na.rm=T))/sd(lvesv_bsa_tteb,na.rm=T)-
                  (lvm_bsa_tte1-mean(lvm_bsa_tteb,na.rm=T))/sd(lvm_bsa_tteb,na.rm=T)-
                  (lav_bsa_tte1-mean(lav_bsa_tteb,na.rm=T))/sd(lav_bsa_tteb,na.rm=T)-
                  (log_ntbnp1-mean(log_ntbnpb,na.rm=T))/sd(log_ntbnpb,na.rm=T)-
                  (log_tnt1-mean(log_tntb,na.rm=T))/sd(log_tntb,na.rm=T))/(9-num_na1),
    comp2_cb = (-(mlvwt_z-mean(mlvwtb_z,na.rm=T))/sd(mlvwtb_z,na.rm=T)+
                  (ewvs_z-mean(ewvsb_z,na.rm=T))/sd(ewvsb_z,na.rm=T)+
                  (swvs_z-mean(swvsb_z,na.rm=T))/sd(swvsb_z,na.rm=T)+
                  (flvedv_bsa-mean(flvedvb_bsa,na.rm=T))/sd(flvedvb_bsa,na.rm=T)+
                  (flvesv_bsa-mean(flvesvb_bsa,na.rm=T))/sd(flvesvb_bsa,na.rm=T)-
                  (flvmass_bsa-mean(flvmassb_bsa,na.rm=T))/sd(flvmassb_bsa,na.rm=T)-
                  (flav_bsa-mean(flavb_bsa,na.rm=T))/sd(flavb_bsa,na.rm=T)-
                  (log_ntbnp-mean(log_ntbnpb,na.rm=T))/sd(log_ntbnpb,na.rm=T)-
                  (log_tnt-mean(log_tntb,na.rm=T))/sd(log_tntb,na.rm=T))/(9-num_na2),
    dmlvwt = -z_score(., mlvwt2_dif),
    dmlvwtz =  -z_score(., mlvwt2_zdif),
    dlvmass = -z_score(., flvmass2_dif),
    dlav = -z_score(., flav2_dif_bsa),
    dlvesv = z_score(., flvesv2_dif_bsa),
    dlvedv = z_score(., flvedv2_dif_bsa),
    dswvs = z_score(., swvs2_zdif),
    dewvs = z_score(., ewvs2_zdif),
    dbnp = -z_score(., ntbnp2_dif),
    dtnt = -z_score(., tnt2_dif),
    dbnp_z = -z_score(., log_ntbnp2_chg),
    dtnt_z = -z_score(., log_tnt2_chg),
    num_naz = dmlvwtz %in% NA +dlvmass %in% NA+ dlav %in% NA+
      dlvesv %in% NA+dlvedv %in% NA+ dswvs %in% NA+
      dewvs %in% NA+
      ntbnp2_chg_z %in% NA+tnt2_chg_z%in% NA,
    
    delta_comp = comp2_cb-compb_c,
    dcomp = (dmlvwtz+dlvedv+dlvesv+dlvmass+dewvs+dswvs+ntbnp2_chg_z+tnt2_chg_z+dlav)/(9-num_naz),
    dcomp_ = (delta_comp-mean(compb_c,na.rm=T))/sd(compb_c),
    
  ) %>% 
  select(cohort, valsartan, compb_c, comp1_cb, comp2_cb,
         dcomp,
         dmlvwt,
         dmlvwtz,dlvmass,dlav,dlvesv,dlvedv,dswvs,dewvs,dbnp,dtnt,
         dbnp_z,dtnt_z,
         mlvwtb_z,puberty,lvef_cmrb,bmib,ageb,gender) %>% 
  mutate(pt = row_number())

#######################################

raw_tbl<-  
  vanish %>%
  mutate(mlvwt2_dif = mlvwt2_dif*10) %>%
  #filter(valsartan=="A") %>% 
  select(cohort, mlvwt2_dif, mlvwt2_zdif, flvmass2_dif,flav2_dif_bsa, flvesv2_dif_bsa,
         flvedv2_dif_bsa, swvs2_zdif, ewvs2_zdif, ntbnp2_dif, tnt2_dif) %>% 
  pivot_longer(cols = 2:11) %>% 
  mutate(name = fct_relevel(name, c("mlvwt2_dif","mlvwt2_zdif", "flvmass2_dif","flav2_dif_bsa", "flvesv2_dif_bsa",
                                    "flvedv2_dif_bsa", "swvs2_zdif", "ewvs2_zdif", "ntbnp2_dif", "tnt2_dif"),
  )) %>% 
  group_by(name) %>% 
  group_map(~stats:::t.test.formula(value~cohort, data = ., .keep =T)) %>% 
  map_dfr(~broom::tidy(.x)) %>% 
  mutate(term =  c("mlvwt2_dif","mlvwt2_zdif", "flvmass2_dif","flav2_dif_bsa", "flvesv2_dif_bsa",
                   "flvedv2_dif_bsa", "swvs2_zdif", "ewvs2_zdif", "ntbnp2_dif", "tnt2_dif"),
         term = factor( term, levels = c("mlvwt2_dif" ,"mlvwt2_zdif", "flvmass2_dif","flav2_dif_bsa", "flvesv2_dif_bsa",
                                         "flvedv2_dif_bsa", "swvs2_zdif", "ewvs2_zdif", "ntbnp2_dif", "tnt2_dif"),
                        labels = c("dmlvwt","dmlvwtz","dlvmass","dlav","dlvesv","dlvedv","dswvs","dewvs", "dbnp","dtnt")),
         # term = fct_reorder(term, desc(statistic))
  ) %>% 
  select(term, est_raw =estimate, .low_raw = conf.low, .high_raw = conf.high, 
         .est1_raw = estimate1, .est2_raw= estimate2, p_raw= p.value, stat_raw= statistic)

#########################
z_tbl<-
  simple_df %>% 
  mutate(dmlvwt = dmlwt*10) %>% 
  #filter(valsartan=="P") %>% 
  #mutate(dcomp = comp2_cb-compb_c) %>%
  select(pt,cohort, dcomp, dmlvwt, dmlvwtz,dlvmass,dlav,dlvesv,dlvedv,dswvs,dewvs, dbnp,dtnt) %>% 
  pivot_longer(cols = 3:13) %>% 
  mutate(name = fct_relevel(name, c("dcomp","dmlvwt", "dmlvwtz","dlvmass","dlav","dlvesv","dlvedv","dswvs","dewvs", "dbnp","dtnt"),
  )) %>% 
  group_by(name) %>% 
  group_map(~stats:::t.test.formula(value~cohort, data = ., .keep =T)) %>% 
  map_dfr(~broom::tidy(.x)) %>% 
  mutate(term = c("dcomp", "dmlvwt", "dmlvwtz","dlvmass","dlav","dlvesv","dlvedv","dswvs","dewvs", "dbnp","dtnt"),
         term = fct_relevel(term, c("dcomp","dmlvwt", "dmlvwtz","dlvmass","dlav","dlvesv","dlvedv","dswvs","dewvs", "dbnp","dtnt"),
         ))
##########################################
tbl_comb<- z_tbl %>% left_join(raw_tbl) %>% 
  mutate(term = factor(term, 
                       levels = c("dcomp", "dmlvwt", "dmlvwtz","dlvmass","dlav","dlvesv","dlvedv","dswvs","dewvs", "dbnp","dtnt"),
                       labels = c("Composite outcome", 
                                  "Max LV wall thickness <br> <span style=font-size:8pt;>(mm)</span>",
                                  
                                  "Max LV wall thickness <br> <span style=font-size:8pt;>(z-score)</span>",
                                  "LV mass <br> index <span style=font-size:8pt;>(g/m<sup>2</sup>)</span>",
                                  "LA volume  <br> index <span style=font-size:8pt;>(ml/m<sup>2</sup>)</span>",
                                  "LV end-systolic volume  <br> index <span style=font-size:8pt;>(ml/m<sup>2</sup>)</span>",
                                  "LV end-diastolic volume  <br> index <span style=font-size:8pt;>(ml/m<sup>2</sup>)</span>",
                                  "S´ velocity <br> <span style=font-size:8pt;>(z-score)</span>",
                                  "E´ velocity <br> <span style=font-size:8pt;>(z-score)</span>",
                                  "NT-proBNP <br> <span style=font-size:8pt;>(pg/ml)</span>", 
                                  "Troponin T <br> <span style=font-size:8pt;>(ng/L)</span>")),
         term = fct_reorder(term, desc(statistic)),
         term = fct_relevel(term,"Composite outcome", after = Inf))

################
plot1<- 
  tbl_comb %>% 
  ggplot(aes(x=term, y=estimate, ymin = conf.low, ymax = conf.high))+
  scale_x_discrete()+
  #geom_rect(aes(xmin= 0, xmax = Inf, ymin = 0, ymax= Inf), fill = "#d5f5dd", alpha =.5)+
  #geom_rect(aes(xmin= 0, xmax = Inf, ymin = 0, ymax= -Inf), fill = "#f7aba6", alpha =.5)+
  geom_segment(aes(y = 0, yend = 0, x= 0, xend=10.6), linetype =2, size = .5)+
  geom_errorbar(width=0)+
  geom_point(size = 4, color = "#227399")+
  scale_y_continuous(breaks = seq(-1,1,.2))+
  theme_clean()+
  theme(plot.background = element_rect(fill = "#f5f8fa", color = "#f5f8fa"),
        axis.line.y = element_blank(),
        axis.text.y = ggtext::element_markdown())+
  labs(x="",
       y = "Difference in disease progression (ΔZ)")+
  coord_flip(xlim = c(0.8,11.7),
             ylim = c(-.7,.7),
             #ylim = c(-1.1,.7),
             clip = "off")+
  annotate("text", x = 11.4, y= c(-.4,.4), 
           label = c("More in early HCM","More in preclinical HCM"), 
           size = 2.5, fontface="bold", color = "#202324")+
  annotate("text", x = 12, y= 0, 
           label = "Cardiac remodeling", vjust =0,
           size = 4, fontface="bold", color = "#227399")+
  geom_segment(aes(y=-.15, yend =-.7, x= 11.7, xend = 11.7), arrow = arrow(type = "closed", length = unit(.02, "npc")))+
  geom_segment(aes(y=.15, yend =.7, x= 11.7, xend = 11.7), arrow = arrow(type = "closed", length = unit(.02, "npc")))

plot2<-
  tbl_comb %>% 
  mutate(base1 = paste(if_else(abs(.est1_raw)<1 ,as.character(round(.est1_raw,2)),
                               as.character(round(.est1_raw,1)), "")),
         base2 = paste(case_when(abs(.est2_raw)<1~as.character(round(.est2_raw,2)),
                                 is.na(.est2_raw)~"",
                                 T~as.character(round(.est2_raw,1)))),
         raw_est = case_when(abs(est_raw)<1~as.character(round(est_raw,2)),
                             is.na(est_raw)~"",
                             T~as.character(round(est_raw,1))),
         low = case_when(abs(.low_raw)<1~as.character(round(.low_raw,2)),
                         is.na(.low_raw)~"",
                         T~as.character(round(.low_raw,1))),
         high = case_when(abs(.high_raw)<1~as.character(round(.high_raw,2)),
                          is.na(.high_raw)~"",
                          T~as.character(round(.high_raw,1))),
         .label = case_when(is.na(est_raw)~"",
                            T~paste(raw_est, "\n (", low, " to ", high, ")", sep = ""))
  ) %>% 
  ggplot(aes(x=term))+
  scale_x_discrete()+
  geom_text(aes(y = 2, label = base1), size = 3)+
  geom_text(aes(y = 4.5, label = base2), size = 3)+
  geom_text(aes(y = 7, label = .label), size = 3)+
  theme_void()+
  theme(plot.background = element_rect(fill = "#f5f8fa", color = "#f5f8fa"))+
  labs(x="",
       y = "")+
  coord_flip(xlim = c(0.8,11.7),
             ylim = c(1.5,8.5),
             clip = "off")+
  annotate("text", y= 2, x = 11, label ="Early", hjust = .5, size = 3)+
  annotate("text", y= 4.5, x = 11, label ="Preclinical", hjust = .5, size = 3)+
  annotate("text", y= 7, x = 11, label ="Difference", hjust = .5, size = 3)+
  annotate("text", y= 5, x = 12, label ="Change during follow-up \n in original units", 
           hjust = .5, size = 3, fontface = "bold")



plot1+plot2+plot_layout(widths = c(1,.74))
ggsave("fig4.png", dpi = 1200, height = 12*1.1, width = 16*1.1, units = "cm")
tbl_comb
