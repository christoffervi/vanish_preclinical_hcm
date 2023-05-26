# Function to estimates slopes and other things.

norm_slope2 <- function(data,
                        var_name, timepoints,
                        base, one , two,
                        group = valsartan, cohort = cohort,
                        filter1 = c("P", "A"), filter2 = 2)
{
  if(timepoints == 2) {
    
    vuf <- data %>% 
      select(group = {{group}},
             cohort = {{cohort}},
             base = {{base}}, two= {{two}}) %>% 
      filter(group %in% filter1, cohort == filter2#, !is.na(two)
      )
    
    vuf_sd <- sd(vuf$base, na.rm = T)
    
    if(vuf$base %>% unique() %>% length() ==1) {
      
      vuf_mean <- mean(vuf$base, na.rm=T)
      vuf_q1 <- quantile(vuf$base, probs = .25, na.rm=T)
      vuf_q3 <- quantile(vuf$base, probs = .75, na.rm=T)
      vuf_dist <- "mean"
      
    } else {
      
      vuf_mean <- case_when(shapiro.test(vuf$base) %>% tidy() %>% pull(2) >.05 ~
                              mean(vuf$base, na.rm = T), 
                            T~median(vuf$base, na.rm = T))
      vuf_q1 <- quantile(vuf$base, probs = .25, na.rm=T)
      vuf_q3 <- quantile(vuf$base, probs = .75, na.rm=T)
      vuf_dist <- case_when(shapiro.test(vuf$base) %>% tidy() %>% pull(2)>.05~"mean", 
                            T~ "median")
    }
    
    vuf %>% 
      t.test(data =.,.$base,.$two, paired =T)%>% broom::tidy() %>% 
      mutate(group = filter2,
             across(.cols = c("estimate", "statistic", "conf.low", "conf.high"),~.x*-1),
             term = var_name,
             percent_change = estimate / vuf_mean * 100,
             z_change = estimate / vuf_sd,
             z_low = conf.low / vuf_sd,
             z_hi = conf.high /vuf_sd,
             base_est = vuf_mean,
             q1 = vuf_q1,
             q3 = vuf_q3,
             dist = vuf_dist,
             base_sd = vuf_sd,
             std.error = abs(conf.high-estimate)/1.96) %>% 
      select(term, estimate,std.error, statistic, p.value, conf.low, conf.high,group,
             percent_change, z_change, z_low, z_hi, base_est,q1,q3,dist,base_sd,method)
    
  } else {  
    vuf <- data %>% 
      select(group = {{group}},
             cohort = {{cohort}},
             base = {{base}}, one = {{one}}, two= {{two}}) %>% 
      filter(group %in% filter1, cohort == filter2)
    #Computing the standard deviation at baseline
    vuf_sd <- sd(vuf$base, na.rm = T)
    
    if(vuf$base %>% unique() %>% length() ==1){
      vuf_mean = mean(vuf$base, na.rm=T)
      vuf_q1 <- quantile(vuf$base, probs = .25, na.rm=T)
      vuf_q3 <- quantile(vuf$base, probs = .75, na.rm=T)
      vuf_dist <- "mean"
    } else {
      
      vuf_mean <- case_when(shapiro.test(vuf$base) %>% tidy() %>% pull(2) >.05 ~
                              mean(vuf$base, na.rm = T), 
                            T~median(vuf$base, na.rm = T))
      vuf_q1 <- quantile(vuf$base, probs = .25, na.rm=T)
      vuf_q3 <- quantile(vuf$base, probs = .75, na.rm=T)
      vuf_dist <- case_when(shapiro.test(vuf$base) %>% tidy() %>% pull(2)>.05~
                              "mean", T~ "median")
    }
    #Creating the linear model dataset
    vuf %>% 
      pivot_longer(cols = c(base, one, two)) %>% 
      mutate(name = case_when(name=="base"~0,
                              name=="one"~.5,
                              T~1)) %>% 
      lm(value~name, data = .) %>% broom::tidy(conf.int=T) %>% 
      mutate(group = filter2,
             
             term = if_else(term=="name", var_name, term),
             percent_change = estimate[2] / estimate[1] * 100,
             z_change = estimate[2] / vuf_sd,
             z_low = conf.low[2] / vuf_sd,
             z_hi = conf.high[2] /vuf_sd,
             base_est = vuf_mean,
             q1 = vuf_q1,
             q3 = vuf_q3,
             dist = vuf_dist,
             base_sd = vuf_sd, 
             method = "linear"
             
      ) %>%
      
      select(term, estimate,std.error, statistic, p.value, conf.low, conf.high,group,
             percent_change, z_change, z_low, z_hi, base_est,q1,q3,dist,base_sd,method)
  }}


###########
#Plot

rbind(
  norm_slope2(vanish_2, "mlvwt_slope", 3, mlvwtb, mlvwt1, mlvwt, filter1 = c("A", "P")),
  norm_slope2(vanish_2, "mlvwt", 2, mlvwtb, mlvwt1, mlvwt, filter1 = c("A", "P")),
  #norm_slope2(vanish_2, "lvmaxt", 2, lvmaxt_cmrb, NA, lvmaxt_cmr2, filter1 = c("A", "P")),
  #norm_slope2(vanish_2, "lvh", 2, aha_avg_se_cmrb, NA, aha_avg_se_cmr2, filter1 = c("A", "P")),
  norm_slope2(vanish_2, "mlvwt_z_slope", 3, mlvwtb_z, mlvwt1_z, mlvwt_z, filter1 = c("A", "P")),
  norm_slope2(vanish_2, "mlvwt_z", 2, mlvwtb_z, mlvwt1_z, mlvwt_z, filter1 = c("A", "P")),
  norm_slope2(vanish_2, "ewvs_slope", 3, ewvsb, ewvs1, ewvs, filter1 = c("A", "P")),
  #  norm_slope2(vanish_2, "ewvs", 2, ewvsb, ewvs1, ewvs, filter1 = c("A", "P")),
  norm_slope2(vanish_2, "ewvs_z_slope", 3, ewvsb_z, ewvs1_z, ewvs_z, filter1 = c("A", "P")),
  norm_slope2(vanish_2, "ewvs_z", 2, ewvsb_z, ewvs1_z, ewvs_z, filter1 = c("A", "P")),
  norm_slope2(vanish_2, "swvs_slope", 3, swvsb, swvs1, swvs, filter1 = c("A", "P")),
  #  norm_slope2(vanish_2, "swvs", 2, swvsb, swvs1, swvs, filter1 = c("A", "P")),
  norm_slope2(vanish_2, "swvs_z_slope", 3, swvsb_z, swvs1_z, swvs_z, filter1 = c("A", "P")),
  norm_slope2(vanish_2, "swvs_z", 2, swvsb_z, swvs1_z, swvs_z, filter1 = c("A", "P")),
  norm_slope2(vanish_2, "la_vol_ind", 2, flavb_bsa, NA, flav_bsa, filter1 = c("A", "P")),
  #  norm_slope2(vanish_2, "la_vol", 2, flavb, NA, flav, filter1 = c("A", "P")),
  norm_slope2(vanish_2, "lvmass_ind", 2, flvmassb_bsa, NA, flvmass_bsa, filter1 = c("A", "P")),
  norm_slope2(vanish_2, "lvmass", 2, flvmassb, NA, flvmass, filter1 = c("A", "P")),
  #  norm_slope2(vanish_2, "lvmass", 2, flvmassb, NA, flvmass, filter1 = c("A", "P")),
  norm_slope2(vanish_2, "lvedv_ind", 2, flvedvb_bsa, NA, flvedv_bsa, filter1 = c("A", "P")),
  #  norm_slope2(vanish_2, "lvedv", 2, flvedvb, NA, flvedv, filter1 = c("A", "P")),
  norm_slope2(vanish_2, "lvesv_ind", 2, flvesvb_bsa, NA, flvesv_bsa, filter1 = c("A", "P")),
  #  norm_slope2(vanish_2, "lvesv", 2, flvesvb, NA, flvesv, filter1 = c("A", "P")),
  norm_slope2(vanish_2, "ntbnp_slope", 3, ntbnpb, ntbnp1, ntbnp, filter1 = c("A", "P")),
  norm_slope2(vanish_2, "ntbnp", 2, ntbnpb, ntbnp1, ntbnp, filter1 = c("A", "P")),
  norm_slope2(vanish_2, "tnt", 2, tntb, tnt1, tnt, filter1 = c("A", "P"))
  
  
) %>% 
  filter(term!= "(Intercept)" & !str_detect(term, "slope")) %>%
  mutate(term = case_when(str_detect(term, "lves")~"LV systolic volume <br> index (ml/m<sup>2</sup>)", 
                          str_detect(term, "sw")~"S` velocity <br> (z-score)", 
                          str_detect(term, "mlvwt_z")~"Max LV wall <br> thickness <br> (z-score)", 
                          str_detect(term, "mlvwt")~"Max LV wall <br> thickness <br> (cm)", 
                          str_detect(term, "lved")~"LV diastolic volume <br> index (ml/m<sup>2</sup>)", 
                          str_detect(term, "lvmass_")~"LV mass <br> index (g/m<sup>2</sup>)", 
                          str_detect(term, "lvmas")~"LV mass <br> (g)", 
                          str_detect(term, "bnp")~"NT-proBNP <br> (pg/ml)", 
                          str_detect(term, "tnt")~"Troponin T <br> (ng/L)", 
                          str_detect(term, "ewv")~"E´ velocity <br> (z-score)", 
                          str_detect(term, "la")~"LA volume <br> index (ml/m<sup>2</sup>)", 
                          str_detect(term, "lves")~"LV systolic volume <br> (ml/m<sup>2</sup>)",
                          str_detect(term, "lvef")~"LV ejection fraction <br> (\u03945 %-points)",
                          T~term)) %>% 
  mutate(across(.cols = contains("z"), ~case_when(str_detect(term, "LA|Max|Trop|pro|mass")~.x*-1,T~.x)),
         across(.cols = contains("z"), ~case_when(str_detect(term, "Trop")~NA_real_,T~.x)),
         #across(.cols = c("z_low", "z_hi"), ~case_when(str_detect(term, "tnt")~NA_real_,T~.x)),
         term = fct_reorder(factor(term), desc(z_change)),
         across(.cols =  c("estimate", "conf.low", "conf.high"),~case_when(abs(.x)>5~round(.x,0),
                                                                           abs(.x)>1~round(.x,1),
                                                                           T~round(.x,2))),
         label_text = paste(sep = "", estimate, " (", conf.high," to ", conf.low,")"),
         p= case_when(p.value<0.001~"<0.001",
                      str_detect(term, "Trop")~NA_character_,
                      p.value>.99~">0.99",
                      p.value>0.1~as.character(format(round(p.value,2), nsmall = 2)),
                      T~as.character(round(p.value,3)))
  ) %>% 
  ggplot(aes(x= term, y = z_change, ymin = z_low, ymax = z_hi))+
  geom_segment(aes(x=term, xend=term, y=-1.7, yend = 1.3), linetype = 3, color = "gray79")+
  geom_segment(aes(x=11.4, xend=11.4, y= -.25, yend = -1.3), arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
  geom_segment(aes(x=11.4, xend=11.4, y= .25, yend = 1.3), arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
  geom_segment(aes(x=0.6, xend=0.6, y= -1.5, yend = 1))+
  geom_errorbar(width = .2)+ 
  geom_segment(aes(y = 0, yend= 0, x= .6, xend=11.4), linetype =2)+
  geom_point(aes(fill = term), size=4, show.legend = F, shape=21)+
  geom_point(aes(x=11, y=-1.38), size=6, show.legend = F, shape="*")+
  geom_text(aes(x= term, y=2.3, label = label_text), family = "Roboto", size = 3.5)+
  geom_text(aes(x= term, y=3.5, label = p), family = "Roboto", size = 3.5)+
  annotate("text",
           x= c(11.9,11.9,12.6,12.6,12.6),
           y = c(-.75,.75, 2.3,3.5,-0.3),
           label = c("Worsening", "Improvement", "Absolute change \n(95% CI)", "P",
                     "Normalized change from baseline"),
           size = c(3.5,3.5,4.5,4.5,4.5), vjust =1,
           color = c("#5F1F0A","#407726","black","black","black"),
           fontface = "bold", family = "Roboto"
  )+
  scale_y_continuous(breaks = seq(-2,1,.5))+
  scico::scale_fill_scico_d(palette = "oleron")+
  labs(y = "*z*-score")+
  ggthemes::theme_clean()+ 
  coord_flip(ylim = c(-1.9,4.0),
             xlim = c(0.6,13), 
             expand = F)+
  theme(axis.text.y = element_markdown(family = "Roboto"),
        axis.title.y = element_blank(),
        axis.title.x = element_markdown( hjust = .31),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.y = element_blank())
ggsave("eFigure1.pdf", dpi = 2500, units = "cm", height = 15, width = 19, device = cairo_pdf)

