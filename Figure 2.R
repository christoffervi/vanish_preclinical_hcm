
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
                          str_detect(term, "lvmas")~"LV mass <br> index (g/m<sup>2</sup>)", 
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
         term = fct_reorder(term, desc(z_change)),
         across(.cols =  c("estimate", "conf.low", "conf.high"),~case_when(abs(.x)>5~round(.x,0),
                                                                           abs(.x)>1~round(.x,1),
                                                                           T~round(.x,2))),
         label_text = paste(sep = "", estimate, " (", conf.high," to ", conf.low,")"),
         p= case_when(p.value<0.001~"<0.001",
                      str_detect(term, "Trop")~NA_character_,
                      T~as.character(round(p.value,3)))
  ) %>% 
  ggplot(aes(x= term, y = z_change, ymin = z_low, ymax = z_hi))+
  geom_segment(aes(x=term, xend=term, y=-1.7, yend = 1.3), linetype = 3, color = "gray79")+
  geom_segment(aes(x=10.4, xend=10.4, y= -.25, yend = -1.3), arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
  geom_segment(aes(x=10.4, xend=10.4, y= .25, yend = 1.3), arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
  geom_segment(aes(x=0.6, xend=0.6, y= -1.5, yend = 1))+
  geom_errorbar(width = .2)+ 
  geom_segment(aes(y = 0, yend= 0, x= .6, xend=10.4), linetype =2)+
  geom_point(aes(fill = term), size=4, show.legend = F, shape=21)+
  geom_point(aes(x=10, y=-1.38), size=6, show.legend = F, shape="*")+
  geom_text(aes(x= term, y=2.3, label = label_text), family = "Roboto", size = 3.5)+
  geom_text(aes(x= term, y=3.5, label = p), family = "Roboto", size = 3.5)+
  annotate("text",
           x= c(10.9,10.9,11.6,11.6,11.6),
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
             xlim = c(0.6,12), 
             expand = F)+
#   This is going to be confusing.  None of these p-values appears in the figure.  
  #Plus the figure shows graphics which don't match to the numbers to the right.
# Consider:
# 1. Adding p-values to Figure 2
# 2.  Add clear labels at the top of Figure 2:  "Normalized Change from Baseline" above the forest plot; 
  #and "Absolute Change (95% CI)" above the numbers.  
  #Both labels large and at the same height.

  theme(axis.text.y = element_markdown(family = "Roboto"),
        axis.title.y = element_blank(),
        axis.title.x = element_markdown( hjust = .31),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.y = element_blank())
ggsave("Figure2.png", dpi = 1300, units = "cm", height = 14, width = 18)
ggsave("Figure2.tiff", dpi = 1300, units = "cm", height = 14, width = 18, compression = "lzw")



####################

vanish_2 %>% mutate(tnt_lod2 = case_when(tnt>5~T, tntb>5~T, 
                                         T~F)) %>% select(tnt_lod2, convertors) %>% arrange(tnt_lod2, convertors) %>% print(n=34)
