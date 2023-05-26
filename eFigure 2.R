vanish_2 <- vanish_2 %>% mutate(flvmassb_bsa_10 = flvmassb_bsa/10,
                                flvesvb_bsa_10 = flvesvb_bsa/10,
                                flvesvb_bsa_5 = flvesvb_bsa/5,
                                lv_ratio_01 = lv_ratio,
                                flvedvb_bsa_10 = flvedvb_bsa/10,
                                flavb_bsa_10 = flavb_bsa/10,
                                flavb_bsa_5 = flavb_bsa/5,
                                #progressors = case_when(mlvwt2_zdif>1~1, T~0),
                                #progressors = factor(progressors, labels = c("no", "yes")),
                                lvef_cmrb_5 = lvef_cmrb/5,
                                lvef_cmrb_10 = lvef_cmrb/10,
                                bmib_5 = bmib/5
)


#Making dataset above better for plotting
fig3_df<-
  lm_df %>% 
  filter(pred=="var_removed") %>% 
  mutate(term = case_when(str_detect(term, "lves")~"LV systolic volume <br> (\u03945 ml/m<sup>2<sup/>)", 
                          str_detect(term, "sw")~"S` velocity <br> (\u0394z-score)", 
                          str_detect(term, "mlvw")~"Max LV wall <br> thickness <br> (\u0394z-score)", 
                          str_detect(term, "lv_ra")~"LV thickness to <br> diameter ratio <br> (\u03940.01 units)", 
                          str_detect(term, "lved")~"LV diastolic volume <br> (\u039410 ml/m<sup>2<sup/>)", 
                          str_detect(term, "lvmas")~"LV mass <br> (\u039410 g/m<sup>2<sup/>)", 
                          str_detect(term, "bnp")~"NT-proBNP <br> \u0394log(pg/ml)", 
                          str_detect(term, "tnt")~"Troponin T <br> (\u0394pg/ml)", 
                          str_detect(term, "ewv")~"E´ velocity <br> (\u0394z-score)", 
                          str_detect(term, "lav")~"LA volume <br> (\u03945 ml/m<sup>2<sup/>)", 
                          str_detect(term, "lves")~"LV systolic volume <br> (10 ml/m<sup>2<sup/>)",
                          str_detect(term, "lvef")~"LV ejection fraction <br> (\u03945 %-points)",
                          T~term),
         term = fct_reorder(term, desc(p.value)))

#Main box and whisker plot
fig3_a<-
  fig3_df %>% 
  ggplot(aes(x=term, y= estimate, ymin = conf.low, ymax= conf.high, group = pred))+
  #geom_errorbar(position = position_dodge(width = .5), width = .2)+
  # geom_point(aes(fill = term), shape = 22, size = 5,
  #            position = position_dodge(width = .5),
  #            show.legend = F)+
  geom_errorbar(aes(color = term),position = position_dodge(width = .5), 
                width = 0, show.legend = F, size = 1.2)+
  geom_point(aes(color = term), size = 5,
             position = position_dodge(width = .5),
             show.legend = F)+
  geom_hline(aes(yintercept = 0), linetype =2)+
  geom_segment(aes(y = 0.05, yend = 0.3, x= 11.6, xend =11.6), color = "black", arrow = arrow(length = unit(0.02,"npc"),type = "closed" ))+
  geom_segment(aes(y = -0.05, yend = -0.3, x= 11.6, xend =11.6), color = "black", arrow = arrow(length = unit(0.02,"npc"),type = "closed" ))+
  ggtext::geom_richtext(aes(y= c(-0.175), x = 12.2,
                            label = c("<span style=color:#5F1F0A>**Higher values**</span> <br> associated <br> with progression")),
                        size = 3, label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt") # remove padding
           )+
  ggtext::geom_richtext(aes(y= c(0.175), x = 12.2,
                            label = c("<span style=color:#407726>**Lower values**</span> <br> associated <br> with progression")),
                        size = 3,label.color = NA, # remove background and outline
                        label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  )+
  scale_y_continuous(breaks = seq(-10,10,.1))+
  labs(x="",
       y= "\u0394 *z*-score of composite outcome excluding independent variable")+
  #scale_fill_brewer(palette = "Accent")+
  scale_fill_scico_d(palette = "batlow")+
  scale_color_scico_d(palette = "batlow")+
  theme_clean(base_family = "Roboto")+
  theme(plot.background = element_rect(fill ="white", color = "white"),
                      plot.title = element_markdown(family = "Roboto"),
                      axis.title.y = element_markdown(family = "Roboto"),
                      axis.title.x = element_markdown(family = "Roboto"),
                      axis.text.y = element_markdown(size = 9,family = "Roboto"),
                      axis.line.y = element_blank(),
                      panel.grid.major.x = element_line(color = "grey", linetype = 3),
                      panel.grid.major.y = element_blank())+
  coord_flip(ylim = c(-0.501,.301),xlim = c(0.4,12.5), expand = F, clip = "off")#+  ggforce::facet_row(~pred)

#p-value
fig3_tab1<- 
  fig3_df %>% 
  ggplot()+
  geom_text(aes(x= term, y=1, label = round(p.value,3)), size = 3,family = "Roboto")+
  annotate("text", x=11.8, y=1, label = "P-value", color = "black", size=3, fontface = 2)+
  theme_void(base_family = "Roboto")+
  coord_flip(clip = "off",xlim = c(0.4,12.5), expand = F
  )


fig3_tab3_df<-
  vanish_2 %>% select(flvedvb_bsa, flvesvb_bsa, flvmassb_bsa,
                      swvsb_z, ewvsb_z, flavb_bsa, lv_ratio,
                      mlvwtb_z, log_ntbnpb, tntb, lvef_cmrb) %>% 
  summarise(across(.cols = everything(), ~mean(.x, na.rm = T))) %>% 
  pivot_longer(cols = everything(), values_to = "mean") %>% 
  mutate(name = factor(name, levels = c("lvef_cmrb", "flvesvb_bsa","mlvwtb_z", "lv_ratio" ,"swvsb_z","flavb_bsa",
                                        "tntb","log_ntbnpb","ewvsb_z",
                                        "flvedvb_bsa", "flvmassb_bsa"
  ),
  labels = c("LV ejection fraction <br> (\u03945 %-points)",
             "LV systolic volume <br> (\u03945 ml/m<sup>2<sup/>)",
             "Max LV wall <br> thickness <br> (\u0394z-score)",
             "LV ratio",
             "S` velocity <br> (\u0394z-score)",
             "LA volume <br> (\u03945 ml/m<sup>2<sup/>)", 
             "Troponin T <br> (\u0394pg/ml)",
             "NT-proBNP <br> \u0394log(pg/ml)", 
             "E´ velocity <br> (\u0394z-score)", 
             "LV diastolic volume <br> (\u039410 ml/m<sup>2<sup/>)", 
             "LV mass <br> (\u039410 g/m<sup>2<sup/>)")))

fig3_tab3_df2<-  
  vanish_2 %>% select(flvedvb_bsa, flvesvb_bsa, flvmassb_bsa,
                      swvsb_z, ewvsb_z, flavb_bsa,lv_ratio,
                      mlvwtb_z, log_ntbnpb, tntb,lvef_cmrb) %>% 
  summarise(across(.cols = everything(), ~sd(.x, na.rm = T))) %>% 
  pivot_longer(cols = everything(), values_to = "sd") %>% 
  mutate(name = factor(name, levels = c("lvef_cmrb","flvesvb_bsa","mlvwtb_z","lv_ratio", "swvsb_z","flavb_bsa",
                                        "tntb","log_ntbnpb","ewvsb_z",
                                        "flvedvb_bsa", "flvmassb_bsa"
  ),
  labels = c("LV ejection fraction <br> (\u03945 %-points)",
             "LV systolic volume <br> (\u03945 ml/m<sup>2<sup/>)",
             "Max LV wall <br> thickness <br> (\u0394z-score)",
             "LV ratio",
             "S` velocity <br> (\u0394z-score)",
             "LA volume <br> (\u03945 ml/m<sup>2<sup/>)", 
             "Troponin T <br> (\u0394pg/ml)",
             "NT-proBNP <br> \u0394log(pg/ml)", 
             "E´ velocity <br> (\u0394z-score)", 
             "LV diastolic volume <br> (\u039410 ml/m<sup>2<sup/>)", 
             "LV mass <br> (\u039410 g/m<sup>2<sup/>)")))

fig3_tab3_df<- left_join(fig3_tab3_df, fig3_tab3_df2) %>% 
  mutate(.lower = mean-(1.96*sd),
         .upper = mean+(1.96*sd))

fig3_tab3 <- 
  fig3_tab3_df %>% 
  mutate(name = fct_rev(name),
         mean = case_when(abs(mean)>10~round(mean,0),
                          abs(mean)>1~round(mean,1),
                          T~round(mean,2)),
         
         .lower = case_when(abs(.lower)>10~round(.lower,0),
                            abs(.lower)>1~round(.lower,1),
                            T~round(.lower,2)),
         
         .upper = case_when(abs(.upper)>10~round(.upper,0),
                            abs(.upper)>1~round(.upper,1),
                            T~round(.upper,2))) %>% 
  ggplot()+
  geom_text(aes(x = name, y=1, label = paste(mean, " (", .lower, " to ",.upper,")", sep = "")), size = 3,family = "Roboto")+
  annotate("text", x=11.8, y=1, label = "mean (95% CI) \nat baseline", color = "black", size=3,
           fontface =2,family = "Roboto")+
  theme_void(base_family = "Roboto")+
  coord_flip(clip = "off",xlim = c(0.4,12.5), expand =F )

lm_df_r <-
  bind_rows(lm(composite2_no_s~swvsb_z, data = vanish_2) %>% broom::glance(conf.int=T) %>% mutate( term = "swvsb_z"),
            lm(composite2_no_e~ewvsb_z, data = vanish_2) %>% broom::glance(conf.int=T) %>% mutate( term = "ewvsb_z"),
            lm(composite2_no_mlvwt~mlvwtb_z, data = vanish_2) %>% broom::glance(conf.int=T) %>% mutate( term = "mlvwtb_z"),
            lm(composite2_no_mlvwt~lv_ratio, data = vanish_2) %>% broom::glance(conf.int=T) %>% mutate( term = "lv_ratio"),
            lm(composite2_no_lvedv~flvedvb_bsa_10, data = vanish_2) %>% broom::glance(conf.int=T) %>% mutate( term = "flvedvb_bsa_10"),
            lm(composite2_no_lvesv~flvesvb_bsa_10, data = vanish_2) %>% broom::glance(conf.int=T) %>% mutate( term = "flvesvb_bsa_10"),
            lm(composite2_no_lvmass~flvmassb_bsa_10, data = vanish_2) %>% broom::glance(conf.int=T) %>% mutate( term = "flvmassb_bsa_10"),
            lm(composite2_no_lav~flavb_bsa_5, data = vanish_2) %>% broom::glance(conf.int=T) %>% mutate( term = "flavb_bsa_5"),
            lm(composite2_no_bnp~log_ntbnp, data = vanish_2) %>% broom::glance(conf.int=T) %>% mutate( term = "log_ntbnp"),
            lm(composite2_no_tnt~tntb, data = vanish_2) %>% broom::glance(conf.int=T) %>% mutate( term = "tntb"),
            lm(composite2~lvef_cmrb_5, data = vanish_2) %>% broom::glance(conf.int=T) %>% mutate( term = "lvef_cmrb_5")
  )%>% select(term, 1:5) %>% arrange(desc(statistic))
lm_df_r


fig3_tab4
fig3_tab4<-             
  lm_df_r %>% 
  mutate(r= round(r.squared,2),
         term = fct_reorder(term, statistic)) %>% 
  ggplot()+
  geom_text(aes(x = term, y=1, label = r), size = 3,family = "Roboto")+
  annotate("text", x=11.8, y=1, label = "R²",  
           color = "black", size=3, fontface = "bold", family = "Roboto")+
  theme_void()+
  coord_flip(clip = "off", xlim = c(0.4,12.5), expand = F)




fig3_a+fig3_tab3+fig3_tab4+fig3_tab1+plot_layout(widths = c(6,2,1,1))
ggsave("figure3_2022-12-16.png", height = 16, width = 18, dpi = 1200, units = "cm")
ggsave("figure3_2023-01-03.tiff", height = 16, width = 18, dpi = 1200, units = "cm", compression = "lzw")

lm_df_r
