vanish_df_2 <-
  vanish_df_2 %>% mutate(progressors = case_when(mlvwt2_zdif>1~1, T~0))

#Moddeling this
glm(progressors~mlvwtb_z+swvsb_z+ageb+ewvsb_z+flvedvb_bsa, family = "binomial", data = vanish_df_2) %>% broom::tidy(conf.int = T) %>% filter(term!="(Intercept)") %>% select(term, .estimate = estimate, .lower = conf.low, .higher = conf.high, p= p.value) %>% mutate(across(.cols = 2:5, ~round(.x,2))) %>%  flextable() %>% theme_vader()

#Seeing the effect of each additional variable.
glm(progressors~mlvwtb_z+swvsb_z+ageb+ewvsb_z+flvedvb_bsa, family = "binomial", data = vanish_df_2) %>% 
  anova(test = "Chisq") %>% broom::tidy()%>% 
  mutate(across(.cols = c(Deviance,5, p.value), ~round(.x,2))) %>%  flextable() %>% theme_vader()




glm(progressors~mlvwtb_z+swvsb_z+ageb+ewvsb_z+flvesvb_bsa+flav_bsa+
      lvef_cmrb+log_ntbnpb, family = "binomial", data = vanish_2 %>% 
      mutate(flvesvb_bsa=flvesvb_bsa/5,
             flav_bsa = flav_bsa/5,
             mlvwtb_z = mlvwtb_z*5,
             )) %>% 
  broom::tidy(conf.int = T, exponentiate = T) %>% 
  mutate(.est = exp(estimate),
         term = case_when(term=="mlvwtb_z"~"Max LV wall thickness<br> (\u0394 0.2 z-score)",
                          term=="swvsb_z"~"S` velocity <br> (\u0394z-score)",
                          term=="ageb"~"Age in years",
                          term=="log_ntbnpb"~"NT proBNP <br> (\u0394 log-fold change)",
                          term=="ewvsb_z"~"E` velocity <br> (\u0394z-score)",
                          term=="flvesvb_bsa"~"LV systolic volume <br> (\u03945 ml/m<sup>2<sup/>",
                          term=="flav_bsa"~"LA volume <br> (\u03945 ml/m<sup>2<sup/>)",
                          term=="lvef_cmrb"~"LV ejection fraction <br> (\u03941 %)",
                          
                          T~term
         ),
         term = fct_reorder(factor(term), statistic)) %>% 
  filter(term!= "(Intercept)") %>% 
  ggplot(aes(x=estimate, xmin = conf.low, xmax = conf.high, y= term, fill = term))+
  geom_vline(aes(xintercept =1), color = "red", linetype = 2)+
  geom_errorbar(width = .1)+
  geom_point(size = 4, show.legend = F, shape = 21)+
  scale_fill_scico_d(palette = "oleron")+
  geom_text(aes(x= 4, label = round(p.value,2)), size = 3.5, family = "Roboto")+
  annotate("text", x= 4, y = 8.5, label = "P value", fontface = "bold", size = 3.5, family = "Roboto")+
  labs(x= "Odds ratio and 95% confidence interval",
       y="",
       title = "Predictors of conversion to HCM in sarcomere variant carriers")+
  scale_x_log10(breaks = c(.4,.6,1.6,2.5,1))+

    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major.x = element_line(color = "gray78", linetype = 3),
  axis.text.y = element_markdown(family = "Roboto", color = "black"),
  axis.title = element_markdown(family = "Roboto", color = "black")
  )
ggsave(filename = "Carolyn.pdf", device = cairo_pdf, height = 12, width = 18, units = "cm", dpi =2200)  
