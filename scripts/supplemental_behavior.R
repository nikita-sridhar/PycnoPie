#pycnopie - BEHAVIOR STUFF

#finest scale
urch_bhvr_tank <- behavior_cleaned %>%
  
  select(-urchin_position, -urchin_location_count) %>%
  unique() %>% 
  
  #adding crevice use/on kelp at day/numrecord scale
  group_by(Trial, Treatment, Day_numrecord, Pred_treatment, Urch_habitat_treatment) %>%
  reframe(pcnt_in_crev = ((sum(crev_count, na.rm=TRUE))/Total_num_urchin) *100,
            pcnt_on_kelp = ((mean(Num_urch_on_kelp, na.rm = TRUE))/Total_num_urchin *100)) %>%
  distinct(Trial, Treatment,Day_numrecord,Pred_treatment, Urch_habitat_treatment, .keep_all = TRUE) 
  


#avg over trials 
urch_bhvr_treatment <- urch_bhvr_tank %>%
  group_by(Treatment,Pred_treatment, Urch_habitat_treatment) %>% 
  summarise(mean_onkelp = mean(pcnt_on_kelp, na.rm = TRUE),
            sd_onkelp = sd(pcnt_on_kelp, na.rm = TRUE), 
            mean_crev = mean(pcnt_in_crev, na.rm = TRUE),
            sd_crev = sd(pcnt_in_crev, na.rm = TRUE))
  

#################################################################################
#models
################################################################################

ggplot(urch_bhvr_tank, aes(x = pcnt_on_kelp)) + #right skew - use gamma 
  geom_density() +
  facet_wrap(vars(Treatment)) 
ggplot(urch_bhvr_tank, aes(x = pcnt_in_crev)) + #right skew - use gamma 
  geom_density() +
  facet_wrap(vars(Treatment)) 


urch_bhvr_forgamma <- urch_bhvr_tank %>%
  mutate(pcnt_in_crev = ifelse(pcnt_in_crev <= 0, 0.0001, pcnt_in_crev),
         pcnt_on_kelp = ifelse(pcnt_on_kelp <= 0, 0.0001, pcnt_on_kelp))


onkelp_model <- glmmTMB(pcnt_on_kelp ~ 
                        Pred_treatment + Urch_habitat_treatment + 
                          Pred_treatment*Urch_habitat_treatment + 
                          (1|Trial),  
                        data = urch_bhvr_forgamma,
                        family = Gamma(link = "log"))

crev_model <- glmmTMB(pcnt_in_crev ~ 
                          Pred_treatment + Urch_habitat_treatment + 
                          Pred_treatment*Urch_habitat_treatment + 
                          (1|Trial),  
                        data = urch_bhvr_forgamma,
                        family = Gamma(link = "log"))

car::Anova(crev_model)
car::Anova(onkelp_model)

#crev
car::Anova(crev_model) %>%
  data.frame() %>%
  as_tibble(rownames = " ", colnames = " ") %>%
  mutate(Pr..Chisq. = ifelse(Pr..Chisq. < 0.0001, "< 0.0001",round(Pr..Chisq., 3)),
         " " = case_when(row_number() == 1 ~ "Predator treatment",
                         row_number() == 2 ~"Urchin source habitat",
                         row_number() == 3 ~"Predator treatment:Urchin source habitat")) %>%
  flextable() %>%
  #header formatting
  add_header_lines(values = "In crevice") %>%
  italic(i = 1, part = "header") %>%
  bold(i = 2, part = "header") %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>%
  set_header_labels("Pr..Chisq." = "Pr(>Chisq)") %>%
  #body formatting
  colformat_double(j = c(1:4), digits = 3) %>%
  autofit()  

#onkelp
car::Anova(onkelp_model) %>%
  data.frame() %>%
  as_tibble(rownames = " ", colnames = " ") %>%
  mutate(Pr..Chisq. = ifelse(Pr..Chisq. < 0.0001, "< 0.0001",round(Pr..Chisq., 3)),
         " " = case_when(row_number() == 1 ~ "Predator treatment",
                         row_number() == 2 ~"Urchin source habitat",
                         row_number() == 3 ~"Predator treatment:Urchin source habitat",
                         row_number() == 4 ~"Residuals")) %>%
  flextable() %>%
  #header formatting
  add_header_lines(values = "On kelp") %>%
  italic(i = 1, part = "header") %>%
  bold(i = 2, part = "header") %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>%
  set_header_labels("Pr..Chisq." = "Pr(>Chisq)") %>%
  #body formatting
  colformat_double(j = c(1:4), digits = 3) %>%
  autofit()  


################################################################################
# post-hoc tests
################################################################################

EMM_crev <- emmeans(crev_model, ~ Pred_treatment*Urch_habitat_treatment)
EMM_onkelp <- emmeans(onkelp_model, ~  Pred_treatment*Urch_habitat_treatment)

EMM_crev
EMM_onkelp

#specifying weights for contrasts
contrast_weights <- list(
  "a) Control kelp forest vs. Control barren" = c(control_b = -1,
                                                  star_b = 0,
                                                  control_kf = 1,
                                                  star_kf = 0),
  
  "b) Star kelp forest vs. Star barren" = c(control_b = 0,
                                            star_b = -1,
                                            control_kf = 0,
                                            star_kf = 1),
  
  "c) Star kelp forest vs. Control kelp forest" = c(control_b = 0,
                                                    star_b = 0,
                                                    control_kf = -1,
                                                    star_kf = 1),
  
  "d) Star barren vs. Control barren" = c(control_b = -1,
                                          star_b = 1,
                                          control_kf = 0,
                                          star_kf = 0))


crevmod_contrasts <- contrast(EMM_crev, contrast_weights)
onkelpmod_contrasts <- contrast(EMM_onkelp, contrast_weights)


crevmod_contrasts %>% 
  data.frame()  

onkelpmod_contrasts%>%
  data.frame()

#for plots extracting the marginal means from summary

EMM_crev_summary <- summary(EMM_crev) %>%
  mutate(behavior = "crev") %>%
  select(-asymp.LCL, -asymp.UCL)

EMM_onkelp_summary <- summary(EMM_onkelp) %>%
  mutate(behavior = "onkelp") %>%
  select(-asymp.LCL, -asymp.UCL)

EMM_summary <- rbind(EMM_onkelp_summary, EMM_crev_summary)


##########################################################
#behavior plots
##########################################################
dw <- 0.9 # dodge width
off <- dw / 4

labels <- c("crev" = "In crevice",
            "moving" = "Moving",
            "onkelp" = "On kelp")


#plotting
ggplot(EMM_summary, aes(x = Urch_habitat_treatment, y = emmean,
                                fill = Pred_treatment)) +
  
  geom_col(position = position_dodge(width = 0.9) )+
  
  geom_errorbar(aes(ymin = emmean - SE,
                    ymax = emmean + SE),
                position = position_dodge(width = 0.9),
                width = 0.1) +
  
  scale_fill_brewer(palette = "Accent") +
  
  facet_wrap(~behavior, labeller = as_labeller(labels), scales = "free_y") +
  
  labs(y = "Estimated marginal mean % of urchins displaying behavior",
       x = "Urchin habitat",
       caption = "Error bars represent marginal mean +/- 1 SE",
       fill = "Predator treatment") +
  
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.3), add = c(0, 1)))  +
 
 theme_test() 
