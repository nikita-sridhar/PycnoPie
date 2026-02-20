#pycnopie - BEHAVIOR STUFF



#################################################################################
#models
################################################################################

#note: behavior_processed is equivalent to urch_bhvr_tank in pycnocline

#distribution is weird for both but I think gamma?
ggplot(behavior_processed, aes(x = pcnt_on_kelp)) + #right skew - use gamma 
  geom_density() +
  facet_wrap(vars(Treatment)) 
ggplot(behavior_processed, aes(x = pcnt_in_crev)) + #right skew - use gamma 
  geom_density() +
  facet_wrap(vars(Treatment)) 


urch_bhvr_forgamma <- behavior_processed %>%
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
                         row_number() == 3 ~"Predator treatment:Urchin source habitat",
                         row_number() == 4 ~"Residuals")) %>%
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

EMM_crev <- emmeans(crev_model, ~ Pred_treatment + Urch_habitat_treatment + Pred_treatment*Urch_habitat_treatment)
EMM_onkelp <- emmeans(onkelp_model, ~ Pred_treatment + Urch_habitat_treatment + Pred_treatment*Urch_habitat_treatment)

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


##########################################################
#behavior plots
##########################################################
dw <- 0.9 # dodge width
off <- dw / 4

#info for geom_bracket facet wrap for both panels significance
annotation_data <- data.frame(
  supp = c("crev","onkelp"),
  xmin = 
)


#double check the dataframes look right for this, and change pycnocline to this too.

urch_bhvr_treatment <- behavior_processed %>%
  group_by(Treatment, Pred_treatment, Urch_habitat_treatment) %>%
  summarise(#on kelp
    mean_onkelp = mean(pcnt_on_kelp, na.rm = TRUE),
    sd_onkelp = sd(pcnt_on_kelp, na.rm = TRUE), 
    
    #crevice
    mean_crev = mean(pcnt_in_crev, na.rm = TRUE),
    sd_crev = sd(pcnt_in_crev, na.rm = TRUE)) %>%
  
  
  #reshaping data for facetwrap
  pivot_longer(cols = c("mean_onkelp","sd_onkelp","mean_crev","sd_crev"),
               names_to = c("sumstat", "behavior"),
               names_sep = "_",
               values_to = "value") %>%
  
  pivot_wider(names_from = sumstat, values_from = value)

labels <- c("crev" = "In crevice",
            "onkelp" = "On kelp")

#plotting
ggplot(urch_bhvr_treatment, aes(x = Urch_habitat_treatment, y = mean,
                                fill = Pred_treatment)) +
  
  geom_col(position = position_dodge(width = 0.9) )+
  
  geom_errorbar(aes(ymin = mean - sd,
                    ymax = mean + sd),
                position = position_dodge(width = 0.9),
                width = 0.1) +
  
  scale_fill_brewer(palette = "Accent") +
  
  facet_wrap(~behavior, labeller = as_labeller(labels), scales = "free_y") +
  
  labs(y = "Mean % of urchins displaying behavior",
       x = "Urchin habitat",
       caption = "Error bars represent mean +/- 1 SD",
       fill = "Predator treatment") +
  
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.3), add = c(0, 1)))  +
 
 theme_bw() 
