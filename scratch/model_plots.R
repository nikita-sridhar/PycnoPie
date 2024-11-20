#' Model plots for PycnoPie 
#' AUTHOR: Nikita Sridhar
#' DATE: 11/13/24
#' 
#'Run models.R first

##############################################################################
############               kelp plots                   ######################
##############################################################################

predicted_k1 <- ggpredict(model = k1, terms = c("Pred_treatment","Urch_habitat_treatment"))

#plot of estimates
ggplot(predicted_k1, aes(group, predicted, fill = x)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position=position_dodge()) +
  labs(title = "Predicted values of % kelp grazed from GLMM", x = "Urchin habitat", y = "% kelp grazed", fill = "Pred treatment") +
  theme_classic() +
  scale_fill_manual(values = c('#DAF7A6','#5A5A5A') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Predator treatment")

predicted_k2 <- ggpredict(model = k2, terms = c("dist_slice_avgpyc1","dist_slice_avgpyc2","Urch_habitat_treatment", "Pred_treatment"))

#plot of estimates
ggplot(predicted_k2, aes(group, predicted, fill = x)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position=position_dodge()) +
  ylab("% Change Kelp Grazed")

##############################################################################
############               behavior model                   ###################
##############################################################################

#plot things--------------------------------------------------------------------

predicted_b1 <- ggpredict(model = b1, terms = c("Pred_treatment","Urch_habitat_treatment","pyc_slice_count"))
predicted_b2 <- ggpredict(model = b2, terms = c("Pred_treatment","Urch_habitat_treatment","pyc_slice_count"))
predicted_b4 <- ggpredict(model = b4, terms = c("Pred_treatment","urchin_slice_count"))
predicted_b5 <- ggpredict(model = b5, terms = c("Pred_treatment","Urch_habitat_treatment"))


#predicted values plot
plot(ggeffects::ggpredict(model = b2, 
                          terms = c("pyc_slice_count","Pred_treatment","up_dn_pyc1","up_dn_pyc2")), 
     color = "Pred_treatment") 

plot(ggeffects::ggpredict(model = b4, 
                          terms = c("urchin_slice_count","Pred_treatment","up_dn_pyc1","up_dn_pyc2")), 
     color = "Pred_treatment") 

#same as above but in ggplot
ggplot(predicted_b4, aes(group, predicted, fill = x)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position=position_dodge()) +
  labs(title = "Predicted values of % kelp grazed", x = "Urchin habitat", y = "% kelp grazed", fill = "Pred treatment")


plot(effects::allEffects(b1))


ggplot(predicted_b5, aes(group, predicted, fill = x)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position=position_dodge()) +
  labs(title = "Predicted values of % kelp grazed from GLMM", x = "Urchin habitat", y = "% kelp grazed", fill = "Pred treatment") +
  theme_classic() +
  scale_fill_manual(values = c('#DAF7A6','#5A5A5A') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Predator treatment")
