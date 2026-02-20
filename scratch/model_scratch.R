#model and making sense of the model:
#first run data_exploration_scratch1

library(glmmTMB)
library(DHARMa)
library(ggeffects)
library(effects)

##############################################################################
############               kelp model                   ######################
##############################################################################


#reformat data------------------------------------------------------------------

kelp_processed$Urch_habitat_treatment <- as.factor(kelp_processed$Urch_habitat_treatment)
kelp_processed$Pred_treatment <- as.factor(kelp_processed$Pred_treatment)

kelp_processed$dist_slice_avgpyc1[is.nan(kelp_processed$dist_slice_avgpyc1)]<-NA
kelp_processed$dist_slice_avgpyc2[is.nan(kelp_processed$dist_slice_avgpyc2)]<-NA

kelp_processed$dist_slice_avgpyc1 <- as.numeric(kelp_processed$dist_slice_avgpyc1)
kelp_processed$dist_slice_avgpyc2 <- as.numeric(kelp_processed$dist_slice_avgpyc2)

#step 1: figure out distribution------------------------------------------------

#plot distribution of data to see if it is normal - or gamma, etc.
ggplot(kelp_processed, aes(x=pcnt_grazed))+
  geom_histogram() +
  facet_wrap(vars(Treatment))

#this test not always accurate bc with higher sample size, it seems less normal
shapiro.test(kelp_processed$pcnt_grazed)


#transformations?????
transformed_kelp <- kelp_processed %>%
  mutate(pcnt_grazed = case_when(pcnt_grazed <= 0 ~ 0.00001, .default = pcnt_grazed),
         log10kelp = log10(pcnt_grazed)) #making negative and 0 values = 0.000001 

transformed_kelp <- kelp_processed %>%
  filter(pcnt_grazed > 0)  %>%
  mutate(log10kelp = log10(pcnt_grazed),
         recipkelp = 1/pcnt_grazed) #making negative and 0 values = 0.000001 

ggplot(transformed_kelp, aes(x=log10kelp))+
  geom_histogram() +
  facet_wrap(vars(Treatment))

qqPlot(transformed_kelp$log10kelp)

ggplot(transformed_kelp, aes(x=recipkelp))+
  geom_histogram() +
  facet_wrap(vars(Treatment))

qqPlot(transformed_kelp$recipkelp)

#NOTE: realized that actually bad idea to transform raw data pre using a link function for a glmm
#a glmm can handle non-normal

#step 2: build model------------------------------------------------------------

#for normally distributed, can take on any values.
#but, for betta or gamma, can't be lower than 0. making
#another dataset here to try out those fits too see if that mod is better

kelp_nonzero <- kelp_processed %>%
  filter(pcnt_grazed > 0 )


k1 <- glmmTMB::glmmTMB(pcnt_grazed ~ 
                      
                      Urch_habitat_treatment + 
                      Pred_treatment + 
                      Urch_habitat_treatment*Pred_treatment +
                      
                      (1| Trial/Tank/Kelp_ID),  
                      
                      data = kelp_processed,
                      
                      family = gaussian(link = "identity"))


#for k2, want to include other metrics like avg distance away from pycno - but this data is only for star treatment - 
#changing to dist away to 100 for no star treatment ("far" away)

#when I add kelp ID as random effect to this model it doesn't run.....
k2 <- glmmTMB::glmmTMB(pcnt_grazed ~ 
                         
                         Urch_habitat_treatment +
                         Pred_treatment +
                         Urch_habitat_treatment*Pred_treatment +
                         up_dn_pyc1_ratio +
                         up_dn_pyc2_ratio +
                         dist_slice_avgpyc1 +
                         dist_slice_avgpyc2 +
                      

                         
                         (1| Trial/Tank),  
                       
                       data = kelp_processed,
                       
                       family = gaussian(link = "identity"))

sjPlot::tab_model(k2)

##using k1!! bc pycno related var in k2 not significant

#step 3: check assumptions------------------------------------------------------

kelp_residuals <- DHARMa::simulateResiduals(k1)
plot(kelp_residuals)

DHARMa::testZeroInflation(kelp_residuals)


#plot things--------------------------------------------------------------------

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

#reformat data------------------------------------------------------------------

behavior_processed$Urch_habitat_treatment <- as.factor(behavior_processed$Urch_habitat_treatment)
behavior_processed$Pred_treatment <- as.factor(behavior_processed$Pred_treatment)
behavior_processed$up_dn_pyc1 <- as.factor(behavior_processed$up_dn_pyc1)
behavior_processed$up_dn_pyc2 <- as.factor(behavior_processed$up_dn_pyc2)

#step 1: figure out distribution------------------------------------------------

#plot distribution of data to see if it is normal - or gamma, etc.
ggplot(behavior_processed, aes(x=urchin_slice_count))+
  geom_histogram() +
  facet_wrap(vars(Treatment))

#looks pretty right skewed! - im continuing with normal but its NOT - need to change this later!!


#step 2: build model------------------------------------------------------------

#with everything
b1 <- glmmTMB::glmmTMB(urchin_slice_count ~ 
                                 
                                 #fixed effects
                                 Urch_habitat_treatment + 
                                 Pred_treatment + 
                                 Urch_habitat_treatment*Pred_treatment +
                                 pyc_slice_count +
                                 dist_slice_pyc1 +
                                 dist_slice_pyc2 +
                                 up_dn_pyc1 +
                                 up_dn_pyc2 +
                                 
                                 #random effects
                                 (1| Trial/Tank),  
                               
                               #specify dataset
                               data = behavior_processed,
                               
                               #specify family - normally distributed? gamma? beta? binomial?
                               family = gaussian(link = "identity"))
 
sjPlot::tab_model(b1)

#after removing the less significant stuff
b2 <- glmmTMB::glmmTMB(urchin_slice_count ~ 
                         
                         #fixed effects
                         Pred_treatment + 
                         pyc_slice_count +
                         up_dn_pyc1 +
                         up_dn_pyc2 +
                         
                         #random effects
                         (1| Trial/Tank),  
                       
                       #specify dataset
                       data = behavior_processed,
                       
                       #specify family - normally distributed? gamma? beta? binomial?
                       family = gaussian(link = "identity"))

sjPlot::tab_model(b2)


#switching response var to pycno
b3 <- glmmTMB::glmmTMB(pyc_slice_count ~ 
                         
                         #fixed effects
                         Urch_habitat_treatment + 
                         Pred_treatment + 
                         Urch_habitat_treatment*Pred_treatment +
                         urchin_slice_count +
                         dist_slice_pyc1 +
                         dist_slice_pyc2 +
                         up_dn_pyc1 +
                         up_dn_pyc2 +
                         
                         #random effects
                         (1| Trial/Tank),  
                       
                       #specify dataset
                       data = behavior_processed,
                       
                       #specify family - normally distributed? gamma? beta? binomial?
                       family = gaussian(link = "identity"))

sjPlot::tab_model(b3)

#after removing the less significant stuff
b4 <- glmmTMB::glmmTMB(pyc_slice_count ~ 
                         
                         #fixed effects
                         Pred_treatment + 
                         urchin_slice_count +
                         dist_slice_pyc1 +
                         dist_slice_pyc2 +
                         up_dn_pyc1 +
                         up_dn_pyc2 +
                         
                         #random effects
                         (1| Trial/Tank),  
                       
                       #specify dataset
                       data = behavior_processed,
                       
                       #specify family - normally distributed? gamma? beta? binomial?
                       family = gaussian(link = "identity"))

sjPlot::tab_model(b4)


#step 3: check assumptions------------------------------------------------------

b1_residuals <- DHARMa::simulateResiduals(b1)
plot(b1_residuals)

DHARMa::testZeroInflation(b1_residuals)

#plot things--------------------------------------------------------------------

predicted_b1 <- ggpredict(model = b1, terms = c("Pred_treatment","Urch_habitat_treatment","pyc_slice_count"))
predicted_b2 <- ggpredict(model = b2, terms = c("Pred_treatment","Urch_habitat_treatment","pyc_slice_count"))
predicted_b4 <- ggpredict(model = b4, terms = c("Pred_treatment","urchin_slice_count"))


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

#test for normality (this is the reason using a GLMM not a LM - also why using the gaussian link function)
ggplot(kelp_processed, aes(x=sqrt(pos_pcnt_change))) +
  geom_histogram() +
  facet_wrap(vars(Treatment))

qqPlot(kelp_processed$pos_pcnt_change) #looks like not normal, right-skew


#building model
kelp_mod <- glmmTMB::glmmTMB(sqrt(pos_pcnt_change) ~ 
                               
                               Urch_habitat_treatment + 
                               Pred_treatment + 
                               Urch_habitat_treatment* Pred_treatment +
                               
                               
                               (1|Trial),  
                             
                             data = kelp_processed,
                             
                             family = gaussian(link = "identity")) 

sjPlot::tab_model(kelp_mod)

predicted_kelp_mod <- ggpredict(model = kelp_mod, 
                                terms = c("Pred_treatment", 
                                          "Urch_habitat_treatment")) %>% 
  rename(Pred_treatment = x,
         Urch_habitat_treatment = group) 


#post hoc tests
EMM <- emmeans(kelp_mod, ~ Pred_treatment|Urch_habitat_treatment, type = "response")
EMM
plot(EMM)

CON <- pairs(EMM) 
CON #a bit sus that values are same for kf/b

CON %>%
  data.frame()  %>%
  mutate(p.value = ifelse(p.value < 0.0001, "< 0.0001",
                          round(p.value,3))) %>%
  flextable() %>%
  set_header_labels(contrast = 'Contrast',
                    emmean = 'Estimate',
                    SE = 's.e.',
                    df = 'd.f.',
                    t.ratio = "t-ratio",
                    p.value = "p value") %>%
  italic(j = c(3:7), part = 'header') %>%
  colformat_double(j = c(2:6), digits = 2) %>%
  align(align = 'center', part = 'header') %>%
  align(align = 'left', part = 'body') %>%
  font(fontname = 'Times', part = 'all') %>%
  bold(~p.value < 0.05, j = "p.value") %>%
  fontsize(size = 12, part = 'all') %>%
  autofit()  %>%
  theme_vanilla()

EMM <- emmeans(kelp_mod, ~ Urch_habitat_treatment|Pred_treatment, type = "response")
EMM
plot(EMM)

CON <- pairs(EMM) %>% summary(infer = TRUE)
CON


######################################################################
#11-24-25

#testing linear models w kelp weight as diff (not % and including neg values)
kelp_processed <- kelp_processed %>% 
  mutate(across(c(Pred_treatment, Urch_habitat_treatment), as.factor))

#releveling reference groups
kelp_processed$Pred_treatment <- relevel(kelp_processed$Pred_treatment, ref = "Control")
kelp_processed$Urch_habitat_treatment <- relevel(kelp_processed$Urch_habitat_treatment, ref = "KF")

#investigating distributions 
ggplot(kelp_processed, aes(x = weight_diff)) +
  geom_density() +
  facet_wrap(vars(Treatment))

#test for normality
qqPlot(kelp$weight_diff)



