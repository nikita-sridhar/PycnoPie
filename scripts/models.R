#'Models for PycnoPie
#' AUTHOR: Nikita Sridhar
#' DATE: 11/13/24

#Run 00_cleaning and 01_manipulation first, or load behavior_processed and kelp_processed

library(glmmTMB)
library(DHARMa)
library(ggeffects)
library(effects)

##############################################################################
############               kelp model                   ######################
##############################################################################

#plot distribution--------------------------------------------------------------

ggplot(kelp_processed, aes(x=pcnt_grazed))+
  geom_histogram() +
  facet_wrap(vars(Treatment))

#test for normality
shapiro.test(kelp_processed$pcnt_grazed)


#build models--------------------------------------------------------------------

#k1 = simple kelp grazed model
k1 <- glmmTMB::glmmTMB(pcnt_grazed ~ 
                         
                         Urch_habitat_treatment + 
                         Pred_treatment + 
                         Urch_habitat_treatment*Pred_treatment +
                         
                         (1| Trial/Tank/Kelp_ID),  
                       
                       data = kelp_processed,
                       
                       family = gaussian(link = "identity"))

sjPlot::tab_model(k1)

#k2 = kelp grazed model with averaged pycno summary stats
# (averaged from behavior scale to trial scale)

k2 <- glmmTMB::glmmTMB(pcnt_grazed ~ 
                         
                         Urch_habitat_treatment +
                         Pred_treatment +
                         Urch_habitat_treatment*Pred_treatment +
                         up_dn_pyc1_ratio +
                         up_dn_pyc2_ratio +
                         dist_slice_avgpyc1 +
                         dist_slice_avgpyc2 +
                         
                         
                         (1| Trial/Tank),  #doesn't run when kelp ID is also random effect 
                       
                       data = kelp_processed,
                       
                       family = gaussian(link = "identity"))

sjPlot::tab_model(k2)


#check assumptions-------------------------------------------------------------

k1_residuals <- DHARMa::simulateResiduals(k1)
plot(k1_residuals)

DHARMa::testZeroInflation(k1_residuals)



##############################################################################
############               behavior model                   ###################
##############################################################################

#plot distribution--------------------------------------------------------------

ggplot(behavior_processed, aes(x=urchin_slice_count))+
  geom_histogram() +
  facet_wrap(vars(Treatment))

#looks pretty right skewed! - im continuing with normal but its NOT - need to change this later!!

#build models-------------------------------------------------------------------

#b1 = urchin count as a function of all var of interest (in a tank)
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
                         (1| Trial/Tank),  #need a per urchin replication random effect?
                       
                       #specify dataset
                       data = behavior_processed,
                       
                       #specify family - normally distributed? gamma? beta? binomial?
                       family = gaussian(link = "identity"))

sjPlot::tab_model(b1)

#b2 = urchin count as a function of only significant FEs
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


#b3 = pycno count as a function of all var
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

#b4 = pycno count as a function of only significant var
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


#check assumptions-------------------------------------------------------------

b1_residuals <- DHARMa::simulateResiduals(b1)
plot(b1_residuals)

DHARMa::testZeroInflation(b1_residuals)





