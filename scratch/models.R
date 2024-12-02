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


#b5 = simple model: urchin variance-to-mean ratio as a function of pred treatment and urchin habitat treatment
ggplot(behavior_processed, aes(x=var_to_mean))+
  geom_histogram() +
  facet_wrap(vars(Treatment))

b5 <- glmmTMB::glmmTMB(var_to_mean ~ 
                         
                         Urch_habitat_treatment + 
                         Pred_treatment + 
                         Urch_habitat_treatment*Pred_treatment +

                         (1| Trial/Tank),  
                       
                       data = behavior_processed,
                       
                       family = gaussian(link = "identity"))

sjPlot::tab_model(b5)

#b6 = simple model: urchin crevice use as a function of pred treatment and urchin habitat treatment
b6 <- glmmTMB::glmmTMB(pcnt_in_crev ~ 
                         
                         Urch_habitat_treatment + 
                         Pred_treatment + 
                         Urch_habitat_treatment*Pred_treatment +
                         
                         (1| Trial/Tank),  
                         
                         data = behavior_processed,
                       
                       family = gaussian(link = "identity"))

sjPlot::tab_model(b6)

#b7 - urch associated w kelp - model won't run - bc of 0 inflation?
ggplot(behavior_processed, aes(x=pcnt_on_kelp))+
  geom_histogram() +
  facet_wrap(vars(Treatment))

b7 <- glmmTMB::glmmTMB(pcnt_on_kelp ~ 
                         
                         Urch_habitat_treatment + 
                         Pred_treatment + 
                         Urch_habitat_treatment*Pred_treatment +
                         
                         (1| Trial/Tank),  
                       
                       data = behavior_processed,
                       
                       family = gaussian(link = "identity"))

sjPlot::tab_model(b7)

#b8 - urchin distrbution wrt inflow
#a version - urchin count at inflow (slices 1 and 2) as response variable, see how this varies across treatments
#b version - urchin count as response variable, treatments and slice/dist from inflow as FE (but adding 3 way int terms is funky - instead try to combine this into response var as above)

urch_count_at_inflow <- behavior_processed %>%
  filter(slice %in% c(1,2))
#currently, slices include half slices which are for crev, removing this for model
urch_count_non_crev <- behavior_processed %>%
  filter(slice %in% c(1,2,3,4,5,6,7,8))


b8a <- glmmTMB::glmmTMB(urchin_slice_count ~ 
                         
                         Urch_habitat_treatment + 
                         Pred_treatment + 
                         Urch_habitat_treatment*Pred_treatment +
                         
                         (1| Trial/Tank),  
                       
                       data = urch_count_at_inflow,
                       
                       family = gaussian(link = "identity"))

sjPlot::tab_model(b8a)


b8b <- glmmTMB::glmmTMB(urchin_slice_count ~ 
  
                        Urch_habitat_treatment + 
                        Pred_treatment + 
                        slice +
                        Urch_habitat_treatment*Pred_treatment*slice +
                        
                        (1| Trial/Tank),  
                      
                      data = urch_count_non_crev,
                      
                      family = gaussian(link = "identity"))

sjPlot::tab_model(b8b)
#note: pred treatment is only sig when slice added as a 3-way interaction term. weird that random effect for tank/trial is 0....

#check assumptions-------------------------------------------------------------

b1_residuals <- DHARMa::simulateResiduals(b1)
plot(b1_residuals)

DHARMa::testZeroInflation(b1_residuals)





