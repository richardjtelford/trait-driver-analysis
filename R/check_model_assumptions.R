# Code to check model assumptions for regression/anova analyses

#function to make residual and qq plot
check_model_assumption <- function(mod, df2, response){
  df1 <- augment(mod)
  
  resid_plot <- ggplot(df1, aes(x = .fitted, y = .resid)) + 
    geom_point() +
    geom_smooth(method = "loess" , formula = "y ~ x", se = FALSE, colour = "orange") +
    theme_minimal()
  
  qq_plot <- ggplot(df2, aes(sample = response)) + 
    stat_qq_line(colour = "grey") +
    stat_qq() +
    theme_minimal()
  
  p <- wrap_plots(resid_plot, qq_plot)
  return(p)
}

# trait-climate relationship on trait mean
out <- summarised_boot_moments_climate %>% 
  ungroup() %>% 
  filter(year == 2016,
         TTtreat %in% c("control"),
         plasticity == "fixed") %>% 
  nest(data = -c(trait_trans)) %>% 
  mutate(mod = map(data, ~lm(mean ~ value, data = .x)),
         plot = map(data, ~check_model_assumption(mod, .x, .x$mean)))
walk(out$plot, print)


#effect of treatment * year on trait mean
out2 <- effect_size %>% 
  filter(direction == "convergence") %>% 
  nest(data = -c(trait_trans)) %>% 
  mutate(mod = map(data, ~lm(mean ~ TTtreat * year, data = .x)),
         plot = map(data, ~check_model_assumption(mod, .x, .x$mean)))
walk(out2$plot, print)


#effects of treatment * year on the happy moments
library("MASS")
hh <- sum_boot_moment_fixed %>% 
  ungroup() %>% 
  filter(trait_trans == "Thickness_mm_log") %>% 
  mutate(skew_min = min(skew),
         skew_plus = skew + (-skew_min) + 0.1)
model3 <- lm(skew_plus ~ year*TTtreat, hh)
p1 <- check_model_assumption(model3, hh, hh$skew_plus)

model3_log <- lm(log(skew_plus) ~ year*TTtreat, hh)
p2 <- check_model_assumption(model3, hh, hh$skew_plus)

bc <- boxcox(model3)
lambda <- bc$x[which.max(bc$y)]
model3_bc <- lm(((skew_plus^lambda-1)/lambda) ~ year*TTtreat, hh)
p3 <- check_model_assumption(model3_bc, hh, hh$skew_plus)

p1 / p2 / p3          


hh <- sum_boot_moment_fixed %>% 
  ungroup() %>% 
  filter(trait_trans == "C_percent")
model3 <- lm(var ~ year*TTtreat, hh)
check_model_assumption(model3, hh)
bc <- boxcox(model3)
lambda <- bc$x[which.max(bc$y)]
model3_bc <- lm(((var^lambda-1)/lambda) ~ year*TTtreat, hh)
check_model_assumption(model3_bc, hh)

library("car")
bc.car <- powerTransform(model3)
bc.car$lambda
summary(bc.car)
hh$var_trans <- bcPower(hh$var, lambda=bc.car$lambda)
model3_car <- lm(var_trans ~ year*TTtreat, hh)
check_model_assumption(model3_car, hh)

#sum_boot_moment_fixed %>% ungroup() %>% distinct(trait_trans)
C_percent        
CN_ratio         
dC13_permil      
dN15_permil      
Dry_Mass_g_log   
LDMC             
Leaf_Area_cm2_log
N_percent        
NP_ratio         
P_percent        
SLA_cm2_g        
Thickness_mm_log 



# Stuff
nest_dd <- dd %>% 
  nest(data = -c(trait_trans)) %>% 
  mutate(mod = map(data, ~lm(mean ~ value, data = .x)))

dd2 <- nest_dd %>% 
    mutate(plot = map(data, ~ autoplot(mod)))

wrap_plot(plot.list, nrow = nrow, ncol = ncol)
walk(dd2$plot, print)
walk(dd2$mod, plot, which = 1)

print(dd2$plot[[2]]) 

library("ggfortify")
pp <- happymoment_analysis %>% 
  filter(trait_trans == "C_percent") %>% 
  mutate(plot = map(data, ~ autoplot(mod)))
walk(pp$plot, print)
walk(pp$mod, plot, which = 1:2)

