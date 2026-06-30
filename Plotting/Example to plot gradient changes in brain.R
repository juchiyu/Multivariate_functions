## Plotting comparisons (between groups) in brain
### Library
library(ggplot2)
library(tidyverse)
library(ggseg)
library(ggsegGlasser)
library(grid)
library(gridExtra)

## Step 1: Load Data
load("GradientResult.RData")

## compute the difference with GLM ----
grad.reg.wide.diff$diagnostic_group <- sub.dx[grad.reg.wide$Subject,"diagnostic_group"]

all_roi_results <- grad.reg.wide.diff %>%
  select(-Subject) %>%
  ungroup() %>%
  group_by(network, gradient, ROI) %>%
  do(tidy(lm(value ~ diagnostic_group, data = .))) %>%
  ungroup() %>%
  group_by(term) %>%
  mutate(p_FDR = p.adjust(p.value, method = "fdr"))

## results from t-test filtered by p_FDR ----
all_roi_test <- all_roi_results %>%
  filter(term == "diagnostic_groupcontrol") %>%
  mutate(roi = str_remove(ROI, "_ROI"),
         label = case_when(str_starts(ROI, "L") ~ str_c("lh_", roi),
                           str_starts(ROI, "R_") ~ str_c("rh_", roi)),
         statistic.cor = ifelse(p_FDR < 0.05, statistic, 0)) %>%
  filter(ROI != "L_10pp_ROI") %>%
  as.data.frame() %>%
  group_by(gradient)

## results from t-test filtered by p_FDR (subcortical) ----
grad.reg.wide.sub <- grad.reg.wide.diff %>%
  select(-Subject) %>%
  filter(network == "Subcortical") %>%
  ### Mapping atlases to fit aseg in the plotting function ----
mutate(Aseg = rep(rep(c(rep("Right-Hippocampus", 2), # right hippocampus
                        rep("Right-Amygdala", 2), # right amygdala
                        rep("Right-Thalamus-Proper", 4), # right thalamas
                        rep("Right-Nucleus-Accumbuns", 2), # right NAcc
                        rep("Right-Pallidum", 2), # right GP
                        rep("Right-Putamen", 2), # right putaman
                        rep("Right-Caudate", 2), # right caudate
                        rep("Left-Hippocampus", 2), # left hippocampus
                        rep("Left-Amygdala", 2), # left amygdata
                        rep("Left-Thalamus-Proper", 4), # left thalamas
                        rep("Left-Nucleus-Accumbuns", 2), # left Nacc
                        rep("Left-Pallidum", 2), # left GP
                        rep("Left-Putamen", 2), # left putaman
                        rep("Left-Caudate", 2)), # left caudate
                      each = 3), each = 419)) %>%
  ungroup() %>%
  group_by(network, gradient, Aseg) %>%
  do(tidy(lm(value ~ diagnostic_group, data = .))) %>%
  ungroup() %>%
  group_by(term) %>%
  mutate(p_FDR = p.adjust(p.value, method = "fdr"))

all_subroi_results <- grad.reg.wide.sub %>% 
  filter(term == "diagnostic_groupcontrol") %>%
  mutate(statistic.cor = ifelse(p_FDR < 0.05, statistic, 0)) %>%
  filter(!(Aseg %in% c("Left-Nucleus-Accumbuns", "Right-Nucleus-Accumbuns"))) %>%
  as.data.frame() %>%
  group_by(gradient)

### Organize subcortical result to be plotted later ----
for (grad in c(1:3)){
  gradient_raw_data_sub.G <- all_subroi_results %>%
    as.data.frame() %>%
    select(gradient, network, statistic.cor, label = Aseg) %>%
    group_by(label, gradient) %>%
    filter(gradient == paste0("grad",grad))
  
  gradient_raw_aseg_sub.G <- aseg %>%
    as.tibble %>% left_join(gradient_raw_data_sub.G)
  gradient_raw_aseg_sub.G$gradient <- paste0("grad",grad)
  
  if (grad == 1){
    gradient_raw_aseg_sub <- gradient_raw_aseg_sub.G
  }else{
    gradient_raw_aseg_sub <- rbind(gradient_raw_aseg_sub, gradient_raw_aseg_sub.G)
  }
}

gradAseg.test <- gradient_raw_aseg_sub %>% as_brain_atlas()


## plot the F statistics (filtered by FDR test)
gradient_test_brain <- group_diff_grad %>%
  ggplot() +
  geom_brain(mapping = aes(fill = statistic.cor),
             atlas = glasser) +
  facet_wrap(~gradient, ncol = 1, labeller = labeller(gradient = 
                                                        c("grad1" = "Gradient 1: Somatosensory vs. Frontoparietal",
                                                          "grad2" = "Gradient 2: Auditory/Motion vs. Vision",
                                                          "grad3" = "Gradient 3: Default mode vs. Frontoparietal")
  )) +
  scale_fill_distiller(name = "F value", palette = "RdBu", limits = c(-7,7), values = c(0, 0.25, 0.5, 0.75, 1), guide = "none") +
  ggtitle("Gradients: Controls > SSDs\n(significant results; FDR-corrected)")+
  theme(axis.text.y.left = element_blank(), 
        axis.text.x.bottom = element_blank(),
        strip.text = element_text(hjust = 0.70),
        plot.margin=unit(c(1,-0.5,1,-0.1), "cm")) + 
  theme_brain(text.family = "Calibri")


## plot the t statistics (filtered by FDR test) subcortical
asegbrain.test <- ggplot() +
  geom_brain(atlas = gradAseg.test,
             colour = "grey30",
             mapping = aes(fill = statistic.cor),
             size=.5,side = "coronal"
  ) +
  facet_wrap(~gradient, ncol = 1, labeller = labeller(gradient = 
                                                        c("grad1" = "",
                                                          "grad2" = "",
                                                          "grad3" = "")
  )) +
  scale_fill_distiller(name = expression(~~italic(t)), palette = "RdBu", limits = c(-7,7), values = c(0, 0.25, 0.5, 0.75, 1)) +
  ggtitle("\n") +
  theme(axis.text.y.left = element_blank(), 
        axis.text.x.bottom = element_blank(),
        plot.margin=unit(c(1,1,1,-0.5), "cm")) + 
  theme_brain(text.family = "Calibri")

## Combined
grid.arrange(grobs = list(gradient_test_brain, asegbrain.test), 
             ncol = 2,
             widths = c(0.72, 0.28))
