## Visualization of gradients ----
### Note: This example illustrates the changes in gradient scores between two participant
### groups (i.e., from Controls to SSDs).
library(ggplot2)
library(tidyverse)

### Step 1: Data
load("GradData2plot.RData")
grad2plot2 # This is a data frame organized to have participants on the rows and gradient by groups on the columns

### Step 2: Load network information with colors
load("Color4grad.RData")
net.col.idx # A named vector of color for each network
ROI.idx # A named vector of the network to which each ROI belongs

## Step 3: Compute network means
net_means <- aggregate(grad2plot2, 
                       by = list(ROI.idx[rownames(grad2plot2)]),
                       FUN = mean) %>% column_to_rownames(var = "Group.1")

## Step 4: Plot arrows with labels
p.arrow <- ggplot(grad2plot2, aes(x = grad1.case, y = grad2.case)) + 
  geom_vline(xintercept = 0, 
             size = 1, color = "black", alpha = 0.2) +
  geom_hline(yintercept = 0, 
             size = 1, color = "black", alpha = 0.2) +
  ### You can uncomment this part if you want to print the dots for each ROI
  # geom_point(aes(color = ROI.idx[rownames(grad2plot2)]), # color of dots; make sure the network are selected according to the rownames to avoid mismatch in order
  #            size = 2, # size of dots
  #            shape = 19, # shape of dots
  #            alpha = 0.2) + # transparency of dots (e.g., 0-1)
  geom_text(data = net_means, aes (x = grad1.control, y = grad2.control, label = rownames(net_means)),
            size = 3, # size of labels 
            color = net.col.idx[rownames(net_means)], # color of labels
            fontface = "bold", # bold font
            alpha = 0.7) + # transparency of labels
  coord_fixed(ratio = 1) + 
  annotate("segment", # print arrows
           x = grad2plot2$grad1.control, # x-coordinates of the starts of arrows
           y = grad2plot2$grad2.control, # y-coordinates of the starts of arrows
           xend = grad2plot2$grad1.case, # x-coordinates of the ends of arrows
           yend = grad2plot2$grad2.case, # y-coordinates of the ends of arrows
           color = recode(ROI.idx[rownames(grad2plot2)], !!!net.col.idx), # color
           alpha = 0.2, # transparency
           arrow = arrow(length = unit(0.2, "cm"), 
                         type = "closed", angle = 20), # Style of arros
           linewidth = 0.8) + # linewidth of arrows
  xlab("Gradient 1") +
  ylab("Gradient 2") +
  theme(panel.background = element_rect(fill = "transparent", 
                                        color = "black"),
        legend.position = "none")

p.arrow
