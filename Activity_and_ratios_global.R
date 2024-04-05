### Global ----------------------------------------------------------------
library(glmmTMB)
library(visreg)
library(dplyr)
library(ggplot2)
library(maps)
library(car)
library(coin)
library(performance)

library(MuMIn)

SE <- function(x){
  y <- na.omit(x)
  sd(y)/sqrt(length(y))
}

### Data ------------------------------------------------------------------
## Full 
act_full <- readxl::read_xlsx("Input/Pu_global_JB.xlsx")

## Analysis
act <- act_full %>% group_by(Glacier) %>% filter(n() > 3)
act$Pu239_240_L <- log(act$Pu239_240)

boxplot(act$Pu239_240~act$Glacier)
ggplot(act, aes(x = Pu239_240)) +
  geom_density() + facet_wrap(.~Glacier, scales = "free")

act_a <- aggregate(OM ~ Glacier, FUN = mean, data = act)
act_a <- left_join(act_a, aggregate(Pu238 ~ Glacier, FUN = mean, data = act))
act_a <- left_join(act_a, aggregate(Pu239_240 ~ Glacier, FUN = mean, data = act))
act_a <- left_join(act_a, aggregate(Pu238to239_240 ~ Glacier, FUN = mean, data = act))
act_a <- left_join(act_a, aggregate(Pu240to_239 ~ Glacier, FUN = mean, data = act))

meta <- readxl::read_xlsx("Input/Meta.xlsx")

glacier_scale <- left_join(act_a, meta, by = "Glacier")
glacier_scale$Pu239_240_L <- log(glacier_scale$Pu239_240)
glacier_scale$Pu238_L <- log(glacier_scale$Pu238)

write.csv(glacier_scale, "Output/glacier_scale.csv")

df_prec <- read.csv("Output/precipitation.csv")[,-1]

glacier_scale_p <- left_join(glacier_scale, df_prec, by = "Glacier") ### TUTAJ KONIEC! Najpierw należy wygenerować dane z nowymi nazwami lodowców. 

act_full$Glacier <- as.factor(act_full$Glacier)
levels(act_full$Glacier)[levels(act_full$Glacier) == "Morterasch"] <- "Morteratsch"

## Maps 
act_full$Latitude <- as.numeric(act_full$Latitude)
act_full$Longitude <- as.numeric(act_full$Longitude)
act_full$Hemisphere <- as.factor(act_full$Hemisphere)

act_full$Macro_region <- factor(act_full$Macro_region, 
                                c("McMurdo Dry Valleys", "Antarctic Peninsula", "South America", "Africa", 
                                  "Asia", "Southern Europe", "European (sub)Arctic", "North American continent", "Svalbard"))

act_full$Hemisphere <- factor(act_full$Hemisphere, c("Southern", "Northern"))

## Mean ratios per regions
act_reg <- aggregate(OM ~ Region, FUN = mean, data = act_full)
act_reg <- left_join(act_reg, aggregate(Pu238 ~ Region, FUN = mean, data = act_full))
act_reg <- left_join(act_reg, aggregate(Pu239_240 ~ Region, FUN = mean, data = act_full))
act_reg <- left_join(act_reg, aggregate(Latitude ~ Region, FUN = mean, data = act_full))
act_reg <- left_join(act_reg, aggregate(Longitude ~ Region, FUN = mean, data = act_full))

# Northern
mean_N <- aggregate(cbind(Pu238to239_240, Pu240to_239) ~ Region, data = subset(act_full, Hemisphere == "Northern"), FUN = mean)

mean_N$SE_238to_239 <- aggregate(cbind(Pu238to239_240) ~ Region, data = subset(act_full, Hemisphere == "Northern"), FUN = SE)[,2]
mean_N$SE_240to_239 <- aggregate(cbind(Pu240to_239) ~ Region, data = subset(act_full, Hemisphere == "Northern"), FUN = SE)[,2]

mean_N$min238 <- mean_N$Pu238to239_240 - (1.96*mean_N$SE_238to_239)
mean_N$max238 <- mean_N$Pu238to239_240 + (1.96*mean_N$SE_238to_239)

mean_N$min240 <- mean_N$Pu240to_239 - (1.96*mean_N$SE_240to_239)
mean_N$max240 <- mean_N$Pu240to_239 + (1.96*mean_N$SE_240to_239)

mean_N <- subset(mean_N, Region != "Qilian")

# Southern
mean_S <- aggregate(cbind(Pu238to239_240, Pu240to_239) ~ Region, data = subset(act_full, Hemisphere == "Southern"), FUN = mean)

mean_S$SE_238to_239 <- aggregate(cbind(Pu238to239_240) ~ Region, data = subset(act_full, Hemisphere == "Southern"), FUN = SE)[,2]
mean_S$SE_240to_239 <- aggregate(cbind(Pu240to_239) ~ Region, data = subset(act_full, Hemisphere == "Southern"), FUN = SE)[-2,2]

mean_S$min238 <- mean_S$Pu238to239_240 - (1.96*mean_S$SE_238to_239)
mean_S$max238 <- mean_S$Pu238to239_240 + (1.96*mean_S$SE_238to_239)

mean_S$min240 <- mean_S$Pu240to_239 - (1.96*mean_S$SE_240to_239)
mean_S$max240 <- mean_S$Pu240to_239 + (1.96*mean_S$SE_240to_239)

### Models and test  ---------------------------------------------------
## Activity concentrations and OM correlation on a glacier scale 
cor_by_glacier <- by(act_full[,c(14,15,18)], act_full$Glacier, cor, method = "spearman")

## Pu239+240_activity ~ predictors
M_full_gl_1 <- glmmTMB(Pu239_240_L ~ OM + Elevation + Glacier_type + prec_GLF +
                         (1|Macro_region), 
                       REML = FALSE, data = glacier_scale_p); summary(M_full_gl_1)

options(na.action = "na.fail")

res_1 <- dredge(M_full_gl_1, trace = 2)
subset(res_1, delta <= 2, recalc.weights = FALSE); options(na.action = "na.omit")
get.models(res_1, delta <= 2, REML = TRUE)



M_sim_gl_1 <- glmmTMB(Pu239_240_L ~ OM +
                        (1|Macro_region), 
                      REML = TRUE, data = glacier_scale_p); summary(M_sim_gl_1)

check_model(M_sim_gl_1); Anova(M_sim_gl_1)

## Pu239+240_activity ~ hemispheres
glacier_scale$Hemisphere <- as.factor(glacier_scale$Hemisphere)

Nor_act <- subset(glacier_scale, Hemisphere == "Northern")$Pu239_240
Sou_act <- subset(glacier_scale, Hemisphere == "Southern")$Pu239_240

hist(log(Nor_act))
hist(log(Sou_act))

mean(Nor_act, na.rm = TRUE); SE(Nor_act)
mean(Sou_act, na.rm = TRUE); SE(Sou_act)

median(Nor_act, na.rm = TRUE); IQR(Nor_act, na.rm = TRUE)
median(Sou_act, na.rm = TRUE); IQR(Sou_act, na.rm = TRUE)

wilcox_test(Pu239_240 ~ Hemisphere, 
            data = glacier_scale, distribution = "exact") 

## Pu239+240_activity ~ regions
kruskal.test(act_full$Pu239_240, act_full$Macro_region)
dunn.test::dunn.test(act_full$Pu239_240, act_full$Macro_region, 
                     method = "bh")

## Pu238_activity ~ predictors
glacier_scale_p_f <- glacier_scale_p[complete.cases(glacier_scale_p$Pu238), ]

M_full_gl_2 <- glmmTMB(Pu238_L ~ OM + Elevation + Glacier_type +
                         (1|Macro_region), 
                       REML = FALSE, data = glacier_scale_p_f); summary(M_full_gl_2)

options(na.action = "na.fail")

res2 <- dredge(M_full_gl_2, trace = 2)
sub2 <- subset(res2, delta <= 2, recalc.weights = FALSE); options(na.action = "na.omit")

get.models(res2, delta <= 2, REML = TRUE)

M_sim_gl_2 <- glmmTMB(Pu238_L ~ OM +
                        (1|Macro_region), 
                      REML = TRUE, data = glacier_scale_p_f); summary(M_sim_gl_2)

check_model(M_sim_gl_2); Anova(M_sim_gl_2)

## Pu238_activity ~ hemispheres
glacier_scale$Hemisphere <- as.factor(glacier_scale$Hemisphere)

hist(log(subset(glacier_scale, Hemisphere == "Northern")$Pu238))
hist(log(subset(glacier_scale, Hemisphere == "Southern")$Pu238)) # Normality deviation

wilcox_test(Pu238 ~ Hemisphere, 
            data = glacier_scale, distribution = "exact")

## Pu238_activity ~ regions
kruskal.test(act_full$Pu238, act_full$Macro_region)
dunn.test::dunn.test(act_full$Pu238, act_full$Macro_region, 
                     method = "bh")

## Pu238/239+240 between Hemispheres
hist(subset(glacier_scale, Hemisphere == "Northern")$Pu238to239_240)
hist(subset(glacier_scale, Hemisphere == "Southern")$Pu238to239_240)

mean(subset(glacier_scale, Hemisphere == "Northern")$Pu238to239_240, na.rm = TRUE)
median(subset(glacier_scale, Hemisphere == "Northern")$Pu238to239_240, na.rm = TRUE)
IQR(subset(glacier_scale, Hemisphere == "Northern")$Pu238to239_240, na.rm = TRUE)

mean(subset(glacier_scale, Hemisphere == "Southern")$Pu238to239_240, na.rm = TRUE)
median(subset(glacier_scale, Hemisphere == "Southern")$Pu238to239_240, na.rm = TRUE)
IQR(subset(glacier_scale, Hemisphere == "Southern")$Pu238to239_240, na.rm = TRUE)

wilcox_test(Pu238to239_240 ~ Hemisphere, 
            data = glacier_scale, distribution = "exact")

## Pu240/Pu239 between Hemispheres
hist(subset(glacier_scale, Hemisphere == "Northern")$Pu240to_239)
hist(subset(glacier_scale, Hemisphere == "Southern")$Pu240to_239)

mean(subset(glacier_scale, Hemisphere == "Northern")$Pu240to_239, na.rm = TRUE)
median(subset(glacier_scale, Hemisphere == "Northern")$Pu240to_239, na.rm = TRUE)
IQR(subset(glacier_scale, Hemisphere == "Northern")$Pu240to_239, na.rm = TRUE)

mean(subset(glacier_scale, Hemisphere == "Southern")$Pu240to_239, na.rm = TRUE)
median(subset(glacier_scale, Hemisphere == "Southern")$Pu240to_239, na.rm = TRUE)
IQR(subset(glacier_scale, Hemisphere == "Southern")$Pu240to_239, na.rm = TRUE)

wilcox_test(Pu240to_239 ~ Hemisphere, 
            data = glacier_scale, distribution = "exact")

### Raw data visualization ---------------------------------------------
## Pu239+240_activity ~ OM within glaciers
act_full$Pu239_240_L <- log(act_full$Pu239_240)

ggplot(data = subset(act_full, Macro_region != "McMurdo Dry Valleys" & Macro_region != "Africa"), 
       aes(x = OM, y = Pu239_240_L, color = Glacier)) + 
  geom_point() + geom_smooth(method = "lm") + facet_wrap(.~Macro_region, scales = "free") + 
  theme_classic() + theme(legend.key.size = unit(0.13, 'cm'),
                          plot.title = element_text(hjust = 1, size = 12)) + guides(colour = guide_legend(ncol = 2)) +
  xlab("Organic matter content (%)") + 
  ylab(expression(paste("log("^"239+240", "Pu (Bq kg" ^-1,"))"))) + 
  ggtitle(expression(paste("Relation of organic matter content and "^"239+240","Pu activitiy concentration on a glacier scale")))

ggsave("Output/Fig. S1_239_and_OM_glacier_scale.png", width = 24, height = 12, units = "cm", dpi = 200)

## Pu238_activity ~ OM within glaciers
act_full$Pu238_L <- log(act_full$Pu238)

ggplot(data = subset(act_full, Macro_region != "McMurdo Dry Valleys" & Macro_region != "Africa"), 
       aes(x = OM, y = Pu238_L, color = Glacier)) + 
  geom_point() + geom_smooth(method = "lm") + facet_wrap(.~Macro_region, scales = "free") + 
  theme_classic() + theme(legend.key.size = unit(0.13, 'cm'),
                          plot.title = element_text(hjust = 1, size = 12)) + guides(colour = guide_legend(ncol = 2)) +
  xlab("Organic matter content (%)") + 
  ylab(expression(paste("log("^"238", "Pu (Bq kg" ^-1,"))"))) + 
  ggtitle(expression(paste("Relation of organic matter content and "^"238","Pu activitiy concentration on a glacier scale")))

ggsave("Output/Fig. S2_238_and_OM_glacier_scale.png", width = 24, height = 12, units = "cm", dpi = 200)

## Pu238/239+240 ~ Pu240/239
# mean data - Northern
rat_cor_N <- ggplot(mean_N, aes(y = Pu240to_239, x = Pu238to239_240, color = Region))
rat_cor_N + geom_point() + 
  geom_errorbar(aes(ymin = min240, ymax = max240)) + 
  geom_errorbarh(aes(xmin = min238, xmax = max238)) + 
  theme_classic() + theme(text = element_text(size = 20)) +
  xlab(expression(paste(""^"238","Pu/",""^"239+240", "Pu ratio"))) + 
  ylab(expression(paste(""^"240","Pu/",""^"239", "Pu ratio"))) +
  geom_rect(aes(xmin = 0.025, xmax = 0.04, ymin = 0.175, ymax = 0.19), 
            fill = "grey", color = "black", alpha = 0.1) + # GF + SNAP
  geom_rect(aes(xmin = 0.38, xmax = 0.54, ymin = 0.186, ymax = 0.348), 
            fill = "darkred", color = "black" , alpha = 0.01) + # Chernobyl 
  geom_rect(aes(xmin = 0.0, xmax = 0.54, ymin = 0.01, ymax = 0.07), 
            fill = "darkorange", color = "black" , alpha = 0.01) # WGP
ggsave("Output/ratios_northern3.png", width = 20, height = 12, units = "cm", dpi = 300)

# mean data - Southern
rat_cor_S <- ggplot(mean_S, aes(y = Pu240to_239, x = Pu238to239_240, color = Region))
rat_cor_S + geom_point() + 
  geom_errorbar(aes(ymin = min240, ymax = max240)) + 
  geom_errorbarh(aes(xmin = min238, xmax = max238)) + 
  theme_classic() + theme(text = element_text(size = 20)) +
  xlab(expression(paste(""^"238","Pu/",""^"239+240", "Pu ratio"))) + 
  ylab(expression(paste(""^"240","Pu/",""^"239", "Pu ratio"))) + 
  geom_rect(aes(xmin = 0.135, xmax = 0.145, ymin = 0.175, ymax = 0.19), 
            fill = "grey", color = "black", alpha = 0.1) + # GF + SNAP 
  geom_rect(aes(xmin = 0.0, xmax = 0.3, ymin = 0.32, ymax = 0.363), 
            fill = "darkblue", color = "black" , alpha = 0.07) + # PPG
  geom_rect(aes(xmin = 0.0, xmax = 0.3, ymin = 0.035, ymax = 0.045), 
            fill = "darkgreen", color = "black" , alpha = 0.07) + # FP
  geom_rect(aes(xmin = 0.05, xmax = 0.06, ymin = 0.028, ymax = 0.037), 
            fill = "black", color = "black") # Nagasaki
ggsave("Output/ratios_southern3.png", width = 20, height = 12, units = "cm", dpi = 300)

## Pu239+240_activity ~ regions
PU_239_1 <- ggplot(act_full, aes(y = Macro_region, x = Pu239_240, fill = Macro_region))
PU_239_1 + geom_boxplot(outlier.shape = NA) + geom_jitter(alpha = 0.25, size = 2) + 
  theme_classic() + scale_fill_brewer(palette = "Pastel1", name = "Macro region") + 
  xlab(expression(paste(""^"239+240", "Pu (Bq kg" ^-1,")"))) + ylab("") + 
  guides(fill = FALSE) + theme(text = element_text(size = 20), axis.text.y = element_text(angle = 0), 
                               plot.title = element_text(hjust = 1, size = 18)) + 
  annotate(geom = "text", x = 8, y = 4, label = "BDL", color = "black") + 
  ggtitle(expression(paste("Macro-regional distribution of "^"239+240","Pu activitiy concentration")))
ggsave("Output/Pu239_Macro_Regions.png", width = 20, height = 12, units = "cm", dpi = 300)

## Pu239+240_activity ~ hemispheres
PU_239_2 <- ggplot(act_full, aes(y = Hemisphere, x = Pu239_240))
PU_239_2 + geom_boxplot(outlier.shape = NA, aes(fill = Hemisphere), alpha = 0.5) + 
  geom_jitter(aes(color = Hemisphere), alpha = 0.3, size = 3) + 
  theme_classic() + 
  scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1") +
  xlab(expression(paste(""^"239+240", "Pu (Bq kg" ^-1,")"))) + ylab("") + 
  guides(fill = FALSE, color = FALSE) + theme(text = element_text(size = 25), axis.text.y = element_text(angle = 0), 
                                              plot.title = element_text(hjust = 0.5, size = 14)) 
ggsave("Output/Pu239_Hemispheres.png", width = 16, height = 12, units = "cm", dpi = 300)

## Pu238_activity ~ regions
PU_238_1 <- ggplot(act_full, aes(y = Macro_region, x = Pu238, fill = Macro_region))
PU_238_1 + geom_boxplot(outlier.shape = NA) + geom_jitter(alpha = 0.25, size = 2) + 
  theme_classic() + scale_fill_brewer(palette = "Pastel1", name = "Macro region") + 
  xlab(expression(paste(""^"238", "Pu (Bq kg" ^-1,")"))) + ylab("") + 
  guides(fill = FALSE) + theme(text = element_text(size = 20), axis.text.y = element_text(angle = 0), 
                               plot.title = element_text(hjust = 1, size = 18)) + 
  annotate(geom = "text", x = 0.4, y = 4, label = "BDL", color = "black") + 
  ggtitle(expression(paste("Macro-regional distribution of "^"238","Pu activitiy concentration")))
ggsave("Output/Pu238_Macro_Regions.png", width = 20, height = 12, units = "cm", dpi = 300)

# Pu238_activity ~ hemispheres
PU_238_2 <- ggplot(act_full, aes(y = Hemisphere, x = Pu238))
PU_238_2 + geom_boxplot(outlier.shape = NA, aes(fill = Hemisphere), alpha = 0.5) + 
  geom_jitter(aes(color = Hemisphere), alpha = 0.3, size = 3) + 
  theme_classic() + 
  scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1") +
  xlab(expression(paste(""^"238", "Pu (Bq kg" ^-1,")"))) + ylab("") + 
  guides(fill = FALSE, color = FALSE) + theme(text = element_text(size = 25), axis.text.y = element_text(angle = 0), 
                                              plot.title = element_text(hjust = 0.5, size = 14)) 
ggsave("Output/Pu238_Hemispheres.png", width = 16, height = 12, units = "cm", dpi = 300)

## 238/239+240Pu ~ regions
PU_238_1 <- ggplot(act_full, aes(y = Macro_region, x = Pu238to239_240, fill = Macro_region))
PU_238_1 + geom_boxplot(outlier.shape = NA) + geom_jitter(alpha = 0.25, size = 2) + 
  theme_classic() + scale_fill_brewer(palette = "Pastel1", name = "Macro region") + 
  xlab(expression(paste(""^"238","Pu/",""^"239+240", "Pu ratio"))) + ylab("") + 
  guides(fill = FALSE) + theme(text = element_text(size = 20), axis.text.y = element_text(angle = 0), 
                               plot.title = element_text(hjust = 1, size = 20)) + 
  annotate(geom = "text", x = 0.04, y = 4, label = "NA", color = "black") + 
  ggtitle(expression(paste(""^"238","Pu/",""^"239+240", "Pu variation between macro-regions")))
ggsave("Output/Pu238_to_239_240_Macro_Regions.png", width = 16, height = 12, units = "cm", dpi = 300)

## 238/239+240Pu ~ hemispheres
PU_ratio_2 <- PU_238_2 <- ggplot(act_full, aes(y = Hemisphere, x = Pu238to239_240))
PU_ratio_2 + geom_boxplot(outlier.shape = NA, aes(fill = Hemisphere), alpha = 0.5) + 
  geom_jitter(aes(color = Hemisphere), alpha = 0.3, size = 3) + 
  theme_classic() + ggtitle(expression(paste(""^"238","Pu/",""^"239+240", "Pu between hemispheres"))) +
  scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1") +
  xlab(expression(paste(""^"238","Pu/",""^"239+240", "Pu ratio"))) + ylab("") + 
  guides(fill = FALSE, color = FALSE) + theme(text = element_text(size = 20), axis.text.y = element_text(angle = 0), 
                                              plot.title = element_text(hjust = 0.5, size = 20)) 
ggsave("Output/Pu238to239_240_Hemispheres.png", width = 16, height = 12, units = "cm", dpi = 300)

## 240/239Pu ~ regions
PU_ratio_3 <- ggplot(act_full, aes(y = Macro_region, x = Pu240to_239, fill = Macro_region))
PU_ratio_3 + geom_boxplot(outlier.shape = NA) + geom_jitter(alpha = 0.25, size = 2) + 
  theme_classic() + scale_fill_brewer(palette = "Pastel1", name = "Macro region") + 
  xlab(expression(paste(""^"240","Pu/",""^"239", "Pu ratio"))) + ylab("") + 
  guides(fill = FALSE) + theme(text = element_text(size = 20), axis.text.y = element_text(angle = 0), 
                               plot.title = element_text(hjust = 1, size = 20)) + xlim(0.05, 0.31) +
  ggtitle(expression(paste(""^"240","Pu/",""^"239", "Pu variation between macro-regions")))
ggsave("Output/Pu240_to_239_Macro_Regions.png", width = 16, height = 12, units = "cm", dpi = 300)

## 240/239Pu Hemispheres
PU_ratio_4 <- ggplot(act_full, aes(y = Hemisphere, x = Pu240to_239))
PU_ratio_4 + geom_boxplot(outlier.shape = NA, aes(fill = Hemisphere), alpha = 0.5) + 
  geom_jitter(aes(color = Hemisphere), alpha = 0.3, size = 3) + 
  theme_classic() + ggtitle(expression(paste(""^"240","Pu/",""^"239", "Pu between hemispheres"))) +
  scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1") +
  xlab(expression(paste(""^"240","Pu/",""^"239", "Pu ratio"))) + ylab("") + xlim(0.05, 0.31) +
  guides(fill = FALSE, color = FALSE) + theme(text = element_text(size = 20), axis.text.y = element_text(angle = 0), 
                                              plot.title = element_text(hjust = 0.5, size = 20)) 
ggsave("Output/Pu240to239_Hemispheres.png", width = 16, height = 12, units = "cm", dpi = 300)

### Models visualization ----------------------------------------------
## Pu239+240_activity
v1_sim <- visreg(M_sim_gl_1, "OM", type = "conditional", rug = 1, scale = "response", plot = FALSE)

v1_sim_g <- ggplot() + geom_line(data = v1_sim[["fit"]], aes(x = OM, y = visregFit), size = 2, color = "#7b4e2b") + 
  geom_ribbon(data = v1_sim[["fit"]], aes(x = OM, ymin = visregLwr, ymax = visregUpr), alpha = 0.1) +
  geom_point(data = v1_sim[["res"]], aes(x = OM, y = visregRes), size = 3, color = "#7b4e2b", alpha = 0.5)

v1_sim_g + theme_classic() + xlab("Organic matter (%)") +  
  ylab(expression(paste("log("^"239+240", "Pu (Bq kg" ^-1,"))"))) + 
  theme(text = element_text(size = 20), axis.text.y = element_text(angle = 0), 
        plot.title = element_text(hjust = 0.5, size = 18)) +
  ggtitle(expression(paste("Relation of "^"239+240","Pu and organic matter")))
ggsave("Output/Pu239_OM.png", width = 20, height = 12, units = "cm", dpi = 300)

## Pu238_activity
v2_sim <- visreg(M_sim_gl_2, "OM", type = "conditional", rug = 1, scale = "response", plot = FALSE)

v2_sim_g <- ggplot() + geom_line(data = v2_sim[["fit"]], aes(x = OM, y = visregFit), size = 2, color = "#7b4e2b") + 
  geom_ribbon(data = v2_sim[["fit"]], aes(x = OM, ymin = visregLwr, ymax = visregUpr), alpha = 0.1) +
  geom_point(data = v2_sim[["res"]], aes(x = OM, y = visregRes), size = 3, color = "#7b4e2b", alpha = 0.5)

v2_sim_g + theme_classic() + xlab("Organic matter (%)") +  
  ylab(expression(paste("log("^"238", "Pu (Bq kg" ^-1,"))"))) + 
  theme(text = element_text(size = 20), axis.text.y = element_text(angle = 0), 
        plot.title = element_text(hjust = 0.5, size = 18)) +
  ggtitle(expression(paste("Relation of "^"238","Pu and organic matter")))
ggsave("Output/Pu238_OM.png", width = 20, height = 12, units = "cm", dpi = 300)

### Map ------------------------------------------------------------------------
## Global map
world <- map_data("world")
glacier_scale$Latitude <- as.numeric(glacier_scale$Latitude)
glacier_scale$Longitude <- as.numeric(glacier_scale$Longitude)

#Pu239+240 - Map with captions
ggplot() +
  geom_map(data = world, map = world,
           aes(long, lat, map_id = region),
           color = "white", fill = "lightgray", size = 0.1) +
  geom_point(data = act_reg, aes(x = Longitude, y = Latitude, color = OM, size = Pu239_240)) +
  theme_classic() + xlab("Longtitude") + ylab("Latitude") +
  scale_color_continuous(name = "Organic matter (%)") + theme(legend.position = "bottom") +
  scale_size(range = c(2, 8), name = expression(paste(""^"239+240", "Pu (Bq kg" ^-1,")"))) +
  geom_text(data = act_reg, aes(x = Longitude, y = Latitude + 3, label = Region), size = 5) +
  geom_hline(yintercept = 0) +
  annotate("text", x = 37, y = -3, label = "Below detection limit",
           colour = "red", size = 3, fontface = "bold")
ggsave("Output/Pu239_map_names.png", width = 24, height = 14, units = "cm", dpi = 500)

#Pu239+240 - Map without captions
ggplot() +
  geom_map(data = world, map = world,
           aes(long, lat, map_id = region),
           color = "white", fill = "lightgray", size = 0.2) +
  geom_point(data = act_reg, aes(x = Longitude, y = Latitude, color = OM, size = Pu239_240)) +
  theme_classic() + xlab("Longtitude") + ylab("Latitude") +
  scale_color_gradient(name = "Organic matter (%)", low =  "#fce1cc",
                       high = "#4a2e19") + theme(legend.position = "bottom") +
  scale_size(range = c(6, 18), name = expression(paste(""^"239+240", "Pu (Bq kg" ^-1,")"))) +
  geom_hline(yintercept = 0) + theme(text = element_text(size = 25))
ggsave("Output/Pu239_no_map_names.png", width = 50, height = 30, units = "cm", dpi = 500)

#Pu238 - Map with captions
ggplot() +
  geom_map(data = world, map = world,
           aes(long, lat, map_id = region),
           color = "white", fill = "lightgray", size = 0.1) +
  geom_point(data = act_reg, aes(x = Longitude, y = Latitude, color = OM, size = Pu238)) +
  theme_classic() + xlab("Longtitude") + ylab("Latitude") +
  scale_color_continuous(name = "Organic matter (%)") + theme(legend.position = "bottom") +
  scale_size(range = c(2, 8), name = expression(paste(""^"238", "Pu (Bq kg" ^-1,")"))) +
  geom_text(data = act_reg, aes(x = Longitude, y = Latitude + 3, label = Region), size = 5) +
  annotate("text", x = 37, y = -3, label = "Below detection limit",
           colour = "red", size = 3, fontface = "bold") + 
  geom_hline(yintercept = 0)
ggsave("Output/Pu238_map_names.png", width = 48, height = 28, units = "cm", dpi = 500)

#Pu238 - Map without captions
ggplot() +
  geom_map(data = world, map = world,
           aes(long, lat, map_id = region),
           color = "white", fill = "lightgray", size = 0.2) +
  geom_point(data = act_reg, aes(x = Longitude, y = Latitude, color = OM, size = Pu238)) +
  theme_classic() + xlab("Longtitude") + ylab("Latitude") +
  scale_color_gradient(name = "Organic matter (%)", low =  "#fce1cc",
                       high = "#4a2e19") + theme(legend.position = "bottom") +
  scale_size(range = c(6, 18), name = expression(paste(""^"238", "Pu (Bq kg" ^-1,")"))) +
  geom_hline(yintercept = 0) + theme(text = element_text(size = 25)) + 
  guides(size  = guide_legend(order = 1),
         fill = guide_legend(order = 2))
ggsave("Output/Pu238_no_map_names.png", width = 50, height = 30, units = "cm", dpi = 500)