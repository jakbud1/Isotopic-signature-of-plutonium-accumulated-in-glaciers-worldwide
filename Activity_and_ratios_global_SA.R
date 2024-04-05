### Global 
library(ggplot2)

### Data
df_sa <- readxl::read_xlsx("Input/Pu_global_JB.xlsx", sheet = 2)

df_sa$Glacier <- factor(df_sa$Glacier, c("Exploradores", "Tyndall", "El Morado", "Iver"))
df_sa$Glacier_year <- factor(df_sa$Glacier_year, c("Exploradores 2018", "Exploradores 2019", "Tyndall", "El Morado", "Iver"))

### Plots
## Ratio 238/239+240
SA1 <- ggplot(df_sa, aes(x = Glacier_year, y = Pu238to239_240, fill = Region)) 
SA1 + geom_boxplot(outlier.shape = NA, alpha = 0.5) + geom_jitter(aes(color = Region), size = 2.5) + scale_fill_brewer(palette = "Dark2") + 
  scale_color_brewer(palette = "Dark2") + 
  theme_classic() + geom_hline(yintercept = 0.13, size = 1.5, linetype = "dashed") +
  ylab(expression(paste(""^"238","Pu/",""^"239+240", "Pu ratio"))) + 
  annotate(geom = "text", x = 1.5, y = 0.65, label = "250m a.s.l", color = "orange") + 
  annotate(geom = "text", x = 3, y = 0.65, label = "700m a.s.l", color = "orange") + 
  annotate(geom = "text", x = 4, y = 0.65, label = "3300m a.s.l", color = "darkgreen") + 
  annotate(geom = "text", x = 5, y = 0.65, label = "4400m a.s.l", color = "darkgreen") + 
  annotate(geom = "text", x = 5, y = 0.05, label = "NA", color = "black") + 
  xlab("") + ggtitle(expression(paste(""^"238","Pu/",""^"239+240", "Pu ratio"))) + 
  theme(text = element_text(size = 15), axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5, size = 18))
ggsave("Output/SA_238to239_240.png", width = 18, height = 12, units = "cm", dpi = 300)

## Ratio 240/239
SA2 <- ggplot(df_sa, aes(x = Glacier_year, y = Pu240to_239, fill = Region)) 
SA2 + geom_boxplot(outlier.shape = NA, alpha = 0.5) + geom_jitter(aes(color = Region), size = 2.5) + scale_fill_brewer(palette = "Dark2") + 
  scale_color_brewer(palette = "Dark2") + geom_hline(yintercept = 0.18, size = 1.5, linetype = "dashed") +
  theme_classic() + 
  ylab(expression(paste(""^"240","Pu/",""^"239", "Pu ratio"))) + 
  annotate(geom = "text", x = 1.5, y = 0.22, label = "250m a.s.l", color = "orange") + 
  annotate(geom = "text", x = 3, y = 0.22, label = "700m a.s.l", color = "orange") + 
  annotate(geom = "text", x = 4, y = 0.22, label = "3300m a.s.l", color = "darkgreen") + 
  annotate(geom = "text", x = 5, y = 0.22, label = "4400m a.s.l", color = "darkgreen") + xlab("") +
  ggtitle(expression(paste(""^"240","Pu/",""^"239", "Pu ratio"))) + 
  theme(text = element_text(size = 15), axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5, size = 18))
ggsave("Output/SA_240to239.png", width = 18, height = 12, units = "cm", dpi = 300)

## Activity 239+240 
SA3 <- ggplot(df_sa, aes(x = Glacier_year, y = Pu239_240, fill = Region)) 
SA3 + geom_boxplot(outlier.shape = NA, alpha = 0.5) + geom_jitter(aes(color = Region), size = 2.5) + scale_fill_brewer(palette = "Dark2") + 
  scale_color_brewer(palette = "Dark2") + 
  theme_classic() + 
  ylab(expression(paste(""^"239+240", "Pu (Bq kg" ^-1,")"))) + 
  annotate(geom = "text", x = 1.5, y = 18, label = "250m a.s.l", color = "orange") + 
  annotate(geom = "text", x = 3, y = 18, label = "700m a.s.l", color = "orange") + 
  annotate(geom = "text", x = 4, y = 18, label = "3300m a.s.l", color = "darkgreen") + 
  annotate(geom = "text", x = 5, y = 18, label = "4400m a.s.l", color = "darkgreen") + xlab("") +
  ggtitle(expression(paste(""^"239+240", "Pu activity concentration"))) + 
  theme(text = element_text(size = 15), axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5, size = 18))
ggsave("Output/SA_activity239_240.png", width = 18, height = 12, units = "cm", dpi = 300)

## Activity 238 
SA4 <- ggplot(df_sa, aes(x = Glacier_year, y = Pu238, fill = Region)) 
SA4 + geom_boxplot(outlier.shape = NA, alpha = 0.5) + geom_jitter(aes(color = Region), size = 2.5) + 
  scale_fill_brewer(palette = "Dark2") + 
  scale_color_brewer(palette = "Dark2") + 
  theme_classic() + 
  ylab(expression(paste(""^"238", "Pu (Bq kg" ^-1,")"))) + 
  annotate(geom = "text", x = 1.5, y = 8, label = "250m a.s.l", color = "orange") + 
  annotate(geom = "text", x = 3, y = 8, label = "700m a.s.l", color = "orange") + 
  annotate(geom = "text", x = 4, y = 8, label = "3300m a.s.l", color = "darkgreen") + 
  annotate(geom = "text", x = 5, y = 8, label = "4400m a.s.l", color = "darkgreen") +
  annotate(geom = "text", x = 5, y = 0, label = "BDL", color = "black") + xlab("") +
  ggtitle(expression(paste(""^"238", "Pu activity concentration"))) + 
  theme(text = element_text(size = 15), axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5, size = 18))
ggsave("Output/SA_activity238.png", width = 18, height = 12, units = "cm", dpi = 300)