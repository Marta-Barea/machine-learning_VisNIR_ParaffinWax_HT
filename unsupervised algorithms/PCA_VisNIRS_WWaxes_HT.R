# Load Required Libraries
library(doParallel)
library(readxl)
library(prospectr)
library(cluster)
library(factoextra)
library(data.table)
library(egg)
library(viridis)

# Load Parallelization
cl <- makePSOCKcluster(8)
registerDoParallel(cl)

# Load data
pw_data <- read_excel("~/Documents/Doctorado/Tesis Doctoral/Investigación Cepsa/Vis-NIR/XDS-NIR_FOSS/Estudio según Tipo de Parafina e Hidrotratamiento/NIRS_HT_PW.xlsx",
                      sheet = "HT_Class")

# First derivative and Savitzky Golay Smoothing
pw_data$Sample <- as.factor(pw_data$Sample)

sgvec <- savitzkyGolay(X = pw_data[,-1], p = 3, w = 11, m = 1)
pw_sg <- cbind.data.frame(Sample = pw_data$Sample, sgvec)

pw_mw <- as.data.frame(pw_sg)
pw_mw$Sample = as.numeric(as.factor(pw_sg$Sample))

# PCA
pw_pca <- prcomp(pw_mw[,-1], scale = FALSE)

# Visualizing PCA results
pw_pca
summary(pw_pca)

# Visualizing eigenvalues (scree plot)
fviz_eig(pw_pca,
         xlab = "Principal Components (PCs)",
         ylab = "Explained Variance (%)",
         main = "",
         addlabels = TRUE,
         ggtheme = theme_minimal(),
         barcolor = "#404788FF",
         barfill = "#404788FF",
         linecolor = "#000000")

# Score plot for PC1 and PC2
scores_pca <- cbind.data.frame(predict(pw_pca),
                               waxes = pw_data$Sample)

m_labels <- as.matrix(pw_data[,-1])
rownames(m_labels) <- pw_data$Sample

# Score plot for PC1 and PC2
scatter_plot <- ggplot(scores_pca, aes(x = PC1, y = PC2, col = waxes, label = rownames(m_labels))) +
  geom_hline(yintercept = 0, lty = "dashed", alpha = 0.3) +
  geom_vline(xintercept = 0, lty = "dashed", alpha = 0.3) +
  geom_point(alpha = 0.1, size = 6, color = "black") +  
  geom_point(alpha = 1, size = 2, shape = 16) +      
  guides(color = guide_legend(title = "Hydroprocessing Grade")) + 
  labs(x = "PC1 (55.0%)", y = "PC2 (23.5%)", title = "") + 
  theme(axis.title = element_text(size = 12), 
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 12)) +
  theme_test() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis(discrete = TRUE)

scatter_plot

# Load plot
loadings <- cbind.data.frame(pw_pca$rotation[,c(1,2)])
setDT(loadings, keep.rownames = TRUE)[]

ld <- melt(loadings, "rn")

loadings_plot <- ggplot(ld, aes(x = rn, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme_test()+ 
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8, hjust = 1, angle = 90),
        axis.title = element_text(size = 8)) +
  labs(x = "Wavelength (nm)", y = "Loadings PCs", title = "") +
  scale_x_discrete(limits = loadings$rn,
                   breaks = loadings$rn[seq(1, length(loadings$rn), by = 100)]) +
  scale_fill_manual(values = c("#FDE725FF", "#440154FF")) +
  geom_hline(yintercept = c(0.05, -0.05), linetype = "dotted")
  
loadings_plot

# Combining plots
scatterloadings_plot <- ggarrange(scatter_plot, loadings_plot,
                                  ncol = 1,
                                  nrow = 2,
                                  labels = c("A", "B"))

scatterloadings_plot

# Stop Parallelization
stopCluster(cl)