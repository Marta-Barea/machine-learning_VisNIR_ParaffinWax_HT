# Load Required Libraries
library(doParallel)
library(readxl)
library(prospectr)
library(cluster)
library(purrr)
library(factoextra)

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

pw_mw <- as.matrix(pw_sg[,-1])
rownames(pw_mw) <- pw_sg$Sample

# Linkage methods to assess
m <- c("average", "single", "complete", "ward")
names(m) <- c("average", "single", "complete", "ward")

# Compute coefficient
ac <- function(x) {
  agnes(pw_mw, method = x)$ac
}

# Print method and coefficient
map_dbl(m, ac)      

# Dissimilarity matrix
d <- dist(pw_mw, method = "euclidean")

# Hierarchical clustering 
hc1 <- hclust(d, method = "ward.D2")

# Generate colors from labels 
labels_cols_generator <- function(labels, order, colors = NULL) {
  result <- c()
  color_equivalence <- list()
  generator <- 1
  
  labels_sorted <- c()
  
  for (index in order) {
    labels_sorted <- c(labels_sorted, labels[[index]])
  }
  
  for (label in labels_sorted) {
    if (is.null(color_equivalence[[label]])) {
      if (is.null(colors)) {
        color_equivalence[[label]] <- generator
      } else {
        color_equivalence[[label]] <- colors[[generator]]
      }
      generator = generator + 1
    }
    result <- c(result, color_equivalence[[label]])
  }
  
  result
}

labels_cols = labels_cols_generator(hc1$labels,
                                           hc1$order, 
                                           c("#FDE725FF","#287D8EFF","#440154FF","#73D055FF","#404788FF"))


# Dendrogram
set.seed(5665)

dendrogram <- fviz_dend(x = hc1, 
                        show_labels = TRUE, 
                        cex = 0.7,
                        lwd = 0.5,
                        main = "",
                        xlab = "Samples",
                        ylab = "Dendogram using Ward's linkage and Euclidean distance",
                        sub = "",
                        ggtheme = theme_classic(),
                        horiz = FALSE,
                        k_colors = c("black"),
                        label_cols = labels_cols,
                        color_labels_by_k = TRUE,
                        type = "rectangle")

dendrogram

# Stop Parallelization
stopCluster(cl)