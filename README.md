# **Vis-NIRS Wax Analysis Scripts**

This repository contains a suite of R scripts and a Shiny web application for analyzing wax samples using Visible-Near Infrared (Vis-NIR) spectroscopy. These tools include preprocessing, classification, clustering, and dimensionality reduction methods tailored to study the effects of hydroprocessing on waxes.

---

## 🔍 **Overview of Scripts**

1. **Spectra_VisNIRS_Waxes_Type.R**
   - Prepares Vis-NIR spectral data for analysis using smoothing, normalization, and scatter correction.

2. **Gaussian_SVM_VisNIRS_Waxes_HT.R**
   - Implements Gaussian Support Vector Machine (SVM) models to classify hydroprocessing grades.

3. **RF_VisNIRS_Waxes_HT.R**
   - Applies Random Forest classification to determine wax hydroprocessing levels.

4. **HCA_VisNIRS_Waxes_HIFI.R**
   - Performs Hierarchical Cluster Analysis (HCA) with dendrogram visualizations to group wax samples.

5. **PCA_VisNIRS_Waxes_HIFI.R**
   - Conducts Principal Component Analysis (PCA) and visualizes eigenvalues, scores, and loadings.

6. **app.R**
   - A Shiny application for uploading, preprocessing, visualizing, and classifying wax data.

---

## 🛠️ **System Requirements**

### Software
- **R version 4.4.0** (2024-04-24, "Puppy Cup")
- RStudio (optional but recommended)

### R Packages and Versions
| **Task**                          | **Package**         | **Version**   |
|------------------------------------|---------------------|---------------|
| Spectral preprocessing             | `prospectr`         | 0.2.7         |
| Clustering (HCA)                   | `stats`             | 4.4.0         |
| Dendrogram visualization           | `factoextra`        | 1.0.7         |
| PCA                                | `stats`             | 4.4.0         |
| PCA visualization                  | `factoextra`        | 1.0.7         |
| Machine learning (SVM, RF)         | `caret`             | 6.0-94        |
| Random Forest models               | `ranger`            | 0.17.0        |
| Data manipulation                  | `dplyr`, `data.table`, `stringr` | 1.1.4, 1.16.2, 1.5.1 |
| Radar charts                       | `ggplot2`, `ggiraphExtra` | 3.5.1, 0.3.0 |
| Visualization                      | `ggplot2`, `viridis`, `egg` | 3.5.1, 0.6.5, 0.4.5 |
| Web application                    | `shiny`             | 1.9.1         |
| Web themes                         | `shinythemes`       | 1.2.0         |

---

## 🚀 **How to Use**

### Running Scripts
1. Open the R script in RStudio.
2. Update file paths and parameters as needed.
3. Run the script to analyze wax data.

### Running the Shiny Application
1. Place `app.R`, `weighted_rf.rds`, and `test_data.xlsx` in the same folder.
2. In your R console, run: 
   
   <pre markdown="1"> ```R shiny::runApp("app.R") ``` </pre>

3. Use the web interface to:
- 📁 **Upload** `.csv` or `.xlsx` data files.
- 🛠️ **Preprocess** data using advanced filtering techniques.
- 📊 **Visualize** spectra and PCA results.
- 🤖 **Predict** hydroprocessing grades with AI.

---

### 📂 **Example Dataset**
A sample dataset (`test_data.xlsx`) is included for demonstration purposes. It contains Vis-NIR spectral readings and hydroprocessing grades for various wax samples.

---

### ✨ **Features**
- **Preprocessing**: Savitzky–Golay smoothing and scatter correction.
- **Clustering**: HCA with dendrogram visualization.
- **Dimensionality Reduction**: PCA with eigenvalues, score, and loading plots.
- **Machine Learning**: SVM and Random Forest models for hydroprocessing classification.
- **Web Application**: Intuitive Shiny interface for non-technical users.

---

### 🤝 **Contributors**
- **Nebux Cloud, S.L.**
  - Experts in AI-driven data analysis solutions.
- **University of Cádiz (AGR-291 Research Group)**
  - Specializing in hydrocarbon characterization and spectroscopy.

---

### 📜 **License**
This project is licensed under the GNU GENERAL PUBLIC License. See `LICENSE` for details.
