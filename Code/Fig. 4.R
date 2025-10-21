# R script: PCA + count modeling for publications of drones & robots in biodiversity monitoring
# - Reads an input spreadsheet (replace path & sheet names as needed)
# - Performs preprocessing and imputation (mice)
# - Conducts PCA on scaled predictors (prcomp / factoextra)
# - Selects PCs (scree / broken-stick) and fits Poisson / NegBin models using PCs
# - Produces diagnostic plots and biplot for interpretation
#
# Required packages:
# install.packages(c("readxl","dplyr","tidyr","mice","factoextra","ggplot2",
#                    "MASS","car","broom","performance"))
#
# Author: ChatGPT (script adapted for your MEE revision project)
# Date: 2025-10-15

library(readxl)
library(dplyr)
library(tidyr)
library(mice)
library(factoextra)
library(ggplot2)
library(MASS)        # glm.nb
library(car)         # vif
library(broom)
library(performance) # check_overdispersion
library(here)

setwd(here::here())
# --------------------------
# 1) Load data
# --------------------------


dat_path <- "Data/Popularity heteriogeneity analysis.xlsx"
sheet_name <- "Sheet1"  # change if different

raw <- read_excel(dat_path, sheet = sheet_name)

# Quick look
glimpse(raw)
# Make lower-case column names (optional)
raw <- raw %>% rename_with(~ tolower(gsub("\\s+","_",.)))

# --------------------------
# 2) Create / select predictor matrix
# --------------------------
# Choose predictors relevant to explaining popularity 
df <- raw %>%
  mutate(
    country=country,
    n_pubs = as.integer(number_of_publications),
    gdp_total = as.numeric(`gdp_(billion_us_$)`),
    gdp_percap = as.numeric(`gdp_per_capita_(us_$_)`),
    population = as.numeric(`population_(thousands)`),
    topo_var = as.numeric(raw$vrm_mean),    # e.g. VRM, std elevation
    terra_protected_pct = as.numeric(most_recent_value_terrestrial_pa_percentage),
    marine_protected_pct = as.numeric(most_recent_value_marine_pa_percentage),
    r_d_expenditure = as.numeric(raw$`most_recent_value_r&d_expenditure`),
    n_univ = as.numeric(number_of_institutions),
  ) %>%
  dplyr::select(country, n_pubs, 
         gdp_total, gdp_percap, population, topo_var,
         terra_protected_pct, marine_protected_pct,
         r_d_expenditure, n_univ)

# --------------------------
# 3) Inspect missingness & distributions
# --------------------------
summary(df)
sapply(df, function(x) sum(is.na(x)))

# Visualize distributions quickly (optional)
num_cols <- c("n_pubs", "gdp_total","gdp_percap","population","topo_var",
              "terra_protected_pct","marine_protected_pct","r_d_expenditure","n_univ")
# basic histograms
par(mfrow=c(3,3))
for (cname in num_cols) {
  hist(df[[cname]], main=cname, xlab="", breaks=30)
}

par(mfrow=c(1,1))

# --------------------------
# 4) Transformations
# --------------------------
# Count responses often have many zeros and are skewed. Predictors like GDP, pop: log-transform.

df <- df %>%
  mutate(
    log_gdp_total   = log1p(gdp_total),
    log_gdp_percap  = log1p(gdp_percap),
    log_population  = log1p(population),
    log_topo_var    = log1p(topo_var),
    log_n_univ      = log1p(n_univ),
    log_r_d_exp     = log1p(r_d_expenditure),
    log_terra_pa    = log1p(terra_protected_pct),
    log_marine_pa   = log1p(marine_protected_pct)
  )
md.pattern(df) # show missingness pattern

# Preferred predictor list for PCA (adjust if you want to include/exclude):
# preds vector as you defined earlier:
preds <- c(
  "log_gdp_total", "log_gdp_percap", "log_population",
  "log_topo_var", "log_terra_pa", "log_marine_pa",
  "log_r_d_exp", "log_n_univ"
)

# QUICK CHECK: which countries have missing predictor values?
na_by_country <- df %>%
  dplyr::select(country, all_of(preds)) %>%
  mutate(missing_any = if_any(all_of(preds), is.na)) %>%
  filter(missing_any)

if (nrow(na_by_country) > 0) {
  message("Countries with any missing predictor (inspect):")
  print(na_by_country$country)
} else {
  message("No missing predictors found.")
}


# --------------------------
# 5) Impute missing predictor values with mice (multivariate imputation)
# --------------------------
# Keep a dataset for PCA. We will impute only predictors, not the response columns.

library(mice)
imp <- mice(df[preds], m = 5, method = "pmm", seed = 2025)
# Diagnostics: check convergence plots if desired
plot(imp) 

# Complete dataset: use pooled/complete dataset (here we use the first imputed dataset)
complete_preds <- complete(imp, action = 1)

# Merge back the imputed predictors into df
df_imputed <- df %>%
  dplyr::select(country, n_pubs) %>%
  bind_cols(complete_preds)

df_imputed <- df_imputed %>% filter(country != "Antarctica")
# --------------------------
# 6) PCA (on scaled predictors)
# --------------------------
# Remove rows with NA in response if you want to model only countries with counts
df_pca <- df_imputed %>% drop_na(n_pubs)
rownames(df_pca) <- df_pca$country
# Standardize predictors and run PCA
pred_matrix <- scale(df_pca %>% dplyr::select(all_of(preds)))
colnames(pred_matrix)<-c("GDP (total)", "GDP (percap)", "Population", "VRM",   "Terrestrail PA", "Marine PA", "R & D expenditure","#University")

pca_res <- prcomp(pred_matrix, center = TRUE, scale. = TRUE)

library(factoextra)

# PCA summary
summary(pca_res)   # variance explained
fviz_eig(pca_res, addlabels = TRUE, ylim = c(0, 60)) # scree plot

# Broken-stick criterion (helper)
# factoextra provides function:
fviz_nbclust(pred_matrix, FUN = function(x, k) { NA }, method = "wss") # just to load; not used
# We'll rely on scree + cumulative variance; show cumulative var:
var_explained <- summary(pca_res)$importance[2,]
cumvar <- summary(pca_res)$importance[3,]
plot(cumvar, type='b', xlab='PC', ylab='Cumulative variance explained')
abline(h = 0.7, lty=2, col="grey") # 70% rule line (optional)

# Examine loadings for first few PCs
loadings <- pca_res$rotation
round(loadings[, 1:4], 3)

# Create a biplot (factoextra)

tiff("Result/Geographic_Distribution_DR_PCA.tiff", unit="in", width=7, height=5, res=400, pointsize=10)
library(ggrepel)
# --- Defensive checks & prepare coords -------------------------------------
# Make sure df_pca (the dataframe you used for PCA) exists and was the source
# when you ran: pca_res <- prcomp(pred_matrix, ...)
if (!exists("df_pca")) stop("df_pca not found: make sure you created df_pca from the same data used for PCA")

# Build coords from PCA scores (PC1/PC2) and attach country & n_pubs
coords <- as.data.frame(pca_res$x[, 1:2])
colnames(coords)[1:2] <- c("PC1","PC2")   # ensure names
# attach country & n_pubs from df_pca - must align in row order
coords$country <- df_pca$country
coords$n_pubs  <- as.numeric(df_pca$n_pubs)   # ensure numeric

# Create the log variable (safe: handles NA and non-numeric)
coords$log_pubs <- ifelse(is.na(coords$n_pubs), NA, log1p(coords$n_pubs))

# Quick diagnostics (print to console)
message("---- diagnostics ----")
print("coords column names:")
print(names(coords))
message("summary of n_pubs:")
print(summary(coords$n_pubs))
message("summary of log_pubs:")
print(summary(coords$log_pubs))
message("number of rows in coords:")
print(nrow(coords))
message("number of NA in log_pubs:")
print(sum(is.na(coords$log_pubs)))
message("---------------------")

# If log_pubs has zero length (safeguard), force creation
if (!"log_pubs" %in% names(coords) || length(coords$log_pubs) == 0) {
  coords$log_pubs <- log1p(as.numeric(coords$n_pubs))
  warning("Re-created coords$log_pubs from coords$n_pubs")
}

# Check PC columns present and numeric
if (!all(c("PC1","PC2") %in% names(coords))) stop("PC1/PC2 missing in coords")
if (!is.numeric(coords$PC1) || !is.numeric(coords$PC2)) stop("PC1/PC2 are not numeric")

# --- Prepare variable arrows from PCA loadings ------------------------------
vars <- as.data.frame(pca_res$rotation[, 1:2])
vars$var <- rownames(vars)

# scale arrow length (tune arrow_mul if arrows too short/long)
arrow_mul <- max(abs(coords$PC1), abs(coords$PC2)) * 0.6
vars$PC1 <- vars$PC1 * arrow_mul
vars$PC2 <- vars$PC2 * arrow_mul

# choose labels (top 5% by n_pubs)
lab_dat <- subset(coords, n_pubs >= quantile(coords$n_pubs, 0.95, na.rm = TRUE))

# --- Plot: filled points (shape=21) - no arbitrary limits -------------------
gg <- ggplot(coords, aes(x = PC1, y = PC2)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey80") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey80") +
  geom_point(aes(fill = log_pubs), shape = 21, size = 3, colour = "black", stroke = 0.2) +
  scale_fill_gradient(low = "white", high = "#2b8c8c", name = "log(pub+1)") +
  geom_segment(data = vars,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")), colour = "darkred") +
  geom_text_repel(data = lab_dat, aes(x = PC1, y = PC2, label = country),
                  size = 3, box.padding = 0.35, point.padding = 0.3, segment.color = "grey60") +
  geom_text_repel(data = vars, aes(x = PC1, y = PC2, label = var),
            colour = "darkred", nudge_x = 0.05, size = 3) +
  theme_minimal() +
  labs(title = NULL, x = "PC1 (39.7%)", y = "PC2 (22.1%)") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),   # remove major gridlines
    panel.grid.minor = element_blank(),   # remove minor gridlines
    panel.border = element_rect(          # add rectangular border
      colour = "black", fill = NA, linewidth = 0.6
    )
  )

print(gg)

dev.off()



# Extract PC scores and bind to dataframe
scores <- as.data.frame(pca_res$x)
df_model <- bind_cols(df_pca, scores)

# --------------------------
# 7) Choose PCs to use in models
# --------------------------
# Typical approaches:
#  - Keep PCs that together explain ~60-80% variance
#  - Or keep PCs with eigenvalues > 1 (Kaiser) if using correlation matrix (prcomp on scaled makes this equivalent)
# Let's inspect:
eigvals <- (pca_res$sdev)^2
eig_df <- data.frame(PC = paste0("PC", 1:length(eigvals)),
                     Eigen = eigvals,
                     Variance = var_explained,
                     Cumulative = cumvar)
print(eig_df)

# Example: choose PC1:PC3 (adjust if needed)
pcs_to_use <- c("PC1","PC2","PC3")

# --------------------------
# 8) Fit count models: Poisson then check overdispersion -> Negative Binomial if needed
# We'll create separate models for drones and for robots
# --------------------------

fit_count_model_safe <- function(response, df, pcs, offset_col = NULL, disp_thresh = 1.3) {
  # build formula string
  form_str <- paste(response, "~", paste(pcs, collapse = " + "))
  if (!is.null(offset_col)) {
    # ensure offset column exists
    if (!offset_col %in% names(df)) stop("offset_col not found in df")
    form_str <- paste(form_str, "+ offset(log(", offset_col, "))")
  }
  form <- as.formula(form_str)
  
  # fit Poisson
  mod_pois <- glm(formula = form, data = df, family = poisson(link = "log"))
  
  # 1) Try to get dispersion from performance::check_overdispersion safely
  od_test <- NULL
  disp <- NA_real_
  if (requireNamespace("performance", quietly = TRUE)) {
    od_test_try <- try(performance::check_overdispersion(mod_pois), silent = TRUE)
    if (!inherits(od_test_try, "try-error") && !is.null(od_test_try)) {
      od_test <- od_test_try
      # try multiple possible element names
      if (!is.null(od_test_try$statistic)) disp <- od_test_try$statistic
      else if (!is.null(od_test_try$dispersion_ratio)) disp <- od_test_try$dispersion_ratio
      else if (!is.null(od_test_try$dispersion)) disp <- od_test_try$dispersion
    }
  }
  
  # 2) Fallback: compute Pearson dispersion manually if we don't have a number
  if (is.na(disp)) {
    # Pearson chi-square dispersion: sum(pearson_resid^2) / df.residual
    pearson_chisq <- sum(residuals(mod_pois, type = "pearson")^2, na.rm = TRUE)
    df_resid <- df.residual(mod_pois)
    if (df_resid > 0) {
      disp <- pearson_chisq / df_resid
      od_test <- list(fallback = TRUE,
                      pearson_chisq = pearson_chisq,
                      df_resid = df_resid,
                      dispersion_ratio = disp)
    } else {
      stop("Model has non-positive residual df; cannot compute dispersion.")
    }
  }
  
  # print what we found
  message(sprintf("Dispersion ratio = %.3f (threshold = %.2f)", disp, disp_thresh))
  if (!is.null(od_test) && !is.null(od_test$p.value)) {
    message("Overdispersion test p-value: ", od_test$p.value)
  } else if (!is.null(od_test$pearson_chisq)) {
    message("Pearson Chi-square = ", od_test$pearson_chisq,
            ", df = ", od_test$df_resid)
  }
  
  # decide if NB is needed
  if (!is.na(disp) && disp > disp_thresh) {
    message("Overdispersion detected -> fitting Negative Binomial (glm.nb).")
    mod_nb_try <- try(glm.nb(formula = as.formula(paste(response, "~", paste(pcs, collapse = " + "))),
                             data = df), silent = TRUE)
    if (inherits(mod_nb_try, "try-error")) {
      warning("glm.nb failed; returning Poisson model and dispersion info.")
      return(list(pois = mod_pois, nb = NULL, disp = disp, od_test = od_test))
    } else {
      return(list(pois = mod_pois, nb = mod_nb_try, disp = disp, od_test = od_test))
    }
  } else {
    message("No substantial overdispersion detected -> keeping Poisson.")
    return(list(pois = mod_pois, nb = NULL, disp = disp, od_test = od_test))
  }
}


res <- fit_count_model_safe(response, df, pcs)


# --------------------------
# 9) Summarise & interpret
# --------------------------
# Look at model summary
summary(res$nb)

# View exponentiated coefficients (interpretable as multiplicative effects)
exp(coef(res$nb))

# or tidy version with confidence intervals
library(broom)
tidy(res$nb, conf.int = TRUE, exponentiate = TRUE)
# --------------------------
# 10) Visualize relationships: predicted vs observed (example for drones)
# --------------------------
df_model <- df_model %>%
  mutate(pred= predict(res$nb, type = "response"))

ggplot(df_model, aes(x = pred, y = n_pubs)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  labs(x = "Predicted pubs (drones)", y = "Observed pubs (drones)",
       title = "Predicted vs observed: drone publications") +
  theme_minimal()

# Biplot with variables displayed for interpretation (again)
fviz_pca_var(pca_res, col.var = "contrib") +
  ggtitle("Variable contributions to PCs (higher = stronger)")

# --------------------------


# End of script








#EXTRA CODE
#========================================================================================
#extract country-based mean vrm from vrm_50KMmn_GMTEDmd.tif
#========================================================================================
# install if needed
packages <- c("terra", "sf", "rnaturalearth", "dplyr")
lapply(packages, require, character.only = TRUE)

# Replace path with your actual file location
vrm <- terra::rast("Data/Data_explanary variables for popularity/Raw data/vrm_50KMmn_GMTEDmd.tif")

# Inspect metadata
vrm
plot(vrm, main = "Vector Ruggedness Measure (50 km mean)")

# Natural Earth countries (scale = "medium" is fine)
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# (Optional) ensure same CRS
world <- st_transform(world, crs(vrm))

# Use terra::extract with na.rm = TRUE to ignore ocean cells
vrm_by_country <- terra::extract(vrm, world, fun = mean, na.rm = TRUE)


# The first column from extract() is ID matching the row order in 'world'
head(vrm_by_country)

world$VRM_mean <- vrm_by_country[,2]   # assign mean values
vrm_country_df <- world %>%
  st_drop_geometry() %>%
  dplyr::select(iso_a3, name, VRM_mean)

head(vrm_country_df)
write.csv(vrm_country_df, "Data/Data_explanary variables for popularity/vrm_by_country.csv")

#===============================================================================
#extract country-based number of institution/university from--
#--ScimagoIR 2025 - Overall Rank.csv
#===============================================================================
# Load packages (install if needed)
if(!require(readr)) install.packages("readr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(countrycode)) install.packages("countrycode")

library(readr)
library(dplyr)
library(countrycode)

# ---- 1. Read the SCImago CSV file ----
# Replace the path below with your actual file name
scimago <- read_delim("Data/Data_explanary variables for popularity/Raw data/ScimagoIR 2025 - Overall Rank.csv", 
                      delim = ";", show_col_types = FALSE)
# ---- 2. Convert ISO3 country codes (e.g., CHN, USA) to full country names ----
scimago <- scimago %>%
  mutate(
    Country_full = countrycode(Country, origin = "iso3c", destination = "country.name"),
    # handle unmatched codes manually
    Country_full = case_when(
      Country == "MUL" ~ "Multinational",
      Country == "XKX" ~ "Kosovo",
      is.na(Country_full) ~ Country,   # fallback: keep original code if still NA
      TRUE ~ Country_full
    )
  )


# ---- 3. Collapse the data to count institutions per country ----
country_counts <- scimago %>%
  group_by(Country_full) %>%
  summarise(Number_of_institutions = n()) %>%
  arrange(desc(Number_of_institutions))

# ---- 4. Save the result as a clean CSV ----
write_csv(country_counts, "Data/Data_explanary variables for popularity/scimago_institutions_by_country.csv")

# ---- 5. Print first few lines to console ----
print(head(country_counts, 10))
# ==============================================================================


