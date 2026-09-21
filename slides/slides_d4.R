texts <- c(doc1 = "We're keeping 5 of the the computers?",
           doc2 = "Wow! He's got 3 laptops...",
           doc3 = "Computers and laptops are both computers.")

pacman::p_load(textclean, quanteda, install = TRUE)

texts <- replace_contraction(texts)
toks <- tokens(texts, remove_numbers = TRUE, remove_punct = TRUE) |>
  tokens_tolower()

toks_stem <- tokens_wordstem(toks, language = "en")

dtm <- dfm(toks_stem)

dtm_filtered <- dfm_trim(dtm, min_termfreq = 2)

as.matrix(dtm_filtered)

dtm_tfidf <- dfm_tfidf(dtm_filtered)

as.matrix(dtm_tfidf)


pacman::p_load(tidyverse, text2map, 
  text2map.corpora, quanteda,
  install = TRUE)

df <- load_corpus("corpus_annual_review") |>
    distinct(title, .keep_all = T)

corp <- corpus(df, text_field = "abstract", docid_field = "title")
toks <- tokens(corp, remove_punct = TRUE) |> 
  tokens_tolower() |> 
  tokens_ngrams(n = 1:2) 

dfm_obj <- dfm(toks)
dfm_obj <- dfm_remove(dfm_obj, pattern = stopwords("en"))
dfm_obj <- dfm_select(dfm_obj, min_nchar = 3)
dfm_obj <- dfm_trim(dfm_obj, min_termfreq = 2)

stops <- stopwords("en")
dfm_obj <- dfm_remove(dfm_obj, 
                      pattern = c(paste0("^", stops, "_"), paste0("_", stops, "$")), 
                      valuetype = "regex")

tfidf_dfm <- dfm_tfidf(dfm_obj)

mat <- as.matrix(tfidf_dfm)
mat <- mat[, apply(mat, 2, var) > 0]

pca_themes <- prcomp(mat, center = T, scale. = T)

plot(pca_themes, type = "l", main = "Scree Plot of Latent Dimensions")

eigenvalues <- pca_themes$sdev^2
variance_explained <- eigenvalues / sum(eigenvalues) * 100
variance_explained |> round(2)

pca_themes$rotation[, 1] %>% #switch out number for different dimensions
  sort(decreasing = T) %>% 
  head(10)

pacman::p_load(ggrepel, install = T)

doc_df <- as.data.frame(pca_themes$x[, c(1, 8)])
colnames(doc_df) <- c("PC1", "PC8")
doc_df$title <- rownames(doc_df)

word_df <- as.data.frame(pca_themes$rotation[, c(1, 8)])
colnames(word_df) <- c("PC1", "PC8")
word_df$word <- rownames(word_df)

threshold <- 0.08 
filtered_words <- word_df %>% 
  filter(abs(PC1) > threshold | abs(PC8) > threshold) %>%
  mutate(
    dim_type = case_when(
      abs(PC1) > threshold & abs(PC8) > threshold ~ "Both Dims",
      abs(PC1) > threshold ~ "PC1 Only",
      abs(PC8) > threshold ~ "PC8 Only"
    )
  )

scale_factor <- max(abs(doc_df$PC1)) / max(abs(filtered_words$PC1)) * 0.65

filtered_words <- filtered_words %>% 
  mutate(PC1_scaled = PC1 * scale_factor,
             PC8_scaled = PC8 * scale_factor)

plot <- ggplot() +
  geom_text(data = doc_df, 
            aes(x = PC1, y = PC8, label = title),
            color = "black", alpha = 0.5, size = 2.5) +
  geom_text_repel(data = filtered_words, 
                  aes(x = PC1_scaled, y = PC8_scaled, label = word, color = dim_type),
                  fontface = "bold", size = 3.5,
                  box.padding = 0.35, point.padding = 0.2,
                  max.overlaps = Inf)

plot + scale_color_manual(values = c(
    "PC1 Only"  = "#440154ff", 
    "PC8 Only"  = "#2a788eff", 
    "Both Dims" = "#7ad151ff"
  )) +
  labs(x = "Principal Component 1",
       y = "Principal Component 8",
       color = "High Loading On:") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    legend.position = "bottom"
  )
