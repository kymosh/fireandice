packages <- c('dplyr', 'vegan', 'terra')
lapply(packages, library, character.only = T)


# ------------------------------------------------------------------
# Principal Component Analysis
# ------------------------------------------------------------------

canopy.stack <- rast(file.path(dir, 'canopy_metrics_50m.tif'))
df <- as.data.frame(canopy.stack, cells = TRUE, na.rm = TRUE)
#write.csv(df, file.path(dir, 'canopy_metrics_50m_df.csv'))

pca <- rda(df[ , -1], scale = TRUE)
summary(pca)

scores.df <- scores(pca, display = 'sites')

# create empty raster and fill by cell index
pc1.r <- canopy.stack[[1]]
values(pc1.r) <- NA_real_
values(pc1.r)[df$cell] <- scores.df[ ,1]
plot(pc1.r, main = 'PC1: Structural Development')

pc2.r <- canopy.stack[[1]]
values(pc2.r) <- NA_real_
values(pc2.r)[df$cell] <- scores.df[, 2]

plot(pc2.r, main = 'PC2: Gap / Fragmentation')

# broken stick plot
screeplot(pca, bstick = TRUE)

# PC1 vs PC2 Scatter
set.seed(112)
idx <- sample(nrow(scores.df), 50000)

plot(scores.df[idx, 1],
     scores.df[idx, 2],
     pch = 16,
     cex = 0.3,
     col = rgb(0,0,0,0.2),
     xlab = "PC1: Structural Development",
     ylab = "PC2: Gap / Fragmentation")

biplot(pca, scaling = 2)

ordiplot(pca, type = "none", scaling = 2)
text(pca, display = "species", scaling = 2, cex = 0.7)

# plot zq and zpcum
zq.vars     <- grep("zq", names(df), value = TRUE)
zpcum.vars  <- grep("zpcum", names(df), value = TRUE)

df.vert <- df[, c(zq.vars, zpcum.vars)]
pca.vert <- rda(df.vert, scale = TRUE)

scores.vert <- scores(pca.vert, display = "sites")

set.seed(1)
idx <- sample(nrow(scores.vert), 50000)

plot(scores.vert[idx,1],
     scores.vert[idx,2],
     pch = 16,
     cex = 0.3,
     col = rgb(0,0,0,0.2),
     xlab = "PC1 (Vertical)",
     ylab = "PC2 (Vertical)")
