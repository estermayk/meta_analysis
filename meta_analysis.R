#----packages----
install.packages("metafor",dependencies=TRUE,repos="https://cloud.r-project.org")
library(metafor)

#extracting and scaling first year of collection and length of collection
data <- read.csv('data/meta_analysis.csv')
data$first_collection <- as.numeric(sub("-.*", "", data$collection_period))
data$length_of_collection <- as.numeric(sub(".*-", "", data$collection_period)) - data$first_collection
data$first_collection_scaled <- scale(data$first_collection, center = TRUE, scale = TRUE)
data$length_of_collection_scaled <- scale(data$length_of_collection, center = TRUE, scale = TRUE)

#---- some playing with plots ----

#estimating SE
data$SE <- (data$upper95 - data$lower95) / (2 * 1.96)
data$location <- factor(data$location)
levels(data$location)
data$relativeby <- factor(data$relativeby)
levels(data$relativeby)
data$location <- relevel(data$location, ref = "plains")

#formatting for plotting
meta <- rma(yi = data$hedgesg, sei = data$SE, data = data, method = "REML")

summary(meta)

#plotting
funnel(meta)

plot(data$hedgesg, (1/data$SE), xlab = "Effect Size (Hedges' g)", ylab = "Precision (1/SE)")

forest(meta, slab = paste(data$paper, data$year), xlab = "Effect Size (Hedges' g)")

#---- a look at modelling ----

data$var <- data$SE^2

write.csv(data,"data/metafinal.csv", row.names = FALSE)

meta2 <- rma.mv(yi = hedgesg, 
                     V = var, 
                     random = ~ 1 | paper, 
                     data = data)
summary(meta2)

meta3 <- rma.mv(yi = hedgesg, 
                     V = var, 
                     mods = ~ relativeby + location + pca1, 
                     random = ~ 1 | species/paper, 
                     data = data)

summary(meta3)
print(meta3)

#---- plotting by species ----

data$label <- paste(data$paper, data$year, sep = ", ")

list(unique(data$legend))

colours <- c("seagreen3", "skyblue2", "thistle2", "olivedrab3", "plum3", "slategray3", 
            "pink2", "green4", "cyan3", "magenta2", "purple4")

colourpallet <- setNames(colours, c("Blarina brevicuada", "Eptesicus fuscus", "Microtus pennsylvanicus", "Peromyscus leucopus", "Clethrionomys gapperi", "Geomys bursarius", "Myotis lucifugus", "Scuirus carolinensis", "Sorex cinereus", "Tamiasciurus hudsonicus", "Procyon loctor"))



forest(meta3,cex.lab=0.8,cex.axis=0.8,addfit=TRUE,shade="zebra", order = 'obs', slab = data$label, colout = colourpallet[data$legend])


layout(matrix(c(1, 2), nrow = 1), widths = c(3, 1))
forest(meta3,cex.lab=0.8,cex.axis=0.8,addfit=TRUE,shade="zebra", order = 'obs', slab = data$label, colout = row_colors)
par(mar = c(5, 4, 4, 4) + 1)
legend("right", legend = names(colourpallet), fill = colourpallet, cex = 0.7, inset = c(-0.43, 0), xpd = TRUE, bty = "n")
?forest

#---- plotting by ecoregion ----

ecoregion_coulors <- c("plains" = "lightgreen", "east" = "lightblue", 
                      "west" = "lightcoral", "desert" = "khaki")
row_colours <- ecoregion_colours[data$location]

forest(meta3,cex.lab=0.8,cex.axis=0.8,addfit=TRUE,shade="zebra", order = 'obs', slab = data$label, colout = row_colors)


#---- egger's test ----

regtest(meta, model = "rma")



