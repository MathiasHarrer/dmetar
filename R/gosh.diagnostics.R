#gosh.diagnostics <- function(data, km.centers, eps = 0.15, min.pts = 20) {
#
#    data <- data
#    km.centers <- km.centers
#    eps <- eps
#    sav <- data
#    min.pts <- min.pts
#
#    message("This may take some time to process and for plots to load...")
#
#    library(metafor)
#    library(cluster)
#    library(HSAUR)
#    library(fpc)
#    library(factoextra)
#    library(dplyr)
#    library(magicfor)
#    library(cowplot)
#    library(reshape2)
#
#
#    # Create full dataset from gosh output
#    dat.full <- sav$res
#    dat.full <- cbind(dat.full, sav$incl)
#
#    # Create dataset for k-Means
#    dat.km <- data.frame(scale(dat.full$I2, center = TRUE, scale = TRUE), scale(dat.full$estimate, center = TRUE,
#        scale = TRUE))
#    colnames(dat.km) <- c("I2", "estimate")
#
#    # Create dataset for DBSCAN DBSCAN can become too computationally intensive for very large GOSH data.  For
#    # N_gosh > 10.000, N = 10.000 data points are therefore randomly sampled.
#
#    if (nrow(dat.full) < 10000) {
#        dat.db.full <- dat.full
#    } else {
#        dat.db.full <- dat.full[sample(1:nrow(dat.full), 10000), ]  #Sample 10.000 rows
#    }
#
#    dat.db <- data.frame(scale(dat.db.full$I2, center = TRUE, scale = TRUE), scale(dat.db.full$estimate, center = TRUE,
#        scale = TRUE))
#    colnames(dat.db) <- c("I2", "estimate")
#
#    # K-Means
#    km <- kmeans(dat.km, centers = km.centers)  # Kmeans
#    km.clusterplot <- fviz_cluster(km, data = dat.km, stand = FALSE, ellipse = FALSE, show.clust.cent = FALSE,
#        geom = "point", ggtheme = theme_minimal(), shape = 16, ylab = "Effect Size (z-score)", xlab = "I2 (z-score)",
#        main = "K-means Algorithm", pointsize = 0.5) + coord_flip()
#
#    # DBSCAN
#    db <- fpc::dbscan(dat.db, eps = eps, MinPts = min.pts)
#    db.clusterplot <- fviz_cluster(db, data = dat.db, stand = FALSE, ellipse = FALSE, show.clust.cent = FALSE,
#        shape = 16, geom = "point", ggtheme = theme_minimal(), ylab = "Effect Size (z-score)", xlab = "I2 (z-score)",
#        main = "DBSCAN Algorithm (black dots are outliers)") + coord_flip()
#
#
#    # Add to dfs
#    dat.km.full <- dat.full
#    dat.km.full$cluster <- km$cluster
#    dat.db.full$cluster <- db$cluster
#
#
#    ##################################################### Extract the Percentages############################ K-Means############################################
#
#    dat.km.full$cluster <- as.factor(dat.km.full$cluster)
#    n.levels.km <- nlevels(dat.km.full$cluster)
#
#    # Loop for the total n of studies
#
#    dat.km.full.total <- dat.km.full[, -c(1:6, ncol(dat.km.full))]
#    n.cluster.tots <- apply(dat.km.full.total, 2, sum)
#    n.cluster.tots <- data.frame(unlist(as.matrix(n.cluster.tots)))
#    colnames(n.cluster.tots) <- c("n.tots")
#
#
#    # Loop for the cluster-wise n of studies
#
#    n <- sapply(split(dat.km.full.total, dat.km.full$cluster), function(x) apply(x, 2, sum))
#
#
#    # Calculate Percentages
#
#    deltas <- as.data.frame(apply(n, 2, function(x) (x/n.cluster.tots$n.tots) - mean(x/n.cluster.tots$n.tots)))
#
#    # Generate the plot
#
#    Study <- rep(1:nrow(deltas), n.levels.km)
#    delta.df <- melt(deltas)
#    delta.df[, 3] <- Study
#    delta.df$variable <- as.factor(delta.df$variable)
#    colnames(delta.df) <- c("Cluster", "Delta_Percentage", "Study")
#
#    km.plot <- ggplot(data = delta.df, aes(x = Study, y = Delta_Percentage, group = Cluster)) + geom_line(aes(color = Cluster)) +
#        geom_point(aes(color = Cluster)) + scale_x_continuous(name = "Study", breaks = seq(0, nrow(deltas),
#        1)) + scale_y_continuous(name = "Delta Percentage") + theme(axis.text = element_text(size = 5)) +
#        ggtitle("Cluster imbalance (K-Means algorithm)") + geom_hline(yintercept = 0, linetype = "dashed")
#
#
#    ##################################################### Cook's Distance Plot############################ K-Means############################################
#
#    m.cd.km <- by(delta.df, as.factor(delta.df$Cluster), function(x) lm(Delta_Percentage ~ Study, data = x))
#    m.cd.km$`0` <- NULL
#    m.cd.km <- lapply(m.cd.km, cooks.distance)
#    m.cd.km.df <- data.frame(Cooks.Distance = matrix(unlist(m.cd.km)))
#    m.cd.km.df$Cluster <- as.factor(rep(1:(n.levels.km), each = nrow(deltas)))
#    m.cd.km.df$Study <- rep(1:nrow(deltas), times = (n.levels.km))
#    outlier.cd.km <- 3 * mean(m.cd.km.df$Cooks.Distance)
#
#    km.cd.plot <- ggplot(data = m.cd.km.df, aes(x = Study, y = Cooks.Distance, group = Cluster)) + geom_line(aes(color = Cluster)) +
#        geom_point(aes(color = Cluster)) + scale_x_continuous(name = "Study", breaks = seq(0, nrow(deltas),
#        1)) + scale_y_continuous(name = "Cook's Distance") + theme(axis.text = element_text(size = 5)) + ggtitle("Cluster imbalance (Cook's Distance)") +
#        geom_hline(yintercept = outlier.cd.km, linetype = "dashed") + geom_hline(yintercept = 0, linetype = "dashed")
#
#
#    ##################################################### Extract the Percentages############################ DBSCAN############################################
#
#    dat.db.full$cluster <- as.factor(dat.db.full$cluster)
#    n.levels.db <- nlevels(dat.db.full$cluster)
#
#    # Loop for the total n of studies
#
#    dat.db.full.total <- dat.db.full[, -c(1:6, ncol(dat.db.full))]
#
#    n.cluster.tots <- apply(dat.db.full.total, 2, sum)
#    n.cluster.tots <- data.frame(unlist(as.matrix(n.cluster.tots)))
#    colnames(n.cluster.tots) <- c("n.tots")
#
#
#    # Loop for the cluster-wise n of studies
#
#    n <- sapply(split(dat.db.full.total, dat.db.full$cluster), function(x) apply(x, 2, sum))
#
#
#    # Calculate Percentages
#
#    deltas <- as.data.frame(apply(n, 2, function(x) (x/n.cluster.tots$n.tots) - mean(x/n.cluster.tots$n.tots)))
#
#    # Generate the plot
#
#    Study <- rep(1:nrow(deltas), n.levels.db)
#    delta.df <- melt(deltas)
#    delta.df[, 3] <- Study
#    delta.df$variable <- as.factor(delta.df$variable)
#    colnames(delta.df) <- c("Cluster", "Delta_Percentage", "Study")
#    delta.df <- filter(delta.df, !Cluster == 0)  #Zero Class (Outliers are removed)
#
#    db.plot <- ggplot(data = delta.df, aes(x = Study, y = Delta_Percentage, group = Cluster)) + geom_line(aes(color = Cluster)) +
#        geom_point(aes(color = Cluster)) + scale_x_continuous(name = "Study", breaks = seq(0, nrow(deltas),
#        1)) + scale_y_continuous(name = "Delta Percentage") + theme(axis.text = element_text(size = 5)) +
#        ggtitle("Cluster imbalance (Density-Based Clustering)") + geom_hline(yintercept = 0, linetype = "dashed")
#
#
#    ##################################################### Cook's Distance Plot############################ DBSCAN############################################
#
#    m.cd.db <- by(delta.df, as.factor(delta.df$Cluster), function(x) lm(Delta_Percentage ~ Study, data = x))
#    m.cd.db$`0` <- NULL
#    m.cd.db <- lapply(m.cd.db, cooks.distance)
#    m.cd.db.df <- data.frame(Cooks.Distance = matrix(unlist(m.cd.db)))
#    m.cd.db.df$Cluster <- as.factor(rep(1:(n.levels.db - 1), each = nrow(deltas)))
#    m.cd.db.df$Study <- rep(1:nrow(deltas), times = (n.levels.db - 1))
#    outlier.cd.db <- 3 * mean(m.cd.db.df$Cooks.Distance)
#
#    db.cd.plot <- ggplot(data = m.cd.db.df, aes(x = Study, y = Cooks.Distance, group = Cluster)) + geom_line(aes(color = Cluster)) +
#        geom_point(aes(color = Cluster)) + scale_x_continuous(name = "Study", breaks = seq(0, nrow(deltas),
#        1)) + scale_y_continuous(name = "Cook's Distance") + theme(axis.text = element_text(size = 5)) + ggtitle("Cluster imbalance (Cook's Distance)") +
#        geom_hline(yintercept = outlier.cd.db, linetype = "dashed")
#
#    # Generate Output Plot
#
#    plot_grid(km.clusterplot, db.clusterplot, km.plot, db.plot, km.cd.plot, db.cd.plot, nrow = 3)
#
#}
#
