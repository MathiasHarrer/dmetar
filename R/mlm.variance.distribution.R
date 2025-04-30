#' Calculate I-squared values and variance distribution for multilevel meta-analysis models
#'
#' This function calculates values of \eqn{I^2} and the variance distribution for multilevel meta-analysis
#' models fitted with \code{\link[metafor]{rma.mv}}.
#'
#'
#' @usage mlm.variance.distribution(x)
#'
#' @param x An object of class \code{rma.mv}. Must be a multilevel model with two random effects (three-level meta-analysis model).
#'
#' @details This function estimates the distribution of variance in a three-level meta-analysis
#' model (fitted with the \code{\link[metafor]{rma.mv}} function). The share of variance attributable to
#' sampling error, within and between-cluster heterogeneity is calculated,
#' and an estimate of \eqn{I^2} (total and for Level 2 and Level 3) is provided. The function uses the formula by
#' Cheung (2014) to estimate the variance proportions attributable to each model component and to derive the \eqn{I^2} estimates.
#'
#'
#' @references
#'
#' Harrer, M., Cuijpers, P., Furukawa, T.A, & Ebert, D. D. (2019).
#' \emph{Doing Meta-Analysis in R: A Hands-on Guide}. DOI: 10.5281/zenodo.2551803. \href{https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/mlma.html}{Chapter 12}.
#'
#'Cheung, M. W. L. (2014). Modeling dependent effect sizes with three-level meta-analyses: a structural equation modeling approach. \emph{Psychological Methods, 19}(2), 211.
#'
#' @author Mathias Harrer & David Daniel Ebert
#'
#' @aliases var.comp
#'
#' @import ggplot2
#' @importFrom stats model.matrix
#'
#' @return Returns a data frame containing the results. A plot summarizing the variance distribution and \eqn{I^2} values can be generated using \code{plot}.
#'
#' @export mlm.variance.distribution
#' @export var.comp
#'
#' @examples
#' # Use dat.konstantopoulos2011 from the "metafor" package
#' library(metafor)
#'
#' # Build Multilevel Model (Three Levels)
#' m = rma.mv(yi, vi, random = ~ 1 | district/school, data=dat.konstantopoulos2011)
#'
#' # Calculate Variance Distribution
#' mlm.variance.distribution(m)
#'
#' # Use alias 'var.comp' and 'Chernobyl' data set
#' data("Chernobyl")
#' m2 = rma.mv(yi = z, V = var.z, data = Chernobyl, random = ~ 1 | author/es.id)
#' res = var.comp(m2)
#'
#' # Print results
#' res
#'
#' # Generate plot
#' plot(res)



mlm.variance.distribution = var.comp = function(x){
  
  m = x
  
  # Check class
  if (!(class(m)[1] %in% c("rma.mv", "rma"))){
    stop("x must be of class 'rma.mv'.")
  }
  
  # Check for four level model
  if (!(m$sigma2s %in% c(2, 3))){
    stop("The model you provided does not seem to be a three or four-level model. This function can only be used for three or four-level models.")
  }
  
  # Check for right specification (nested model)
  if (sum(grepl("/", as.character(m$random[[1]]))) < 1){
    stop("Model must contain nested random effects. Did you use the '~ 1 | cluster/effect-within-cluster' notation in 'random'? See ?metafor::rma.mv for more details.")
  }
  
  # Get variance diagonal and calculate total variance
  n = m$k.eff
  vector.inv.var = 1/(diag(m$V))
  sum.inv.var = sum(vector.inv.var)
  sum.sq.inv.var = (sum.inv.var)^2
  vector.inv.var.sq = 1/(diag(m$V)^2)
  sum.inv.var.sq = sum(vector.inv.var.sq)
  num = (n-1)*sum.inv.var
  den = sum.sq.inv.var - sum.inv.var.sq
  est.samp.var = num/den
  
  # Calculate variance proportions
  level1=((est.samp.var)/(sum(m$sigma2)+est.samp.var)*100)
  level2=((m$sigma2[length(m$sigma2)])/(sum(m$sigma2)+est.samp.var)*100)
  level3=((m$sigma2[length(m$sigma2)-1])/(sum(m$sigma2)+est.samp.var)*100)
  if (length(names(m$s.names))==3){
    level4=((m$sigma2[length(m$sigma2)-2])/(sum(m$sigma2)+est.samp.var)*100)}
  
  # Prepare df for return
  Level = paste("Level", seq(1,m$sigma2s+1))
  Variance = c(level1, level2, level3, if(length(m$sigma2) == 3) level4 else NULL)
  df.res=data.frame(Variance)
  colnames(df.res) = c("% of total variance")
  rownames(df.res) = Level
  I2 = c("---", round(Variance[2:length(Variance)], 2))
  df.res = as.data.frame(cbind(df.res, I2))
  
  totalI2 = sum(Variance[2:length(Variance)])
  
  # Generate plot
  df1 = data.frame("Level" = c("Sampling Error", "Total Heterogeneity"),
                   "Variance" = c(df.res[1,1], sum(df.res[2:length(Variance),1])),
                   "Type" = rep(1,2))
  
  df2 = data.frame("Level" = rownames(df.res),
                   "Variance" = df.res[,1],
                   "Type" = rep(2,length(Variance)))
  
  df = as.data.frame(rbind(df1, df2))
  
  
  if (length(Variance) == 4) {
    color_palette <- c("darkseagreen2", "pink1", "pink2", "pink3",
                       "darkseagreen3", "pink4")
  } else {
    color_palette <- c("darkseagreen2", "pink1", "pink2",
                       "darkseagreen3", "pink4")
  }
  
  g = ggplot(df, aes(fill=Level, y=Variance, x=as.factor(Type))) +
    coord_cartesian(ylim = c(0,1), clip = "off") +
    geom_bar(stat="identity", position="fill", width = 1, color="black") +
    scale_y_continuous(labels = scales::percent)+
    theme(axis.title.x=element_blank(),
          axis.text.y = element_text(color="black"),
          axis.line.y = element_blank(),
          axis.title.y=element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_line(lineend = "round"),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.background = element_rect(linetype="solid",
                                           colour ="black"),
          legend.title = element_blank(),
          legend.key.size = unit(0.75,"cm"),
          axis.ticks.length=unit(.25, "cm"),
          plot.margin = unit(c(1,3,1,1), "lines")) +
         scale_fill_manual(values = color_palette) +
    
    # Add Annotation
    
    # Total Variance
    annotate("text", x = 1.5, y = 1.05,
             label = paste("Total Variance:",
                           round(sum(m$sigma2)+est.samp.var, 3))) +
    
    # Sampling Error
    annotate("text", x = 1, y = (df[1,2]/2+df[2,2])/100,
             label = paste("Sampling Error Variance: \n", round(est.samp.var, 3)), size = 3) +
    
    # Total I2
    annotate("text", x = 1, y = ((df[2,2])/100)/2-0.02,
             label = bquote("Total"~italic(I)^2*":"~.(round(df[2,2],2))*"%"), size = 3) +
    annotate("text", x = 1, y = ((df[2,2])/100)/2+0.05,
             label = paste("Variance not attributable \n to sampling error: \n", round(sum(m$sigma2),3)), size = 3) +
    
    # Level 1
    annotate("text", x = 2, y = (df[1,2]/2+df[2,2])/100, label = paste("Level 1: \n",
                                                                       round(df$Variance[3],2), "%", sep=""), size = 3) +
    
    # Level 2
    annotate("text", x = 2, y = (if(length(Variance) == 4) df[6,2] else 0+df[5,2]+(df[4,2]/2))/100,
           label = bquote(italic(I)[Level2]^2*":"~.(round(df[4,2],2))*"%"), size = 3) +
    
    # Level 3
    annotate("text", x = 2, y = (if(length(Variance) == 4) df[6,2] else 0+(df[5,2]/2))/100,
             label = bquote(italic(I)[Level3]^2*":"~.(round(df[5,2],2))*"%"), size = 3) +
    
    # Level 4
    if(length(Variance) == 4) {
      annotate("text", x = 2, y = (df[6,2]/2)/100,
               label = bquote(italic(I)[Level4]^2*":"~.(round(df[6,2],2))*"%"), size = 3)
    }
  
  returnlist = list(results = df.res,
                    totalI2 = totalI2,
                    plot = g)
  class(returnlist) = c("mlm.variance.distribution", "list")
  
  invisible(returnlist)
  
  returnlist
  
}

