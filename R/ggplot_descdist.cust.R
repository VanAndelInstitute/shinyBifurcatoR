#' ggplot Empirical distribution as in \code{\link[fitdistrplus]{descdist}}
#'
#' Generate compact plot following the \code{\link[fitdistrplus]{descdist}} function as customizable ggplot.
#'
#' @author Issoufou Liman
#' @inheritParams fitdistrplus::descdist
#' @param title,subtitle Title and Subtitle
#' @param xlab,ylab These are respectively x and y labels.
#' @param obs_geom_size,boot_geom_size,dist_geom_pts_size The size of the geom_point to be used for the empirical distributoion (default to 4), bootstrapping (default to 0.02), theoritical distribution (default to 5), respectively.
#' @param dist_geom_line_size The size of the geom_line to be used for the empirical distributoion. The default is 0.6.
#' @param axis_text_size,axis_title_size,plot_title_size,plot_subtitle_size,strip_text_size,legend_text_size = 12,
#' Text size respectively corresponding to axis text (default to 12), axis title (default to 12), plot title (default to 20), subtitle (default to 17), strip text (default to 18), and legend (default to 12).
#' @seealso \code{\link[fitdistrplus]{descdist}}.
#' @details see \code{\link[fitdistrplus]{descdist}}.
#' @references
#' Marie Laure Delignette-Muller, Christophe Dutang (2015). fitdistrplus: An R Package for Fitting Distributions. Journal of Statistical Software, 64(4), 1-34. http://www.jstatsoft.org/v64/i04/.
#' @importFrom scales rescale_none
#' @import ggplot2 data.table decisionSupportExtra
#' @importFrom reshape2 melt
#' @export
ggplot_descdist.cust <- function(data){

  plot_lims <- lapply(unique(data$group), function(x){
    data.l = data$value[data$group == x]
    if(!is.data.frame(data.l)) data.l <- as.data.frame(data.l)
    lapply(data.l, function(i){
      ggbuild_descdist_data(i, boot = 50, graph = TRUE)
    })
  })

  #We need to grab the observed x, y for all groups and make into 1 DF
  observed = data.table::rbindlist(lapply(1:length(plot_lims), function(i) data.frame(Group = unique(data$group)[i],type="Observed", plot_lims[[i]][[1]]$data_list$observed_dist)))
  boots = data.table::rbindlist(lapply(1:length(plot_lims), function(i) data.frame(Group = unique(data$group)[i],type="Boot" , plot_lims[[i]][[1]]$data_list$boot_data)))
  points.df = rbind(observed,boots)

  norm = data.frame(dist = "Normal",plot_lims[[1]][[1]]$data_list$norm_dist)
  expo = data.frame(dist = "Exponential",plot_lims[[1]][[1]]$data_list$exp_dist)
  uni = data.frame(dist = "Uniform",plot_lims[[1]][[1]]$data_list$unif_dist)
  logistic = data.frame(dist = "Logisitc",plot_lims[[1]][[1]]$data_list$logistic_dist)
  shapes.df = rbind(norm,expo,uni,logistic)

  lnorm = data.frame(dist = "Log normal",plot_lims[[1]][[1]]$data_list$lnorm_dist)
  gamma = data.frame(dist = "Gamma",plot_lims[[1]][[1]]$data_list$gamma_dist)
  lines.df = rbind(lnorm,gamma)
  beta = plot_lims[[1]][[1]]$data_list$beta_dist

  xmax = ceiling(max(c(shapes.df$x,points.df$x)))
  ymax = ceiling(max(c(shapes.df$y,points.df$y)))

  lines.df = lines.df[lines.df$x <= xmax+1,]

  beta=beta[beta$x <= xmax+1,]

  p.desc =
    ggplot() + theme_classic(16) +
    geom_polygon(data = beta,
                 aes(x, y,alpha="beta")) +

    scale_y_continuous(breaks = seq(0,ymax,2), labels = rev(seq(0,ymax,2))) +

    geom_point(data=points.df,aes(x,y,size = type,color=Group)) +

    geom_point(data=shapes.df,aes(x=x,y=y,shape =dist ),size=3,stroke=1.5) +

    geom_line(data=lines.df,aes(x =x,y=y,linetype = dist)) +

    coord_cartesian(xlim = c(0,xmax),ylim=c(0,ymax)) +

    scale_color_manual(values=c("darkorchid4","springgreen3","darkblue","orange"))+
    scale_size_manual(values = c(0.5,5)) +
    scale_shape_manual(values = c( 7, 3,8, 2)) +
    guides(color = "none",
           alpha = guide_legend(title = NULL),
           linetype = guide_legend(title = NULL),
           size = guide_legend(title = NULL),
           shape = guide_legend(title = NULL)) +
    labs(title = "Cullen - Frey", caption= "Weibull is similar to log-normal and Gamma",
           x = "Square of Skewness",
           y = "Kurtosis")
  return(p.desc)
}
