#' uses mixR to plot most likely mixture fit based on provided family
#'
#' @description A utility function for plotting descriptive data.
#'
#' @param x vector of data to be passed to mixR
#' @param family one of normal, lnorm, weibull, gamma
#'
#' @noRd
#'
#' @import ggplot2 ggtext mixR
#'
ggplot_mixfit_desc <- function(x,family="normal",color){
  x = as.numeric(unlist(x))
  try.mix = try(mixR::select(x=x,ncomp = c(1,2),family=family),silent=T)
  if(class(try.mix) != "try-error"){
    #The selectEM object is quirky and doesn't like to save as a local var in a function
    var.eq = mixR::select(x=x,ncomp = c(1,2),family=family)$equal.var[mixR::select(x=x,ncomp = c(1,2),family=family)$best=="*"]
    params =  mixR::mixfit(x=x,ncomp = mixR::select(x=x,ncomp = c(1,2),family=family)$ncomp[mixR::select(x=x,ncomp = c(1,2),family=family)$best=="*"] ,family = family,ev = I(var.eq=="Y"))
    plot.df =  plot(params,
                    add_hist = F,
                    add_legend = F,trans=1) +
      theme_classic(11) +theme(legend.position = "none")

    fam = ifelse(family == "lnorm","log normal",family)

    if( mixR::select(x=x,ncomp = c(1,2),family=family)$ncomp[mixR::select(x=x,ncomp = c(1,2),family=family)$best=="*"] == 1){
      plot.df = plot.df  +
        scale_fill_manual(values = color)+
        labs(title=fam) +
        annotate(geom="text",x = params$mu,y = 1.25*layer_scales(plot.df)$y$range$range[2],label = paste0(round(params$mu,2), " (",round(params$sd,2),")" ),color=color)+
        theme(legend.position="none",plot.title = element_markdown(size=8))

    } else {
      plot.df = plot.df  +
        scale_fill_manual(values = colorRampPalette(c("white",color))(3)[2:3]) +
        theme_classic(11) +
        labs(title=fam)+
        annotate(geom="text",x = params$mu[1],y = 1.25*layer_scales(plot.df)$y$range$range[2]
,label = paste0(round(params$mu[1],2), " (",round(params$sd[1],2),")" ),hjust=.8,color=colorRampPalette(c("white",color))(3)[2])+
        annotate(geom="text",x = params$mu[2],y = 1.25*layer_scales(plot.df)$y$range$range[2],label = paste0(round(params$mu[2],2), " (",round(params$sd[2],2),")" ),hjust=0.2,color=colorRampPalette(c("white",color))(3)[3])+
       theme(legend.position="none",plot.title = element_markdown(size=8))
    }

  } else {
    plot.df = ggplot() + ggtitle(paste0(family ,": Unable to fit"))+coord_cartesian(clip="off")
  }
  print(plot.df)

}
