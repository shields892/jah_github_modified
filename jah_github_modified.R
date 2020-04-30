#General Data Manipulation and Analysis
windowsFonts(CenturySch=windowsFont(family="Century Schoolbook"))
windowsFonts(Times=windowsFont(family="Times New Roman"))
library(extrafont)
extrafont::loadfonts(device="win")
library(dplyr)
library(ggplot2)
library(ggthemes)
library(GGally)
library(readxl)
library(data.table)
library(tidyr)
library(broom)
library(tibble)
library(stringr)
library(cowplot)
library(ggrepel)
library(scales)
library(zoo)
library(gtable)
library(grid)
library(vegan)
library(lme4)
library(writexl)
library(truncnorm)
#library(gridExtra)
#For Mapping
#library(sp)
#library(rgdal)
#library(rgeos)
#library(maps)
#library(maptools)
#library(ggsn)
#library(ggmap)

#Custom Functions
{
  #ggplotcolors for vegan PCA
  ggplotcolors <- function(n, h=c(0, 360) +15){
    if ((diff(h)%%360) < 1) h[2] <- h[2] - 360/n
    hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 45)
  }
  #Extract PCA data and grouping for further analysis or publication-quality plots.
  pca_data <- function(data,cols,groups,scale=T,var_scaling=5){
    #Select data to be used in PCA
    if(is.numeric(cols)){index = cols}
    if(is.character(cols) & length(cols)==2){index = match(cols[1],names(data)) : match(cols[2],names(data))}
    if(is.character(cols) & length(cols)>2){index = match(cols,names(data))}
    
    aes_index <- match(groups,names(data))
    plot_data <- na.omit(data[,c(aes_index,index)])
    #Generate PCA with some creative subsetting of plot_data
    pca <- prcomp(plot_data[,(length(aes_index)+1):length(plot_data)],scale.=scale)
    #Percent variance explained by PCs
    pervar <- round(pca$sdev^2 / sum(pca$sdev^2) * 100,2)
    #Plot values and limits
    require(dplyr)
    vars <- as.data.frame(pca$rotation[,1:2]) %>% 
      rownames_to_column() %>%
      gather(pc,value,PC1,PC2) %>% 
      full_join(data.frame(pc = c("PC1","PC2"),sdev = pca$sdev[1:2]) %>% mutate(pc = as.character(pc)),by="pc") %>% 
      mutate(loading = value * sdev * var_scaling) %>% 
      select(rowname,pc,loading) %>% 
      spread(pc,loading)
    samples <- pca$x[,1:2]
    
    #sample grouping
    sample_groups = plot_data[,match(groups,names(plot_data))]
    sample_pc1_pc2 = cbind.data.frame(sample_groups,samples)
    
    output <- list(pervar=pervar,vars=vars,samples=sample_pc1_pc2)
    output
  }
  #'Clean' looking PCA plotting function.
  pca_plot <- function(data,cols,color=NA,shape=NA,label=NA,scale=T,var_scaling=5,text_size=8,legend_position="top",font_family="Times",
                       axis_alpha = 0.5, geom_type = "text",point_size = 4,repel_variables=F,repel_samples=F,point_outline=F){
    #Select data to be used in PCA
    if(is.numeric(cols)){index = cols}
    if(is.character(cols) & length(cols)==2){index = match(cols[1],names(data)) : match(cols[2],names(data))}
    if(is.character(cols) & length(cols)>2){index = match(cols,names(data))}
    
    aes_index <- match(na.omit(c(color,shape,label)),names(data))
    plot_data <- na.omit(data[,c(aes_index,index)])
    #Generate PCA with some creative subsetting of plot_data
    pca <- prcomp(plot_data[,(length(aes_index)+1):length(plot_data)],scale.=scale)
    #Percent variance explained by PCs
    pervar <- round(pca$sdev^2 / sum(pca$sdev^2) * 100,2)
    #Plot values and limits
    vars <- as.data.frame(pca$rotation[,1:2]) %>% 
      rownames_to_column() %>%
      gather(pc,value,PC1,PC2) %>% 
      full_join(data.frame(pc = c("PC1","PC2"),sdev = pca$sdev[1:2]) %>% mutate(pc = as.character(pc)),by="pc") %>% 
      mutate(loading = value * sdev * var_scaling) %>% 
      select(rowname,pc,loading) %>% 
      spread(pc,loading)
    
    samples <- as.data.frame(pca$x[,1:2])
    #coloring
    if(!is.na(color)){col_group = as.factor(as.data.frame(plot_data[,match(color,names(plot_data))])[,1])
    samples = cbind(samples,col_group)}
    #shaping
    if(!is.na(shape)){sha_group = as.factor(as.data.frame(plot_data[,match(shape,names(plot_data))])[,1])
    samples = cbind(samples,sha_group)}
    #labeling
    if(!is.na(label)){lab_group = as.factor(as.data.frame(plot_data[,match(label,names(plot_data))])[,1])
    samples = cbind(samples,lab_group)}
    
    ggplot(samples,aes(PC1,PC2)) +
      geom_hline(yintercept=0,alpha=axis_alpha) +
      geom_vline(xintercept=0,alpha=axis_alpha) +
      
      {if(!is.na(shape) & !is.na(color)) {geom_point(aes(shape = sha_group, color = col_group),size=point_size)}} +
      {if(!is.na(shape) &  is.na(color)) {geom_point(aes(shape = sha_group),size=point_size)}} +
      {if( is.na(shape) & !is.na(color)) {geom_point(aes(color = col_group),size=point_size)}} +
      {if( is.na(shape) &  is.na(color)) {geom_point(size=point_size)}} +
      
      {if(!is.na(label) & repel_samples == F){geom_text(data = samples,aes(label = lab_group),size = text_size*0.352777778,family=font_family)}} +
      {if(!is.na(label) & repel_samples == T){geom_text_repel(data = samples,aes(label = lab_group),size = text_size*0.352777778,family=font_family)}} +
      
      {if(point_outline == T & !is.na(shape)){go <- geom_point(data=samples,aes(PC1,PC2),size=point_size,color="black",shape=21)}} +
      {if(point_outline == T & is.na(shape)){go <- geom_point(data=samples,aes(PC1,PC2,shape=sha_group),size=point_size,color="black")}} +
      
      {if( repel_variables == T & geom_type == "text") {geom_text_repel(data=vars,aes(PC1,PC2,label=rowname),family=font_family,size=text_size*0.352777778)}} +
      {if( repel_variables == T & geom_type == "label"){geom_label_repel(data=vars,aes(PC1,PC2,label=rowname),family=font_family,size=text_size*0.352777778)}} +
      {if( repel_variables == F & geom_type == "text") {geom_text(data=vars,aes(PC1,PC2,label=rowname),family=font_family,size=text_size*0.352777778)}} +
      {if( repel_variables == F & geom_type == "label"){geom_label(data=vars,aes(PC1,PC2,label=rowname),family=font_family,size=text_size*0.352777778)}} +
      
      theme(legend.title = element_blank(),
            legend.position = legend_position,
            text = element_text(size=text_size),
            legend.text = element_text(size=text_size)) +
      labs(x = paste("PC1 [",pervar[1],"% ]"),
           y = paste("PC2 [",pervar[2],"% ]"))
  }
  #Round all numeric entries in a data frame.
  round_df <- function(df, digits) {
    nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
    df[,nums] <- round(df[,nums], digits = digits)
    (df)
  }
  #Generate 95% CI for slope and r2 using the balanced bootstrap approach
  lm.bal <- function(x,y,n=10000,method="ols"){
    # method can be set to ordinary least squares ("ols") or reduced major axis ("rma") and is set to ols by default
    slope.observed <- if(method=="ols"){(cov(x,y)/var(x))} else if(method=="rma"){(sign(cov(x,y))*sd(y)/sd(x))}
    int.observed <- mean(y)-slope.observed*mean(x)
    r2.observed <- cor(x,y)^2
    random <- cbind(rep(x,n-1),rep(y,n-1),sample(1:((n-1)*(length(x)))))
    random <- as.data.frame(cbind(random[order(random[,3]),][,1:2],sort(rep(1:(n-1),length(x)))))
    colnames(random) <- c("x","y","rep")
    if(method=="ols"){dist <- random %>% group_by(rep) %>% summarize(slope=cov(x,y)/var(x),intercept=mean(y)-slope*mean(x),r2=cor(x,y)^2) %>% .[,-1]}
    if(method=="rma"){dist <- random %>% group_by(rep) %>% summarize(slope=sign(cov(x,y))*sd(y)/sd(x),intercept=mean(y)-slope*mean(x),r2=cor(x,y)^2) %>% .[,-1]}
    dist <- rbind(dist,c(slope.observed,int.observed,r2.observed))
    # dist[n,] <- c(slope.observed,int.observed,r2.observed) # Kept getting an error about the incorrect number of rows being assigned so I switched it out with the line above
    slope.95ci <- c(sort(dist$slope)[(n*0.025)],sort(dist$slope)[(n*0.975)])
    sig.slope <- ifelse((sign(slope.95ci[1])==sign(slope.95ci[2]))==T,"< 0.05","> 0.05")
    int.95ci <- c(sort(dist$intercept)[(n*0.025)],sort(dist$intercept)[(n*0.975)])
    r2.95ci <- c(sort(dist$r2)[(n*0.025)],sort(dist$r2)[(n*0.975)])
    #Output
    list("dist" = dist, #bootstrapped distribution
         "slope.observed" = slope.observed, #observed slope (based on chosen method)
         "slope.sig" = sig.slope, #Test for overlap of 95% with 0; significance implies non-overlap
         "slope.95ci" = slope.95ci, #95% confidence intervals for the slope
         "int.observed" = int.observed, #observed intercept
         "int.95ci" = int.95ci,  #95% confidence intervals for the intercept
         "r2.observed" = r2.observed, #observed pearson's r^2
         "r2.95ci" = r2.95ci #95% Confidence Intervals for R2
    )
  }
  #Use lm.bal and print the results into a ggplot object
  lm_eqn = function(x,y,method="ols",location="tl",n=10000,xs=1,ys=1,
                    m=T,m.95=T,int=T,int.95=T,r2=T,r2.95=F,p=T,
                    family="Times",size=7,color="black",xman=NA,yman=NA,ylog=F,
                    coef.digits=3){
    # x = x-axis data set, a vector
    # y = y-axis data set, a vector
    # method = regression method, either "ola" or "rma" for ordinary least squares or reduced major axis, respectively
    # location = positioning options, tl, tr, bl, br - i.e., top-left, top-right, bottom-left, bottom-right
    # xs = x-axis position scalar, defaults to 1 and is proportional to the plotting area
    # ys = y-axis position scalar, defaults to 1 and is proportional to the plotting area
    # m, m.95, r2, r2.95, p = T/F values for presence/absence of each data option
    ## m = slope, m.95 = 95% confidence intervals for slope, r2 = pearson's r-squared,
    ## r2.95 = 95% confience intervals for r-squared, p = p-value
    # xman, yman = manual values for placement of output in case of strange scaling problems
    # ylog = adjusts scaling for scale_y_logarithmic
    
    fontfamily = family
    fontsize = size
    
    model = lm.bal(x,y,n,method)
    
    eq.1.t = substitute(list(Slope~"="~b~"("*blow,bhigh*")"),
                        list(b = format(model$slope.observed, digits =coef.digits),
                             blow = format(model$slope.95ci[1], digits=coef.digits),
                             bhigh = format(model$slope.95ci[2], digits=coef.digits)))
    eq.1.f = substitute(list(Slope~"="~b),list(b = format(model$slope.observed, digits =coef.digits)))
    eq.1 = if(m.95==T){eq.1.t} else if(m.95==F){eq.1.f}
    
    eq.intercept.t = substitute(list(Intercept~"="~b~"("*blow,bhigh*")"),
                                list(b = format(model$int.observed, digits =coef.digits),
                                     blow = format(model$int.95ci[1], digits=coef.digits),
                                     bhigh = format(model$int.95ci[2], digits=coef.digits)))
    eq.intercept.f = substitute(list(Intercept~"="~b),list(b = format(model$int.observed, digits =coef.digits)))
    eq.intercept = if(int.95==T){eq.intercept.t} else if(int.95==F){eq.intercept.f}  
    
    
    eq.2.t = substitute(list(r^2~"="~r2~"("*r2low,r2high*")"),
                        list(r2 = format(model$r2.observed, digits=4),
                             r2low = format(model$r2.95ci[1], digits=2),
                             r2high = format(model$r2.95ci[2], digits=2)))
    eq.2.f = substitute(list(r^2~"="~r2),list(r2 = format(model$r2.observed, digits=2)))
    eq.2 = if(r2.95==T){eq.2.t} else if(r2.95==F){eq.2.f}
    
    eq.3 = substitute(list(p^phantom(0)*pv),list(pv = model$slope.sig))
    
    eq = list(eq.1,eq.intercept,eq.2,eq.3)
    
    if(m==F) {eq[[1]]<-NA}
    if(int==F) {eq[[2]]<-NA}
    if(r2==F) {eq[[3]]<-NA}
    if(p==F) {eq[[4]]<-NA}
    
    eq = eq[!is.na(eq)]
    
    xr = max(x)-min(x)
    yr = max(y)-min(y)
    if(ylog == T){
      if(location=="tl"){
        xloc <- rep(min(xman)*xs,length(eq))    
        yloc <- c(10^(max(yman)), #First Line
                  10^(max(yman)-0.05*yr*ys), #Second Line
                  10^(max(yman)-0.10*yr*ys), #Third Line
                  10^(max(yman)-0.15*yr*ys))[1:length(eq)] #Fourth Line
      }
      if(location=="tr"){
        xloc <- rep(max(xman)-0.2*xr*xs,length(eq))
        yloc <- c(10^(max(yman)), #First Line
                  10^(max(yman)-0.05*yr*ys), #Second Line
                  10^(max(yman)-0.10*yr*ys), #Third Line
                  10^(max(yman)-0.15*yr*ys))[1:length(eq)] #Fourth Line
      }
      if(location=="bl"){
        xloc <- rep(min(xman)*xs,length(eq))
        yloc <- rev(c(10^(min(yman)),
                      10^(min(yman)+0.05*yr*ys),
                      10^(min(yman)+0.10*yr*ys),
                      10^(min(yman)+0.15*yr*ys))[1:length(eq)])
      }
      if(location=="br"){
        xloc <- rep(max(xman)-0.2*xr*xs,length(eq))
        yloc <- rev(c(10^(min(yman)),
                      10^(min(yman)+0.05*yr*ys),
                      10^(min(yman)+0.10*yr*ys),
                      10^(min(yman)+0.15*yr*ys))[1:length(eq)])
      }
    }
    if(is.na(xman)==F & ylog==F){
      if(location=="tl"){
        xloc <- rep(min(xman)*xs,length(eq))    
        yloc <- c(max(yman), #First Line
                  max(yman)-0.05*yr*ys, #Second Line
                  max(yman)-0.10*yr*ys, #Third Line
                  max(yman)-0.15*yr*ys)[1:length(eq)] #Fourth Line
      }
      if(location=="tr"){
        xloc <- rep(max(xman)-0.2*xr*xs,length(eq))
        yloc <- c(max(yman),
                  max(yman)-0.05*yr*ys,
                  max(yman)-0.10*yr*ys,
                  max(yman)-0.15*yr*ys)[1:length(eq)]
      }
      if(location=="bl"){
        xloc <- rep(min(xman)*xs,length(eq))
        yloc <- rev(c(min(yman),min(yman)+0.05*yr*ys,min(yman)+0.10*yr*ys,min(yman)+0.15*yr*ys)[1:length(eq)])
      }
      if(location=="br"){
        xloc <- rep(max(xman)-0.2*xr*xs,length(eq))
        yloc <- rev(c(min(yman),
                      min(yman)+0.05*yr*ys,
                      min(yman)+0.10*yr*ys,
                      min(yman)+0.15*yr*ys)[1:length(eq)])
      }
    }
    if(is.na(xman)==T & ylog==F){
      if(location=="tl"){
        xloc <- rep(min(x)*xs,length(eq))    
        yloc <- c(max(y),max(y)-0.05*yr*ys,max(y)-0.10*yr*ys,max(y)-0.15*yr*ys)[1:length(eq)]
      }
      if(location=="tr"){
        xloc <- rep(max(x)-0.2*xr*xs,length(eq))
        yloc <- c(max(y),max(y)-0.05*yr*ys,max(y)-0.10*yr*ys,max(y)-0.15*yr*ys)[1:length(eq)]
      }
      if(location=="bl"){
        xloc <- rep(min(x)*xs,length(eq))
        yloc <- rev(c(min(y),min(y)+0.05*yr*ys,min(y)+0.10*yr*ys,min(y)+0.15*yr*ys)[1:length(eq)])
      }
      if(location=="br"){
        xloc <- rep(max(x)-0.2*xr*xs,length(eq))
        yloc <- rev(c(min(y),min(y)+0.05*yr*ys,min(y)+0.10*yr*ys,min(y)+0.15*yr*ys)[1:length(eq)])
      }
    }
    annotate("text",color=color,family=fontfamily,size=fontsize*0.352777778,parse=T,hjust=0,x=xloc,y=yloc,label=as.character(as.expression(eq)))
  }

  
  # Custom ggplot geom to perform the lm_eqn function... does not work with groups yet, I don't think...
  {
    lmeqn <- ggproto("lmeqn", Stat,
                     compute_group = function(data,scales,method="ols",location="tl",n=10000,xs=1,ys=1,
                                              m=T,m.95=T,int=T,int.95=T,r2=T,r2.95=F,p=T,
                                              family="Times",size=7,xman=NA,yman=NA,ylog=F,
                                              coef.digits=3)
                     {
                       names(data) <- c("x","y")
                       
                       fontfamily = family
                       fontsize = size
                       
                       model = lm.bal(data$x,data$y,n,method)
                       
                       eq.1.t = substitute(list(Slope~"="~b~"("*blow,bhigh*")"),
                                           list(b = format(model$slope.observed, digits =coef.digits),
                                                blow = format(model$slope.95ci[1], digits=coef.digits),
                                                bhigh = format(model$slope.95ci[2], digits=coef.digits)))
                       eq.1.f = substitute(list(Slope~"="~b),list(b = format(model$slope.observed, digits =coef.digits)))
                       eq.1 = if(m.95==T){eq.1.t} else if(m.95==F){eq.1.f}
                       
                       eq.intercept.t = substitute(list(Intercept~"="~b~"("*blow,bhigh*")"),
                                                   list(b = format(model$int.observed, digits =coef.digits),
                                                        blow = format(model$int.95ci[1], digits=coef.digits),
                                                        bhigh = format(model$int.95ci[2], digits=coef.digits)))
                       eq.intercept.f = substitute(list(Intercept~"="~b),list(b = format(model$int.observed, digits =coef.digits)))
                       eq.intercept = if(int.95==T){eq.intercept.t} else if(int.95==F){eq.intercept.f}  
                       
                       
                       eq.2.t = substitute(list(r^2~"="~r2~"("*r2low,r2high*")"),
                                           list(r2 = format(model$r2.observed, digits=4),
                                                r2low = format(model$r2.95ci[1], digits=2),
                                                r2high = format(model$r2.95ci[2], digits=2)))
                       eq.2.f = substitute(list(r^2~"="~r2),list(r2 = format(model$r2.observed, digits=2)))
                       eq.2 = if(r2.95==T){eq.2.t} else if(r2.95==F){eq.2.f}
                       
                       eq.3 = substitute(list(p^phantom(0)*pv),list(pv = model$slope.sig))
                       
                       eq = list(eq.1,eq.intercept,eq.2,eq.3)
                       
                       if(m==F) {eq[[1]]<-NA}
                       if(int==F) {eq[[2]]<-NA}
                       if(r2==F) {eq[[3]]<-NA}
                       if(p==F) {eq[[4]]<-NA}
                       
                       eq = eq[!is.na(eq)]
                       
                       xr = max(data$x)-min(data$x)
                       yr = max(data$y)-min(data$y)
                       
                       if(ylog == T){
                         if(location=="tl"){
                           xloc <- rep(min(xman)*xs,length(eq))    
                           yloc <- c(10^(max(yman)), #First Line
                                     10^(max(yman)-0.05*yr*ys), #Second Line
                                     10^(max(yman)-0.10*yr*ys), #Third Line
                                     10^(max(yman)-0.15*yr*ys))[1:length(eq)] #Fourth Line
                         }
                         if(location=="tr"){
                           xloc <- rep(max(xman)-0.2*xr*xs,length(eq))
                           yloc <- c(10^(max(yman)), #First Line
                                     10^(max(yman)-0.05*yr*ys), #Second Line
                                     10^(max(yman)-0.10*yr*ys), #Third Line
                                     10^(max(yman)-0.15*yr*ys))[1:length(eq)] #Fourth Line
                         }
                         if(location=="bl"){
                           xloc <- rep(min(xman)*xs,length(eq))
                           yloc <- rev(c(10^(min(yman)),
                                         10^(min(yman)+0.05*yr*ys),
                                         10^(min(yman)+0.10*yr*ys),
                                         10^(min(yman)+0.15*yr*ys))[1:length(eq)])
                         }
                         if(location=="br"){
                           xloc <- rep(max(xman)-0.2*xr*xs,length(eq))
                           yloc <- rev(c(10^(min(yman)),
                                         10^(min(yman)+0.05*yr*ys),
                                         10^(min(yman)+0.10*yr*ys),
                                         10^(min(yman)+0.15*yr*ys))[1:length(eq)])
                         }
                       }
                       if(is.na(xman)==F & ylog==F){
                         if(location=="tl"){
                           xloc <- rep(min(xman)*xs,length(eq))    
                           yloc <- c(max(yman), #First Line
                                     max(yman)-0.05*yr*ys, #Second Line
                                     max(yman)-0.10*yr*ys, #Third Line
                                     max(yman)-0.15*yr*ys)[1:length(eq)] #Fourth Line
                         }
                         if(location=="tr"){
                           xloc <- rep(max(xman)-0.2*xr*xs,length(eq))
                           yloc <- c(max(yman),
                                     max(yman)-0.05*yr*ys,
                                     max(yman)-0.10*yr*ys,
                                     max(yman)-0.15*yr*ys)[1:length(eq)]
                         }
                         if(location=="bl"){
                           xloc <- rep(min(xman)*xs,length(eq))
                           yloc <- rev(c(min(yman),min(yman)+0.05*yr*ys,min(yman)+0.10*yr*ys,min(yman)+0.15*yr*ys)[1:length(eq)])
                         }
                         if(location=="br"){
                           xloc <- rep(max(xman)-0.2*xr*xs,length(eq))
                           yloc <- rev(c(min(yman),
                                         min(yman)+0.05*yr*ys,
                                         min(yman)+0.10*yr*ys,
                                         min(yman)+0.15*yr*ys)[1:length(eq)])
                         }
                       }
                       if(is.na(xman)==T & ylog==F){
                         if(location=="tl"){
                           xloc <- rep(min(data$x)*xs,length(eq))    
                           yloc <- c(max(data$y),max(data$y)-0.05*yr*ys,max(data$y)-0.10*yr*ys,max(data$y)-0.15*yr*ys)[1:length(eq)]
                         }
                         if(location=="tr"){
                           xloc <- rep(max(data$x)-0.2*xr*xs,length(eq))
                           yloc <- c(max(data$y),max(data$y)-0.05*yr*ys,max(data$y)-0.10*yr*ys,max(data$y)-0.15*yr*ys)[1:length(eq)]
                         }
                         if(location=="bl"){
                           xloc <- rep(min(data$x)*xs,length(eq))
                           yloc <- rev(c(min(data$y),min(data$y)+0.05*yr*ys,min(data$y)+0.10*yr*ys,min(data$y)+0.15*yr*ys)[1:length(eq)])
                         }
                         if(location=="br"){
                           xloc <- rep(max(data$x)-0.2*xr*xs,length(eq))
                           yloc <- rev(c(min(data$y),min(data$y)+0.05*yr*ys,min(data$y)+0.10*yr*ys,min(data$y)+0.15*yr*ys)[1:length(eq)])
                         }
                       }
                       data.frame(fontfamily,
                                  fontsize = fontsize*0.352777778,
                                  hjust=0,
                                  x=xloc,
                                  y=yloc,
                                  label = as.character(as.expression(eq)))})
    
    geom_lmeqn <- function(mapping = NULL, data = NULL, geom = "text",
                           position = "identity", na.rm = FALSE, show.legend = NA, 
                           inherit.aes = T,...) {
      layer(
        stat = lmeqn, data = data, mapping = mapping, geom = geom, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm,parse=T,...)
      )
    }
  }
  
  #Perform a one-sample balanced bootstrap using mean as the sampling statistic - output designed for stat_summary in ggplot
  mean_ci_bal <- function(x,n=10000,ci=95){
    mean_observed = mean(x)
    data = data.frame(dist = rep(x,(n-1)),
                      index = sample(rep(seq(1,n-1),length(x)),length(x)*(n-1))) %>% 
      group_by(index) %>% summarize(mean = mean(dist)) %>% 
      select(mean)
    dist = c(data$mean,mean_observed)
    data.frame(y = mean(dist),
               ymin = sort(dist)[(100-ci)/2/100*length(dist)],
               ymax = sort(dist)[(100+ci)/2/100*length(dist)])
  }
  
  #Perform a one-sample balanced bootstrap using mean as the sampling statistic (incdata=T was modified by Mike Shields on 2018_06_13)
  one.samp.bal <- function(a,n=10000,incdata=F){
    if(length(a)>1){
      ran.a <- cbind(matrix(sample(rep(a,n-1)),nrow=length(a)),a)
      dist <- sort(colMeans(ran.a))
      mean_dist = mean(dist)
      ci2.5 = dist[0.025*length(dist)]
      ci97.5 = dist[0.975*length(dist)]
      p.value = ifelse(sign(ci2.5)==sign(ci97.5),"Significant (p < 0.05)","Non-significant (p > 0.05)")
      if(incdata==F){
        output <- as.data.frame(list("mean" = mean_dist,
                                     "ci2.5" = ci2.5,
                                     "ci97.5" = ci97.5,
                                     "p.value" = p.value))
      }
      if(incdata==T){
        temp <- list("mean" = mean_dist,
                     "ci2.5" = ci2.5,
                     "ci97.5" = ci97.5,
                     "p.value" = p.value,
                     "dist" = dist)
        output <- data.frame(temp)
      }}
    else if(length(a) < 2) {
      output <- data.frame(mean = NA,
                           ci2.5 = NA,
                           ci97.5 = NA,
                           p.value = NA)
    }
    output
  }

  #Perform a two-sample balanced bootstrap using mean as the sampling statistic, defaults to unpaired
  two.samp.bal <- function(a,b,n=10000,paired=F,dist=F){
    a <- na.omit(a)
    b <- na.omit(b)    
    diff.observed <- mean(b) - mean(a)  
    if (paired==F) {
      shuffled <- rep(c(a,b),n)
      shuffled <- sample(shuffled,length(shuffled))
      a.random <- colMeans(matrix(shuffled[1:(length(a)*n)],nrow=length(a),ncol=n))
      b.random <- colMeans(matrix(shuffled[(length(a)*n+1):length(shuffled)],nrow=length(b),ncol=n))
      diff.random <- c(scale(b.random-a.random,scale=F),diff.observed)
      p.value <- round(sum(abs(diff.random) >= abs(diff.observed)) / (n+1), floor(log10((n+1))))
      #Output
      if (dist==F){data.frame(diff.observed,p.value)}
      else if (dist==T){
        list("dist" = diff.random,
             "diff.observed" = diff.observed,
             "p.value" = p.value)
      }
    }
    else if (paired==T) {
      diff.sample <- b - a
      shuffled <- rep(diff.sample,(n-1))
      shuffled <- sample(shuffled,length(shuffled))
      diff.random <- c(colMeans(matrix(shuffled,nrow=length(a),ncol=(n-1))),diff.observed)
      diff.95ci <- c(sort(diff.random)[(n*0.025)],sort(diff.random)[(n*0.975)])
      p.value <- ifelse((sign(diff.95ci[1])==sign(diff.95ci[2]))==T,"< 0.05","> 0.05")
      #Output
      if (dist==F){
        as.data.frame(list(
          "diff.observed" = diff.observed,
          "ci.2.5" = diff.95ci[1],
          "ci.97.5" = diff.95ci[2],
          "p.value" = p.value))
      }
      else if (dist==T) {
        list("dist" = diff.random, #bootstrapped distribution
             "obs" = diff.observed, #observed difference in means
             "ci.2.5" = diff.95ci[1], # 2.5 percentile
             "ci.97.5" = diff.95ci[2], # 97.5 percentile
             "p" = p.value) #two-tailed p value
      }
    }
  }
}
#jah_ggplot_theme
{
  base_font_size = 8 #Default Font size in points.
  base_font_family = "Times" #Default font family.
  unit <- function(...,units="mm"){grid::unit(...,units=units)} #Set default units to mm
  margin <- function(...,unit="mm"){ggplot2::margin(...,unit=unit)} #Same as above, but for the margin comment.
  
  theme_set(theme(line = element_line(color="black",size=0.25,linetype=1,lineend="butt",arrow=F,inherit.blank=T), 
                  rect = element_rect(fill=NA,color="black",size=0.25,linetype=1,inherit.blank=T), 
                  text = element_text(family="Times",face="plain",color="black",size=base_font_size,hjust=0.5,vjust=0.5,
                                      angle=0,lineheight=0.9,margin=margin(0,0,0,0),debug=F), 
                  aspect.ratio = 1,
                  axis.title.x = element_text(margin=margin(2.75,0,0,0),inherit.blank = T),
                  axis.title.x.top = element_text(margin=margin(0,0,2.75,0),inherit.blank = T), 
                  axis.title.y = element_text(vjust = 1,angle=90,margin=margin(0,2.75,0,0),inherit.blank = T), 
                  axis.title.y.right = element_text(vjust = 0,angle=-90,margin=margin(0,0,0,2.75),inherit.blank = T), 
                  axis.text = element_text(inherit.blank = T), 
                  axis.text.x = element_text(margin=margin(0.75,0,0,0),inherit.blank = T),
                  axis.text.x.top = element_text(vjust=0,margin=margin(0,0,0.75,0),inherit.blank = T), 
                  axis.text.y = element_text(hjust=1,margin=margin(0,0.75,0,0),inherit.blank = T), 
                  axis.text.y.right = element_text(hjust=0,margin=margin(0,0,0,0.75),inherit.blank = T), 
                  axis.ticks = element_line(size=0.2,inherit.blank = T), 
                  axis.ticks.x = element_line(size=0.2,inherit.blank = T),
                  axis.ticks.y = element_line(size=0.2,inherit.blank = T), 
                  axis.ticks.length = unit(1), 
                  axis.line = element_line(inherit.blank = T), 
                  axis.line.x = element_line(inherit.blank = T), 
                  axis.line.y = element_line(inherit.blank = T),
                  legend.background = element_blank(), #default is element_rect() 
                  legend.margin = margin(2,2,2,2), 
                  legend.spacing = unit(4), 
                  legend.spacing.x = unit(4),
                  legend.spacing.y = unit(4),
                  legend.key = element_blank(), #default is element_rect() 
                  legend.key.size = unit(2.5), 
                  legend.key.height = unit(2.5),
                  legend.key.width = unit(2.5),
                  legend.text = element_text(inherit.blank = T), 
                  legend.text.align = 0, 
                  legend.title = element_text(hjust=0,inherit.blank = T),
                  legend.title.align = 0.5, # alignment of legend title (number from 0 (left) to 1 (right))
                  legend.position = "right", # the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector) 
                  legend.direction = "vertical", # layout of items in legends ("horizontal" or "vertical")
                  legend.justification = "center", #anchor point for positioning legend inside plot, default is "center"
                  legend.box = "horizontal", #arrangment of multiple legends ("horizontal" or "vertical")
                  legend.box.just = "left", # justification of each legend within the overall bounding box, when there are multiple legends ("top", "bottom", "left", or "right")
                  legend.box.margin = margin(0,0,0,0), 
                  legend.box.background = element_blank(), #background of box around multiple legends
                  legend.box.spacing = unit(4),
                  panel.background = element_rect(fill="white",color=NA,inherit.blank = T), 
                  panel.border = element_rect(fill=NA,color="black",inherit.blank = T), 
                  panel.spacing = unit(2), #Space between facets
                  panel.spacing.x = unit(2), #Space between facets
                  panel.spacing.y = unit(2), #Space between facets
                  panel.grid = element_blank(),
                  panel.grid.major = element_line(inherit.blank = T),
                  panel.grid.minor = element_line(inherit.blank = T),
                  panel.grid.major.x = element_line(inherit.blank = T),
                  panel.grid.major.y = element_line(inherit.blank = T),
                  panel.grid.minor.x = element_line(inherit.blank = T),
                  panel.grid.minor.y = element_line(inherit.blank = T),
                  panel.ontop = F, #Place the panel on top of the plot? Not sure why this is here.
                  plot.background = element_rect(color=NA),
                  plot.title = element_text(hjust=0,vjust=1,margin=margin(0,0,2.75,0),inherit.blank = T), 
                  plot.subtitle = element_text(hjust=0,vjust=1,margin=margin(0,0,1.5,0),inherit.blank = T),
                  plot.caption = element_text(hjust=1,vjust=1,margin=margin(1.5,0,0,0),inherit.blank = T), 
                  plot.margin = margin(1.5,1.5,1,1),
                  strip.background = element_rect(color=NA,inherit.blank = T),
                  strip.placement = "inside", #'inside' or 'outside' relative to the axis ticks/text
                  strip.text = element_text(inherit.blank = T),
                  strip.text.x = element_text(margin=margin(2,0,2,0),inherit.blank = T),
                  strip.text.y = element_text(margin=margin(0,2,0,2),inherit.blank = T),
                  strip.switch.pad.grid = unit(1),
                  strip.switch.pad.wrap = unit(1),
                  complete = T, 
                  validate = T))
}
#A few function modifications...
{
  ggplot <- function(...) ggplot2::ggplot(...) + scale_color_hue(h = c(0,270),l=40) + scale_fill_hue(h = c(0,270),l=40)
  geom_lm <- function(...,formula=y~x,method=lm,se=F){ggplot2::geom_smooth(...,formula=formula,method=method,se=se)}
  ggsave <- function(
    filename, plot = ggplot2::last_plot(), device = NULL,
    path = NULL, scale = 1, width = NA, height = NA,
    units = c("mm"), dpi = 600, limitsize = TRUE, border=50, ...){
    
    if(is.na(width)){width = 2*height}
    if(is.na(height)){height = 2*width}
    
    cowplot::ggsave2(filename=filename,
                     plot=plot,
                     device=device,
                     path=path,
                     scale=scale,
                     width=width,
                     height=height,
                     units=units,
                     dpi=dpi,
                     limitsize=limitsize,
                     ...)
    system(paste0("magick convert \"",filename,"\" -trim -bordercolor white -border ",border," \"",filename,"\""))
  }
}
