revi.mod.dc <- readRDS("hDCRWS_revi_150K_50K_50t_1h.rds")
revi <- revi.mod.dc$summary
revi <- revi %>% filter(id!="REVI 386")
revi$space_rhat=rowMeans(matrix( gelman.diag(revi.mod.dc$mcmc$x,multivariate = F)$psrf[,1], ncol = 2, byrow = F))
revi$b_rhat=gelman.diag(revi.mod.dc$mcmc$b,multivariate = F)$psrf[,1]

saveRDS(revi, "mod_hDCRWS_revi_150K_50K_50t_1h.rds")



d <- data.table()
#for every modelstep
for(i in 1:nrow(revi.mod.dc$summary)){
  
  #chain 1, first three timestemps
  g1 <- revi.mod.dc$mcmc$x[i,,,1]
  g2 <- revi.mod.dc$mcmc$x[i,,,2] #chain2
  
  df.g<-data.table("timestep"=i,
                   "sample"=1:(revi.mod.dc$mcmc.settings$posterior.samples/revi.mod.dc$mcmc.settings$thinning),
                   "b.5"=revi.mod.dc$summary$b.5[i],
                   "id"=revi.mod.dc$summary$id[i])
  df.g$chain<-1
  df.g <- rbind(df.g, df.g)
  df.g$chain[c(0.5*nrow(df.g)+1):c(nrow(df.g))] <- 2
  df.g$lon <- c(g1[1,], g2[1,])
  df.g$lat <- c(g1[2,], g2[2,])
  d <- rbind(d, df.g)
  
}

#d now has all samples in long format, 1.8m rows.

#too much data. Make density plot of posterior samples, trim low density points.

denspoints <- function(df.g, n=50, y=0.1){
  
  f1 <- kde2d(df.g$lon, df.g$lat, n =n, 
              lims = c(min(df.g$lon), max(df.g$lon), min(df.g$lat), max(df.g$lat))
              #,h=c(width.SJ(df.g$lon),width.SJ(df.g$lat))
  )
  #persp(f1, phi = 30, theta = 20, d = 5)
  
  df.g2 <- expand.grid("lon"=seq(min(df.g$lon), max(df.g$lon), length.out = n),
                       "lat"=seq(min(df.g$lat), max(df.g$lat), length.out = n))
  #make sure point is in middle of bin
  df.g2$lon <- df.g2$lon + c(max(df.g$lon)-min(df.g$lon))/n/2
  df.g2$lat <- df.g2$lat + c(max(df.g$lat)-min(df.g$lat))/n/2
  
  df.g2$dens <- as.numeric(c(f1$z))
  df.g2$timestep=df.g$timestep[1]
  #trim by taking the yth quantile from the kernel grid
  #df.g2 <- df.g2[df.g2$dens > quantile(df.g2$dens, 0.05),]
  df.g2 <- df.g2[df.g2$dens> quantile(c(f1$z),y),]
  
  #df.g2$timestep <- unique(df.g$timestep)
  
  #normalize trimmed density to make each slice comparable
  #df.g2$dens <- c(df.g2$dens-min(df.g2$dens))/c(max(df.g2$dens-min(df.g2$dens)))
  #df.g2$id <- unique(df.g$id)
  #df.g2$b.5 <- unique(df.g$b.5)
  df.g2
  #df.g2[,c(4,1,2,3)]
  
  
}

 splitsamp <- split(d, d$timestep)
 dd1 <- do.call(rbind, 
                lapply(splitsamp, FUN=function(x){denspoints(x, n=50, 0.1)})
 )
#dd.all <- denspoints(d, n=50, y=0.1)

saveRDS(dd1, file="revi_150K_50K_2chain_spatialdensity.rds")
 
