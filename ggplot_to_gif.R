## A function to save a series of ggplots as a GIF

pacman::p_load(caTools, RColorBrewer, data.table, datasets, ggplot2, png, Rmpfr, viridis)
setwd("~/Desktop")

saveGIF <- function(plots) {
  num.frames <- length(plots)
  
  pngs <- mapply(plots, c(1:num.frames), FUN=function(p, i) {
    png.fn <- paste0("plot_", i, ".png")
    png(png.fn)
    print(plots[1])
    dev.off()
    return(png.fn)
  })

  frames <- mapply(pngs, c(1:num.frames), FUN=function(png.fn, i) {
    frame <- readPNG(png.fn)
    
    orig.dimen <- dim(frame)
    frame <- aperm(frame, c(3,1,2), resize=TRUE)
    dim(frame) <- c(orig.dimen[3], prod(orig.dimen[1:2]))
    frame <- t(frame)
    
    file.remove(png.fn)
    return(frame)
  }, SIMPLIFY=FALSE)
  
  frame.colors <- lapply(frames, FUN=function(frame) {
    return(unique(frame))
  })
  frame.colors <- unique(as.vector(frame.colors))

  color.index <- paste(frame[,1], frame[,2], frame[,3], sep=".")
  ## make sure factor is correctly ordered
  color.index <- as.numeric(factor(color.index, levels=unique(color.index)))

  if (nrow(frame.colors) > 256) {
    color.clusters <- kmeans(frame.colors, centers=256)
    color.index <- color.clusters$cluster[color.index]
    frame.colors <- color.clusters$centers
  }

  frame.colors <- rgb(red=frame.colors[,1],
                      green=frame.colors[,2],
                      blue=frame.colors[,3],
                      alpha=frame.colors[,4])
  
  frame <- matrix(color.index-1, nrow=orig.dimen[1])
  storage.mode(frame) <- "integer"

  # write.gif(frame, "test1.gif", col=as.list(frame.colors), scale="never")
  write.gif(frame, "test.gif", col="rainbow", scale="never")
}

plots <- lapply(c(1:100), FUN=function(x) {
  ggplot() + 
    geom_point(aes(x=rnorm(100, mean=1), 
                              y=rnorm(100, mean=1), 
                              color=rnorm(100))) +
    scale_colour_gradientn(colours=rainbow(10), guide=FALSE) +
    theme_bw() +
    theme(
      axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank())
  })


X11.options(antialias="none")
saveGIF("tmp.gif",verbose=TRUE,debug=TRUE)
showGIF("tmp.gif")
