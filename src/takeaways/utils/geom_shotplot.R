geom_shotplot <- function(...){
  list(annotation_custom(grid::rasterGrob(png::readPNG("full-rink.png"), 
                                          width = unit(1,"npc"), 
                                          height = unit(1,"npc"))),
       geom_point(alpha=0.5),
       coord_flip())
}
