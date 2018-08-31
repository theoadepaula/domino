#ler png
library(grid)
library(png)

logo=readPNG("C:\\Users\\theo.paula\\OneDrive\\Dados Zero\\lp1.png")
rast <- rasterGrob(logo, interpolate = T)

#colocar a logo no ggplot
annotation_custom(rast, ymin=260, xmin=4)