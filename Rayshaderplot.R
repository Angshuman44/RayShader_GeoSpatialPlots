library(sf)
library(tidyverse)
library(rayshader)
library(MetBrewer)
library(colorspace)
library(stars)
# To install the latest version from Github:

setwd("~/Documents/Anshu/Rayshader")
data <- st_read("kontur_population_IN_20231101.gpkg")
library(raster)
library(sp)
library(geodata)

bharat <- geodata::gadm("IND", level = 1, path = tempdir()) |> st_as_sf() 


glimpse(bharat)

Biharst <- bharat |> 
  filter(NAME_1 == "Assam") |> 
  st_transform(crs = st_crs(data))

Biharst |> 
  ggplot() +
  geom_sf()

# do intersection on data to limit kontur to florida

st_bihar <- st_intersection(data, Biharst)

# define aspect ratio based on bounding box

bb <- st_bbox(st_bihar)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(data))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(data))

Biharst |> 
  ggplot() +
  geom_sf() +
  geom_sf(data = bottom_left) +
  geom_sf(data = bottom_right, color = "red")

width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) |> 
  st_sfc(crs = st_crs(data))

height <- st_distance(bottom_left, top_left)

# handle conditions of width or height being the longer side

if (width > height) {
  w_ratio <- 1
  h_ratio <- height / width
} else {
  h_ration <- 1
  w_ratio <- width / height
}


# convert to raster so we can then convert to matrix

size <- 5000

bihar_rast <- st_rasterize(st_bihar, 
                             nx = floor(size * w_ratio),
                             ny = floor(size * h_ratio))

mat <- matrix(bihar_rast$population, 
              nrow = floor(size * w_ratio),
              ncol = floor(size * h_ratio))



# create color palette

c1 <- met.brewer("OKeeffe2")
swatchplot(c1)

texture <- grDevices::colorRampPalette(c1, bias = 2)(256)
swatchplot(texture)

# plot that 3d thing!
library(rgl)
options(rgl.printRglwidget = TRUE)

#rgl::rgl.close()
close3d()
mat |> 
  height_shade(texture = texture) |> 
  plot_3d(heightmap = mat,
          zscale = 100 / 5,
          solid = FALSE,
          shadowdepth = 0)

render_camera(theta = -20, phi = 45, zoom = .8)

outfile <- "~/Documents/Anshu/Rayshader/final_plot.png"

{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if (!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }
  render_highquality(
    filename = outfile,
    interactive = FALSE,
    lightdirection = 250,
    lightaltitude = c(30, 83),
    lightcolor = c(c1[2], "white"),
    lightintensity = c(650,150),
    samples = 450,
    width = 5000,
    height = 5000
  )
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}



# Set the directory where data should be downloaded
#data_dir <- "~/Documents/Anshu/Rayshader"

# Download administrative boundaries of India
#india_boundary <- geodata::gadm(country = "IND", level = 0, path = data_dir)

# Plot the boundary
#plot(india_boundary, main = "Administrative Boundary of India")


#library(ggplot2)

# Convert spatial object to data frame
#india_boundary_df <- as.data.frame(india_boundary)

# Plot using ggplot2 with geom_polygon
#ggplot(india_boundary_df, aes(x = long, y = lat, group = group)) +
#  geom_polygon() +
#  labs(title = "Administrative Boundary of India")
