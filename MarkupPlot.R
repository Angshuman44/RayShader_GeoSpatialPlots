library(magick)
library(MetBrewer)
library(colorspace)
library(ggplot2)
library(glue)
library(stringr)
setwd("~/Documents/Anshu/Rayshader")
memory.limit(size = 8000)

img <- image_read("Uttarakhand.png")

colors <- met.brewer("OKeeffe2")
swatchplot(colors)

text_color <- darken(colors[7], .25)
swatchplot(text_color)
gc()


annot <- glue("This map shows the population density of Uttarakhand ",
              "Population estimates are bucketed into 400 meter (about 1/4 mile) ",
              "hexagons.") |> 
  str_wrap(45)

img |> 
  image_crop(gravity = "center",
             geometry = "6000x4500+0-150") |> 
  image_annotate("Uttarakhand Population Density",
                 gravity = "northwest",
                 location = "+100+1000",
                 color = text_color,
                 size = 100,
                 weight = 700,
                 font = "El Messiri") |> 
  image_annotate(annot,
                 gravity = "north",
                 location = "-500+1500",
                 color = text_color,
                 size = 75,
                 font = "El Messiri") |> 

  
  image_annotate(glue("Plot by Angshuman Mandal, JRF, IIT Roorkee | ",
                      "Data: Kontur Population India(2023)"),
                 gravity = "south",
                 location = "+0+100",
                 font = "El Messiri",
                 color = alpha(text_color, .5),
                 size = 50) |> 
  image_write("titled_final_plot.png")





