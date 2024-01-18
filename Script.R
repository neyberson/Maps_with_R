# Packages


library(terra)
library(giscoR)
library(sf)
library(tidyverse)
library(ggtern)
library(elevatr)
library(png)
library(rayshader)
library(magick)


# COUNTRY BORDERS

country_sf <- giscoR::gisco_get_countries(
  country = "CO",
  resolution = "1"
)

# LINKS
urls <- c(
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/18P_20220101-20230101.tif",
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/18N_20220101-20230101.tif",
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/19N_20220101-20230101.tif",
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/17N_20220101-20230101.tif",
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/18M_20220101-20230101.tif",
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/19M_20220101-20230101.tif"
)

for(url in urls){
    download.file(
        url = url,
        destfile = basename(url),
        mode = "wb"
    )
}

# LOAD TILES

raster_files <-list.files(
    path = getwd(),
    pattern = "tif",
    full.names = T
)

crs <- "EPSG:4326"
for(raster in raster_files){
    rasters <- terra::rast(raster)
    
    country <- country_sf |>
        sf::st_transform(
          crs = terra::crs(
            rasters
          )
        )
    
    land_cover <- terra::crop(
        rasters,
        terra::vect(
          country
        ),
        snap = "in",
        mask = T
      ) |>
      terra::aggregate(
        fact = 5,
        fun = "modal"
      ) |>
      terra::project(crs)
    
      terra::writeRaster(
        land_cover,
        paste0(
            raster,
            "_colombia",
            ".tif"
        )
      )
}

# LOAD VIRTUAL LAYER

r_list <- list.files(
  path = getwd(),
  pattern = "_colombia",
  full.names = T
)

land_cover_vrt <- terra::vrt(
  r_list,
  "colombia_land_cover_vrt.vrt.",
  overwrite = T
)


# FETCH ORIGINAL COLORES

ras <- terra::rast(
  raster_files[[1]]
)

raster_color_table <- do.call(
  data.frame,
  terra::coltab(ras)
)

head(raster_color_table)

hex_code <- ggtern::rgb2hex(
  r = raster_color_table[,2],
  g = raster_color_table[,3],
  b = raster_color_table[,4]
)

# ASSIGN COLORS TO RASTER

cols <- hex_code[c(2:3, 5:6, 8:12)]

from <- c(1:2, 4:5, 7:11)
to <- t(col2rgb(cols))
land_cover_vrt <- na.omit(land_cover_vrt)

land_cover_colombia <- terra::subst(
  land_cover_vrt,
  from = from,
  to = to,
  names = cols
)

terra::plotRGB(land_cover_colombia)


# DIGITAL ELEVATION MODEL

elev <- elevatr::get_elev_raster(
  locations = country_sf,
  z = 6, clip = "locations"
)


crs_bogota <-
  "+proj=tcea +lon_0=-72.9052734 +datum=WGS84 +units=m +no_defs"

land_cover_colombia_resampled <- terra::resample(
  x = land_cover_colombia,
  y = terra::rast(elev),
  method = "near"
) |>
  terra::project(crs_bogota)

terra::plotRGB(land_cover_colombia_resampled)

img_file <- "land_cover_colombia.png"

terra::writeRaster(
  land_cover_colombia_resampled,
  img_file,
  overwrite = T,
  NAflag = 255
)

img <- png::readPNG(img_file)

# RENDER SCENE

elev_bogota <- elev |>
  terra::rast() |>
  terra::project(crs_bogota)

elmat <- rayshader::raster_to_matrix(
  elev_bogota
)

h <- nrow(elev_bogota)
w <- ncol(elev_bogota)

elmat |>
  rayshader::height_shade(
    texture = colorRampPalette(
      cols[9]
    )(256)
  ) |>
  rayshader::add_overlay(
    img,
    alphalayer = 1
  ) |>
  rayshader::plot_3d(
    elmat,
    zscale = 20,
    solid = F,
    shadow = T,
    shadow_darkness = 1,
    background = "white",
    windowsize = c(
      w / 5, h / 5
    ),
    zoom = .5,
    phi = 60,
    theta = 0
  )

rayshader::render_camera(
  zoom = .58
)

#RENDER OBJECT

filename <- "3d_land_cover_colombia.png"

rayshader::render_highquality(
  filename = filename,
  preview = T,
  ligth = F,
  envioronment_ligth = "air_museum_playground_4k.hdr"
)