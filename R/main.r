###################################################
#                3D river basin maps with R
#                 Milos Popovic
#                 2023/11/21
###################################################

libs <- c(
    "tidyverse", "sf", "giscoR",
    "elevatr", "terra", "rayshader"
)

installed_libs <- libs %in% rownames(
    installed.packages()
)

if (any(installed_libs == F)) {
    install.packages(
        libs[!installed_libs]
    )
}

invisible(lapply(
    libs, library,
    character.only = T
))

sf::sf_use_s2(F)

# 1. COUNTRY SF
#---------------

country_sf <- giscoR::gisco_get_countries(
    country = "PL",
    resolution = "1"
)

country_bbox <- sf::st_bbox(
    country_sf
)

# 2. GET RIVERS
#--------------

url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_eu_shp.zip"

download.file(
    url = url,
    destfile = basename(url),
    mode = "wb"
)

unzip(basename(url))
filename <- list.files(
    path = "HydroRIVERS_v10_eu_shp",
    pattern = ".shp",
    full.names = T
)

print(country_bbox)

bbox_wkt <- "POLYGON((
    14.12290 49.00285,
    14.12290 54.83568,
    24.14544 54.83568,
    24.14544 49.00285,
    14.12290 49.00285
))
"

country_rivers <- sf::st_read(
    filename,
    wkt_filter = bbox_wkt
)

plot(sf::st_geometry(country_sf), col = "red")
plot(sf::st_geometry(country_rivers), add = T)

# 3. GET BASINS
#---------------

url <- "https://data.hydrosheds.org/file/HydroBASINS/standard/hybas_eu_lev04_v1c.zip"

download.file(
    url = url,
    destfile = basename(url),
    mode = "wb"
)

list.files()

unzip(basename(url))

country_basin <- sf::st_read(
    "hybas_eu_lev04_v1c.shp"
) |>
    sf::st_intersection(country_sf) |>
    dplyr::select(HYBAS_ID)

# 4. CLIP RIVERS TO BASINS
#-------------------------

country_river_basin <- sf::st_intersection(
    country_rivers,
    country_basin
)

unique(country_river_basin$HYBAS_ID)

# 5. PALETTE
#-----------

palette <- hcl.colors(
    n = 10,
    palette = "Dark 3"
) |>
    sample()

# pal <- colorRampPalette(
#     palette
# )(15)

names(palette) <- unique(
    country_river_basin$HYBAS_ID
)

pal <- as.data.frame(
    palette
) |>
    tibble::rownames_to_column(
        "HYBAS_ID"
    ) |>
    dplyr::mutate(
        HYBAS_ID = as.numeric(HYBAS_ID)
    )

country_river_basin_pal <- country_river_basin |>
    dplyr::left_join(
        pal,
        by = "HYBAS_ID"
    )

crs_lambert <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_frfs"

country_basin_pal <- sf::st_transform(
    country_basin,
    crs = crs_lambert
) |>
    dplyr::inner_join(
        pal,
        by = "HYBAS_ID"
    ) |>
    dplyr::mutate(
        HYBAS_ID = as.factor(HYBAS_ID)
    )

# 6. WIDTH
#----------

unique(country_river_basin_pal$ORD_FLOW)

country_river_width <- country_river_basin_pal |>
    dplyr::mutate(
        width = as.numeric(
            ORD_FLOW
        ),
        width = dplyr::case_when(
            width == 3 ~ 14,
            width == 4 ~ 12,
            width == 5 ~ 10,
            width == 6 ~ 8,
            width == 7 ~ 7,
            width == 8 ~ 6,
            TRUE ~ 0
        )
    ) |>
    sf::st_as_sf() |>
    sf::st_transform(crs = crs_lambert)

# 7. DEM
#-------

elevation_raster <- elevatr::get_elev_raster(
    locations = country_sf,
    z = 8, clip = "locations"
) |>
    terra::rast() |>
    terra::project(crs_lambert)

elevation_matrix <- rayshader::raster_to_matrix(
    elevation_raster
)

# 8. RENDER SCENE
#----------------

h <- nrow(elevation_raster)
w <- ncol(elevation_raster)

elevation_matrix |>
    rayshader::height_shade(
        texture = colorRampPalette(
            c(
                "grey90",
                "grey60"
            )
        )(256)
    ) |>
    rayshader::add_overlay(
        rayshader::generate_polygon_overlay(
            geometry = country_basin_pal,
            extent = elevation_raster,
            heightmap = elevation_matrix,
            linecolor = palette,
            palette = palette,
            data_column_fill = "HYBAS_ID"
        ), alphalayer = .6 
    ) |>
    rayshader::add_overlay(
        rayshader::generate_line_overlay(
            geometry = country_river_width,
            extent = elevation_raster,
            heightmap = elevation_matrix,
            color = country_river_width$palette,
            linewidth = country_river_width$width,
            data_column_width = "width"
        ),
        alphalayer = 1
    ) |>
    rayshader::plot_3d(
        elevation_matrix,
        zscale = 10,
        solid = F,
        shadow = F,
        windowsize = c(
            w / 5, h / 5
        ),
        zoom = .515,
        phi = 85,
        theta = 0
    )

    rayshader::render_camera(
        phi = 89,
        zoom = .675,
        theta = 0
    )
    
# 9. RENDER OBJECT
#-----------------

u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/limpopo_golf_course_4k.hdr"

download.file(
    url = u,
    destfile = basename(u),
    mode = "wb"
)

rayshader::render_highquality(
    filename = "poland-3d-river-basins.png",
    preview = T,
    light = F,
    environment_light = basename(u),
    rotate_env = 0,
    intensity_env = .85,
    ground_material = rayrender::diffuse(
        color = "grey10"
    ),
    interactive = F,
    parallel = T,
    width = w,
    height = h
)
