
library(tidyverse)
library(icon)
library(leaflet)
library(countdown)


# workaround to get htmlwidgets working
options(
  htmltools.preserve.raw = FALSE
)

icon_download   <- fontawesome("download", style = "solid")
icon_edit       <- fontawesome("edit", style = "solid")
icon_github     <- simple_icons("github")
icon_thumbs_up  <- fontawesome("thumbs-up", style = "solid")
icon_js         <- simple_icons("javascript")
icon_md         <- simple_icons("markdown")
icon_css3       <- simple_icons("css3")
icon_r          <- simple_icons("r")
icon_html5      <- simple_icons("html5")
icon_keyboard   <- fontawesome("keyboard", style = "solid")
icon_images     <- fontawesome("images", style = "solid")
icon_columns    <- fontawesome("columns", style = "solid")
icon_palette    <- fontawesome("palette", style = "solid")
icon_code       <- fontawesome("code")

pal_gsem <- list(
    white     = "#FFFFFF",
    gsemlight = "#A3AFBF",
    gsemblue  = "#465F7F",
    gsemdark  = "#233040",
    black     = "#000000",
    pink     = "#CF0063",
    red	     = "#F42941",
    blue	   = "#0067C5",
    burgundy = "#96004B",
    green    =	"#007E64",
    yellow	 = "#F1AB00",
    marine	 = "#00B1AE",
    purple	 = "#4B0B71",
    orange	 = "#FF5C00"

)

pal_facs <- list(

)

# countdown functions with custom palette and easier positioning
countdown_gsem<- function(minutes,
                          seconds, ...){
  countdown(
    minutes = minutes,
    seconds = seconds,
    color_background          = pal_gsem$gsemblue,
    color_border              = pal_gsem$white,
    color_text                = pal_gsem$white,
    color_running_background  = pal_gsem$marine,
    color_running_text        = pal_gsem$white,
    color_finished_background = pal_gsem$red,
    color_finished_text       = pal_facs$white, ... )
}
