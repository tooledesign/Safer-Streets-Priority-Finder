# oh yea, custom themes
my_theme <- function(){
  fresh::create_theme(
    fresh::bs4dash_status(
      primary = "#264653",
      secondary = "#277da1",
      success = '#43aa8b',
      info = "#219ebc",
      warning = '#f8961e',
      danger = "#f94144",
      light = '#f4d35e',
      dark = '#03071e'  
    ),
    fresh::bs4dash_font(
      size_base = ".85rem",
      size_lg = NULL,
      size_sm = NULL,
      size_xs = NULL,
      size_xl = NULL,
      weight_light = NULL,
      weight_normal = NULL,
      weight_bold = 7500,
      family_sans_serif = NULL,
      family_monospace = NULL,
      family_base = NULL
    ),
    fresh::bs4dash_button(
      default_background_color = '#fff',
      default_color = NULL,
      default_border_color = NULL,
      padding_y_xs = NULL,
      padding_x_xs = NULL,
      line_height_xs = NULL,
      font_size_xs = NULL,
      border_radius_xs = '3px'
    ),
    fresh::bs4dash_color(
      blue = '#264653',
      lightblue = NULL,
      navy = NULL,
      cyan = NULL,
      teal = NULL,
      olive = NULL,
      green = NULL,
      lime = NULL,
      orange = NULL,
      yellow = NULL,
      fuchsia = NULL,
      purple = NULL,
      maroon = NULL,
      red = NULL,
      black = NULL,
      gray_x_light = NULL,
      gray_600 = NULL,
      gray_800 = NULL,
      gray_900 = NULL,
      white = NULL
    )
  )
}

 