raz_colours <- c(
  snyggroed_2 = "#aa0000ff",
  roed_brun = "#671e00ff",
  gulblek = "#e9c549ff",
  gulsenaps = "#db9a08ff",
  gulsmuts_2 = "#c59d2cff",
  gulsmuts = "#d5a611ff",
  gul_beige = "#e2b659ff",
  groen_havs_1 = "#97915cff",
  groen_havs_2 = "#a49e66ff",
  groen_havs_3 = "#898046ff",
  land_gul = "#fceea3ff",
  blaa_havs_blek = "#dddec4ff",
  blaa_havs_maettad = "#abc1b1")


print_raz_colours <- function() {
  for (nm in names(raz_colours)) {
    hex <- raz_colours[nm]
    # crayon has `style` / `make_style` to define arbitrary color
    style_fun <- crayon::make_style(hex, bg=T)
    cat(style_fun(paste0("              ")))
    cat(paste0(" ", nm, " (", hex, ")\n"))
  }
}

