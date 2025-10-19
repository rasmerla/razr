#' @export
raz_colours <- c(
  snyggroed_2 = "#aa0000",
  roed_brun = "#671e00",
  gulblek = "#e9c549",
  gulsenaps = "#db9a08",
  gulsmuts_2 = "#c59d2c",
  gulsmuts = "#d5a611",
  gul_beige = "#e2b659",
  groen_havs_1 = "#97915c",
  groen_havs_2 = "#a49e66",
  groen_havs_3 = "#898046",
  land_gul = "#fceea3",
  havsbeige_blek = "#dddec4",
  blaa_havs_maettad = "#abc1b1",
  havsgraa = "#687a7e",
  havsblaa = "#7992b0",
  mörkblå = "#5b7082",
  ärgrön = "#bcc3b3",
  saliva = "#a8a99d",
  aska = "#aeb3b6",
  duvblå = "#93a5ad",
  bauhausblå = "#8dbcc0",
  larimar = "#8cb4bd",
  tistel = "#79acc4",
  dalaälv = "#236877",
  daggkåpa = "#6d9380",
  mörk_jade = "#7c9692",
  lammöra = "#aec4ba",
  tällberg = "#8e3e36",
  vildvid = "#bc6a41",
  hobborn = "#dba54f",
  kvitten = "#ebcf72"
  )


#' Get nice colours printed
#'
#' @returns A printed list with colours
#' @export
#'
print_raz_colours <- function() {
  for (nm in names(raz_colours)) {
    hex <- raz_colours[nm]
    # crayon has `style` / `make_style` to define arbitrary color
    style_fun <- crayon::make_style(hex, bg=T)
    cat(style_fun(paste0("              ")))
    cat(paste0(" - ", hex, " - ", nm, "\n"))
  }
}
