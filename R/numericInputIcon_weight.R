# numericInputIcon_weight <- function(id){
#   span(shinyWidgets::numericInputIcon(
#     inputId = id,
#     label   = NULL,
#     value   = 0,
#     icon    = list(NULL, "%"),
#     width   = "100%"),
#     style   = "font-family:IBM Plex Sans")
# }

numericInputIcon_weight <- function(id){
  span(shinyWidgets::autonumericInput(
    inputId = id,
    label = NULL,
    value = 0,
    width = "100%",
    decimalPlaces = 2,
    decimalCharacter = ".",
    currencySymbol = "%",
    currencySymbolPlacement = "s",
    minimumValue = 0,
    maximumValue = 100
  ))
}