# numericInputIcon_value <- function(id){
#   span(shinyWidgets::numericInputIcon(
#     inputId = id,
#     label   = NULL,
#     value   = 0,
#     icon    = list("$", NULL),
#     width   = "100%"),
#     style   = "font-family:IBM Plex Sans")
# }

numericInputIcon_value <- function(id){
  span(shinyWidgets::autonumericInput(
    inputId = id,
    label = NULL,
    value = 0,
    icon = list("$", NULL),
    width = "100%",
    decimalPlaces = 0,
    decimalCharacter = ".",
    digitGroupSeparator = ",",
    currencySymbol = "$",
    currencySymbolPlacement = "p",
    minimumValue = 0,
    maximumValue = 10^9
  ))}


# numericInputIcon_value <- function(id){
#   span(shinyWidgets::currencyInput(
#     inputId = id,
#     label   = NULL,
#     value   = 0,
#     format  = "dollar",
#     # icon    = list("$", NULL),
#     width   = "100%"),
#     style   = "font-family:IBM Plex Sans")
# }