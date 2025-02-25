numericInput_totalValue <- function(){
     
     span(shinyWidgets::autonumericInput(
          inputId                 = "total_value",
          label                   = "Total value to allocate",
          value                   = 10^6,
          icon                    = list("$", NULL),
          width                   = "100%",
          decimalPlaces           = 0,
          decimalCharacter        = ".",
          digitGroupSeparator     = ",",
          currencySymbol          = "$",
          currencySymbolPlacement = "p",
          minimumValue            = 0,
          maximumValue            = 10^9),
          style                   = "font-family:IBM Plex Sans")
}