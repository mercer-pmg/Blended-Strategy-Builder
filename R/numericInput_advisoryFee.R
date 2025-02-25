numericInput_advisoryFee <- function(){
     
     span(shinyWidgets::autonumericInput(
          inputId = "advisory_fee",
          label   = "Advisory fee",
          value   = 0,
          width = "100%",
          decimalPlaces = 2,
          decimalCharacter = ".",
          digitGroupSeparator = ",",
          currencySymbol = "%",
          currencySymbolPlacement = "s",
          minimumValue = 0,
          maximumValue = 100),
          style   = "font-family:IBM Plex Sans")
     
}