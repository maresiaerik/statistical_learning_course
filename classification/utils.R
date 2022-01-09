specific_to_generic_postion_mapper = list(
  "CB" = "DEF",
  "RB" = "DEF",
  "LB" = "DEF",
  "LCB" = "DEF",
  "RCB" = "DEF",
  "CAM" = "MID",
  "CDM" = "MID",
  "RCM" = "MID",
  "RDM" = "MID",
  "CM" = "MID",
  "LAM" = "MID",
  "LCM" = "MID",
  "LDM" = "MID",
  "RWB" = "MID",
  "LWB" = "MID",
  "RAM" = "MID",
  "RM" = "MID",
  "LM" = "MID",
  "LF" = "ATT",
  "LS" = "ATT",
  "LW" = "ATT",
  "CF" = "ATT",
  "RF" = "ATT",
  "RS" = "ATT",
  "RW" = "ATT",
  "ST" = "ATT"
)

convert_string_currency_to_numeric = function(string_numeric) {
  multiplier_list = list("K" = 1000, "M" = 1000000)
  string_numeric = substr(string_numeric, 2, nchar(string_numeric))
  last_char = substr(string_numeric, nchar(string_numeric), nchar(string_numeric) + 1)
  value = substr(string_numeric, 1, nchar(string_numeric)-1)
  multiplier = multiplier_list[[last_char]]
  if (is.null(multiplier)) {
    value = string_numeric
    multiplier = 1
  }
  numeric_value = as.numeric(unlist(value))
  real_value = numeric_value * multiplier
  return (real_value / 1000000)
}

extract_first_player_position  = function(full_position_string) {
  sapply(
    strsplit(full_position_string, split = " "),
    getElement,
    1
  )
}