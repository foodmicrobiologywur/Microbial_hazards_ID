### function to color based on "Original, in tab 3 and 4
render <- c(
  "function(data, type, row){",
  "  if(type === 'display'){",
  "    var color = /Recontamination$/.test(data) ? 'red' : (/recontamination$/.test(data) ? 'red' : 'black');",
  "    data = '<span style=\"color: ' + color + ';\">' + data + '</span>';",
  "  }",
  "  return data;",
  "}"
)