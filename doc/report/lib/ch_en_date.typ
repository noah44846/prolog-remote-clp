#let short_date(date) = {
  text(date.display(
    "[day padding:none].[month padding:none].[year padding:none repr:full]"
  ))
}

#let long_date(date) = {
  text(date.display("[day padding:none] [month padding:none repr:long] [year padding:none repr:full]"))
}
