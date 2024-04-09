// ---------------------------------------------------------------------------
// Copyright © 2024 Haute école d'ingénierie et d'architecture de Fribourg
// SPDX-License-Identifier: Apache-2.0
// ---------------------------------------------------------------------------
// Author : Jacques Supcik <jacques.supcik@hefr.ch>
// Date   : 23 February 2024
// ---------------------------------------------------------------------------
// Typst funtions to format dates in french. Currently, Typst dooes not
// support date formating in languages other than english. This is a
// workaround to provide french date formatting. It might be obsolete
// in the future if Typst supports other languages.
// ---------------------------------------------------------------------------

#let french_months = (
  "janvier", 
  "février",
  "mars",
  "avril",
  "mai",
  "juin",
  "juillet",
  "août",
  "septembre",
  "octobre",
  "novembre",
  "décembre",
)

#let short_date(date) = {
  text(date.display(
    "[day padding:none].[month padding:none].[year padding:none repr:full]"
  ))
}

#let long_date(date) = {
  let m = french_months.at(date.month() - 1)
  text(date.display("[day padding:none] " + m + " [year padding:none repr:full]"))
}