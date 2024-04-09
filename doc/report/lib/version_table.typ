// ---------------------------------------------------------------------------
// Copyright © 2024 Haute école d'ingénierie et d'architecture de Fribourg
// SPDX-License-Identifier: Apache-2.0
// ---------------------------------------------------------------------------
// Author : Jacques Supcik <jacques.supcik@hefr.ch>
// Date   : 23 February 2024
// ---------------------------------------------------------------------------
// Version table
// ---------------------------------------------------------------------------

#import "@preview/tablex:0.0.8": tablex, hlinex, vlinex
#import "ch_en_date.typ": short_date

//
// Produce a  version table. Note that the versions array needs to
// be ordered from the oldest to the newest.
//
#import "lang_str.typ": lang_str

#let version_table(reverse: true, versions, lang) = {
  if reverse {
    versions = versions.rev()
  }
  heading(numbering: none, outlined: false, lang_str("version_table_title", lang))
  set par(justify: false)
  tablex(
    columns: (4.5em, 6.5em, 1fr), // 4 columns
    rows: auto, // at least 1 row of auto size
    align: (center, center, center), auto-lines: false, (), vlinex(),
    vlinex(), vlinex(), (), [*Version*],
    [*Date*], [*Modifications*], hlinex(),
    ..(..versions.map(v => {
      (v.version, short_date(v.date), v.changes)
    })).flatten(),
  )
}