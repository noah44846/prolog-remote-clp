// ---------------------------------------------------------------------------
// Copyright © 2024 Haute école d'ingénierie et d'architecture de Fribourg
// SPDX-License-Identifier: Apache-2.0
// ---------------------------------------------------------------------------
// Author : Jacques Supcik <jacques.supcik@hefr.ch>
// Date   : 23 February 2024
// ---------------------------------------------------------------------------
// Template for student reports at the Haute école d'ingénierie et
// d'architecture de Fribourg
// ---------------------------------------------------------------------------

#import "@preview/tablex:0.0.8": tablex, hlinex, vlinex
#import "version_table.typ": version_table
#import "policy.typ": declaration_of_honor
#import "ch_en_date.typ": short_date, long_date
#import "lang_str.typ": lang_str

#let heading_text(..content) = text(font: "New Computer Modern", ..content)

#let code_block(code) = block(
  fill: luma(250),
  inset: 10pt,
  stroke: 1pt + luma(220),
  radius: 5pt,
  code,
)

//
// Report
//
#let report(
  lang: "fr",
  title: none,
  subtitle: none,
  theme_color: rgb(195, 40, 35, 255),
  type: none,
  year: none,
  location: [Fribourg],
  versions: (),
  departement: none,
  profile: none,
  authors: (),
  supervisors: (),
  supervisors_label: [Superviseurs],
  experts: (),
  experts_label: [Experts],
  clients: (),
  clients_label: [Mandants],
  doc,
) = {
  set text(font: "New Computer Modern", size: 12pt)
  set heading(numbering: "1.")
  set par(justify: true)

  // Heading formating for level 1
  show heading.where(level: 1): it => {
    if (it.numbering != none) {
      block(
        fill: theme_color.lighten(80%), width: 100%, inset: 18pt, {
          heading_text(
            weight: "medium", size: 18pt, stack(
              dir: ttb, [
                #it.supplement #numbering("1", ..counter(heading).at(it.location()))
              ],
              v(5mm),
              heading_text(weight: "semibold", size: 20pt, it.body),
            ),
          )
        },
      )
      v(20pt)
    } else {
      heading_text(weight: "semibold", size: 18pt, it)
      v(12pt)
    }
  }

  // Heading formating for all levels
  show heading: it => {
    if it.level <= 1 {
      pagebreak(weak: true)
      it
    } else {
      block(inset: (y: 10pt), heading_text(it))
    }
  }

  // Title page : Logo header
  block(inset: (top: -30pt), image("img/logo-heiafr.svg"))
  v(1fr)

  // Document type
  set align(center)
  block(
    heading_text(size: 17pt, weight: "semibold", departement),
  )
  if profile != none {
    block(heading_text(size: 15pt, weight: "regular", profile))
  }
  v(1fr)

  block(heading_text(size: 18pt, weight: "medium", type))
  block(heading_text(size: 16pt, weight: "thin", year))
  v(1fr)

  // Document title and subtitle in a red block
  {
    set text(fill: white, stretch: 90%)
    set par(justify: false)
    block(fill: theme_color, width: 100%, inset: 10mm, {
      heading_text(weight: "bold", size: 28pt, title)
      if subtitle != none {
        parbreak()
        heading_text(weight: "medium", size: 18pt, subtitle)
      }
    })
  }
  v(1fr)

  // Author
  {
    set text(size: 16pt)
    set align(center)
    let count = authors.len()
    let ncols = calc.min(count, 3)
    grid(
      columns: (1fr,) * ncols, row-gutter: 24pt, ..authors.map(
        author => [
          #text(
            size: 16pt,
            weight: "semibold",
            heading_text(author.firstname + " " + author.lastname)) 
        ],
      ),
    )
    v(1fr)

    // Clients (if dedfined)
    if clients.len() > 0 {
      parbreak()
      grid(
        columns: (1fr, 1fr),
        row-gutter: 6pt,
        align(right, heading_text(emph(clients_label) + [: #h(5mm)])),
        align(left, heading_text(clients.join(linebreak()))),
      )
    }

    // Supervisors
    parbreak()
    grid(
      columns: (1fr, 1fr),
      row-gutter: 6pt,
      align(right, heading_text(supervisors_label + [: #h(5mm)])),
      align(left, heading_text(supervisors.join(linebreak()))),
    )

    // Experts (if defined)
    if experts.len() > 0 {
      parbreak()
      grid(
        
        columns: (1fr, 1fr),
        row-gutter: 6pt,
        align(right, heading_text(emph(experts_label) + [: #h(5mm)])),
        align(left, heading_text(experts.join(linebreak()))),
      )
    }
    v(2fr)
  }
  // Title page : Footer
  tablex(
    columns: (9em, 1fr, 9em),
    align: (left + horizon, center + horizon, right + horizon),
    auto-lines: false,
    image("img/logo-isc.svg", width: 22mm),
    block(
      heading_text(size: 14pt, location + [, ] + long_date(versions.last().date))
    ) + 
    block(
      heading_text([Version ] + versions.last().version), 
    ),
    image("img/logo-hesso.svg", width: 35mm),
  )
  set align(left)

  // Table of version
  pagebreak()
  set page(numbering: "i")
  version_table(versions, lang)

  // Table of content
  show outline.entry.where(
      level: 1
  ): it => {
    text(size: 12pt, it)
  }
  outline(indent: auto)
  pagebreak()

  // Main content
  set page(header: [
    #set text(8pt)
    #{subtitle}
    #h(1fr) #{text(weight: "semibold", authors.first().firstname + " " + authors.first().lastname)}
    #h(1fr) #{text(weight: "semibold", title)}
    #line(length: 100%, stroke: 0.5pt)
  ])

  // Footer
  set page(numbering: "1 / 1")
  counter(page).update(1)

  // Style
  show link: underline

  // Add custom syntax highlighting
  set raw(syntaxes: "syntaxes/SWI-Prolog.sublime-syntax") 

  doc

  // Declaration of honor
  declaration_of_honor(authors, lang)

  // Bibliography
  bibliography(style: "association-for-computing-machinery", "../bibliography.yml")

  // Table of figures
  outline(title: lang_str("fig_table_title", lang), target: figure.where(kind: image))
}