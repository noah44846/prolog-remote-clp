// ---------------------------------------------------------------------------
// Copyright © 2024 Haute école d'ingénierie et d'architecture de Fribourg
// SPDX-License-Identifier: Apache-2.0
// ---------------------------------------------------------------------------
// Author : Jacques Supcik <jacques.supcik@hefr.ch>
// Date   : 23 February 2024
// ---------------------------------------------------------------------------
// Policy
// ---------------------------------------------------------------------------

//
// Declaration of Honor
//
#let declaration_of_honor(authors, lang) = {
  set heading(numbering: none)

  if lang == "en" [
    = Declaration of honor
    
    In this project, we used generative AI tools, namely Github Copilot @github_copilot for coding and Claude AI @claude_ai for paraphrasing. Copilot was employed as an advanced autocomplete feature, but it
    did not generate a significant portion of the project. Writefull, on the other hand, was
    utilised to enhance the clarity of this document by employing its paraphrasing and grammar checking capabilities.
  ] else if lang == "fr" [= Déclaration d'honneur]

  for author in authors {
    if lang == "en" [
      I, the undersigned #author.firstname #author.lastname, solemnly declare that the submitted work is the result of
      personal effort. I certify that I have not resorted to plagiarism or other forms of fraud. All
      sources of information used and author citations have been clearly acknowledged.
    ] else if lang == "fr" [
      Je, soussigné#if author.gender == "f" [e],
      #author.firstname #author.lastname, déclare sur l'honneur que le travail
      rendu est le fruit d'un travail personnel. Je certifie ne pas avoir eu
      recours au plagiat ou à toute autre forme de fraude.
      Toutes les sources d'information utilisées et les citations d'auteur ont
      été clairement mentionnées.
      #v(4cm)
    ]
  }
}
