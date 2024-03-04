\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{customTemplate}[20/02/2020: (HEIA-FR) School of Engineering Fribourg - LaTeX Template (unofficial)]

% This class is based on the article class
\LoadClass[11pt]{article}

\newif\ifen
\newif\iffr
\newif\ifde

% Default language is english
\entrue
\newcommand{\heianame}{School of Engineering and Architecture of Fribourg}
\newcommand{\faculty}{Department of Computer Science}
\newcommand{\authortxt}{Author}
\newcommand{\supervisortxt}{Supervisor}

\DeclareOption{fr}{
    \enfalse
    \frtrue
    \renewcommand{\heianame}{Haute {\'e}cole d'ing{\'e}nierie et d'architecture de Fribourg}
    \renewcommand{\faculty}{Fili{\`e}re informatique}
    \renewcommand{\authortxt}{Etudiants}
    \renewcommand{\supervisortxt}{Superviseurs}
}

\DeclareOption{de}{
    \enfalse
    \detrue
    
    \renewcommand{\heianame}{Hochshule für Technik und Architektur Freiburg}
    \renewcommand{\faculty}{Studiengang Informatik}
    \renewcommand{\authortxt}{Autor}
    \renewcommand{\supervisortxt}{Vorgesetzter}
}

% Passes all the received arguments to the article class
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}

% Ends options processing
\ProcessOptions \relax

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               EXTENSIONS                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% geometry – Flexible and complete interface to document dimensions
\RequirePackage[a4paper, margin=3cm, bottom=3cm, top=3cm, includefoot]{geometry}

%% inputenc – Accept different input encodings
\RequirePackage[utf8]{inputenc}

%% fontenc – Standard package for selecting font encodings
\RequirePackage[T1]{fontenc}

%% Language
\ifen \RequirePackage[english]{babel} \fi
\iffr \RequirePackage[french]{babel} \fi
\ifde \RequirePackage[ngerman]{babel} \fi

%% graphicx – Enhanced support for graphics
\RequirePackage{graphicx}

%% fancyhdr – Extensive control of page headers and footers in LaTeX2ε
\RequirePackage{fancyhdr}

%% minted – Highlighted source code for LaTeX
\RequirePackage{minted}

%% titling – Control over the typesetting of the \maketitle command
\RequirePackage{titling}

%% Lists
%%\RequirePackage{enumitem}
%% Indentation
%\usepackage{parskip}

%% csquotes – Context sensitive quotation facilities
\usepackage{csquotes}

%% BibLaTeX – Sophisticated Bibliographies in LaTeX
\RequirePackage[backend=biber]{biblatex}

%% tcolorbox – Coloured boxes, for LaTeX examples and theorems, etc
\RequirePackage[skins,minted,breakable]{tcolorbox}

%% hyperref – Extensive support for hypertext in LaTeX
\RequirePackage{hyperref}

%% soul - Hyphenation for letterspacing, underlining, and more 
\RequirePackage{soul}

%% todonotes - Marking things to do in a LaTeX document
\RequirePackage[colorinlistoftodos]{todonotes}

%% glossaries - Create glossaries and lists of acronyms
\RequirePackage[acronym]{glossaries}

%% command - Selectively include/exclude portions of text 
\RequirePackage{comment}

%% morewrites – Always room for a new write stream
% Required by to display todo table, see : https://tex.stackexchange.com/questions/289734/special-package-combination-gives-no-room-for-new-write
\RequirePackage{morewrites}

%% emptypage – Make empty pages really empty
\RequirePackage{emptypage}

%% abstract – Control the typesetting of the abstract environment
\RequirePackage{abstract}

%% tabularx – Tabulars with adjustable-width columns
\RequirePackage{tabularx}

%% gensymb – Generic symbols for both text and math mode
\RequirePackage{gensymb}

%% xifthen – Extended conditional commands
\RequirePackage{xifthen}

%% xstring – Extended string commands
\RequirePackage{xstring}

%% svg - SVG handeling
\RequirePackage[svgpath=images/]{svg}

%% rotating - be able to rotate a figure
\RequirePackage{rotating}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               HEADERS & FOOTERS                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Remove paragraph indentation
\setlength{\parindent}{0pt}

\def\@logolong{\includegraphics[height=2cm]{images/logos/logo_hta}}
\def\@logoshort{\includegraphics[height=1.5cm]{images/logos/logo_hta_short}}

\fancypagestyle{firstpage}{
    \fancyhf{}
}

\let\Oldpart\part
\newcommand{\parttitle}{}
\renewcommand{\part}[1]{\Oldpart{#1}\renewcommand{\parttitle}{#1}}


\fancypagestyle{followingpage}{
    \fancyhf{}
    \fancyhead[RE,LO]{\@title}
    \fancyhead[LE,RO]{\parttitle}
    \fancyfoot[C]{\thepage}
    \renewcommand{\headrulewidth}{0.0pt}
    \renewcommand{\footrulewidth}{0pt}
}

\fancypagestyle{fullheader}{
    \fancyhf{}
    \setlength{\headheight}{60pt}
    \renewcommand{\headrulewidth}{1.0pt}
    \fancyhead[R]{\@subtitle}
    \fancyhead[C]{\includegraphics[height=40pt]{images/logos/logo_hta_short}}
    \ifthenelse{\equal{\@studb}{\empty}}
    {
        \fancyhead[L]{\@studa{}}
    }
    {
        \ifthenelse{\equal{\@studc}{\empty}}
        {
            \fancyhead[L]{\@studa{} \& \@studb}
        }{
            \fancyhead[L]{\@studa{} \& \@studb \& \@studc}
        }
    }
    \fancyfoot[C]{\thepage}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               CUSTOM COMMANDS                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Graphviz ! Do not put empty line in dot script ! 
% \graphviz{FILENAME}{GRAPHVIZ_CODE}{FIGURE_DIMENSION}{CAPTION}
\newcommand{\graphviz}[4]{
    \IfFileExists{#1}{}{\immediate\write18{echo #2 | dot -o"#1.png" -Tpng}}
    \begin{figure}[H]
        \centering
        \includegraphics[#3]{#1.png}
        \caption{#4}\label{#1}
    \end{figure}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Minted
% \begin{code}{LANGUAGE}{TITLE}

% Remove space before and after environment
\BeforeBeginEnvironment{code}{\vspace{0.8em}}
\AfterEndEnvironment{code}{\vspace{0.8em}}

% Remove space before tcolorbox
\setlength{\fboxsep}{0pt}

% Define background and frame color
\definecolor{codeBackgroundColor}{rgb}{0.95,0.95,0.95}
\definecolor{codeFrameColor}{rgb}{0.52,0.54,0.66}

\newtcblisting{code}[3][]{%
    enhanced,
    breakable,% Allow page breaks
    listing engine=minted,
    listing only,#1, title=#3, minted language=#2,
    coltitle=codeBackgroundColor,           % Title color
    colbacktitle=codeFrameColor,      % Title background color
    colframe=codeFrameColor,
    fonttitle=\ttfamily\footnotesize,
    minted options={
        fontfamily=tt,
        linenos=true,
        numberblanklines=true,
        numbersep=12pt,
        numbersep=5pt,
        gobble=0,
        frame=leftline,
        framesep=2mm,
        funcnamehighlighting=true,
        tabsize=4,
        obeytabs=false,
        mathescape=false
        samepage=false,
        showspaces=false,
        showtabs =false,
        texcl=false,
        baselinestretch=1.2,
        fontsize=\footnotesize,
        breaklines=true,
        breakanywhere=true,
        escapeinside=!!,   
    }
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% !! NOT WORKING !!
% Figure
%\img{

% save the meaning of \includegraphics
%\LetLtxMacro\latexincludegraphics\includegraphics

%\renewcommand{\includegraphics}[4][]{%
%    \begin{figure}[H]
%        \centering
%        \latexincludegraphics[#1]{#2}
%         \ifx&#3&
%         \else
%            \caption{fig:#3}
%            \ifx&#4&
%            \else
%                \label{fig:#4}
%            \fi
%        \fi
%    \end{figure}
%}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Figure
% \figure{FILENAME}{FIGURE_DIMENSION}{CAPTION}{LABEL}
\graphicspath{ {images/} }
\newcommand{\myFigure}[4]{
    \begin{figure}[H]
        \centering
        \IfEndWith{#1}{.svg}{\includesvg[#2]{#1}}{\includegraphics[#2]{#1}}
        \caption{#3}\label{#4}
    \end{figure}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               CUSTOMIZATION                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Abstract
\setlength{\abstitleskip}{2em}
\renewcommand{\abstractnamefont}{\Large\bfseries}
\renewcommand{\abstracttextfont}{\normalfont}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Define default value if "variable" are not defined by user
\def\@course{\empty}
\def\revision{\empty} %% If no revision
\def\@lastReleaseDate{\empty} %% If no revision
\def\@supervisora{\empty}
\def\@supervisorb{\empty}
\def\@studa{\empty}
\def\@studb{\empty}
\def\@studc{\empty}

\newcommand*{\subtitle}[1]{\gdef\@subtitle{#1}}
\newcommand*{\course}[1]{\gdef\@course{#1}}
\newcommand*{\supervisors}[2]{\gdef\@supervisora{#1}\gdef\@supervisorb{#2}}
\newcommand*{\students}[3]{\gdef\@studa{#1}\gdef\@studb{#2}\gdef\@studc{#3}}
\newcommand*{\updateRevision}[1]{\gdef\revision{#1}}
\newcommand*{\lastReleaseDate}[1]{\gdef\@lastReleaseDate{#1}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Title page
\newcommand*{\maketitlepage}{
    \begin{titlepage}
        \begin{center}
        \includegraphics[height=2cm]{images/logos/logo_hta}
            \vfill{}
            \hrule
            \vspace{3em}
            {\huge{\textbf{\@title}} \par}
            \vspace{3em}
            \hrule
            \vspace{2em}
            \ifthenelse{\equal{\@subtitle}{\empty}}{}{ {\Large\@subtitle \par}}
            \vspace{.5em}

            
            \ifthenelse{\equal{\@course}{\empty}}{}{{\Large\textsc\@course \par}}
            
            \vfill{}
            \ifthenelse{\equal{\revision}{\empty}}{}{{\Large V.\revision \par}}
            \vfill{}
            
            {\Large{\heianame} \par}
            {\Large{\textbf{\faculty}} \par}
            {\Large\@lastReleaseDate \par}
            
            \vfill{}
        \end{center}

        
        \ifx\@supervisora\empty
            %{\Large{\textbf{\authortxt}} \par}
            %{\Large{\@studa} \par}
            %{\Large{\@studb} \par}
            aa
        \else
            \begin{minipage}[t]{0.5\textwidth}
            \raggedright
            {\Large{\textbf{\authortxt}}\par}
            \Large{\@studa} \\
            \ifthenelse{\equal{\@studb}{\empty}}{}{
                \Large{\@studb}\\
                \Large{\@studc}
            }             
            \end{minipage}
            %{\Large{\@studa} \& \Large{\@studb}}
            \hfill{}
            \begin{minipage}[t]{0.5\textwidth}
            \raggedleft
            {\Large{\textbf{\supervisortxt}}\par}
            \Large{\@supervisora}\\
            \Large{\@supervisorb}
            \end{minipage}
            %{\Large{\@supervisora} \& \Large{\@supervisorb}\par}
        \fi

        %\vfill{}
    \end{titlepage}
}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Page style
\newcommand*{\fullheader}{
    \pagestyle{fullheader}
}

\newcommand*{\smallheader}{
    \pagestyle{followingpage}
}

\newcommand*{\noheader}{
    \fancyhf{}
    \fancyfoot[C]{\thepage}
}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Table of content
\newcommand{\maketableofcontent}{
    \cleardoublepage
    \pagenumbering{roman}
    \tableofcontents
        \cleardoublepage

}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Table of content + figure + table
\newcommand{\maketableofcontentfiguretables}{
    \cleardoublepage
    \pagenumbering{roman}
    \tableofcontents
    \newpage
    \listoffigures
    \newpage
    \listoftables
        \cleardoublepage

}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Revision table
%% Inspired from https://tex.stackexchange.com/questions/137184/how-to-specify-a-date-and-then-use-it-with-a-time-format-defined-using-the-packa
%% and https://tex.stackexchange.com/questions/175568/build-tabular-content-via-foreach
\newcommand{\makerevisiontable}{
    \ifthenelse{\equal{\revision}{\empty}}{}{
        \vfill{}
        \begin{table}[b]
        \section*{Document version}
            \def\arraystretch{1.8}
            \begin{tabularx}{\textwidth}{|r|l|X|}
                \hline
                \textbf{N\degree{} revision} & \textbf{Date} & \textbf{Description} \\
                \hline
                \PrintTableTokens    
                \hline
            \end{tabularx}     
        \end{table}
        \thispagestyle{empty}
    }
}

\newcommand*{\MyTempTableTokens}{}%
\makeatletter
    \newtoks\@tabtoks
    %%% assignments to \@tabtoks must be global, because they are done in \foreach
    \newcommand\AddTableTokens[1]{\global\@tabtoks\expandafter{\the\@tabtoks#1}}
    \newcommand\eAddTableTokens[1]{%
      \protected@edef\MyTempTableTokens{#1}%
      \expandafter\AddTableTokens\expandafter{\MyTempTableTokens}%
    }
    %%% variable should always be operated on always locally or always globally
    \newcommand*\ResetTableTokens{\global\@tabtoks{}}
    \newcommand*\PrintTableTokens{\the\@tabtoks}
\makeatother

\newcounter{RowCounter}
\newcommand{\AddRevision}[3]{%
    \updateRevision{#1}
    \lastReleaseDate{#2}
    \stepcounter{RowCounter}%
    \eAddTableTokens{#1 & #2 & #3\\}%
}%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\endinput


