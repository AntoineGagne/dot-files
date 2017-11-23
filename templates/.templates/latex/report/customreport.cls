%!TEX encoding = UTF-8 Unicode

% --------------------------------------------------------------------------------------
% ----------------------------------- Identification -----------------------------------
% --------------------------------------------------------------------------------------
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{customreport}[11/09/2017 - Rapport - Université Laval]


% --------------------------------------------------------------------------------------
% ------------------------------------ Initial code ------------------------------------
% --------------------------------------------------------------------------------------

% Flags logiques permettant l'insertion des listes des figures et des tableaux
\RequirePackage{ifthen}
\newboolean{InsertFiguresList}
\newboolean{InsertTablesList}
\newboolean{InsertListOfListings}


% --------------------------------------------------------------------------------------
% ------------------------------- Declaration of options -------------------------------
% --------------------------------------------------------------------------------------
\DeclareOption{lof}{\setboolean{InsertFiguresList}{true}}
\DeclareOption{lot}{\setboolean{InsertTablesList}{true}}
\DeclareOption{lol}{\setboolean{InsertListOfListings}{true}}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{report}}


% --------------------------------------------------------------------------------------
% -------------------------------- Execution of options --------------------------------
% --------------------------------------------------------------------------------------
\ProcessOptions\relax


% --------------------------------------------------------------------------------------
% ----------------------------------- Package loading ----------------------------------
% --------------------------------------------------------------------------------------
\LoadClass{report}

\RequirePackage[T1]{fontenc}
\RequirePackage[french]{babel}
\RequirePackage[hyphens]{url}
\RequirePackage{amstext}                                  % For \text
\RequirePackage{array}
\RequirePackage[backend=biber,%
                style=nature,%
                sortcites=true,%
                sorting=nty,%
                isbn=true,%
                url=true,%
                urldate=long,%
               ]{biblatex} % For the bibliography
\RequirePackage{caption}
\RequirePackage{colortbl}
\RequirePackage{etoolbox}
\RequirePackage{fancyhdr}                                 % To have header and footer
\RequirePackage{geometry}                                 % To be able to redefine page dimensions
\RequirePackage{graphicx}
\RequirePackage{hyperref}
\RequirePackage{listings}
\RequirePackage{lmodern}
\RequirePackage{longtable}
\RequirePackage{mathptmx}
\RequirePackage{ragged2e}
\RequirePackage{rotating}
\RequirePackage{tabularx}
\RequirePackage{textcomp}                                 % For \textcelsius, \textmu, \textohm
\RequirePackage{titlesec}
\RequirePackage{xcolor}
\RequirePackage{xparse}                                   % A better way to define environments


%--------------------------------------------------------------------------------------
%-------------------------------------- Main code -------------------------------------
%--------------------------------------------------------------------------------------

% Definition of the `hyperref` package parameters
\hypersetup{plainpages=false,%
            breaklinks=false,%
            colorlinks=true}

% Definition of the page's dimensions (package `geometry`)
\geometry{letterpaper,%
          centering,%
          hmargin={3cm,3cm},%
          vmargin={3cm,3cm},%
          heightrounded,%
          includehead}

% Set the interline size
\renewcommand{\baselinestretch}{1.5} 

% Package `caption` used for:
%    - Redefinition of the separation character in the legends as `babel` defines it
%      because `caption` (or `float`) redefines it as ':'
%    - Uniformizes the legends between `tables`, `longtables` and `supertabular`
%    - Specifies that the legend of the tables will be placed on top.
%      Switches the spacing dimensions (before and after the legend)
\DeclareCaptionLabelSeparator{as-Babel-french}{\space\textendash\space}
\captionsetup{margin=0.1\textwidth,labelsep=as-Babel-french}
\captionsetup[table]{position=top}

% Switch the first name and the last name
\DeclareNameAlias{sortname}{last-first}
\DeclareNameAlias{default}{last-first}

% Redefinition of the `quotation` environment
\let\origquotation\quotation%
\let\endorigquotation\endquotation%

% Definition of a quotify
\DeclareDocumentCommand{\quotify}{m}{%
    \og{}#1\fg{}%
}

% Header and footer by default
\pagestyle{fancy}
\fancyhf{}
\fancyhead[L, LO]{\theclassnumber}
\fancyhead[R, RO]{\thesubject}
\fancyfoot[C, CO]{\leftmark}
\fancyfoot[R, RO]{\thepage}
\renewcommand{\headrulewidth}{1pt}
\renewcommand{\footrulewidth}{1pt}


% Definition of the default graphics path
\graphicspath{{./fig/}}

% Title page definition
\renewcommand{\maketitle}{%
    \thispagestyle{empty}%
    \pagenumbering{roman}%

    \begin{titlepage}%
        \setcounter{page}{999}%                      % Associate the number '0' to the title page

        \begin{minipage}{.45\linewidth}
            \begin{flushleft}
                \includegraphics[width=15em]{ul_logo} \\
                \textsc{Facult\'{e} des sciences et g\'{e}nie}
            \end{flushleft}
        \end{minipage}%
%
        \hfill
%
        \begin{minipage}{.45\linewidth}
            \begin{flushright}
                \@author
            \end{flushright}
        \end{minipage}%
%
        \vspace*{\stretch{1}}
%
        \begin{center}
            \rule{\textwidth}{1.6pt}\vspace*{-\baselineskip}\vspace*{2pt} % Thick horizontal line
            \rule{\textwidth}{0.4pt}\\[\baselineskip] % Thin horizontal line
%
            \parbox{\textwidth}{\centering\large\textsc{\@title}} \\[5ex]
            \parbox{\textwidth}{\centering\normalsize\textsc{\thesubject}} \\
%
            \rule{\textwidth}{0.4pt}\vspace*{-\baselineskip}\vspace{3.2pt} % Thin horizontal line
            \rule{\textwidth}{1.6pt}\\[\baselineskip] % Thick horizontal line
%
            \vspace*{\stretch{2}}
%
            \parbox{\textwidth}{\centering\bfseries Destinataire} \\
            \vspace*{0.5cm}
            \parbox{\textwidth}{\centering \emph{\therecipient}}
        \end{center}
%
        \vspace*{\stretch{1}}
%
        \begin{center}
            \today
        \end{center}
%
        \clearpage%
        \let\maketitle\relax%
    \end{titlepage}
}

% Definition of the functions to set the title page parameters
\def\abstract#1{\def\theabstract{#1}}
\def\thanks#1{\def\thethanks{#1}}
\def\recipient#1{\def\therecipient{#1}}
\def\classnumber#1{\def\theclassnumber{#1}}
\def\classtitle#1{\def\theclasstitle{#1}}
\def\subject#1{\def\thesubject{#1}}

% Definition of the default title page parameters
\def\theabstract{R\'{e}sum\'{e}}
\def\thethanks{Remerciements}
\def\therecipient{Destinataire}
\def\theclassnumber{GLO-4000}
\def\thesubject{Interface personne-machine}

% Definition of the function to create the abstract
\def\makeabstract{%
    \begin{abstract}%
        \theabstract%
    \end{abstract}
}

% Defines an acknowledgments environment
\def\acknowledgmentsname#1{\def\theacknowledgmentsname{#1}}
\def\theacknowledgmentsname{Remerciements}
\newenvironment{acknowledgments}%
        {%
            \renewcommand{\abstractname}{\theacknowledgmentsname}%
            \begin{abstract}%
        }%
        {%
            \end{abstract}%
            \clearpage%
        }%

\def\makethanks{%
    \begin{acknowledgments}%
        \thethanks%
    \end{acknowledgments}
}

% Definition of the maximum hierarchic level covered by the table of contents
\setcounter{tocdepth}{3}            % default = 2

% Definition of the maximum hierarchic level for elements having a numerotation
\setcounter{secnumdepth}{3}         % default = 2

% Defines the highlighting for the programming languages
\lstdefinestyle{custom}{%
    basicstyle=\footnotesize\ttfamily,
    belowcaptionskip=1\baselineskip,
    breaklines=true,
    captionpos=b,
    commentstyle=\ttfamily\color{gray},
    identifierstyle=\color{blue},
    keywordstyle=\bfseries\color{green!40!black},
    language=Python,
    numbers=left,
    numbersep=0.5pt,
    showstringspaces=false,
    showtabs=false,
    stepnumber=1,
    stringstyle=\color{orange},
    tabsize=4,
    xleftmargin=\parindent,
}

% Defines the UTF8 characters and their associated values
\lstset{literate=
  {á}{{\'a}}1 {é}{{\'e}}1 {í}{{\'i}}1 {ó}{{\'o}}1 {ú}{{\'u}}1
  {Á}{{\'A}}1 {É}{{\'E}}1 {Í}{{\'I}}1 {Ó}{{\'O}}1 {Ú}{{\'U}}1
  {à}{{\`a}}1 {è}{{\`e}}1 {ì}{{\`i}}1 {ò}{{\`o}}1 {ù}{{\`u}}1
  {À}{{\`A}}1 {È}{{\'E}}1 {Ì}{{\`I}}1 {Ò}{{\`O}}1 {Ù}{{\`U}}1
  {ä}{{\"a}}1 {ë}{{\"e}}1 {ï}{{\"i}}1 {ö}{{\"o}}1 {ü}{{\"u}}1
  {Ä}{{\"A}}1 {Ë}{{\"E}}1 {Ï}{{\"I}}1 {Ö}{{\"O}}1 {Ü}{{\"U}}1
  {â}{{\^a}}1 {ê}{{\^e}}1 {î}{{\^i}}1 {ô}{{\^o}}1 {û}{{\^u}}1
  {Â}{{\^A}}1 {Ê}{{\^E}}1 {Î}{{\^I}}1 {Ô}{{\^O}}1 {Û}{{\^U}}1
  {œ}{{\oe}}1 {Œ}{{\OE}}1 {æ}{{\ae}}1 {Æ}{{\AE}}1 {ß}{{\ss}}1
  {ű}{{\H{u}}}1 {Ű}{{\H{U}}}1 {ő}{{\H{o}}}1 {Ő}{{\H{O}}}1
  {ç}{{\c c}}1 {Ç}{{\c C}}1 {ø}{{\o}}1 {å}{{\r a}}1 {Å}{{\r A}}1
  {€}{{\EUR}}1 {£}{{\pounds}}1
}

% Use the custom style defined earlier
\lstset{escapechar=@, style=custom}

% Changes the name of the list of listings and the listings caption
\renewcommand\lstlistlistingname{Liste des programmes}
\renewcommand\lstlistingname{Programme}

% Changes the way the chapter are displayed
\titleformat{\chapter}[display]
  {\bfseries\Large}
  {\filright\MakeUppercase{\chaptertitlename} \Huge\thechapter}
  {1ex}
  {\titlerule\vspace{1ex}\filleft}
  [\vspace{1ex}\titlerule]

% Definition of `hyperref` parameters
\AtBeginDocument{%
   \hypersetup{pdftitle={\@title},%
               pdfsubject={\thesubject},%
               pdfauthor={\@author},%
               pdfcreator={pdflatex},%
               pdfkeywords={projet~de~session,~universit\'{e}}}%
}

% Selection of `babel` language by default
\AtBeginDocument{%
   \selectlanguage{french}%
}

% Changes words used by `babel` by default (figure and table)
\AtBeginDocument{%
   \renewcommand{\figurename}{\textsc{Figure}}%
   \renewcommand{\tablename}{\textsc{Tableau}}%
}

% Generates title page
\AtBeginDocument{%
   \maketitle
}

% Table of contents, list of figures and list of tables generation
\AtBeginDocument{%
   \tableofcontents
   \ifthenelse{\boolean{InsertFiguresList}}%
              {\clearpage%
               \listoffigures%
               \addcontentsline{toc}{chapter}{\listfigurename}}{}
   \ifthenelse{\boolean{InsertTablesList}}%
              {\clearpage%
               \listoftables%
               \addcontentsline{toc}{chapter}{\listtablename}}{}
   \ifthenelse{\boolean{InsertListOfListings}}%
              {\clearpage%
               \lstlistoflistings%
               \addcontentsline{toc}{chapter}{\lstlistlistingname}}{}
}

% `page` counter reinitialization (value and style)
\AtBeginDocument{%
   \cleardoublepage%
   \setcounter{page}{1}%
   \pagenumbering{arabic}%
}

% Redefine the appendix page numbering format
\let\origappendix\appendix
\renewcommand\appendix{%
    \origappendix%
    \pretocmd{\chapter}{%
      \clearpage
      \pagenumbering{arabic}%
      \renewcommand*{\thepage}{\thechapter\arabic{page}}%
    }{}{}
}
