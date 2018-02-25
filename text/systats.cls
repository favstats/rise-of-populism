%==========================================================%
% @author: Simon Roth - www.systats.com 
%       Universit?t Stuttgart    
% 	  Institut für Sozialwissenschaften
%       creation date: 2017/02/24, version: 0.1  
%       last update: -        
%==========================================================%


\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{systats}[2017/02/24 v0.1 Multipurpose Sytle for Scientific Paper]
\LoadClass[a4paper, 12pt, oneside]{article}

\RequirePackage[utf8]{inputenc}
\RequirePackage{amsmath}
\RequirePackage{amsfonts}
\RequirePackage{amssymb}
\RequirePackage{graphicx}
\RequirePackage{float}
\RequirePackage{verbatim}
\RequirePackage{subcaption}
\RequirePackage{caption}
\captionsetup{labelfont=bf}
\RequirePackage{listings}
\RequirePackage{url}
\RequirePackage{apacite}
\RequirePackage{setspace}
\RequirePackage{microtype}
\RequirePackage{booktabs}
\RequirePackage{lmodern}
\RequirePackage{url}

\RequirePackage{marginnote}
\newcommand{\sidenote}[1]{{\marginpar{\raggedright\footnotesize
\setstretch{1.025}%
\textit{#1}}}}


\RequirePackage{xcolor}
\RequirePackage[document]{ragged2e}


% FUN package
\RequirePackage{kantlipsum}

% hypenation?
\hyphenation{every-where}

%%% drafting package
\RequirePackage{color}

\newenvironment{drafting}[1]{%
    \leavevmode\color{#1}\ignorespaces%
}{%
}%

%%% citation
%\RequirePackage[round]{natbib}
%\bibliographystyle{apalike} % plain
%  \bibliographystyle{master}
%%% color
\RequirePackage{color}
\definecolor{special}{rgb}{0.3, 0.3, 0.3}



\RequirePackage{hyperref}
\hypersetup{
	linktocpage,
    colorlinks,
    citecolor=special,
    filecolor=black,
    linkcolor=black,
    urlcolor=special,
}

\expandafter\def\expandafter\normalsize\expandafter{%
    \normalsize
    \setlength\abovedisplayskip{0pt}
    \setlength\belowdisplayskip{3pt}
    \setlength\abovedisplayshortskip{0pt}
    \setlength\belowdisplayshortskip{3pt}
}



