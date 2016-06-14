% More secret sauce the \html tag has to be evaluated when you load the help
% file, rather than at package compilation, for the overriding to work.
\newcommand{\html}{\Sexpr[stage=render, results=text]{htmlhelp:::html_raw(#1)}}

\newcommand{\htmlfile}{\Sexpr[stage=render, results=text]{htmlhelp:::html_file(#1)}}

\newcommand{\stylesheet}{\Sexpr[stage=render, results=text]{htmlhelp:::change_stylesheet(#1)}}

\newcommand{\addstylesheet}{\Sexpr[stage=render, results=text]{htmlhelp:::add_stylesheet(#1)}}

\newcommand{\feedbackfooter}{\Sexpr[stage=render, results=text]{htmlhelp:::feedback_footer(#1, #2)}}