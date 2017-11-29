# vim: ft=perl

# See https://tex.stackexchange.com/questions/340276/making-latexmk-use-synctex-1-by-default-with-lualatex
push @extra_pdflatex_options, '-synctex=1';
# If zero, do NOT generate a pdf version of the document. If equal to 1,
# generate a pdf version of the document using pdflatex. If equal to 2,
# generate a pdf version of the document from the ps file, by using the command
# specified by the $ps2pdf variable. If equal to 3, generate a pdf version of
# the document from the dvi file, by using the command specified by the $dvipdf
# variable.
# Equivalent to the -pdf-, -pdf, -pdfdvi, -pdfps options.
$pdf_mode = 1;

# The LaTeX processing program in a version that makes a pdf file instead of a dvi file.
# 
# An example of the use of this variable is to arrange for lualatex, xelatex or
# some similar program to be used instead of pdflatex. Note that lualatex and
# xelatex only produce .pdf files (and not .dvi), so to use them you will also
# need to turn on production of .pdf files, and to turn off the production of
# .dvi (and .ps) files, either by command line options or by suitable settings
# in a configuration file. Thus to use lualatex, the following settings are
# appropriate:
#
# $pdflatex = "lualatex %O %S";
# $pdf_mode = 1; $postscript_mode = $dvi_mode = 0;
# To use xelatex, the corresponding settings are:
#
# $pdflatex = "xelatex %O %S";
# $pdf_mode = 1; $postscript_mode = $dvi_mode = 0;
# 
# Another use of the same variable is to add certain options to the command
# line for the program, e.g.,
#
$pdflatex = 'pdflatex --shell-escape %O %S';

# Switch(es) for the pdflatex program (specified in the variable $pdflatex when silent mode is on.
$latex_silent_switch = "-interaction=batchmode -c-style-errors";

# The command to invoke a pdf-previewer.
# NOTE: Normally you will want to have a previewer run detached, so
# that latexmk doesn’t wait for the previewer to terminate before continuing its
# work. So normally you should prefix the command by "start ", which flags to
# latexmk that it should do the detaching of the previewer itself (by whatever
# method is appropriate to the operating system). But sometimes letting latexmk
# do the detaching is not appropriate (for a variety of non-trivial reasons), so
# you should put the "start " bit in yourself, whenever it is needed.
my %application_chooser_command_by_operating_system_name = (
    'linux' => 'xdg-open',
    'MSWin32' => 'explorer',
    'darwin' => 'open'
);

$pdf_previewer = sprintf("start %s %%O %%S", $application_chooser_command_by_operating_system_name{"$^O"});
$pdf_update_method = 0;

# Whether to run silently. Setting $silent to 1 has the same effect as the
# -quiet of -silent options on the command line.
$silent = 1;

$sleep_time = 5;

# Taken from: http://ctan.mirror.rafal.ca/support/latexmk/example_rcfiles/pdflatexmkrc
# (Thu Nov 23 16:55:27 EST 2017)
# Use of glossaries, extra indexes, epstopdf, and other images conversions
# Thanks to Herb Schulz

# Custom dependency for glossary/glossaries package if you make custom
# glossaries you may have to add items to the @cus_dep_list and corresponding
# sub-routines
add_cus_dep('glo', 'gls', 0, 'makeglo2gls');
sub makeglo2gls {
    system("makeindex -s '$_[0]'.ist -t '$_[0]'.glg -o '$_[0]'.gls '$_[0]'.glo");
}
# The glossaries package, with the [acronym] option, produces a .acn file when
# processed with (xe/pdf)latex and then makeindex to process the .acn into .acr
# and finally runs of (xe/pdf)latex to read in the .acr file. Unfortunately the
# glossary package does just the reverse; i.e. (xe/pdf)latex processing
# produces a .acr files and makeindex then is used to convert the .acr file to
# a .acn file which is then ... . This dependency assumes the glossaries
# package.
add_cus_dep( 'acn', 'acr', 0, 'makeacn2acr' );
sub makeacn2acr {
    system( "makeindex -s \"$_[0].ist\" -t \"$_[0].alg\" -o \"$_[0].acr\" \"$_[0].acn\"" );
}
# for glossary package (Sigh...) --- they can co-exist!		
# add_cus_dep( 'acr', 'acn', 0, 'makeacr2acn' );
# sub makeacr2acn {
#     system( "makeindex -s \"$_[0].ist\" -t \"$_[0].alg\" -o \"$_[0].acn\" \"$_[0].acr\"" );
# }
# example of an added custom glossary type that is used in some of the
# glossary/glossaries example files: this is for the new glossary type command
# \newglossary[nlg]{notation}{not}{ntn}{Notation} from the glossaries package
# NOTE: the glossary package uses a very different command: the <in-ext> and
# <out-ext> are reversed in the calling sequence :-(
add_cus_dep( 'ntn', 'not', 0, 'makentn2not' );
sub makentn2not {
    system("makeindex -s \"$_[0].ist\" -t \"$_[0].nlg\" -o \"$_[0].not\" \"$_[0].ntn\"" );
}
# for the	glossary package (Sigh...) --- they can co-exist!
add_cus_dep( 'not', 'ntn', 0, 'makenot2ntn' );
sub makenot2ntn {
    system("makeindex -s \"$_[0].ist\" -t \"$_[0].nlg\" -o \"$_[0].ntn\" \"$_[0].not\"" );
}

# dependencies for custom indexes using the index package
# examples for sample.tex for index package:
 add_cus_dep( 'adx', 'and', 0, 'makeadx2and' );
sub makeadx2and {
    system( "makeindex -o \"$_[0].and\" \"$_[0].adx\"" );
}
add_cus_dep( 'ndx', 'nnd', 0, 'makendx2nnd' );
sub makendx2nnd {
    system( "makeindex -o \"$_[0].nnd\" \"$_[0].ndx\"" );
}
add_cus_dep( 'ldx', 'lnd', 0, 'makeldx2lnd' );
sub makeldx2lnd {
    system( "makeindex -o \"$_[0].lnd\" \"$_[0].ldx\"" );
}

# Custom dependency and function for nomencl package
add_cus_dep( 'nlo', 'nls', 0, 'makenlo2nls' );
sub makenlo2nls {
    system( "makeindex -s nomencl.ist -o \"$_[0].nls\" \"$_[0].nlo\"" );
}

# Custom dependency and function(s) for epstopdf package

# FOR USERS OF epstopf v1.4 and before: should also work with v1.5 and later
# NOTE: you may get extras runs if you use the .eps extension in the
# \includgraphics command deletes an outdated pdf-image, and triggers a
# pdflatex-run
# add_cus_dep( 'eps', 'pdf', 0, 'cus_dep_delete_dest' );

# FOR USERS OF epstopdf v1.5 and later only:
# load it as \usepackage[update,prepend]{epstopdf}
# detects an outdated pdf-image, and triggers a pdflatex-run
#add_cus_dep( 'eps', 'pdf', 0, 'cus_dep_require_primary_run' );

# Custom dependecy to convert tif to png
add_cus_dep( 'tif', 'png', 0, 'maketif2png' );
sub maketif2png {
    system( "convert \"$_[0].tif\" \"$_[0].png\"" );
}
