#!/usr/bin/env perl
$latex = 'platex -synctex=1 -halt-on-error';
$bibtex = 'pbibtex';
$dvipdf = 'dvipdfmx %O -o %D %S';
$makeindex = 'mendex %O -o %D %S';

# Maximum number of compilation without changing tex file.
# Prevent compiling infinity times when error occured.
$max_repeat = 5;

# Generate dvi and then pdf.
$pdf_mode = 3;

# Use Preview.app to preview pdf.
$pdf_previewer = "open -ga Preview";

# Setting for using Preview.app.
$pdf_update_command = "open -ga Preview %S";

# Use Skim.app to preview pdf.
#$pdf_previewer = "open -ga /Applications/Skim.app";

# Setting for Skim.app to follow update automatically.
#$pvc_view_file_via_temporary = 0;

$out_dir = 'build';
