#!/bin/bash
# Build the working paper. Usage, from the project folder:
#   bash paper/build.sh mac     # Sabon etc. from /Library/Fonts (on the Mac only; keep that PDF on the Mac)
#   bash paper/build.sh draft   # free stand-in fonts; this copy goes to www/ for the app
# Run Rscript paper/make_paper.R first if the model or inputs changed.
set -e
mode=${1:-draft}
cd "$(dirname "$0")"
cp "fonts_${mode}.tex" fonts.tex
xelatex -interaction=nonstopmode -halt-on-error workingpaper.tex > /dev/null
bibtex workingpaper > /dev/null
xelatex -interaction=nonstopmode -halt-on-error workingpaper.tex > /dev/null
xelatex -interaction=nonstopmode -halt-on-error workingpaper.tex | grep -E "Warning: (Reference|Citation)|Output written" || true
if [ "$mode" = "draft" ]; then cp workingpaper.pdf ../www/working_paper.pdf; echo "Copied to www/working_paper.pdf"; fi
