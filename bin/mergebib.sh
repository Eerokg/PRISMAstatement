#!/bin/bash

#cp vignettes/icd.bib vignettes/all.bib
#cat vignettes/other.bib >> vignettes/all.bib
#cat vignettes/gplv3.bib >> vignettes/all.bib

command -v bibtool >/dev/null 2>&1 || {
  echo >&2 "I require bibtool but it's not installed. Aborting."
  exit 1
}

if [ -d vignettes ]; then
  pushd vignettes
  trap popd EXIT
fi

bibtool -s -r biblatex r.bib PRISMA.bib -- preserve.key.case=on > all.bib

# make sure there are no unescaped underscores in the URLs:
if grep '[^\]_' all.bib | grep -v ^@
then
  echo "Underscores found in all.bib entries"
  echo "attempting to fix"
  for run in {1..25}
  do
    perl -pi -e 's/(url.*[^\\])_(.*)/\1\\_\2/g' all.bib
  done
fi
