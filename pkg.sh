#!/bin/bash
pkg=elpa-mirror-1.2.2
mkdir $pkg
cp *.el *.js $pkg
if [[ `uname -s` == *Darwin* ]]; then
   COPYFILE_DISABLE="" tar cvf $pkg.tar $pkg/
else
   tar cvf $pkg.tar $pkg/
fi
rm -rf $pkg/
