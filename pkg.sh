#!/bin/bash
pkg=elpa-mirror-2.1.0
mkdir $pkg
cp *.el *.js $pkg
if [[ `uname -s` == *Darwin* ]]; then
   COPYFILE_DISABLE="" tar cvf $pkg.tar $pkg/
else
   tar cvf $pkg.tar $pkg/
fi
rm -rf $pkg/
