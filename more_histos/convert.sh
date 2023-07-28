#!/bin/bash

# Convert .pdf into (transparent) .png.
# To make an opaque .png, add the -flatten keyword.
# provide the .pdf as argument, like: ./convert.sh <infile>.pdf.
# The output is <infile>.png.

if [[ "$(dpkg -s imagemagick | awk '/ok installed/')" == "" ]]; then
  echo -e "\nERROR: Conversion package not installed."
  read -p "Install Imagemagick? (y/n) " ins
  [[ "${ins}" == "y" ]] && sudo apt-get install imagemagick || exit
fi

[[ "$1" == "" ]] && echo -e "\nERROR: missing infile.\nUsage: ./convert.sh <infile>.pdf"                  && exit

[[ ! -f "$1" ]]  && echo -e "\nERROR: $1 is not a valid input file.\nShould be a .pdf with a valid path." && exit

in=$1
out=$(sed 's/\.pdf/\.png/g' <<< ${in})

convert         \
 -density 300   \
 -trim          \
  ${in}         \
 -quality 100   \
  ${out}
