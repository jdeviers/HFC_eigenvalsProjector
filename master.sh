#!/bin/bash



# -----------------------
# Use with FLIP_IN_EIGEN:
# -----------------------

# find . -type f -name '*[0-9][0-9].csv' > csvlist

# make clean
# make
# while IFS='' read -r line; do

#   dir=$(awk -F "/" '{print $3}' <<< ${line})
#   atom=$(awk -F "/" '{print $4}' <<< ${line} | sed 's/\.csv//g')
#   [[ ! -d "${dir}" ]] && mkdir ${dir}

#   cp ${line} ./
#   ./project.x ${atom}.csv
#   rm ${atom}.csv

#   mv ${atom}.pdf ${dir}
#   mv ${atom}_*.dat ${dir}
# done < "csvlist"

# make clean
# rm csvlist eigen_[0-9].csv



# ---------------------------
# -- Use with  FLIP_IN_PLACE:
# ---------------------------

find . -type f -name '*[0-9][0-9].csv' > csvlist

make clean
make
while IFS='' read -r line; do

  dir=$(awk -F "/" '{print $3}' <<< ${line})
  atom=$(awk -F "/" '{print $4}' <<< ${line} | sed 's/\.csv//g')
  [[ ! -d "${dir}" ]] && mkdir ${dir}

  cp ${line} ./
  ./project.x ${atom}.csv
  rm ${atom}.csv

  mv ${atom}.pdf ${dir}/${atom}_regrouped.pdf
  mv ${atom}_projected.dat csv_dir/${dir}
  rm ${atom}_*.dat
done < "csvlist"

make clean
rm csvlist eigen_[0-9].csv



# -----------------------------
# -- Plot hists of projections:
# -----------------------------
find ./csv_dir/*_1-in_cluster/ -name *_projected.dat > inlist.in
./groupsof3_graphs.R

