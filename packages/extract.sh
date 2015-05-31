#!/usr/bin/env sh

current_dir="$(dirname "$0")"
extract_dir="$current_dir/extract"

# check unzip is avaiable
command -v unzip >/dev/null 2>&1
if [ $? -ne 0 ]; then
   echo "WARN: unzip is NOT found in PATH."
fi

if [ ! -d $extract_dir ]; then
    mkdir -p $extract_dir;

    for zip_file in `ls $current_dir/*.zip`; do
        unzip $zip_file -d $extract_dir;
    done

    for gz_file in `ls $current_dir/*.gz`; do
        tar -xzf $gz_file -C $extract_dir;
    done

    for bz2_file in `ls $current_dir/*.bz2`; do
        tar -xjf $bz2_file -C $extract_dir;
    done
fi
