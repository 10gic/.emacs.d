#!/usr/bin/env sh

current_dir="$(dirname "$0")"
extract_dir="$current_dir/extract"

# check unzip is avaiable
command -v unzip >/dev/null 2>&1
if [ $? -ne 0 ]; then
   echo "WARN: unzip is NOT found in PATH."
fi

if [ ! -d "$extract_dir" ]; then
    mkdir -p "$extract_dir";

    for zip_file in "$current_dir"/*.zip; do
        echo "Extracting $zip_file"
        unzip "$zip_file" -d "$extract_dir" >/dev/null;
    done

    for gz_file in "$current_dir"/*.gz; do
        echo "Extracting $gz_file"
        tar -xzf "$gz_file" -C "$extract_dir" >/dev/null;
    done

    for bz2_file in "$current_dir"/*.bz2; do
        echo "Extracting $bz2_file"
        tar -xjf "$bz2_file" -C "$extract_dir" >/dev/null;
    done
fi
