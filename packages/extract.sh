#!/bin/bash

current_dir="$(dirname "$0")"
extract_dir="$current_dir/extract"

if [[ -f "${current_dir}/extract.done" ]]; then
    # File "${current_dir}/extract.done" is for saving time.
    echo "File ${current_dir}/extract.done exist, extract nothing."
    exit 0;
fi

# check unzip is avaiable
if ! command -v unzip >/dev/null 2>&1; then
    echo "WARN: unzip is NOT found in PATH."
fi

if [[ ! -d "$extract_dir" ]]; then
    mkdir -p "$extract_dir";
fi

for zip_file in "$current_dir"/*.zip; do
    # DIR is the root dir in zip file
    DIR=$(unzip -Z -1 "$zip_file" | head -1)
    if [[ ! -d "$extract_dir/$DIR" ]]; then
        echo "Extracting $zip_file"
        unzip "$zip_file" -d "$extract_dir" >/dev/null;
    fi
done

for gz_file in "$current_dir"/*.gz; do
    DIR=$(tar -tzf "$gz_file" | sed -e 's@/.*@@' | uniq)
    if [[ ! -d "$extract_dir/$DIR" ]]; then
        echo "Extracting $gz_file"
        tar -xzf "$gz_file" -C "$extract_dir" >/dev/null;
    fi
done

for bz2_file in "$current_dir"/*.bz2; do
    DIR=$(tar -tjf "$bz2_file" | sed -e 's@/.*@@' | uniq)
    if [[ ! -d "$extract_dir/$DIR" ]]; then
        echo "Extracting $bz2_file"
        tar -xjf "$bz2_file" -C "$extract_dir" >/dev/null;
    fi
done

if [[ "$(uname -s)" = "Darwin" ]]; then
    # A workaroud for Mac.
    # In Mac, the close button in aquamacs tabbar is too big,
    # Following code resizes it from 15x12 to 10x8
    sips --resampleHeight 8 $extract_dir/tabbar-master/close-tab.tiff
fi

echo "Please remove this file if you add new compress package." >> "${current_dir}/extract.done"
