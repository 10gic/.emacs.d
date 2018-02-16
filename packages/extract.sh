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
    [ -e "$zip_file" ] || continue
    # DIR is the root dir in zip file
    DIR=$(unzip -Z -1 "$zip_file" | head -1)
    if [[ ! -d "$extract_dir/$DIR" ]]; then
        echo "Extracting $zip_file"
        unzip "$zip_file" -d "$extract_dir" >/dev/null;
    fi
done

for gz_file in "$current_dir"/*.gz; do
    [ -e "$gz_file" ] || continue
    DIR=$(tar -tzf "$gz_file" | sed -e 's@/.*@@' | uniq)
    if [[ ! -d "$extract_dir/$DIR" ]]; then
        echo "Extracting $gz_file"
        tar -xzf "$gz_file" -C "$extract_dir" >/dev/null;
    fi
done

for bz2_file in "$current_dir"/*.bz2; do
    [ -e "$bz2_file" ] || continue
    DIR=$(tar -tjf "$bz2_file" | sed -e 's@/.*@@' | uniq)
    if [[ ! -d "$extract_dir/$DIR" ]]; then
        echo "Extracting $bz2_file"
        tar -xjf "$bz2_file" -C "$extract_dir" >/dev/null;
    fi
done

if [[ "$(uname -s)" = "Darwin" ]]; then
    # A workaroud for Mac.
    # 在Mac中，aquamacs tabbar的图标显示太大了
    # 这是因为这些icon（tiff）中包含两个frame，Emacs使用了维度较大的tiff
    # 经过下面操作后tiff中仅会留下一个维度小的frame，这样图标显示正常大小
    sips --cropToHeightWidth 15 15 $extract_dir/tabbar-master/close-tab.tiff
    sips --cropToHeightWidth 15 15 $extract_dir/tabbar-master/left.tiff
    sips --cropToHeightWidth 15 15 $extract_dir/tabbar-master/left_disabled.tiff
    sips --cropToHeightWidth 15 15 $extract_dir/tabbar-master/right.tiff
    sips --cropToHeightWidth 15 15 $extract_dir/tabbar-master/right_disabled.tiff

    # 安装all-the-icons所需要的字体，即复制字体到目录${HOME}/Library/Fonts
    for font_file in "$extract_dir"/all-the-icons.el-*/fonts/*.ttf; do
        [ -e "$font_file" ] || continue
        if [ ! -a ${HOME}/Library/Fonts/${font_file##*/} ]; then
            # 如果${HOME}/Library/Fonts中不存在相应字体文件，就复制它
            cp ${font_file} ${HOME}/Library/Fonts/
        fi
    done
fi

echo "Please remove this file if you add new compress package." >> "${current_dir}/extract.done"
