#!/bin/sh
OUTPUT_DIR="ex_Output/Output"
for i in `seq 0 26`
do
 index=$(printf "%06d" $i)
 ffmpeg -framerate 20 -pattern_type glob -i "ex_Output/Output/output_*.bmp" -vcodec libx264 -pix_fmt yuv420p -r 60 output.mp4
done
