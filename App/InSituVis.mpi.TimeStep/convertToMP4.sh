#!/bin/sh
OUTPUT_DIR="Output/Process0000"
for i in `seq 0 26`
do
 index=$(printf "%06d" $i)
 ffmpeg -framerate 30 -pattern_type glob -i "Output/output_*_*.bmp" -vcodec libx264 -pix_fmt yuv420p -r 60 output_Timestep.mp4
done
