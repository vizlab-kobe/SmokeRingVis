#!/bin/sh

OUTPUT_DIR="Output"
OUTPUT_FILE="output.mp4"

ffmpeg -framerate 4 -pattern_type glob -i "${OUTPUT_DIR}/*.bmp" -vcodec libx264 -pix_fmt yuv420p -r 20 ${OUTPUT_FILE}
