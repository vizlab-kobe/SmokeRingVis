OUTPUT_DIR="./InSituVis.mpi.CameraFocusMulti/ex_Output"

# 指定されたパスが存在し、かつディレクトリである場合のみ削除
if [ -d "$OUTPUT_DIR" ]; then
    echo "Removing directory: $OUTPUT_DIR"
    rm -rf "$OUTPUT_DIR" || { echo "Failed to remove directory: $OUTPUT_DIR"; exit 1; }
fi

#Compile
# g++ -I/usr/include VideoCreate.cpp /usr/lib/x86_64-linux-gnu/libboost_filesystem.so.1.65.1 /usr/lib/x86_64-linux-gnu/libboost_system.so -o VideoCreate

#Make movie
# ffmpeg -framerate 20 -pattern_type glob -i "../InSituVis.mpi.CameraFocusMulti/ex_Output/Output/output_*_999999_*.bmp" -vcodec libx264 -pix_fmt yuv420p -r 60 40*40_5_0_0.4_0.3_0.3.mp4