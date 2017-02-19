ls *.povray | time parallel -j+1 --eta 'povray -V -D +I{}'
eog b0.png
ffmpeg -y -i a%d.png output.gif
eog output.gif
