rm a*.povray
rm a*.png
stack exec -- runhaskell ef2017/demo.hs

# ./render.sh
# ffmpeg -i a%d.0.png output.gif
