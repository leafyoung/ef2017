gen2: clean
	stack exec -- runhaskell demo2.hs
	./render.sh

gen: clean
	stack exec -- runhaskell demo.hs
	./render.sh

render:
	./render.sh

.PHONY: clean

clean:
	rm -f a*.povray a*.png b*.povray b*.png

# ./render.sh
# ffmpeg -i a%d.0.png output.gif
