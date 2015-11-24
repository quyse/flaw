#!/bin/sh

set -e

if [ ! -e "$HOME/.deps/lib/pkgconfig/sdl2.pc" ]; then
	wget https://www.libsdl.org/release/SDL2-2.0.3.tar.gz
	tar xzvf SDL2-2.0.3.tar.gz
	cd SDL2-2.0.3 && ./configure --prefix=$HOME/.deps && make && make install
else
	echo "using cached sdl2"
fi
cat $HOME/.deps/lib/pkgconfig/sdl2.pc
