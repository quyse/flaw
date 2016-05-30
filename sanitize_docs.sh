#!/bin/sh

# $1 - path to 'docs' folder

# save copy of hscolour.css
cp $(find $1 -name hscolour.css | head -n1) $1/

for i in $(ls -F $1 | grep '/$')
do
	pushd $1/$i > /dev/null

	# remove duplicated files
	rm -f \
		haddock-util.js \
		src/hscolour.css \
		hslogo-16.png \
		LICENSE \
		minus.gif \
		ocean.css \
		plus.gif \
		synopsis.png

	# fix *.html
	sed -i -r \
		-e 's/(<script src=")(haddock-util\.js" type="text\/javascript"><\/script>)/\1..\/\2/' \
		-e 's/(<link href=")(ocean\.css" rel="stylesheet" type="text\/css" title="Ocean" \/>)/\1..\/\2/' \
		*.html

	# fix src/*.html
	sed -i -r \
		-e "s/(<link type='text\/css' rel='stylesheet' href=')(hscolour\.css' \/>)/\1..\/..\/\2/" \
		src/*.html

	popd > /dev/null
done
