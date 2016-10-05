#!/bin/sh

# $1 - path to 'public' folder

# get snapshot
SNAPSHOT=$(sed -n -r -e 's/resolver:\s+([a-z0-9\-\.]+)/\1/ip' stack.yaml)

# work with generated docs
pushd $1/docs > /dev/null

# remove non-flaw packages
rm -r $(ls -F | grep -P '^(?!flaw-)(?!all/).+/$')

# fix root *.html
sed -i -r \
	-e 's/(<link href=")ocean\.css(" rel="stylesheet" type="text\/css") title="Ocean"( \/>)/\1flaw.css\2\3/' \
	*.html

# remove unneeded files
rm ocean.css plus.gif minus.gif

for i in $(ls -F | grep '/$')
do
	pushd $i > /dev/null

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
		-e 's/(<link href=")ocean\.css(" rel="stylesheet" type="text\/css") title="Ocean"( \/>)/\1..\/flaw.css\2\3/' \
		-e 's/(<a href)(="\.\.\/flaw-[^"]+">)/\1_preserve\2/g' \
		-e "s/<a href=\"\.\.\/([^\"]+\">)/<a class=\"snapshot\" href=\"https:\/\/www.stackage.org\/haddock\/$SNAPSHOT\/\1/g" \
		-e 's/(<a href)_preserve(="[^"]+">)/\1\2/g' \
		*.html

	# fix src/*.html
	sed -i -r \
		-e "s/(<link type='text\/css' rel='stylesheet' href=')(hscolour\.css' \/>)/\1..\/..\/\2/" \
		src/*.html

	popd > /dev/null
done

popd > /dev/null

# copy static files over
cp -r pages/root/* $1/
