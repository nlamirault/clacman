#
# Makefile for Clacman
# 


SBCL=`which sbcl-git`


all: clacman


help:
	@echo "Clacman (c) Nicolas Lamirault"
	@echo "  dist    : creates a distribution archive"
	@echo "  web     : Generate website in the 'www' directory" 
	@echo "  exe     : Creates an executable"
	@echo "  clean   : clean development environnement"

dist:
	@echo "Create distribution"

web: www/index.xsl www/index.css
	@echo "Make website"
	cd www; $(MAKE)

exe:
	@echo "Create an executable"	

clean:
	find . \( -name "*.fasl" -o -name "lift.dribble" \) -print -exec rm -fr {} \;
	find www -name "index.html" -print -exec rm -fr {} \;
	touch www/index.xsl
	touch www/index.xml

clacman: exe
