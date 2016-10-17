docs: \
	quickbench.1 \
#	quickbench.txt \
#	quickbench.info \

quickbench.1: quickbench.1.md Makefile
	pandoc -s $< -o $@

#quickbench.txt: quickbench.1 Makefile
#	groff -t -e -mandoc -Tascii $< | col -bx >$@

# quickbench.info: quickbench.1.md Makefile
# 	pandoc $< -t texinfo | makeinfo --force --no-split -o $@
