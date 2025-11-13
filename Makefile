

all:
	@echo "Use 'make clean' to cleanup racket backup files"


clean:
	@find . -type f -name "*~" -exec rm -f {} \;
	@echo "Cleaned"
