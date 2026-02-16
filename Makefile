

all:
	@echo "Use 'make clean' to cleanup racket backup files"


clean:
	@find . -type f -name "*~" -exec rm -f {} \;
	@rm -f scribblings/*.css scribblings/*.html scribblings/*.js
	@DIRS=`find . -type d -name "compiled"`; echo $$DIRS; rm -rf $$DIRS
	@echo "Cleaned"
