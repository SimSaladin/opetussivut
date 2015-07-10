#=========================================================
# Makefile for building, installing and cleaning
# up the project easier.
#
# There is also a build target for generating the
# documentation from the comments in the haskell
# files. Use the 'haddock' formatting on all the
# comments for correct output.
#=========================================================


#=========================================================
# DEFAULT - TARGET
#
#	Build the application, generate the documentation
#	and generate the webapp.
#=========================================================
default: build docs webapp


#=========================================================
# CONFIGURE AND BUILD - TARGETS
#=========================================================
.PHONY: configure
configure:
	@ tput setaf 2	# Green color
	@ echo "Preparing cabal for building the project"
	@ #---------------------------------------------------
	@ tput setaf 6	# Cyan color
	@ cabal configure
	@ #---------------------------------------------------
	@ tput sgr0		# Reset color


.PHONY: build
build: configure
	@ tput setaf 2	# Green color
	@ echo "Building the project with cabal"
	@ #---------------------------------------------------
	@ tput setaf 6	# Cyan color
	@ cabal build
	@ #---------------------------------------------------
	@ tput sgr0		# Reset color


#=========================================================
# DOCUMENTATION - TARGET
#=========================================================
.PHONY: docs
docs:
	@ tput setaf 2	# Green color
	@ echo "Creating the documentation"
	@ #---------------------------------------------------
	@ tput setaf 6	# Cyan color
	@ cabal haddock --executables
	@ #---------------------------------------------------
	@ tput sgr0		# Reset color


#=========================================================
# WEBAPP - TARGET
#=========================================================
.PHONY: webapp
webapp: build
	@ tput setaf 2	# Green color
	@ echo "Generating the output in 'webapp' folder"
	@ echo " * Copying all the necessary files"
	@ echo " * Creating folders for testing the application"
	@ #---------------------------------------------------
	@ tput setaf 6	# Cyan color
	@ mkdir -p -v webapp
	@ cp -v ./dist/build/opetussivut/opetussivut ./webapp/update
	@ cp -v ./config.yaml ./webapp/
	@ cp -v ./wrapper.sh ./webapp/
	@ # Temporary code generating the extra folders
	@ mkdir -p -v webapp/sivut
	@ mkdir -p -v webapp/test/opetus/kurssit
	@ mkdir -p -v webapp/test/svenska/opetus/kurssit
	@ mkdir -p -v webapp/test/english/studying/courses
	@ #---------------------------------------------------
	@ tput sgr0		# Reset color


#=========================================================
# TEST THE APPLICATION - TARGETS
#=========================================================
.PHONY: run-fetch
run-fetch: build
	@ tput setaf 2	# Green text
	@ echo "Running with 'fetch' argument"
	@ #---------------------------------------------------
	@ tput setaf 6	# Cyan text
	@ ./webapp/wrapper.sh fetch
	@ #---------------------------------------------------
	@ tput sgr0		# Reset color


.PHONY: run-cache
run-cache: build
	@ tput setaf 2	# Green text
	@ echo "Running with 'cache' argument"
	@ #---------------------------------------------------
	@ tput setaf 6	# Cyan text
	@ ./webapp/wrapper.sh cache
	@ #---------------------------------------------------
	@ tput sgr0		# Reset color


#=========================================================
# CLEAN TARGET
#=========================================================
.PHONY: clean
clean:
	@ tput setaf 2	# Green text
	@ echo "Cleaning the folder"
	@ #---------------------------------------------------
	@ tput setaf 1	# Red text
	@ rm -r -f -v dist
	@ rm -r -f -v webapp
	@ find . -name \*~ -exec rm -v {} +
	@ #---------------------------------------------------
	@ tput sgr0		# Reset color






