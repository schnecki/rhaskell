

test:
	printf "devtools::test()\n" | R --no-save

doc:
	printf "devtools::document()\n" | R --no-save

release: doc
	export NAME=`basename ${PWD}` && \
		echo "PROJECT: $$NAME" && \
		cd .. && \
		R CMD build $$NAME/ && \
		R CMD check $$NAME*.tar.gz && \
    rm -rf $$NAME.Rcheck && mv $$NAME*.tar.gz $$NAME/
		printf "\n\nIN R RUN:\n   r = getOption(\"repos\"); r[\"CRAN\"] = \"http://cran.es.r-project.org\"; options(repos = r); devtools::release()\n"

# && cd .. && R CMD build $$NAME/ && R CMD check $$NAME*.tar.gz && devtools::release() && rm -rf $$NAME.Rcheck && rm $$NAME*.tar.gz

# 	printf "\n\n\
# 1. RUN FOLLOWING COMMAND:\n\
#   cd .. && R CMD build `basename ${PWD}`/ && R CMD check `basename ${PWD}`*.tar.gz\n\
# 2. IF NO ERRORS NOR WARNINGS ARE FOUND RUN:\n\
#   devtools::release() && rm -rf `basename ${PWD}`.Rcheck && rm `basename ${PWD}`*.tar.gz\n\
# "

install: doc
	printf "devtools::document()\\ndevtools::install()\\n\\nq()\\n" | R --no-save
