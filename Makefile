

test:
	printf "devtools::test()\n" | R --no-save

doc:
	printf "devtools::document()\n" | R --no-save

release:
	printf "devtools::release()\n" > /tmp/inp
	R < /tmp/inp

install: doc
	printf "devtools::document()\\ndevtools::install()\\n\\nq()\\n" | R --no-save
