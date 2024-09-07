ifeq ($(OS), Windows_NT)
	MILLW=millw
else
	MILLW=./millw
endif

.PHONY: dev test

dev: fmt test

test:
	$(MILLW) -j 4 build_systems_a_la_carte.test + makette.test + hashette.test


fmt:
	scalafmt .

fmt-check:
	scalafmt --check .

clean:
	$(MILLW) clean
