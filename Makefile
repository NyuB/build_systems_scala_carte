ifeq ($(OS), Windows_NT)
	MILLW=millw
else
	MILLW=./millw
endif

.PHONY: dev test

dev: fmt test

test:
	$(MILLW) build_systems_a_la_carte.test + makette.test


fmt:
	scalafmt .

fmt-check:
	scalafmt --check .

clean:
	$(MILLW) clean
