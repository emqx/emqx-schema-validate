TAG := mylangtool

.PHONY: run
run: $(TAG)
	docker run --name $(TAG) -t --rm -v $(shell pwd)/../emqx/_build/docgen/emqx/schema-en.json:/schema.json -v $(shell pwd)/../emqx/scripts/spellcheck/dicts:/dicts $(TAG) /schema.json

.PHONY: $(TAG)
$(TAG): Dockerfile rebar.config $(wildcard src/*)
	docker build -t $(TAG) .
	docker kill $(TAG) || true
	docker rm $(TAG) || true

_build/default/bin/emqx_schema_validate:
	rebar3 escriptize
