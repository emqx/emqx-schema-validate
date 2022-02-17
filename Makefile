TAG := mylangtool

.PHONY: run
run: $(TAG)
	docker run --name $(TAG) -d $(TAG) || true
	cat ../emqx/_build/emqx/lib/emqx_dashboard/priv/www/static/schema.json | \
		docker exec -i $(TAG) ./emqx_schema_validate -

.PHONY: $(TAG)
$(TAG): Dockerfile dict/en_spelling_additions.txt rebar.config $(wildcard src/*)
	docker build -t $(TAG) .
	docker kill $(TAG) || true
	docker rm $(TAG) || true

_build/default/bin/emqx_schema_validate:
	rebar3 escriptize
