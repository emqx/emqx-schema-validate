.PHONY: run
run: docker/ts
	rebar3 escriptize
	_build/default/bin/emqx_schema_validate ../emqx/_build/emqx/lib/emqx_dashboard/priv/www/static/schema.json

docker/ts: docker/Dockerfile docker/en_spelling_additions.txt
	cd docker; \
  docker build -t mylangtool .
	docker kill langtool || true
	docker rm langtool || true
	touch $@
