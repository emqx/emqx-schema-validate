# Check EMQX Schema Spelling

`emqx_schema_validate path/to/schema.json`

# Run in docker

Mount dict files (.txt suffix) to `/dicts/` dir.

```
docker run --rm -v path/to/dicts:/dicts \
    -v path/to/schema.json:/schema.json \
    ghcr.io/emqx/emqx-schema-validate:0.4.0 /scehma.json
```
