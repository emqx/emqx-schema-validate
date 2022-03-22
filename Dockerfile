FROM erikvl87/languagetool

# Improving the spell checker
# http://wiki.languagetool.org/hunspell-support
USER root

RUN apk add git erlang-dev
RUN wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x ./rebar3

COPY dict/en_spelling_additions.txt .
COPY rebar.config .
COPY src/* src/

RUN ./rebar3 escriptize && cp -p  _build/default/bin/emqx_schema_validate . && \
    rm -rf rebar3 src _build && \
    (echo; cat en_spelling_additions.txt) >> org/languagetool/resource/en/hunspell/spelling.txt

USER languagetool
