FROM erikvl87/languagetool

# Improving the spell checker
# http://wiki.languagetool.org/hunspell-support
USER root

RUN apk add git erlang-dev
RUN wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x ./rebar3

COPY rebar.config .
COPY src/* src/

RUN ./rebar3 escriptize && cp -p  _build/default/bin/emqx_schema_validate /usr/bin && \
    rm -rf rebar3 src _build

RUN mkdir /dicts
COPY run.sh /run.sh
ENTRYPOINT ["/run.sh"]
