#!/bin/sh
# NOTE: mustache templates need \\ because they are not awesome.
exec erl -pa ebin edit deps/*/ebin -boot start_sasl -sname mongo_adapter_dev -s mongo_adapter_dev -s reloader