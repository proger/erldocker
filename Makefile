ifeq ($(shell uname), Darwin)
sync:
	fsevent_watch -F src/ | env PERLIO=:raw perl -ne 's#.*\t.*\t$$ENV{"PWD"}/src/.*erl$$#\2# && print "skip_deps=true\n"' | xargs -tn1 rebar compile
else
sync:
	fanotify_watch -c | env PERLIO=:raw perl -ne 's#.*\t.*\t$$ENV{"PWD"}/src/.*erl$$#\2# && print "skip_deps=true\n"' | xargs -tn1 rebar compile
endif

ACTIVE_APPS := sasl,gproc,erlfsmon,compiler,crypto,syntax_tools,tools,rebar,active
ERLDOCKER_APPS := lager,asn1,crypto,public_key,ssl,mimetypes,hackney,jiffy,erldocker

run:
	-pkill -f 'socat.*31233'
	ERL_LIBS=deps erl -pa ebin -config run/sys.config \
		 -eval '[ok = application:ensure_started(A, permanent) || A <- [$(ACTIVE_APPS),$(ERLDOCKER_APPS)]]'

.PHONY: run
