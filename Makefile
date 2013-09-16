APPS := lager
ACTIVE_APPS := sasl,gproc,erlfsmon,compiler,crypto,syntax_tools,tools,rebar,active
ERLDOCKER_APPS := asn1,crypto,public_key,ssl,mimetypes,hackney,jsx,erldocker

ERL_FLAGS= +sbwt none +swct lazy +swt high +K true

run:
	ERL_LIBS=deps erl -pa ebin -config run/sys.config -sname erldocker \
		 $(ERL_FLAGS) \
		 -eval '[ok = application:ensure_started(A, permanent) || A <- [$(APPS),$(ACTIVE_APPS),$(ERLDOCKER_APPS)]]'

ifeq ($(shell uname), Darwin)
sync:
	fsevent_watch -F src/ | env PERLIO=:raw perl -ne 's#.*\t.*\t$$ENV{"PWD"}/src/.*erl$$#\2# && print "skip_deps=true\n"' | xargs -tn1 rebar compile
else
sync:
	fanotify_watch -c | env PERLIO=:raw perl -ne 's#.*\t.*\t$$ENV{"PWD"}/src/.*erl$$#\2# && print "skip_deps=true\n"' | xargs -tn1 rebar compile
endif

.PHONY: run sync
