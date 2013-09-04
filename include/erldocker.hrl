-define(PROPLISTS(X), either:bind(X, fn:comp(fun either:return/1, fun erldocker_api:proplists_from_json/1))).
-define(PROPLIST(X), either:bind(X, fn:comp(fun either:return/1, fun erldocker_api:proplist_from_json/1))).

