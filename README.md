Statict ref
===

Description
---

Library generate module in runtime with a set from/to_* functions based on KV description.

Usage
---

```erlang
statict_dict:compile(Module, [{DictName, [{K, V},...]}, ...]).
```

I'll get set of functions:

```erlang
Module:from_`DictName`(K) -> {ok, V} | {error, unknown}.
Module:to_`DictName`(V) -> {ok, V} | {error, unknown}.
```
