# simple CAS for working with sets

## notation
* `+` for union
* `*` for intersection
* `-` for difference
* `d` for symmetric difference
* `0` for empty set
* any other symbols for sets
* `=` - equality
* `()` - braces

Priorities (TBD) from less to more:
* 0: `=`
* 1: `+`, `-`, `d`
* 2: `*`
* max: braces

## how to run

```console
$ clj -X set-cas.main/hello
Hello!
```

```console
$ clj
Clojure 1.11.1
user=> (require 'set-cas.main)
nil
user=> (set-cas.main/hello [])
Hello!
nil
```

## run tests
```console
$ clj -Xtest
```
