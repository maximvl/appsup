appsup
======

Erlang Application Supervisor

usage
=====

Check and restart ```lager``` every 5 seconds:

```
appsup:watch(lager, 5000).
```


Stop checking:

```
appsup:unwatch(lager).
```


List apps:

```
appsup:show_apps().
```

ETS table ```restarter``` is availible for reading.
