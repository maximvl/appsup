appsup
======

Erlang Application Supervisor

usage
=====

Check and restart ```lager``` every 5 seconds:

```
appsup:watch(lager, 5000).
```

Check current app every 3 seconds:

```
appsup:watch_me(3000).
```


Stop checking:

```
appsup:unwatch(lager).
```

Current app:
```
appsup:unwatch_me().
```




List apps:

```
appsup:show_apps().
```

ETS table ```restarter``` is public and controls restarter, but api calls should be preffered for writing.
