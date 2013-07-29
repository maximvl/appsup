appsup
======

Erlang Application Supervisor

usage
=====

Check and restart ```lager```/current-app every 5 seconds:

```
appsup:watch(lager, 5000).
appsup:watch_me(3000).
```


Stop checking:

```
appsup:unwatch(lager).
appsup:unwatch_me().
```



List apps:

```
appsup:show_apps().
```

ETS table ```restarter``` is public and controls restarter, but api calls should be preffered for writing.

In config section ```restarts``` for appsup is proplist:

```
{restarts, [{App, Timeout}, {App2, Timeout2}]}
```
