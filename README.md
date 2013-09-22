Erlang Command Line Toolkit
===========================

With [Rebar](https://github.com/rebar/rebar/wiki/Rebar-commands)'s `escriptize` feature, you can easily build a escript out of a typical OTP application. 

**Ecli** is a library that help you to build more powerful escriptized command-line tool.

## Usage

Just add Ecli dep to you `rebar.config`:

```erlang
{deps, [
    {ecli, ".*", 
      {git, "https://github.com/stwind/ecli.git", {branch, "develop"}}}
  ]}.
```

And include `ecli` and `getopt` in your escript:

```erlang
{escript_incl_apps, [ecli, getopt]}.
```

## Features:

* [Subcommand](#subcommand)
* [Output Formatting](#outputting)

### Subcommand

Instead of having a bunch of standalone escript file, it is always helpful to have a unified command line interface to a collection of commands. Just like [npm](https://npmjs.org/) or [vagrant](http://vagrantup.com). Ecli makes this easy.

#### Requirements 

Ecli supposes that your subcommnds are something like this:

```
SCRIPT [command …] [<arg>] [<option>]
```
That is a command that has a script name `SCRIPT` followed by one or more `command`, then argument `arg`, and finally some `option`s. The order of `comamnd`, `arg` and `option` can not be arbitrary, in order to make parsing more easier.


#### Usage

Just call `ecli:start/2` in the escript entry function `main/1`, providing the command-line argumets `Args` and a subcommand spec.

E.g.

```erlang
-module(ectl).

-export([main/1]).

main(Args) ->
	ecli:start(Args, spec()).
	
spec() ->
	%% describe below.
```

#### Command Specification

Take [ectl](https://github.com/stwind/ectl) for example, given the following commands:

```
ectl redbug <node> <pattern> [-c] [-m] [-t] [-p]
ectl ping <node> [-c]
```
you should have a spec like this:

```erlang
[
 {script, "ectl"},
 {vsn, "0.1.0"},
 {config_file, "ectl.config"},
 {commands, 
  [
   {"ping", [node], ectl_ping,
    [
     {cookie, $c, "cookie", string, "Erlang cookie to use"}.
    ]},
   {"redbug", [node, trace_pattern], ectl_redbug,
    [
     {cookie, $c, "cookie", string, "Erlang cookie to use"}.
     {time, $t, "time", {integer, 15000}, "stop trace after this many ms"},
     {msgs, $m, "msgs", {integer, 10}, "stop trace after this many msgs"},
     {proc, $p, "proc", {string, "all"}, "Erlang process all|pid()|atom(RegName)"}
    ]}
  ]}
].
```

The elements are:

* `script`: name of your script, here it is `ectl`.
* `vsn`: version of your script, which will be shown when ran with `--version`
* `config_file`: a file from with to read options, so you don't have to provide them on command-line every call. Options in config file will always be override by the command-line ones.
* `commands`: command options that Ecli will use to decide what function to call for a command invocation.

Now let's look closer to the `commands`, here is the spec of command option:

```erlang
-type spec() :: [option()].
-type option() :: 
        {script, string()} |
        {vsn, string()} |
        {config_file, string()} |
        {commands, [command()]}.
-type command() :: cmd_collection() | cmd_spec().
-type cmd_collection() :: {cmd_name(), [command()]}.
-type cmd_name() :: string().
-type cmd_spec() :: {cmd_name(), [cmd_arg()], cmd_fun(), [cmd_opt()]}.
-type cmd_arg() :: atom() | '...'.
-type cmd_fun() :: {module(), atom()} | module().
-type cmd_opt() :: getopt:option_spec().
```

Take the `ectl ping` command for example:

```erlang
{"ping", [node], ectl_ping,
 [
  {cookie, $c, "cookie", string, "Erlang cookie to use"}.
 ]}
```

The tuple has four elements:

* `"ping"`: the name of the command
* `[node]`: command arguments. Here it is exactly one, and the value will be bound to `node`. You can later find it with `ecli:opt/2`.
* `ectl_ping`: the module and method to execute, by default it will call `module:run/1`, here it is the same to provide the value as `{ectl_ping, run}`, Ecli will call `ectl_ping:run/1` for this call.
* `options`: the options for this command, which has the same format as [getopt](https://github.com/jcomellas/getopt)

To summarize:

* running `ectl ping my_node@localhost` will call `ectl_ping:/run1`.
* running `ectl redbug my_node@localhost "erlang:memory() -> return"` will call `ectl_redbug:run/1`.
* running `ectl dummy` will show usage info of `ectl`, since it dosen't match any command.

#### Value Binding

The function to handle a command call takes on argument, with it you can query the command `option`s and `arg`s by using `ectl:binding/2` and `ectl:opt/2`.

E.g. calling:

```bash
ectl ping my_node@localhost -c my_cookie
```

In the handler function, values can be queried like this:

```erlang
run(Opt) ->
	"my_node@localhost" = ecli:binding(node, Opt),
	"my_cookie" = ecli:opt(cookie, Opt).
```

#### Command Usage

If a command run match a subcommand, but dosen't match its argument, the usage of this subcommand will be shown. This is the same as providing a `-h` options.

Running `ectl ping` will shows:

```bash
Usage: ectl ping <node> [...] [options]

  -c, --cookie  Erlang cookie to use
```

Running `ectl redbug my_node@localhost` will shows:

```bash
Usage: ectl redbug <node> <trace_pattern> [options]

  -c, --cookie  Erlang cookie to use
  -t, --time    stop trace after this many ms [default: 15000]
  -m, --msgs    stop trace after this many msgs [default: 10]
  -p, --proc    Erlang process all|pid()|atom(RegName) [default: all]
```

Running `ectl dummy` will shows:

```bash
Usage: ectl  <command> [<arg>] [options]

  -h, --help     Print this help.
  -v, --version  Print the version and exit.

Available subcommands:

  redbug
  ping

For help on any individual command run `ectl COMMAND -h`
```

And finally running `ectl -v` will shows the script version provided:

```bash
ectl 0.1.0
```

### Outputting

Most of times it would be nice to display the results in table format for better visualizaiton, or json format which could be consumed by programs like [jq](http://stedolan.github.io/jq/) at another end of pipe. 

Ecli has builtin support for `table` format output, you can easily achieve this by adding an `output` option to your command.

First include the `ecli.hrl` lib to your module:

```erlang
-include_lib("ecli/include/ecli.hrl").
```

then add `?OTP_OUTPUT` to your subcommand spec, here is example from `ectl ping`:

```erlang
{"ping", [node, '...'], ectl_ping,
      [
       {cookie, $c, "cookie", string, "Erlang cookie to use"},
       ?OPT_OUTPUT
      ]}
```

Now your command will have a `output` option: 

```bash
$ ./ectl ping
Usage: ectl ping <node> [...] [options]

  -c, --cookie  Erlang cookie to use
  -o, --output  output format: table|json|plain [default: plain]
```

In your command handler function, use the `ecli:output/3` to output the results, here again is example from `ectl ping`:

```bash
$ ./ectl ping my_node@127.0.0.1 -c my_cookie -o table
┌──────────────────────┬────────┐
│ node                 │ result │
├──────────────────────┼────────┤
│ yunio_core@127.0.0.1 │ pang   │
└──────────────────────┴────────┘
```
