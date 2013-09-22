-define(PRINT(Fmt), io:format(Fmt)).
-define(PRINT(Fmt, Args), io:format(Fmt, Args)).

-define(HALT(Fmt), ?HALT(Fmt, [])).
-define(HALT(Fmt, Args), ?HALT(Fmt, Args, 1)).
-define(HALT(Fmt, Args, Code), halt_with(Fmt, Args, Code)).

-define(OPT_OUTPUT, 
        {output, $o, "output", {atom, plain}, "output format: table|json|plain"}).
