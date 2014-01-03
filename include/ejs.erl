-module(ejs).
-export([e/1, eval/1]).

-spec e(_) -> ok.
e(X) -> io:format("~s~n", [eval(X)]).

%% Base Cases

-spec eval(_) -> string().
eval(endl) -> ";\n";
eval(X) when is_list(X) -> X;
eval(X) when is_atom(X) -> atom_to_list(X);
eval(X) when is_integer(X) -> integer_to_list(X);
eval(X) when is_float(X) -> float_to_list(X);

%% Useful simplifications

eval({getElementById, Name, Value}) ->
	eval({[{["document", "getElementById"], [{quotes, Name}]}, Value]});

%% Comments

eval({comment, X}) ->
	"/* " ++ eval(X) ++ " */";
	
%% Arithmetic Operators

eval({'+', X, Y}) ->
	eval(X) ++ " + " ++ eval(Y);
eval({'-', X, Y}) ->
	eval(X) ++ " - " ++ eval(Y);
eval({'*', X, Y}) ->
	eval(X) ++ " * " ++ eval(Y);
eval({'/', X, Y}) ->
	eval(X) ++ " / " ++ eval(Y);
eval({'%', X, Y}) ->
	eval(X) ++ " % " ++ eval(Y);
	
%% Arithmetic Operators (Alternative)

eval({'+', [Z]}) ->
	eval(Z);
eval({'+', [X|Rest]}) ->
	eval(X) ++ " + " ++ eval({'+', Rest});
eval({'-', [Z]}) ->
	eval(Z);
eval({'-', [X|Rest]}) ->
	eval(X) ++ " - " ++ eval({'-', Rest});
eval({'*', [Z]}) ->
	eval(Z);
eval({'*', [X|Rest]}) ->
	eval(X) ++ " * " ++ eval({'*', Rest});
eval({'/', [Z]}) ->
	eval(Z);
eval({'/', [X|Rest]}) ->
	eval(X) ++ " / " ++ eval({'/', Rest});
eval({'%', [Z]}) ->
	eval(Z);
eval({'%', [X|Rest]}) ->
	eval(X) ++ " % " ++ eval({'%', Rest});

%% Unary Arithmetic Operators

eval({'++', X}) ->
	eval({'++', X, post});
eval({'++', X, post}) ->
	eval(X) ++ "++";
eval({'++', X, pre}) ->
	"++" ++ eval(X);
	
eval({'--', X}) ->
	eval({'--', X, post});
eval({'--', X, post}) ->
	eval(X) ++ "--";
eval({'--', X, pre}) ->
	"--" ++ eval(X);
	
eval({'-', X}) ->
	"-" ++ X;

%% Assignment Operators

eval({'=', X, Y}) ->
	eval(X) ++ " = " ++ eval(Y);
eval({'+=', X, Y}) ->
	eval(X) ++ " += " ++ eval(Y);
eval({'-=', X, Y}) ->
	eval(X) ++ " -= " ++ eval(Y);
eval({'*=', X, Y}) ->
	eval(X) ++ " *= " ++ eval(Y);
eval({'/=', X, Y}) ->
	eval(X) ++ " /= " ++ eval(Y);
eval({'%=', X, Y}) ->
	eval(X) ++ " %= " ++ eval(Y);
eval({':', X, Y}) ->
	eval(X) ++ ":" ++ eval(Y);

%% Comparison Operators

eval({'==', X, Y}) ->
	eval(X) ++ " == " ++ eval(Y);
eval({'===', X, Y}) ->
	eval(X) ++ " === " ++ eval(Y);
eval({'!=', X, Y}) ->
	eval(X) ++ " != " ++ eval(Y);
eval({'!==', X, Y}) ->
	eval(X) ++ " !== " ++ eval(Y);
eval({'>', X, Y}) ->
	eval(X) ++ " > " ++ eval(Y);
eval({'>=', X, Y}) ->
	eval(X) ++ " >= " ++ eval(Y);
eval({'<', X, Y}) ->
	eval(X) ++ " < " ++ eval(Y);
eval({'<=', X, Y}) ->
	eval(X) ++ " <= " ++ eval(Y);
	
eval({in, X, Y}) ->
	eval(X) ++ " in " ++ eval(Y);
eval({instanceof, X, Y}) ->
	eval(X) ++ " instanceof " ++ eval(Y);

%% Logical Operators

eval({'&&', X, Y}) ->
	eval(X) ++ " && " ++ eval(Y);
eval({'||', X, Y}) ->
	eval(X) ++ " || " ++ eval(Y);
	
%% Logical Operators (Alternative)

eval({'&&', [Z]}) ->
	eval(Z);
eval({'&&', [X|Rest]}) ->
	eval(X) ++ " && " ++ eval({'&&', Rest});
eval({'||', [Z]}) ->
	eval(Z);
eval({'||', [X|Rest]}) ->
	eval(X) ++ " || " ++ eval({'||', Rest});

%% Unary Logical Operators

eval({'!', X}) ->
	"!" ++ eval(X);
	
%% Bitwise Operators

eval({'&', X, Y}) ->
	eval(X) ++ " & " ++ eval(Y);
eval({'|', X, Y}) ->
	eval(X) ++ " | " ++ eval(Y);
eval({'<<', X, Y}) ->
	eval(X) ++ " << " ++ eval(Y);
eval({'>>', X, Y}) ->
	eval(X) ++ " >>" ++ eval(Y);
eval({'>>>', X, Y}) ->
	eval(X) ++ " >>> " ++ eval(Y);
eval({'^', X, Y}) ->
	eval(X) ++ " ^ " ++ eval(Y);
	
%% Unary Bitwise Operators

eval({'~', X}) ->
	"~" ++ eval(X);

%% Grouping

eval({'()'}) -> "()";
eval({'[]'}) -> "[]";
eval({'{}'}) -> "{}";
eval({'//'}) -> "{}";

eval({'()', X}) ->
	"(" ++ eval(X) ++ ")";
eval({'[]', X}) ->
	"[" ++ eval(X) ++ "]";
eval({'{}', X}) ->
	"{" ++ eval(X) ++ "}";
eval({'//', X}) ->
	"/" ++ eval(X) ++ "/";
	
eval({'""'}) -> "\"\"";
eval({quotes}) -> "''";

eval({'""', X}) -> 
	"\"" ++ eval(X) ++ "\"";
eval({quotes, X}) -> 
	"'" ++ eval(X) ++ "'";

%% Seperators

eval({',', []}) -> "";
eval({',', [Arg]}) -> eval(Arg);
eval({',', [Arg|Args]}) ->
	eval(Arg) ++ ", " ++ eval({',', Args});
	
eval({'.', [Arg]}) -> eval(Arg);
eval({'.', [Arg|Args]}) ->
	eval(Arg) ++ "." ++ eval({'.', Args});
	
%% Variables

eval({var, Name, Value}) ->
	"var " ++ eval({'=', eval(Name), eval(Value)});
eval({var, Name}) ->
	"var " ++ eval(Name);
	
eval({const, Name, Value}) ->
	"const " ++ eval({'=', eval(Name), eval(Value)});
eval({const, Name}) ->
	"const " ++ eval(Name);

eval({delete, X}) ->
	"delete " ++ eval(X);

%% Objects

eval({new, Name}) ->
	"new " ++ eval(Name) ++ eval({'()'});
eval({new, Name, Args}) ->
	"new " ++ eval(Name) ++ eval({'()', {',', eval(Args)}});

eval({el, ID, Object}) ->
	eval(Object) ++ eval({'[]', eval(ID)});

%% Calls

eval({Addr}) when is_list(Addr) ->
	eval({'.', eval(Addr)});
	
eval({Addr, []}) when is_list(Addr) ->
	eval({'.', eval(Addr)}) ++ eval({'()'});
eval({Addr, Args}) when is_list(Addr) ->
	eval({'.', eval(Addr)}) ++ eval({'()', {',', eval(Args)}});

%% Conditional Statements

eval({'?', C, X, Y}) ->
	"( " ++ eval(C) ++ " ) ? " ++ eval(X) ++ " : " ++ eval(Y);
eval({'if', Condition, Statements}) ->
	"if" ++ eval({'()', eval(Condition)}) ++ eval({'{}', eval({statements, eval(Statements)})});
eval({'if', Condition, Statements, ElseStatements}) ->
	"if" ++ eval({'()', eval(Condition)}) ++ eval({'{}', eval({statements, eval(Statements)})}) ++
		"else" ++ eval({'{}', eval({statements, eval(ElseStatements)})});
		
eval({'elseif', Conditions, ElseStatements}) ->
	eval({'elseif', Conditions}) ++ "else" ++ 
		eval({'{}', eval({statements, eval(ElseStatements)})});
eval({'elseif', [{Condition, Statements}|Rest]}) ->
	"if" ++ eval({'()', eval(Condition)}) ++ eval({'{}', eval({statements, eval(Statements)})}) ++
		eval({'elseif-2', Rest});
eval({'elseif-2', [{Condition, Statements}]}) ->
	"else if" ++ eval({'()', eval(Condition)}) ++ eval({'{}', eval({statements, eval(Statements)})});
eval({'elseif-2', [{Condition, Statements}|Rest]}) ->
	"else if" ++ eval({'()', eval(Condition)}) ++ eval({'{}', eval({statements, eval(Statements)})}) ++
		eval({'elseif-2', Rest});

eval({'case', Target, Values}) -> eval({'case', Target, Values, none});
eval({'case', Target, [{Value, Statements}|Rest], DefaultStatements}) ->
	"switch" ++ eval({'()', eval(Target)}) ++ eval({'{}', eval("case " ++ eval(Value) ++ ":\n" ++ eval({statements, eval(Statements)}) ++ "break;\n" ++ 
		eval({'case-2', Rest, DefaultStatements})) });

eval({'case-2', [{Value,Statements}], none}) ->
	"case " ++ eval(Value) ++ ":\n" ++ eval({statements, eval(Statements)}) ++ "break;\n";
eval({'case-2', [{Value,Statements}], DefaultStatements}) ->
	"case " ++ eval(Value) ++ ":\n" ++ eval({statements, eval(Statements)}) ++ "break;\n" ++ 
	"default:\n" ++ eval({statements, eval(DefaultStatements)}) ++ "break;";
eval({'case-2', [{Value, Statements}|Rest], DefaultStatements}) ->
	"case " ++ eval(Value) ++ ":\n" ++ eval({statements, eval(Statements)}) ++ "break;\n" ++ 
		eval({'case-2', Rest, DefaultStatements});

%% Loops

eval({for, InitStatement, Condition, IterStatement, Statements}) ->
	"for" ++ eval({'()', eval(InitStatement) ++ "; " ++ eval(Condition) ++ "; " ++ eval(IterStatement)}) ++
		eval({'{}', eval({statements, Statements})});

eval({forin, Variable, Object, Statements}) ->
	"for" ++ eval({'()', eval({var, Variable}) ++ " in " ++ eval(Object)}) ++
		eval({'{}', eval({statements, Statements})});
		
eval({while, Condition, Statements}) ->
	"while" ++ eval({'()', eval(Condition)}) ++
		eval({'{}', eval({statements, eval(Statements)})});
		
eval({dowhile, Statements, Condition}) ->
	"do" ++ eval({'{}', eval({statements, eval(Statements)})}) ++ "while" ++
		eval({'()', eval(Condition)}) ++";";

eval({with, Object, Statements}) ->
	"with" ++ eval({'()', eval(Object)}) ++
		eval({'{}', eval({statements, eval(Statements)})}) ++ ";";
%% Blocks

eval({function, Name, Arguments, Statements}) ->
	eval("function " ++ eval(Name) ++ eval({'()', eval({',', eval(Arguments)})}) ++ eval({'{}', eval({statements, Statements})}));

eval({function, Arguments, Statements}) ->
	eval({'()', eval("function" ++ eval({'()', eval({',', eval(Arguments)})}) ++ eval({'{}', eval({statements, Statements})}))});

eval({statement, Statement}) ->
	eval(Statement) ++ eval(endl);

eval({statements, []}) -> "";
eval({statements, [Statement]}) ->
	eval({statement, eval(Statement)});
eval({statements, [Statement|Rest]}) ->
	eval({statement, eval(Statement)}) ++ eval({statements, Rest});

%% Error handling

eval({'throw', X}) ->
	"throw " ++ eval(X);
	
eval({'try', Statements}) ->
	"try" ++ eval({'{}', eval({statements, eval(Statements)})});
eval({'try', Statements, CatchStatements}) ->
	"try" ++ eval({'{}', eval({statements, eval(Statements)})}) ++ 
		eval({'try-2', eval(CatchStatements)}); 
eval({'try', Statements, CatchStatements, Finally}) ->
	"try" ++ eval({'{}', eval({statements, eval(Statements)})}) ++ 
		eval({'try-2', CatchStatements}) ++ "finally" ++ 
		eval({'{}', eval({statements, eval(Finally)})});
eval({'try-2', [{Condition, Statements}]}) ->
	"catch" ++ eval({'()', eval(Condition)}) ++
		eval({'{}', eval({statements, eval(Statements)})});
eval({'try-2', [{Condition, Statements}|Rest]}) ->
	"catch" ++ eval({'()', eval(Condition)}) ++
		eval({'{}', eval({statements, eval(Statements)})}) ++
		eval({'try-2', Rest});
		
%% Keywords

eval({break}) ->
	"break";
eval({break, X}) ->
	"break " ++ eval(X);
	
eval({return}) ->
	"return";
eval({return, X}) ->
	"return " ++ eval(X);
	
eval({continue}) ->
	"continue";
eval({continue, X}) ->
	"continue " ++ eval(X);

eval({export, X}) ->
	"export " ++ eval(X);

eval({import, X}) ->
	"import " ++ eval(X);

eval({yield, X}) ->
	"yeild " ++ eval(X);

eval({typeof, X}) ->
	"typeof" ++ {'()', eval(X)};

eval(Uncaught) ->
	io:format("WARNING: Uncaught statement '~p'.~n", [Uncaught]),
	{uncaught, Uncaught}.
