
# Erlang Cheatsheet

## Basic Syntax
- **Atoms**: Constants whose name is their value. Example: `ok`, `error`, `'Some Atom'`.
- **Variables**: Start with an uppercase letter or underscore. Example: `Name`, `_Var`.
- **Tuples**: Fixed-size collections of values. Example: `{ok, Result}`, `{person, "Alice", 30}`.
- **Lists**: Ordered collections of elements. Example: `[1, 2, 3]`, `[Head | Tail]`.

## Data Types
- **Numbers**: `42`, `-17`, `3.14`.
- **Atoms**: `ok`, `false`, `'Hello World'`.
- **Tuples**: `{ok, 123}`, `{person, "Alice", 30}`.
- **Lists**: `[1, 2, 3]`, `"hello"` (a list of characters).
- **Maps**: Key-value pairs. Example: `#{name => "Alice", age => 30}`.

## Common Operators
- **Arithmetic**: `+`, `-`, `*`, `/`.
- **Comparison**: `==`, `/=`, `=:=`, `=/=`, `<`, `=<`, `>`, `>=`.
- **Boolean**: `and`, `or`, `not`, `andalso`, `orelse`.
- **List Operators**: `++` (concatenation), `--` (difference).

## Modules and Functions
- **Defining a Module**:
  ```erlang
  -module(module_name).
  -export([function_name/arity]).
  ```
- **Defining Functions**:
  ```erlang
  add(A, B) -> A + B.
  factorial(0) -> 1;
  factorial(N) -> N * factorial(N - 1).
  ```

## Pattern Matching
- Used for variable assignment, function arguments, and control flow:
  ```erlang
  {ok, Value} = {ok, 42}.
  [Head | Tail] = [1, 2, 3].
  ```

## Control Structures
- **If**:
  ```erlang
  if
      X > 10 -> large;
      X > 5 -> medium;
      true -> small
  end.
  ```
- **Case**:
  ```erlang
  case X of
      1 -> one;
      2 -> two;
      _ -> other
  end.
  ```
- **Receive**:
  ```erlang
  receive
      {hello, Msg} -> io:format("Hello ~s~n", [Msg]);
      _ -> ignore
  end.
  ```

## Common Modules
- **lists**:
  ```erlang
  lists:map(fun(X) -> X * 2 end, [1, 2, 3]). % [2, 4, 6]
  lists:filter(fun(X) -> X > 2 end, [1, 2, 3]). % [3]
  ```
- **maps**:
  ```erlang
  Map = #{name => "Alice", age => 30},
  maps:get(name, Map). % "Alice"
  ```
- **string**:
  ```erlang
  string:join(["hello", "world"], " "). % "hello world"
  ```
- **erlang**:
  ```erlang
  erlang:now(). % Current timestamp
  erlang:exit(Pid, Reason). % Exit a process
  ```

## Concurrency
- **Spawning Processes**:
  ```erlang
  spawn(Module, Function, Args).
  spawn(fun() -> io:format("Hello from process~n") end).
  ```
- **Sending Messages**:
  ```erlang
  Pid ! {hello, "world"}.
  ```
- **Receiving Messages**:
  ```erlang
  receive
      {hello, Msg} -> io:format("Received: ~s~n", [Msg]);
      _ -> io:format("Unknown message~n")
  end.
  ```

## Error Handling
- **Try-Catch**:
  ```erlang
  try SomeExpression of
      Result -> {ok, Result}
  catch
      _:Error -> {error, Error}
  end.
  ```
- **Exit Signals**:
  ```erlang
  exit("Something went wrong").
  ```
- **Error Handling in Processes**:
  ```erlang
  spawn(fun() -> exit(normal) end).
  spawn_link(fun() -> error(badarg) end).
  ```

## OTP (Open Telecom Platform)
- **GenServer Basics**:
  - **Define a GenServer Module**:
    ```erlang
    -module(my_server).
    -behaviour(gen_server).

    -export([start_link/0, init/1, handle_call/3, handle_cast/2, terminate/2]).

    start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

    init([]) -> {ok, #{}}.
    
    handle_call(get_state, _From, State) -> {reply, State, State};
    handle_call({set_state, NewState}, _From, _State) -> {reply, ok, NewState}.
    
    handle_cast({update_state, NewState}, _State) -> {noreply, NewState}.
    
    terminate(_Reason, _State) -> ok.
    ```
- **Starting a GenServer**:
  ```erlang
  my_server:start_link().
  ```
- **Calling a GenServer**:
  ```erlang
  gen_server:call(my_server, {set_state, NewState}).
  ```

## ETS (Erlang Term Storage)
- **Creating a Table**:
  ```erlang
  Table = ets:new(my_table, [set, public]).
  ```
- **Inserting Data**:
  ```erlang
  ets:insert(Table, {key1, "value1"}).
  ```
- **Looking Up Data**:
  ```erlang
  ets:lookup(Table, key1). % [{key1, "value1"}]
  ```
- **Deleting Data**:
  ```erlang
  ets:delete(Table, key1).
  ```

## Mnesia (Distributed Database)
- **Creating a Schema**:
  ```erlang
  mnesia:create_schema([node()]).
  mnesia:start().
  ```
- **Creating a Table**:
  ```erlang
  mnesia:create_table(person, [{attributes, record_info(fields, person)}]).
  ```
- **Transaction Example**:
  ```erlang
  mnesia:transaction(fun() ->
      mnesia:write({person, 1, "Alice", 30})
  end).
  ```

## Useful Commands
- **Compile a Module**:
  ```erlang
  c(module_name).
  ```
- **Run Shell Commands**:
  ```erlang
  1 + 2. % 3
  ```
- **List Processes**:
  ```erlang
  processes().
  ```
- **Get Process Info**:
  ```erlang
  process_info(self()).
  ```

## Built-in Macros
- `?MODULE`: Current module name.
- `?FUNCTION_NAME`: Current function name.
- `?LINE`: Current line number.

## Useful Tips
- **Atoms are constants**: Use them for fixed values.
- **Processes are cheap**: Use them for concurrency; they're lightweight.
- **Pattern matching is powerful**: Utilize it for cleaner, more concise code.
- **Immutability**: All variables are immutable once bound.
