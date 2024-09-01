
# Elixir Cheatsheet

## Basic Syntax
- **Atoms**: Constants whose name is their value. Example: `:ok`, `:error`, `:some_atom`.
- **Variables**: Start with a lowercase letter or underscore. Example: `name`, `_var`.
- **Tuples**: Fixed-size collections of values. Example: `{:ok, result}`, `{:person, "Alice", 30}`.
- **Lists**: Ordered collections of elements. Example: `[1, 2, 3]`, `[head | tail]`.

## Data Types
- **Numbers**: `42`, `-17`, `3.14`.
- **Atoms**: `:ok`, `:false`, `:"Hello World"`.
- **Tuples**: `{:ok, 123}`, `{:person, "Alice", 30}`.
- **Lists**: `[1, 2, 3]`, `"hello"` (a binary string).
- **Maps**: Key-value pairs. Example: `%{name: "Alice", age: 30}`.

## Common Operators
- **Arithmetic**: `+`, `-`, `*`, `/`, `div/2`, `rem/2`.
- **Comparison**: `==`, `!=`, `===`, `!==`, `<`, `<=`, `>`, `>=`.
- **Boolean**: `and`, `or`, `not`, `&&`, `||`, `!`.
- **List Operators**: `++` (concatenation), `--` (difference).

## Modules and Functions
- **Defining a Module**:
  ```elixir
  defmodule ModuleName do
    def function_name(arg1, arg2) do
      arg1 + arg2
    end
  end
  ```
- **Defining Functions**:
  ```elixir
  def add(a, b), do: a + b
  def factorial(0), do: 1
  def factorial(n), do: n * factorial(n - 1)
  ```

## Pattern Matching
- Used for variable assignment, function arguments, and control flow:
  ```elixir
  {:ok, value} = {:ok, 42}
  [head | tail] = [1, 2, 3]
  ```

## Control Structures
- **If**:
  ```elixir
  if x > 10 do
    "large"
  else
    "small"
  end
  ```
- **Case**:
  ```elixir
  case x do
    1 -> "one"
    2 -> "two"
    _ -> "other"
  end
  ```
- **Cond**:
  ```elixir
  cond do
    x > 10 -> "large"
    x > 5 -> "medium"
    true -> "small"
  end
  ```

## Common Modules
- **Enum**:
  ```elixir
  Enum.map([1, 2, 3], fn x -> x * 2 end) # [2, 4, 6]
  Enum.filter([1, 2, 3], fn x -> x > 2 end) # [3]
  ```
- **Map**:
  ```elixir
  map = %{name: "Alice", age: 30}
  Map.get(map, :name) # "Alice"
  ```
- **String**:
  ```elixir
  String.upcase("hello") # "HELLO"
  String.split("hello world", " ") # ["hello", "world"]
  ```
- **Kernel**:
  ```elixir
  is_atom(:ok) # true
  elem({:ok, 123}, 1) # 123
  ```

## Concurrency
- **Spawning Processes**:
  ```elixir
  spawn(fn -> IO.puts("Hello from process") end)
  ```
- **Sending Messages**:
  ```elixir
  send(pid, {:hello, "world"})
  ```
- **Receiving Messages**:
  ```elixir
  receive do
    {:hello, msg} -> IO.puts("Received: #{msg}")
    _ -> IO.puts("Unknown message")
  end
  ```

## Error Handling
- **Try-Catch**:
  ```elixir
  try do
    raise "oops"
  rescue
    RuntimeError -> "rescued a runtime error"
  end
  ```
- **Exit Signals**:
  ```elixir
  Process.exit(self(), :normal)
  ```
- **Error Handling in Processes**:
  ```elixir
  spawn(fn -> exit(:normal) end)
  spawn_link(fn -> raise "badarg" end)
  ```

## OTP (Open Telecom Platform)
- **GenServer Basics**:
  ```elixir
  defmodule MyServer do
    use GenServer

    # Callbacks
    def init(state) do
      {:ok, state}
    end

    def handle_call(:get_state, _from, state) do
      {:reply, state, state}
    end

    def handle_cast({:set_state, new_state}, _state) do
      {:noreply, new_state}
    end
  end

  {:ok, pid} = GenServer.start_link(MyServer, %{})
  ```

## Useful Commands
- **Compile a Module**:
  ```elixir
  elixirc module_name.ex
  ```
- **Run Elixir Code**:
  ```elixir
  elixir -e "IO.puts 1 + 2" # 3
  ```
- **List Processes**:
  ```elixir
  Process.list()
  ```
- **Get Process Info**:
  ```elixir
  Process.info(self())
  ```

## Built-in Macros
- `__MODULE__`: Current module name.
- `__ENV__`: Current environment information.
- `__CALLER__`: Current caller context.

## Useful Tips
- **Atoms are constants**: Use them for fixed values.
- **Processes are lightweight**: Use them for concurrency; they're cheap.
- **Pattern matching is powerful**: Utilize it for cleaner, more concise code.
- **Immutability**: All variables are immutable once bound.
