# Syntax
## Statements
Unlike in other languages, in unreal-lang not necessarily everything is a statement.
### Functions
```
function name do
    ...
end

function name(a: String) do
    ...
end

function name(a: String): Int do
    ...
end
```

> !!Note that functions are so far the only thing that can be written globally. Meaning that everything following this will have to be written inside a function.

### Variables
```
const foo: Bool = ;
let bar: Int = ...;

foo = bar
```
The difference between a const and a let is that a let is mutable, a const not. Basically just like js.

## Expressions
Anything that can be used as an expression aka. to initialise something etc.
### Literals
There are the following literals:
Type | Description | Values
--- | --- | ---
`Int` | A 32 bit signed integer | -2,147,483,648 to 2,147,483,647
`Bool` | A boolean | `true` or `false`
`String` | A type for text aka Strings | i.e. `"Hello world!"`
`Unit` | A rather experimental type that does nothing and also cannot really be stored | _None_
### Function call
```
foo("Bar");
```
### Binary and Unary operations
```
foo < bar
foo > bar
foo == bar
foo != bar
foo >= bar
foo <= bar

foo and bar
foo or bar
```
Operation | Description
--- | ---
`foo < bar` | Check if `foo` is smaller than `bar`.
`foo > bar` | Check if `foo` is greater than `bar`.
`foo == bar` | Check if `foo` is equal to `bar`.
`foo != bar` | Check if `foo` is not equal to `bar`.
`foo >= bar` | Check if `foo` is greater than or equal to `bar`.
`foo <= bar` | Check if `foo` is smaller than or equal to `bar`.
`foo and bar` | Returns `true` if both `foo` and `bar` are `true`.
`foo or bar` | Returns `true` if one of both `foo` or `bar` is `true`.
`-foo` | Turns foo into its negative value
`!foo` | Inverts a boolean.

### If-Expressions
```
if .. then
    ...
else
    ...
end
```
Note that an if expression can also be used as a value:
```
const a: Int = if true then 5 else 6 end;
```


## Further

 