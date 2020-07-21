# Annoying things to remember

- `with` places the new scope at the BOTTOM of the stack, not the top.
- In `rec {...}`, attribute names like `${"foobar"}` are treated identically to plain attribute names, whereas all other types of attributes are inserted afterwards.
