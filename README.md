# ppx-introspector

Boostrapping the next generation

## Ideas

We can recursively dump, expose or explore the data,model,internals the ocaml compiler and the program compiled via reflection or introspection into a stream, a quine that contains the compiler itself into single number,variable, string or term like the idea of metacoq or a goedel number.
We can move code from the compiler into user space, like the idea of a microkernel.
Any part of the top program can be moved into userspace.

This lifting of code into data effectively creates an alternative form of it, we can then use that new form to train a machine learning model on it. 

The resulting model will help predict auto completions or find similar code, it can be used to create a new 
vectorization of the code itself again into numbers, one that is learned. 

Then we can connect the original code to the learned representation and try and regenerate its original form like an
autoencoder to close the loop.

## Installation

```bash
opam install .
```

## Building

```bash
dune build
```

## Testing

```bash
dune test
```


