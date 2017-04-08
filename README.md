# FFI-via-serialisation: Calling Python via MessagePack

This library uses the Python C API to conveniently call Python functions, by serialising all inputs and outputs.

This approach is called _FFI-via-serialisation_, and a simple and convenient alternative to representing the various Python types in Haskell explicitly.

This library uses [MessagePack](http://msgpack.org/) for serialisation because it is reasonably efficient and many Python and Haskell types can be trivially used with it.
In principle, though, any other way of serialisation would do for FFI-via-serialisation just as well.

## When to use this library

You should use this library to call into Python when:

* You want to call Python from Haskell
* You want it to be easy
* All inputs and outputs to your functions are serialisable

You can NOT use this library when:

* You need to hold references to Python objects inside Haskell, so that you can use them more than once
* You want to pass some buffers in a zero-copy fashion

In those cases, you should directly use the more low-level `cpython` package (Haskell library around the Python C API, on top of which this package is built).

## Using it from ghci

Depending on the operating system, using e.g. `ghci -lpython2.7` may not be enough to make this library work from ghci. For example, on Ubuntu 16.04 it works, but on Centos 7.3 it doesn't, failing with error messages such as

```
undefined symbol: _Py_ZeroStruct
```

This is because when ghci loads shared libraries, [it does so with the `RTLD_LOCAL` flag](https://github.com/ghc/ghc/blob/ce66c24ac91ff7893dc91f31292bcebc60df4924/rts/Linker.c#L959), which according to `man dlopen` results in `Symbols defined in this shared object are not made available to resolve references in subsequently loaded shared objects.`. It is unclear why this is not a problem on Ubuntu; it is possible that they modified their Python version to detect when it has to re-`dlopen()` itself with `RTLD_GLOBAL`.

To ensure that the Python C library is correctly loaded in your ghci session, use:

```haskell
import System.Posix.DynamicLinker -- from the `unix` package
_ <- dlopen "libpython2.7.so" [RTLD_NOW, RTLD_GLOBAL]
```

before you call any `cpython` functions from ghci.
