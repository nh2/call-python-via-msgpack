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
