# ghcjs-base/JavaScript.TypedArray.ArrayBuffer.Internal

`SomeArrayBuffer` corresponds to javascript's native array buffer type.

```
newtype SomeArrayBuffer (a :: MutabilityType s) =
  SomeArrayBuffer JSVal deriving Typeable
instance IsJSVal (SomeArrayBuffer m)

type ArrayBuffer           = SomeArrayBuffer Immutable
type MutableArrayBuffer    = SomeArrayBuffer Mutable
type STArrayBuffer s       = SomeArrayBuffer (STMutable s)
```

# ghcjs-base/GHCJS.Buffer.Types

`SomeBuffer` essentially corresponds to `ByteArray#` primitive type,
and contains `ArrayBuffer` and `TypedArray` views.

```
newtype SomeBuffer (a :: MutabilityType s) = SomeBuffer JSVal

type    Buffer         = SomeBuffer Immutable
type    MutableBuffer  = SomeBuffer Mutable
```
