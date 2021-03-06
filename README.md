# A tested version of Stefan Kahrs' red-black trees

This repo contains Stefan Kahrs'
[implementation](https://www.cs.kent.ac.uk/people/staff/smk/redblack/rb.html)
of red-black trees with property-based tests written using
Jacob Stanley's [Hedgehog](https://github.com/hedgehogqa/haskell-hedgehog)
library.

## Development

```Haskell
$ cabal sandbox init
$ cabal install --only-dependencies  --enable-tests
```

### Using `ghcid`

If you want to use Neil Mitchell's
[ghcid](https://github.com/ndmitchell/ghcid) tool run

    $ ./ghcid-dev.sh

While writing tests use:

    $ ./ghci-test.sh

Be aware that if you change the implementation of the library while writing
the tests you will need to Ctrl-C and restart `ghci-test.sh`.