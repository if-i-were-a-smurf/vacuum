# Vacuum: visualising the GHC heap

Vacuum is a library for extracting graph representations of values
from the GHC heap, at runtime. Those graphs may then be further
processed or translated to various representations for visualization
-- like Graphviz, or Ubigraph.

[travis-ci.org](http://travis-ci.org) results: [![Build Status](https://secure.travis-ci.org/thoughtpolice/vacuum.png?branch=master)](http://travis-ci.org/thoughtpolice/vacuum)

# Installation

It's just a `cabal install` away on [Hackage][]:

~~~~~~~~
$ cabal install vacuum
# also install the graphviz package, for SVG/PNG rendering:
$ cabal install vacuum-graphviz
~~~~~~~~

# Gallery of examples

Want to try before you buy, and see what Vacuum output looks like?
Here are some examples:

 * Vacuuming the linked list `[1, 2]` - [Results][e1]

More examples will come in the future.

# Join in

File bugs in the GitHub [issue tracker][].

Master [git repository][gh]:

 * `git clone https://github.com/thoughtpolice/vacuum.git`

There's also a [BitBucket mirror][bb]:

 * `git clone https://bitbucket.org/thoughtpolice/vacuum.git`

# Authors

See [AUTHORS.txt](https://raw.github.com/thoughtpolice/vacuum/master/AUTHORS.txt).

# License

LGPLv3. See `LICENSE.txt` for details.

[main page]: http://thoughtpolice.github.com/vacuum
[issue tracker]: http://github.com/thoughtpolice/vacuum/issues
[gh]: http://github.com/thoughtpolice/vacuum
[bb]: http://bitbucket.org/thoughtpolice/vacuum
[Hackage]: http://hackage.haskell.org/package/pcap-conduit

[e1]: https://raw.github.com/thoughtpolice/vacuum/master/gallery/list.png
