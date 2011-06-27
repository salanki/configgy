
Configgy is deprecated
======================

Configgy was a library that handled two separate things:

- setting up basic logging
- loading config files

Logging has moved into [util-logging](https://github.com/twitter/util/tree/master/util-logging),
which requires scala 2.8 but is slightly more idiomatic, and removes the need for a config file,
because:

Configuration is usually done from within [ostrich](https://github.com/twitter/ostrich) by loading
and interpreting a scala file. This has several advantages, not the least of which is type checking
and validation, and is described a bit more in the ostrich README. The base classes for defining
and loading config files are in [util-core](https://github.com/twitter/util/tree/master/util-core)
and [util-eval](https://github.com/twitter/util/tree/master/util-eval).

Old versions of configgy are preserved on branches. I do still accept & merge patches for these
branches to help maintain code that still uses it.

- scala 2.7: branch "release-1.6"
- scala 2.8: branch "release-2.0"
- scala 2.9: branch "scala-2.9"
