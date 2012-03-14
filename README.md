# Mitigator: A functional approach to mitigating covert channels #

This library defines a basic interface for implementing covert
channel mitigators (e.g., external timing mitigator, or storage
mitigator). Concretely the library exposes a class 'Mitigator' and
monad transformer 'MitM' (read \"mitigated monad\", or
\"monad-in-the-middle\") that keeps the state of all mitigators.
This allows users to mitigate different \"object\" independently.
For example, one can build a mitigator per file-handle.
"Mitigator.Time" implements this interface for time-mitigated
handles and computations; "MIO.Handle" shows an implementation of
the former used to mitigate writes to handles.

## Pre-requisites ##

Before building this code, you must use the following cabal commands
to install required libraries:

    cabal update
    cabal install clock

## Examples ##

The 'Examples' directory contains several examples:

  * handle.hs: shows an example of writing to two file, mitigated at
    different rates.

  * basicHttp.hs: shows an example of mitigating web applications. For
    this, you will need to install the latest version of 'iterIO',
    available at:
    [git://github.com/scslab/iterIO.git](git://github.com/scslab/iterIO.git)

  * xycombinator: shows an example implementation using
    [LIO](https://github.com/scslab/lio) for information flow control
    and the mitigator to execute untrusted web apps.
