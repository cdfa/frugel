# Frugel: an error tolerant live programming environment

Frugel is an error tolerant live programming environment. With this we mean:

- In contrast to traditional IDEs, language services keep functioning in the presence of errors. Most remarkably, any program can be interpreted to obtain (partial) runtime information, but the editor is not more restrictive in the programs it allows than standard text editors.
- This programming environment supports live programming. The programmer is provided with continuous feedback on a focused part of the program behaviour.

With this combination, programmers can easily observe up-to-date dynamic context around errors, which is valuable for debugging and verification and generally "fuses" the edit-compile-run cycle.

To do this, the environment uses "construction sites" to isolate errors.
See my master thesis for more details (to be published).

## Usage

Visit https://cdfa.github.io/frugel/ to try it out online, or download one of the native binaries from the [releases](releases) (RECOMMENDED due to bad performance of the web version).
