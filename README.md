# Frugel: an error tolerant live programming environment

Frugel is an error tolerant live programming environment. With this we mean:

- In contrast to traditional IDEs, language services keep functioning in the presence of errors. Most remarkably, any program can be interpreted to obtain (partial) runtime information, but the editor is not more restrictive in the programs it allows than standard text editors.
- This programming environment supports live programming. The programmer is provided with continuous feedback on a focused part of the program behaviour.

With this combination, programmers can easily observe up-to-date dynamic context around errors, which is valuable for debugging and verification and generally "fuses" the edit-compile-run cycle.

To do this, the environment uses "construction sites" to isolate errors.
See my [master's thesis](https://cdfa.github.io/frugel/thesis.pdf) for more details.

At the moment, I do not have the time and energy to develop this prototype further.
Please create an issue or pick an existing one if you would like to contribute.

## Installation

Visit https://cdfa.github.io/frugel/ to try it out online, or download one of the [native binaries](https://github.com/cdfa/frugel/releases) (Recommended due to bad performance of the web version).

Regarding package managers, `stack install` and `cabal install` should work out of the box.
You can install one of the nix derivations from `default.nix` with `nix-env -f default.nix -iA <derivation>`, e.g. `nix-env -f default.nix -iA frugel-exe`.
If you add Frugel's package cache (see "Building"), you can also directly install a static Linux binary with `nix-env -i <path>`, where the `<path>` is mentioned in the release notes.
Note that older binaries may not be available in the cache.

## Usage

A [demo video](https://archive.org/details/demo_20220123) and [a presentation](https://archive.org/details/presentation_202201) can be found on the Internet Archive.
The demo video shows the features of the programming environment in action with some examples.
The presentation gives an overview of the design and motivations.

## Building

You can build the programming environment with either `stack`, `cabal` or `nix`. Building with cabal has only been tested on Windows and the stack configuration was only tested on OSX. Nix is the only supported system for building the web-version and may provide better reproducibility than cabal or stack.

### Nix

I recommend adding Frugel's package cache to you nix configuration with `cachix use frugel`.
Building was tested with nix version `2.3.16`.
Build native executables for Linux (musl64) with `nix-build -A frugel-static`.
The web version can be built with `nix-build -A frugel-web`.
Building the web version may take more than 16GB of RAM. Part of this may be swapped.

### Stack

```
stack build
```

### Cabal

```
cabal build
```

## Contributing

I recommend using the `shell.nix` environment when working on this project.
It includes several tools and git hooks.
I will try to provide complete documentation of the development environment on my machine soon.

The current implementation of evaluation is a bit of a mess (abuse of the `ExprMeta` fields), since I was in a rush to finish it.
A less confusing version that implements some core functionality can be found in `scout-src/BasicEvaluation.hs`.
