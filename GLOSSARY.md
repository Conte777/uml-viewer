# UML viewer

Draws a project's architecture as a UML diagram built from its sources, one drill level of the module tree at a time.

## Language

**Class**:
A box on the diagram: one module of the scanned project — a Clojure namespace, a TypeScript, Rust or Python source file, a Go package.
_Avoid_: type, struct (in Go a class is the whole package, not one type in it)

**Foreign class**:
A module outside the scanned project that a class depends on, drawn as an oval and named by its full import path.
_Avoid_: external, library

**Scanner**:
The per-language reader that turns a project's sources into classes and the edges between them.
_Avoid_: parser, analyzer
