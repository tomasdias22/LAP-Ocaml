#  Tiling Language Interpreter: From Mondrian to Escher

[cite_start]This repository contains my implementation of a tiling language interpreter developed in **OCaml** for the Languages and Programming Environments course[cite: 3]. 

The project's goal is to interpret simple programs that generate abstract geometric compositions inspired by **Piet Mondrian** and complex tessellations inspired by **M.C. [cite_start]Escher**[cite: 11, 13].

## 🚀 Language Features

The interpreter processes `.pict` text files composed of labels and instructions. [cite_start]Execution always starts at the `main` label[cite: 24, 44].

### Drawing Primitives
* [cite_start]`square color x y w h`: Draws a filled rectangle/square[cite: 53].
* `line color lw x1 y1 x2 y2`: Draws a line[cite: 53].
* [cite_start]`text color x y size words`: Draws text on the screen[cite: 53].
* [cite_start]`image file x y scale rotation`: Loads and draws an image[cite: 53].

### Control & Geometric Transformations
The interpreter's engine is based on an immutable referential system (state `t`) to isolate local transformations.
* `call label`: Executes a block of instructions defined by another label[cite: 53].
* [cite_start]`translate dx dy`: Shifts the origin (0,0) of the coordinate system for subsequent instructions within the label[cite: 53, 58].
* [cite_start]`rotate deg`: Rotates the drawing frame by the specified angle for subsequent instructions[cite: 53, 58].
* `repeatrotate label n deg`: Draws a label *n* times, applying a cumulative rotation at each iteration[cite: 56].
* [cite_start]`repeatplanar label nx ny dx dy sy`: Draws a 2D grid of a label, applying both horizontal and vertical accumulated offsets[cite: 53, 54].

## 🛠️ Technologies & Architecture

* **Language:** OCaml
* [cite_start]**Build System:** Dune [cite: 62]
* **Graphics:** OCaml `Graphics` module
* [cite_start]**Paradigm:** The code was written in a **purely functional** style[cite: 93]. No mutable variables or imperative loops (`for`/`while`) were used. All repetition logic and mathematical state accumulation were handled entirely through recursion and `List.fold_left`.
