# Heuristic Optimization Methods

This repository contains implementations and experiments for several problems and approaches in the field of heuristic / metaheuristic optimization. Each topic is organized into its own directory (module).

The problems originate from the [Heuristic Optimization Methods course at FER](https://www.fer.unizg.hr/en/course/hom).

## Contents

- `cvrptw/` – solving the **Capacitated Vehicle Routing Problem with Time Windows (CVRPTW)**
- `grasp/` – experiments / implementation related to **GRASP (Greedy Randomized Adaptive Search Procedure)**
- `quadassign/` – solving the **Quadratic Assignment Problem (QAP)**

> Each directory is designed as a self-contained unit (code, instances/inputs, run scripts, results, etc.).

## How to Use

1. Enter the directory of the problem you are interested in (`cvrptw`, `grasp`, or `quadassign`).
2. Inspect the project structure and read the instructions/comments in the code (input formats, algorithm parameters, execution method).
3. Run the solution via the designated entry point in that directory (e.g., main script / main class / notebook – depending on the implementation).

If you want the README to be fully precise for execution (commands, dependencies, input formats), add a small `README.md` to each directory containing:
- prerequisites (language + version),
- dependency installation instructions,
- an example run command,
- example input/output.

## Goals

- compare different heuristics / metaheuristics,
- explore the impact of parameters (e.g., RCL size, number of iterations, random seed),
- obtain good (not necessarily optimal) solutions within reasonable time.

## Repository Structure (Recommendation)

If you want to standardize all three modules, it is convenient for each directory to follow the same pattern:

- `src/` – source code
- `data/` – problem instances / inputs
- `results/` – execution results (logs, solutions)
- `docs/` – short documentation / report

## License

Add a license of your choice (e.g., MIT) or state that the repository is for educational purposes.

---

If you share the contents (or at least a file list) of `cvrptw/`, `grasp/`, and `quadassign/`, I can extend the README in the same style with exact run commands and descriptions of the algorithms you actually implemented.

