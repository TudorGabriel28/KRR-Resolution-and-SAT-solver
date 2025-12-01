# KRR Test Cases GUI - Two Subjects

This GUI application now supports both subjects:

- **Subject 1**: Resolution-based theorem proving (FOL and Propositional Logic)
- **Subject 2**: Davis-Putnam SAT Solver

## Folder Structure

```
ResolutionGUI/
├── s1/                          # Subject 1 files
│   ├── test_cases.pl           # Resolution test cases
│   ├── resolution.pl           # Resolution engine
│   └── prop1.txt - prop4.txt   # Propositional test data
├── s2/                          # Subject 2 files
│   ├── davis_putnam_gui.pl     # GUI-compatible Davis-Putnam
│   ├── davis_putnam_solver.pl  # Core DP algorithm
│   └── test1.txt - test6.txt   # SAT test data
└── src/resolutiongui/          # Java source code
    ├── ResolutionGUI.java      # Main entry with subject selection
    ├── MainWindow.java         # Tabbed GUI for both subjects
    ├── PrologConnection.java   # Subject-aware connection
    ├── MessageReceiver.java    # Receives from Prolog
    └── MessageSender.java      # Sends to Prolog
```

## How to Run

```bash
java -cp build\classes resolutiongui.ResolutionGUI
```

Or double-click `run.bat`

## What's New

### Subject Selection Dialog

When you start the application, a dialog appears asking which subject to run:

- **Subject 1: Resolution** - Loads test_cases.pl from s1 folder
- **Subject 2: Davis-Putnam SAT** - Loads davis_putnam_gui.pl from s2 folder

### Tabbed Interface

The main window now has two tabs:

#### Tab 1: Subject 1 - Resolution

- Dropdown: Select from 8 test cases (4 FOL + 4 Propositional)
- Button: "Run Test"
- Button: "Clear Output"
- Result Box: Shows last test result with color coding
- Output Area: Displays Prolog execution output

#### Tab 2: Subject 2 - Davis-Putnam SAT

- Dropdown 1: Test selection (test1 - test6)
- Dropdown 2: Strategy selection
  - `most_frequent` - Select most frequent atom
  - `shortest_clause` - Select atom from shortest clause
- Button: "Run Test"
- Button: "Clear Output"
- Result Box: Shows last test result with color coding
- Output Area: Displays SAT solver output

## Subject 1: Resolution Tests

Same as before:

- `test_fmi` - Student/Course FOL test
- `test_blocks` - Blocks world FOL test
- `test_plus` - Arithmetic FOL test
- `test_john_mice` - John & Mice FOL test
- `test_prop1-4` - Propositional logic tests

**Results:**

- Red background = UNSATISFIABLE (contradiction found)
- Green background = SATISFIABLE (no contradiction)

## Subject 2: Davis-Putnam SAT Solver

### Tests Available

- `test1` through `test6` - Various SAT problems

### Atom Selection Strategies

- **most_frequent**: Selects the atom that appears most frequently in the clauses
- **shortest_clause**: Selects a literal from the shortest clause (heuristic)

### Command Format

The GUI sends commands like: `solve(test1, most_frequent)`

### Example Output

```
Running Davis-Putnam on test1.txt with strategy: most_frequent
Reading clauses from test1.txt...
Clauses loaded. Running Davis-Putnam algorithm...
Result: YES (SATISFIABLE)
Assignment: {p/true; q/false; r/true}
Test test1 completed.
```

**Results:**

- Red background = NO (UNSATISFIABLE)
- Green background = YES (SATISFIABLE)

## Technical Details

### Working Directories

- S1 Prolog process runs in `s1/` directory
- S2 Prolog process runs in `s2/` directory
- Each subject has its own Prolog files and test data

### Socket Communication

- Same port (5003) for both subjects
- Different Prolog files loaded based on selection
- MessageReceiver routes output to correct tab

### Result Detection

- S1: Looks for "Result: SATISFIABLE" or "Result: UNSATISFIABLE"
- S2: Looks for "Result: YES" or "Result: NO"
- Automatically updates the result box with color coding

## Color Coding

### Subject 1 (Resolution)

- 🔴 Light Red = UNSATISFIABLE (contradiction found)
- 🟢 Light Green = SATISFIABLE (consistent)
- 🟠 Light Orange = ERROR

### Subject 2 (Davis-Putnam)

- 🔴 Light Red = NO/UNSATISFIABLE (no satisfying assignment)
- 🟢 Light Green = YES/SATISFIABLE (satisfying assignment found)
- 🟠 Light Orange = ERROR

## Running Specific Subject

The application asks on startup which subject to load. You can only run one subject per application instance. To switch subjects:

1. Close the application
2. Run it again
3. Select the other subject

## Comparing Strategies (Subject 2)

You can compare how different strategies perform:

1. Run test1 with `most_frequent` strategy
2. Click "Clear Output" (optional)
3. Run test1 with `shortest_clause` strategy
4. Compare the results and execution traces

## Notes

- Both subjects can be accessed from the same GUI
- Each subject has isolated Prolog files in separate folders
- The tabbed interface makes it easy to switch between subjects
- Result boxes show quick summary with color coding
- Full output is available in the scrollable text area
