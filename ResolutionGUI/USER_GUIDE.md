# Resolution Test Cases GUI - User Guide

## Overview

This GUI application provides an easy-to-use interface for running resolution-based theorem proving tests. It connects to SWI-Prolog and allows you to execute various First-Order Logic (FOL) and Propositional Logic test cases with a single click.

## Getting Started

### Quick Start

1. Open a terminal in the `ResolutionGUI` folder
2. Run the application:
   - **Windows**: Double-click `run.bat` or run `java -cp build\classes resolutiongui.ResolutionGUI`
   - **Linux/Mac**: Run `./run.sh` or `java -cp build/classes resolutiongui.ResolutionGUI`

### First Launch

When you start the application:

1. A window titled "Resolution Test Cases - GUI" will appear
2. SWI-Prolog will automatically start in the background
3. The initial connection message will appear in the output area
4. The GUI is now ready to run tests

## Interface Elements

### 1. Test Case Selector (Dropdown Menu)

Located at the top of the window, this dropdown contains all available test cases:

**FOL (First-Order Logic) Tests:**

- `test_fmi` - Tests student/course relationships and academic rules
- `test_blocks` - Tests spatial and color relationships in blocks world
- `test_plus` - Tests arithmetic with Peano successor notation
- `test_john_mice` - Tests logical relationships between people and animals

**Propositional Logic Tests:**

- `test_prop1` through `test_prop4` - Various propositional satisfiability tests

### 2. Run Test Button

- Click this button to execute the selected test case
- The test will run in Prolog and results will appear in the output area

### 3. Clear Output Button

- Clears the output text area
- Useful when you want to start fresh or the output becomes too long

### 4. Output Text Area

- Displays all messages from the Prolog resolution engine
- Shows:
  - Test execution status
  - Resolution progress
  - Final results (SATISFIABLE or UNSATISFIABLE)
- Automatically scrolls to show the latest output

## Running Tests

### Step-by-Step Instructions

1. **Select a Test:**

   - Click the dropdown menu at the top
   - Choose the test you want to run (e.g., "test_fmi - FOL Test (Student/Course)")

2. **Execute:**

   - Click the "Run Test" button
   - Watch the output area for results

3. **Read Results:**

   - The output will show the test name and status
   - Look for the result line:
     - `UNSATISFIABLE` means a contradiction was found (proof succeeded)
     - `SATISFIABLE` means no contradiction exists

4. **Run Another Test:**
   - Simply select a different test and click "Run Test" again
   - Previous results remain visible unless you clear them

### Example Output

```
========================================
Running: test_fmi
========================================
Starting test_fmi (FOL - Student/Course)...
Knowledge Base loaded. Running resolution...
Proof found (UNSATISFIABLE).
Result: UNSATISFIABLE - Contradiction found!
test_fmi completed.
```

## Understanding Results

### UNSATISFIABLE

- The knowledge base contains a logical contradiction
- The resolution proof succeeded
- Example: "Ana studies hard AND Ana is in AI course, but Ana doesn't pass KRR" leads to a contradiction

### SATISFIABLE

- The knowledge base is logically consistent
- No contradiction was found
- The clauses can all be true simultaneously

## Test Cases Explained

### FOL Tests

**test_fmi (Student Knowledge Base)**

- Rules about students, courses, and passing grades
- Tests if the given facts lead to a contradiction

**test_blocks (Blocks World)**

- Rules about colored blocks stacked on each other
- Tests spatial and color constraints

**test_plus (Arithmetic)**

- Peano arithmetic with zero and successor function
- Tests mathematical reasoning

**test_john_mice (John's Pets)**

- Complex rules about John, his pets, and their properties
- Tests logical inference chains

### Propositional Tests

These tests read propositional clauses from files (prop1.txt through prop4.txt) and determine satisfiability using resolution.

## Troubleshooting

### Problem: Window appears but no connection message

**Solution:** Check that SWI-Prolog is installed at `C:\Program Files\swipl\bin\swipl-win.exe`. If not, update the path in `PrologConnection.java` and recompile.

### Problem: "Cannot find test_cases_gui.pl"

**Solution:** Make sure you run the application from the `ResolutionGUI` directory where all Prolog files are located.

### Problem: No output when clicking "Run Test"

**Solution:**

- Verify that the Prolog files (resolution.pl, test_cases_gui.pl) are in the correct location
- Check that prop1.txt through prop4.txt exist for propositional tests
- Look at the terminal/console for any error messages

### Problem: Port conflict error

**Solution:** Another application may be using port 5003. Either:

- Close the other application
- Change the PORT constant in `ResolutionGUI.java` to a different number and recompile

## Advanced Usage

### Adding Custom Tests

To add your own test cases:

1. **Edit test_cases_gui.pl:**

   ```prolog
   test_my_custom :-
       send_to_gui('Starting my custom test...'),
       KB = [
           % Your clauses here
           [p, neg(q)],
           [q, neg(r)],
           [neg(p), r]
       ],
       (prove_fol(KB) ->
           send_to_gui('Result: UNSATISFIABLE')
       ;
           send_to_gui('Result: SATISFIABLE')
       ),
       send_to_gui('Test completed.').
   ```

2. **Edit MainWindow.java to add to dropdown:**

   ```java
   testCaseComboBox = new JComboBox<>(new String[] {
       // ... existing tests ...
       "test_my_custom - My Custom Test"
   });
   ```

3. **Recompile:**
   ```bash
   javac -d build/classes -encoding UTF-8 src/resolutiongui/*.java
   ```

## Technical Details

### Architecture

- **GUI Layer:** Java Swing (MainWindow, buttons, text areas)
- **Communication Layer:** Socket-based messaging (MessageSender, MessageReceiver)
- **Logic Layer:** SWI-Prolog resolution engine
- **Port:** TCP port 5003 (configurable)

### File Dependencies

- `resolution.pl` - Core resolution algorithms
- `test_cases_gui.pl` - Test case definitions
- `prop1.txt` - `prop4.txt` - Propositional test data

### Message Flow

1. User clicks "Run Test"
2. Java sends test name to Prolog via socket
3. Prolog executes test and sends output lines back
4. Java displays output in text area in real-time

## Tips for Best Experience

1. **Start Fresh:** Click "Clear Output" before running a new test to see results clearly
2. **Read Carefully:** Some tests produce verbose output - scroll through to find the result
3. **Sequential Testing:** Run one test at a time and wait for completion
4. **Keep Window Open:** Closing the window will terminate the Prolog process

## Keyboard Shortcuts

- **Alt+F4**: Close application (Windows)
- **Cmd+Q**: Quit application (Mac)

## Comparison with Command-Line

### GUI Advantages:

- ✓ No need to type commands
- ✓ Easy test selection from dropdown
- ✓ Clear, formatted output
- ✓ Persistent output history
- ✓ One-click test execution

### Command-Line Advantages:

- ✓ Can run custom Prolog queries
- ✓ Direct access to Prolog REPL
- ✓ Can modify tests on-the-fly

## Support

For issues or questions:

1. Check the README.md for technical details
2. Review the source code comments
3. Verify SWI-Prolog installation and configuration
4. Check that all required files are present in the directory
