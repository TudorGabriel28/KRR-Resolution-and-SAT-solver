# Resolution Test Cases GUI

A Java Swing GUI application for running First-Order Logic (FOL) and Propositional Logic resolution test cases with SWI-Prolog backend.

## Project Structure

```
ResolutionGUI/
├── src/resolutiongui/
│   ├── ResolutionGUI.java      # Main entry point
│   ├── MainWindow.java          # GUI window
│   ├── PrologConnection.java    # Prolog connection manager
│   ├── MessageReceiver.java     # Receives messages from Prolog
│   └── MessageSender.java       # Sends messages to Prolog
├── build.xml                    # Ant build file
├── test_cases_gui.pl            # Modified test cases for GUI
├── resolution.pl                # Resolution engine
├── prop1.txt - prop4.txt        # Propositional test files
└── README.md                    # This file
```

## Prerequisites

1. **Java Development Kit (JDK)** - Java 8 or higher
2. **Apache Ant** - For building the project
3. **SWI-Prolog** - Must be installed at `C:\Program Files\swipl\bin\swipl-win.exe`
   - If installed elsewhere, modify the `swiPrologPath` in `PrologConnection.java`

## Building the Project

Using Apache Ant:

```bash
cd ResolutionGUI
ant compile
```

To create a JAR file:

```bash
ant jar
```

## Running the Application

### Option 1: Using Ant

```bash
ant run
```

### Option 2: Using Java directly

```bash
cd ResolutionGUI
java -cp build/classes resolutiongui.ResolutionGUI
```

### Option 3: Using the JAR

```bash
java -jar dist/ResolutionGUI.jar
```

## How to Use

1. **Start the Application**: Launch the GUI using one of the methods above
2. **Select a Test Case**: Choose from the dropdown menu:
   - **FOL Tests**:
     - `test_fmi` - Student/Course knowledge base
     - `test_blocks` - Blocks world problem
     - `test_plus` - Arithmetic with successor notation
     - `test_john_mice` - John, cats, mice, and hounds
   - **Propositional Tests**:
     - `test_prop1` through `test_prop4` - Various propositional logic scenarios
3. **Run Test**: Click the "Run Test" button
4. **View Results**: Output appears in the text area showing:
   - Test execution progress
   - Resolution process messages
   - Final result (SATISFIABLE or UNSATISFIABLE)
5. **Clear Output**: Click "Clear Output" to reset the display

## Architecture

The application uses a client-server architecture:

- **Java GUI** (Client): Provides the user interface and sends test commands
- **SWI-Prolog** (Server): Executes resolution logic and returns results
- **Socket Communication**: Messages are exchanged via TCP sockets on port 5003

### Communication Flow:

1. GUI starts and launches SWI-Prolog process with socket parameters
2. Prolog process connects back to GUI via socket
3. User selects test and clicks "Run Test"
4. GUI sends test name (e.g., "test_fmi") to Prolog
5. Prolog executes the test and sends output back to GUI
6. GUI displays results in the text area

## Test Cases Overview

### FOL (First-Order Logic) Tests

- **test_fmi**: Tests reasoning about students, courses, and academic performance
- **test_blocks**: Tests spatial reasoning with colored blocks
- **test_plus**: Tests arithmetic reasoning with Peano numbers
- **test_john_mice**: Tests logical relationships between people, pets, and properties

### Propositional Logic Tests

- **test_prop1-4**: Various propositional satisfiability problems

## Troubleshooting

### SWI-Prolog Not Found

- Error: "Cannot run program... error=2, No such file or directory"
- Solution: Update `swiPrologPath` in `PrologConnection.java` with your SWI-Prolog installation path

### Port Already in Use

- Error: "Address already in use"
- Solution: Change `PORT` constant in `ResolutionGUI.java` to a different port number

### No Output in GUI

- Check that `resolution.pl` and `test_cases_gui.pl` are in the same directory as the executable
- Verify that prop1.txt through prop4.txt exist in the working directory

## Modifying the Code

### Adding New Test Cases

1. Add test predicate to `test_cases_gui.pl`:

```prolog
test_my_new_test :-
    send_to_gui('Starting my new test...'),
    KB = [...],
    prove_fol(KB),
    send_to_gui('Test completed.').
```

2. Add to dropdown in `MainWindow.java`:

```java
testCaseComboBox = new JComboBox<>(new String[] {
    ...,
    "test_my_new_test - Description"
});
```

### Customizing the GUI

Edit `MainWindow.java` to modify:

- Window size: `setSize(720, 550)`
- Colors and fonts: Modify component properties in `initComponents()`
- Layout: Adjust `setBounds()` coordinates

## Credits

Based on the Prolog-Java interface example from the ExempluInterfataPrologSwi project.
Implements resolution-based theorem proving for FOL and propositional logic.
