# Testing the Fix

## What Was Fixed

The problem was in how the command line arguments were being passed to SWI-Prolog. The original code used:

```java
String command = swiPrologPath + " -g \"" + goal + "\" " + prologFile + " -- " + port;
prologProcess = runtime.exec(command);
```

This doesn't work correctly on Windows because `Runtime.exec(String)` doesn't parse the command line the same way a shell would.

The fix uses the array form:

```java
String[] command = {
    swiPrologPath,
    "-s", prologFile,
    "--",
    String.valueOf(port)
};
prologProcess = runtime.exec(command);
```

## How to Test

1. **Close any currently running SWI-Prolog windows**

2. **Run the GUI again:**

   ```
   cd d:\master\krr\proj-1\ResolutionGUI
   java -cp build\classes resolutiongui.ResolutionGUI
   ```

   Or double-click `run.bat`

3. **Check the SWI-Prolog window** - it should now show:

   ```
   DEBUG: Command line arguments: [...]
   DEBUG: Found port atom: 5003
   DEBUG: Converted to port number: 5003
   Attempting to connect to port 5003
   Connected to GUI successfully
   Resolution Test Cases loaded. Ready.
   ```

4. **In the GUI:**
   - Select "test_fmi - FOL Test (Student/Course)"
   - Click "Run Test"
5. **Expected output in GUI:**
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

## What Should Happen

- The SWI-Prolog window should NOT show "No port specified in arguments"
- The DEBUG messages should show the port was found and connection established
- When you run a test, you should see output from Prolog in the GUI window
- Tests should execute and show results

## If It Still Doesn't Work

### Check 1: SWI-Prolog Path

Make sure SWI-Prolog is installed at: `C:\Program Files\swipl\bin\swipl-win.exe`

To check, run in PowerShell:

```powershell
Test-Path "C:\Program Files\swipl\bin\swipl-win.exe"
```

If it returns `False`, find where SWI-Prolog is installed and update line 13 in `PrologConnection.java`:

```java
final String swiPrologPath = "YOUR_PATH_HERE\\swipl-win.exe";
```

### Check 2: Files in Same Directory

Verify these files exist in `ResolutionGUI` folder:

- test_cases.pl
- resolution.pl
- prop1.txt through prop4.txt

### Check 3: Port Not in Use

Make sure no other application is using port 5003. If needed, change it in `ResolutionGUI.java` line 17:

```java
static final int PORT = 5003;  // Change to different port like 5004
```

## Removing Debug Output

Once it's working, you can remove the debug lines from `test_cases.pl`:

Remove these lines:

```prolog
format('DEBUG: Command line arguments: ~w~n', [Argv]),
format('DEBUG: Found port atom: ~w~n', [PortAtom]),
format('DEBUG: Converted to port number: ~w~n', [Port]),
```

And remove this line:

```prolog
format('Expected format: swipl -s test_cases.pl -- PORT~n', [])
```

## Summary

✅ Fixed: Command line argument passing to SWI-Prolog
✅ Added: Debug output to diagnose connection issues
✅ Compiled: New Java classes are ready to run

Try running the application now - it should work!
