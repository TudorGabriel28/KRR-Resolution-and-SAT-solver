# 🚀 Resolution Test Cases GUI - Quick Start Guide

## ✅ What You Have Now

A complete Java GUI application for running your resolution theorem proving tests, modeled after the ExempluInterfataPrologSwi example!

## 📁 Location

Your new GUI is in: **`d:\master\krr\proj-1\ResolutionGUI`**

## 🎯 What It Does

- Provides a graphical interface to run all your test cases from `test_cases.pl`
- Displays Prolog output in real-time
- Shows whether each test is SATISFIABLE or UNSATISFIABLE
- No need to type Prolog commands - just click buttons!

## 🏃 How to Run

### Option 1: Double-Click (Easiest)

1. Navigate to `d:\master\krr\proj-1\ResolutionGUI`
2. Double-click **`run.bat`**
3. The GUI window will appear

### Option 2: Command Line

```bash
cd d:\master\krr\proj-1\ResolutionGUI
java -cp build\classes resolutiongui.ResolutionGUI
```

## 🎮 How to Use

1. **Select a Test** from the dropdown menu:

   - `test_fmi` - Student/Course FOL test
   - `test_blocks` - Blocks world FOL test
   - `test_plus` - Arithmetic FOL test
   - `test_john_mice` - John & Mice FOL test
   - `test_prop1` through `test_prop4` - Propositional logic tests

2. **Click "Run Test"** button

3. **Watch the output** appear in the text area showing:

   - Test execution messages
   - Resolution progress
   - Final result: SATISFIABLE or UNSATISFIABLE

4. **Click "Clear Output"** when you want to reset the display

## 📊 Available Tests

### FOL (First-Order Logic) Tests

- ✅ test_fmi - Tests reasoning about students, courses, and grades
- ✅ test_blocks - Tests spatial reasoning with colored blocks
- ✅ test_plus - Tests arithmetic with Peano numbers
- ✅ test_john_mice - Tests complex logical relationships

### Propositional Logic Tests

- ✅ test_prop1 - Propositional satisfiability test 1
- ✅ test_prop2 - Propositional satisfiability test 2
- ✅ test_prop3 - Propositional satisfiability test 3
- ✅ test_prop4 - Propositional satisfiability test 4

## 📋 Project Structure

```
ResolutionGUI/
├── src/resolutiongui/          # Java source code
│   ├── ResolutionGUI.java      # Main application
│   ├── MainWindow.java         # GUI window
│   ├── PrologConnection.java   # Prolog connection
│   ├── MessageReceiver.java    # Receives from Prolog
│   └── MessageSender.java      # Sends to Prolog
├── build/classes/              # Compiled Java classes
├── test_cases_gui.pl           # Modified test cases
├── resolution.pl               # Resolution engine
├── prop1.txt - prop4.txt       # Test data files
├── run.bat                     # Windows launcher
├── README.md                   # Technical documentation
├── USER_GUIDE.md               # User documentation
└── PROJECT_SUMMARY.md          # Complete project overview
```

## 🔧 Requirements

- ✅ Java (JDK 8 or higher) - Already compiled
- ✅ SWI-Prolog installed at `C:\Program Files\swipl\bin\swipl-win.exe`

## ⚠️ If SWI-Prolog is in a Different Location

1. Open `src\resolutiongui\PrologConnection.java`
2. Find line: `final String swiPrologPath = "C:\\Program Files\\swipl\\bin\\swipl-win.exe";`
3. Change it to your SWI-Prolog installation path
4. Recompile: `javac -d build\classes -encoding UTF-8 src\resolutiongui\*.java`

## 📖 Documentation

Three detailed documentation files are included:

1. **README.md** - Technical setup and architecture
2. **USER_GUIDE.md** - Step-by-step user instructions with examples
3. **PROJECT_SUMMARY.md** - Complete project overview and development notes

## 🎨 GUI Features

- **Test Selector Dropdown** - Easy test selection
- **Run Test Button** - Execute selected test
- **Clear Output Button** - Reset the output display
- **Scrollable Output Area** - View all test results
- **Real-time Updates** - See Prolog output as it happens

## 🔍 Example Output

When you run `test_fmi`, you'll see:

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

## 💡 Tips

- **Start Fresh**: Click "Clear Output" before each test for clarity
- **Read Results**: Scroll through output to see the resolution process
- **Sequential Testing**: Run one test at a time
- **Keep Window Open**: Closing the window terminates Prolog

## 🐛 Troubleshooting

### No window appears

- Check that Java is installed: `java -version`
- Make sure you're in the ResolutionGUI folder

### "Cannot find test_cases_gui.pl"

- Run the application from the ResolutionGUI folder
- Don't move the Prolog files

### "Port already in use"

- Another application is using port 5003
- Close the conflicting app or change the port in ResolutionGUI.java

## 🆚 Comparison with Command Line

| Feature        | GUI                   | Command Line          |
| -------------- | --------------------- | --------------------- |
| Ease of Use    | ⭐⭐⭐⭐⭐            | ⭐⭐⭐                |
| Test Selection | Dropdown menu         | Type manually         |
| Output Display | Formatted, scrollable | Raw console           |
| Learning Curve | Immediate             | Need Prolog knowledge |
| Custom Queries | ❌                    | ✅                    |

## 🎓 Based On

This GUI follows the same architecture as your **ExempluInterfataPrologSwi** example:

- Socket communication between Java and Prolog
- Separate threads for sending/receiving messages
- Swing-based graphical interface
- Background Prolog process management

## ✨ What's New Compared to Example

- **Dropdown menu** for test selection (vs text input)
- **Predefined test cases** (vs free-form queries)
- **Specialized for resolution** (vs general purpose)
- **8 ready-to-run tests** for your coursework

## 📞 Next Steps

1. **Try it now**: Run `run.bat` and test `test_fmi`
2. **Explore all tests**: Try each of the 8 test cases
3. **Read the guides**: Check out USER_GUIDE.md for detailed usage
4. **Customize**: Add your own test cases following the examples

## 🎉 You're All Set!

Your Resolution Test Cases GUI is ready to use. Simply run the application and start testing your knowledge bases with a single click!

---

**Need Help?**

- Check README.md for technical details
- Read USER_GUIDE.md for usage instructions
- Review PROJECT_SUMMARY.md for architecture info
