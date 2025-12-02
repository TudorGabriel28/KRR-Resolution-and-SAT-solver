package resolutiongui;

import java.io.IOException;
import java.io.InputStream;
import java.io.PipedOutputStream;
import java.io.PrintStream;
import java.net.ServerSocket;

public class PrologConnection {
    final String swiPrologPath = "C:\\Program Files\\swipl\\bin\\swipl-win.exe";
    String prologFile;
    String workingDir;

    Process prologProcess;
    MessageSender sender;
    MessageReceiver receiver;
    MainWindow mainWindow;
    int port;
    
    public MainWindow getMainWindow() {
        return mainWindow;
    }
    
    public PrologConnection(int _port, MainWindow _mainWindow, String subject) throws IOException, InterruptedException {
        InputStream processIs, processStreamErr;
        port = _port;
        mainWindow = _mainWindow;
        
        if (subject.equals("s1")) {
            prologFile = "test_cases.pl";
            workingDir = "s1";
        } else if (subject.equals("s2")) {
            prologFile = "sat_solver_gui.pl";
            workingDir = "s2";
        } else {
            throw new IllegalArgumentException("Invalid subject: " + subject);
        }
        
        ServerSocket serverSocket = new ServerSocket(port);
        
        receiver = new MessageReceiver(this, serverSocket);
        receiver.start();
        sender = new MessageSender(receiver);
        sender.start();
        
        Runtime runtime = Runtime.getRuntime();
        
        String[] command = {
            swiPrologPath,
            "-s", prologFile,
            "--",
            String.valueOf(port)
        };
        
        java.io.File dir = new java.io.File(workingDir);
        prologProcess = runtime.exec(command, null, dir);
        
        processIs = prologProcess.getInputStream();
        processStreamErr = prologProcess.getErrorStream();
    }
    
    void stopProlog() throws InterruptedException {
        PipedOutputStream pos = this.sender.getPipedOutputStream();
        PrintStream ps = new PrintStream(pos);
        ps.println("halt.");
        ps.flush();
    }
}
