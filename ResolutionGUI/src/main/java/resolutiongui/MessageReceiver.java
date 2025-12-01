package resolutiongui;

import java.io.IOException;
import java.io.InputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.SwingUtilities;

public class MessageReceiver extends Thread {
    ServerSocket serverSocket;
    volatile Socket socket = null;
    PrologConnection connection;

    public synchronized void setSocket(Socket _socket) {
        socket = _socket;
        notify();
    }    
    
    public synchronized Socket getSocket() throws InterruptedException {
        if (socket == null) {
            wait();
        }
        return socket;
    }
    
    public MessageReceiver(PrologConnection _connection, ServerSocket _serverSocket) throws IOException {
        serverSocket = _serverSocket;
        connection = _connection;
    }
    
    @Override
    public void run() {
        try {
            Socket socketAux = serverSocket.accept();
            setSocket(socketAux);
            
            InputStream inputStream = socketAux.getInputStream();
            
            int chr;
            String str = "";
            String currentTestName = "";
            while ((chr = inputStream.read()) != -1) {
                str += (char) chr;
                if (chr == '\n') {
                    final String lineToWrite = str;
                    
                    if (lineToWrite.startsWith("Running: ")) {
                        currentTestName = lineToWrite.substring(9).trim();
                    }
                    
                    if (lineToWrite.contains("Result: ")) {
                        final String testName = currentTestName;
                        final String result = lineToWrite.substring(lineToWrite.indexOf("Result: ") + 8).trim();
                        final String subject = connection.getMainWindow().getCurrentSubject();
                        SwingUtilities.invokeLater(new Runnable() {
                            public void run() {
                                if (subject.equals("s1")) {
                                    connection.getMainWindow().updateS1ResultLabel(testName, result);
                                } else {
                                    connection.getMainWindow().updateS2ResultLabel(testName, result);
                                }
                            }
                        });
                    }
                    
                    str = "";
                    SwingUtilities.invokeLater(new Runnable() {
                        public void run() { 
                            connection.getMainWindow().getOutputTextArea().append(lineToWrite); 
                        }
                    });
                }
            }
        } catch (IOException ex) {
            System.err.println(ex.getMessage());
        }
    }
}
