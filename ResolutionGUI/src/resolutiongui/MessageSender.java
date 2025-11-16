package resolutiongui;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.io.PrintStream;
import java.net.Socket;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Thread that sends messages to Prolog
 */
public class MessageSender extends Thread {
    Socket socket;
    MessageReceiver receiver;
    volatile PipedOutputStream pipedOutputStream = null;
    PipedInputStream pipedInputStream;
    OutputStream outputStream;
    volatile boolean finished = false;

    public final synchronized void setPipedOutputStream(PipedOutputStream _pos) {
        pipedOutputStream = _pos;
        notify();
    }
    
    public synchronized PipedOutputStream getPipedOutputStream() throws InterruptedException {
        if (pipedOutputStream == null) {
            wait();
        }
        return pipedOutputStream;
    }    
    
    public MessageSender(MessageReceiver _receiver) throws IOException {
        receiver = _receiver;
        pipedInputStream = new PipedInputStream();
        setPipedOutputStream(new PipedOutputStream(pipedInputStream));
    }
    
    public void sendMessageToProlog(String message) throws Exception {
        PipedOutputStream pos = getPipedOutputStream();
        PrintStream ps = new PrintStream(pos);
        ps.println(message + ".");
        ps.flush();
    }
    
    public void run() {
        try {
            socket = receiver.getSocket();
            outputStream = socket.getOutputStream();
            int chr;
            while ((chr = pipedInputStream.read()) != -1) {
                outputStream.write(chr);
            }
        } catch (IOException ex) {
            Logger.getLogger(MessageSender.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InterruptedException ex) {
            Logger.getLogger(MessageSender.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
}
