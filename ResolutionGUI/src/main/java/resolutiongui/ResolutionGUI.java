package resolutiongui;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JOptionPane;
import javax.swing.JTabbedPane;

public class ResolutionGUI {
    static final int PORT = 5003;
    
    public static void main(String[] args) throws Exception {
        PrologConnection prologConn;
        
        Object[] options = {"Subject 1: Resolution", "Subject 2: Davis-Putnam SAT"};
        int choice = JOptionPane.showOptionDialog(null,
            "Select which subject to run:",
            "KRR Test Cases",
            JOptionPane.YES_NO_OPTION,
            JOptionPane.QUESTION_MESSAGE,
            null,
            options,
            options[0]);
        
        String subject = (choice == 0) ? "s1" : "s2";
        
        final MainWindow mainWindow = new MainWindow("KRR Test Cases - " + 
            (subject.equals("s1") ? "Resolution" : "Davis-Putnam SAT"), subject);
        
        prologConn = new PrologConnection(PORT, mainWindow, subject);
        mainWindow.setConnection(prologConn);
        mainWindow.setVisible(true);
        
        mainWindow.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                try {
                    mainWindow.connection.stopProlog();                        
                    mainWindow.connection.sender.finished = true;
                } catch (InterruptedException ex) {
                    // Ignored
                }
            }
        });
    }
}
