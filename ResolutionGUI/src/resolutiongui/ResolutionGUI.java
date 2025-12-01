package resolutiongui;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JOptionPane;
import javax.swing.JTabbedPane;

/**
 * Main class for Resolution Test Cases GUI
 * Provides interface to run FOL and Propositional Logic test cases
 */
public class ResolutionGUI {
    static final int PORT = 5003;
    
    public static void main(String[] args) {
        PrologConnection prologConn;
        
        // Ask user which subject to load
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
        
        try {            
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
                        Logger.getLogger(ResolutionGUI.class.getName()).log(Level.SEVERE, null, ex);
                    }
                }
            });
        } catch (IOException ex) {
            Logger.getLogger(ResolutionGUI.class.getName()).log(Level.SEVERE, null, ex);
            System.out.println("Error initializing application");
            JOptionPane.showMessageDialog(null, "Error initializing application: " + ex.getMessage(), 
                "Error", JOptionPane.ERROR_MESSAGE);
        } catch (InterruptedException ex) {
            Logger.getLogger(ResolutionGUI.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
}
