package resolutiongui;

import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;

public class MainWindow extends JFrame {
    
    PrologConnection connection;
    String currentSubject = "s1"; 
    
    private JTabbedPane tabbedPane;
    
    private JPanel s1Panel;
    private JComboBox<String> s1TestCaseComboBox;
    private JButton s1RunTestButton;
    private JButton s1ClearOutputButton;
    private JTextArea s1OutputTextArea;
    private JScrollPane s1ScrollPane;
    private JLabel s1TitleLabel;
    private JLabel s1ResultsLabel;
    private JLabel s1LastResultLabel;
    private JLabel s1OutputLabel;
    
    private JPanel s2Panel;
    private JComboBox<String> s2TestCaseComboBox;
    private JComboBox<String> s2StrategyComboBox;
    private JButton s2RunTestButton;
    private JButton s2ClearOutputButton;
    private JTextArea s2OutputTextArea;
    private JScrollPane s2ScrollPane;
    private JLabel s2TitleLabel;
    private JLabel s2StrategyLabel;
    private JLabel s2ResultsLabel;
    private JLabel s2LastResultLabel;
    private JLabel s2OutputLabel;
    private long s2StartTime;
    
    public MainWindow(String title, String subject) {
        super(title);
        this.currentSubject = subject;
        initComponents();
    }
    
    private void initComponents() {
        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        setLayout(null);
        
        if (currentSubject.equals("s1")) {
            initS1Panel();
            s1Panel.setBounds(10, 10, 700, 550);
            add(s1Panel);
        } else {
            initS2Panel();
            s2Panel.setBounds(10, 10, 700, 550);
            add(s2Panel);
        }
        
        setSize(730, 600);
        setLocationRelativeTo(null);
        setResizable(false);
    }
    
    private void initS1Panel() {
        s1Panel = new JPanel();
        s1Panel.setLayout(null);
        
        s1TitleLabel = new JLabel("Select a Test Case to Run:");
        s1TitleLabel.setBounds(20, 10, 400, 25);
        s1TitleLabel.setFont(new Font("Arial", Font.BOLD, 14));
        s1Panel.add(s1TitleLabel);
        
        s1TestCaseComboBox = new JComboBox<>(new String[] {
            "test_fmi - FOL Test (Student/Course)",
            "test_blocks - FOL Test (Blocks World)",
            "test_plus - FOL Test (Arithmetic)",
            "test_john_mice - FOL Test (John & Mice)",
            "test_prop1 - Propositional Test 1",
            "test_prop2 - Propositional Test 2",
            "test_prop3 - Propositional Test 3",
            "test_prop4 - Propositional Test 4"
        });
        s1TestCaseComboBox.setBounds(20, 45, 340, 30);
        s1TestCaseComboBox.setFont(new Font("Arial", Font.PLAIN, 12));
        s1Panel.add(s1TestCaseComboBox);
        
        s1RunTestButton = new JButton("Run Test");
        s1RunTestButton.setBounds(380, 45, 120, 30);
        s1RunTestButton.setFont(new Font("Arial", Font.BOLD, 12));
        s1RunTestButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                s1RunTestButtonActionPerformed(evt);
            }
        });
        s1Panel.add(s1RunTestButton);
        
        s1ClearOutputButton = new JButton("Clear Output");
        s1ClearOutputButton.setBounds(520, 45, 130, 30);
        s1ClearOutputButton.setFont(new Font("Arial", Font.BOLD, 12));
        s1ClearOutputButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                s1ClearOutputButtonActionPerformed(evt);
            }
        });
        s1Panel.add(s1ClearOutputButton);
        
        s1ResultsLabel = new JLabel("Last Test Result:");
        s1ResultsLabel.setBounds(20, 90, 200, 25);
        s1ResultsLabel.setFont(new Font("Arial", Font.BOLD, 14));
        s1Panel.add(s1ResultsLabel);
        
        s1LastResultLabel = new JLabel("No test run yet");
        s1LastResultLabel.setBounds(20, 120, 630, 50);
        s1LastResultLabel.setFont(new Font("Arial", Font.PLAIN, 13));
        s1LastResultLabel.setBorder(BorderFactory.createCompoundBorder(
            BorderFactory.createLineBorder(java.awt.Color.GRAY, 1),
            BorderFactory.createEmptyBorder(10, 10, 10, 10)
        ));
        s1LastResultLabel.setOpaque(true);
        s1LastResultLabel.setBackground(java.awt.Color.WHITE);
        s1Panel.add(s1LastResultLabel);
        
        s1OutputLabel = new JLabel("Output from Prolog:");
        s1OutputLabel.setBounds(20, 185, 400, 25);
        s1OutputLabel.setFont(new Font("Arial", Font.BOLD, 14));
        s1Panel.add(s1OutputLabel);
        
        s1OutputTextArea = new JTextArea();
        s1OutputTextArea.setEditable(false);
        s1OutputTextArea.setFont(new Font("Courier New", Font.PLAIN, 12));
        s1OutputTextArea.setLineWrap(true);
        s1OutputTextArea.setWrapStyleWord(true);
        
        s1ScrollPane = new JScrollPane(s1OutputTextArea);
        s1ScrollPane.setBounds(20, 220, 630, 260);
        s1ScrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
        s1Panel.add(s1ScrollPane);
    }
    
    private void initS2Panel() {
        s2Panel = new JPanel();
        s2Panel.setLayout(null);
        
        s2TitleLabel = new JLabel("Select Test and Strategy:");
        s2TitleLabel.setBounds(20, 10, 400, 25);
        s2TitleLabel.setFont(new Font("Arial", Font.BOLD, 14));
        s2Panel.add(s2TitleLabel);
        
        s2TestCaseComboBox = new JComboBox<>(new String[] {
            "test1", "test2", "test3", "test4", "test5", "test6"
        });
        s2TestCaseComboBox.setBounds(20, 45, 150, 30);
        s2TestCaseComboBox.setFont(new Font("Arial", Font.PLAIN, 12));
        s2Panel.add(s2TestCaseComboBox);
        
        s2StrategyLabel = new JLabel("Strategy:");
        s2StrategyLabel.setBounds(190, 50, 80, 25);
        s2StrategyLabel.setFont(new Font("Arial", Font.PLAIN, 12));
        s2Panel.add(s2StrategyLabel);
        
        s2StrategyComboBox = new JComboBox<>(new String[] {
            "most_frequent", "shortest_clause"
        });
        s2StrategyComboBox.setBounds(255, 45, 150, 30);
        s2StrategyComboBox.setFont(new Font("Arial", Font.PLAIN, 12));
        s2Panel.add(s2StrategyComboBox);
        
        s2RunTestButton = new JButton("Run Test");
        s2RunTestButton.setBounds(425, 45, 100, 30);
        s2RunTestButton.setFont(new Font("Arial", Font.BOLD, 12));
        s2RunTestButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                s2RunTestButtonActionPerformed(evt);
            }
        });
        s2Panel.add(s2RunTestButton);
        
        s2ClearOutputButton = new JButton("Clear Output");
        s2ClearOutputButton.setBounds(540, 45, 110, 30);
        s2ClearOutputButton.setFont(new Font("Arial", Font.BOLD, 12));
        s2ClearOutputButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                s2ClearOutputButtonActionPerformed(evt);
            }
        });
        s2Panel.add(s2ClearOutputButton);
        
        s2ResultsLabel = new JLabel("Last Test Result:");
        s2ResultsLabel.setBounds(20, 90, 200, 25);
        s2ResultsLabel.setFont(new Font("Arial", Font.BOLD, 14));
        s2Panel.add(s2ResultsLabel);
        
        s2LastResultLabel = new JLabel("No test run yet");
        s2LastResultLabel.setBounds(20, 120, 630, 50);
        s2LastResultLabel.setFont(new Font("Arial", Font.PLAIN, 13));
        s2LastResultLabel.setBorder(BorderFactory.createCompoundBorder(
            BorderFactory.createLineBorder(java.awt.Color.GRAY, 1),
            BorderFactory.createEmptyBorder(10, 10, 10, 10)
        ));
        s2LastResultLabel.setOpaque(true);
        s2LastResultLabel.setBackground(java.awt.Color.WHITE);
        s2Panel.add(s2LastResultLabel);
        
        s2OutputLabel = new JLabel("Output from Prolog:");
        s2OutputLabel.setBounds(20, 185, 400, 25);
        s2OutputLabel.setFont(new Font("Arial", Font.BOLD, 14));
        s2Panel.add(s2OutputLabel);
        
        s2OutputTextArea = new JTextArea();
        s2OutputTextArea.setEditable(false);
        s2OutputTextArea.setFont(new Font("Courier New", Font.PLAIN, 12));
        s2OutputTextArea.setLineWrap(true);
        s2OutputTextArea.setWrapStyleWord(true);
        
        s2ScrollPane = new JScrollPane(s2OutputTextArea);
        s2ScrollPane.setBounds(20, 220, 630, 260);
        s2ScrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
        s2Panel.add(s2ScrollPane);
    }
    
    private void s1RunTestButtonActionPerformed(ActionEvent evt) {
        String selectedItem = (String) s1TestCaseComboBox.getSelectedItem();
        String testName = selectedItem.split(" - ")[0]; 
        
        s1LastResultLabel.setText("Running: " + testName + "...");
        s1LastResultLabel.setBackground(java.awt.Color.LIGHT_GRAY);
        
        s1OutputTextArea.append("\n========================================\n");
        s1OutputTextArea.append("Running: " + testName + "\n");
        s1OutputTextArea.append("========================================\n");
        
        try {
            connection.sender.sendMessageToProlog(testName);
        } catch (Exception e) {
            System.err.println(e.getMessage());
        }
    }
    
    private void s1ClearOutputButtonActionPerformed(ActionEvent evt) {
        s1OutputTextArea.setText("");
        s1OutputTextArea.append("Output cleared. Ready for new test.\n");
    }
    
    private void s2RunTestButtonActionPerformed(ActionEvent evt) {
        String testName = (String) s2TestCaseComboBox.getSelectedItem();
        String strategy = (String) s2StrategyComboBox.getSelectedItem();
        
        s2LastResultLabel.setText("Running: " + testName + " with " + strategy + "...");
        s2LastResultLabel.setBackground(java.awt.Color.LIGHT_GRAY);
        
        s2OutputTextArea.append("Running: " + testName + " with strategy: " + strategy + "\n");
        s2OutputTextArea.append("========================================\n");
        
        s2StartTime = System.currentTimeMillis();

        try {
            String command = "solve(" + testName + ", " + strategy + ")";
            connection.sender.sendMessageToProlog(command);
        } catch (Exception e) {
            System.err.println(e.getMessage());
        }
    }
    
    private void s2ClearOutputButtonActionPerformed(ActionEvent evt) {
        s2OutputTextArea.setText("");
        s2OutputTextArea.append("Output cleared. Ready for new test.\n");
    }
    
    public void updateS1ResultLabel(String testName, String result) {
        s1LastResultLabel.setText("Result: " + result);
        if (result.contains("UNSATISFIABLE") || result.contains("YES")) {
            s1LastResultLabel.setBackground(new java.awt.Color(144, 238, 144)); 
        } else if (result.contains("SATISFIABLE") || result.contains("NO")) {
            s1LastResultLabel.setBackground(new java.awt.Color(255, 182, 193)); 
        } else {
            s1LastResultLabel.setBackground(java.awt.Color.WHITE);
        }
    }

    public void updateS2ResultLabel(String testName, String result) {
        long endTime = System.currentTimeMillis();
        long duration = endTime - s2StartTime;
        
        s2LastResultLabel.setText("Result: " + result + " | Time: " + duration + "ms");
        if (result.contains("SATISFIABLE") || result.contains("YES")) {
            s2LastResultLabel.setBackground(new java.awt.Color(144, 238, 144)); 
        } else {
            s2LastResultLabel.setBackground(new java.awt.Color(255, 182, 193)); 
        }
    }
    
    public JTextArea getOutputTextArea() {
        if (currentSubject.equals("s1")) {
            return s1OutputTextArea;
        } else {
            return s2OutputTextArea;
        }
    }
    
    public void setConnection(PrologConnection conn) {
        this.connection = conn;
    }
    
    public String getCurrentSubject() {
        return currentSubject;
    }
}
