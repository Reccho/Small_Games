using Gtk;
using Gdk;
using Cairo;
using System;
using static System.Console;
using static System.Math;

using Window = Gtk.Window;
using Color = Gdk.Color;
using Rectangle = Cairo.Rectangle;

delegate void Notify();

namespace Sudoku
{
    //Gameboard class
    class Board {
        private byte[,] grid;
        public bool[,] given = new bool[9,9];
        public bool win;
        public event Notify changed;  // fires whenever the board changes
        
        public int currX;
        public int currY;
        
        public Board(int z = 0) {
            this.grid = PuzzleBank.Fetch(z);
            
            for (int i = 0; i < 9; i++)
                for (int j = 0; j < 9; j++)
                    if (grid[i,j] != 0) given[i,j] = true;
            
            this.win = false;
        }
        
        public int this[int x, int y] { get => grid[x, y]; }
        public void Refresh() { changed(); }
        
        public bool Check() =>
            rowCheck() && colCheck() && boxCheck() && dupeCheck();
        
        private bool rowCheck() { //Check each row sum == 45
            for (int r = 0; r < 9; r++) {
                byte sum = 0;
                for (int c = 0; c < 9; c++)
                    sum += grid[r, c];
                if (sum != 45) return false;
            }//for r
            return true;
        }
        private bool colCheck() {//Check each col sum == 45
            for (int c = 0; c < 9; c++) {
                byte sum = 0;
                for (int r = 0; r < 9; r++)
                    sum += grid[r, c];
                if (sum != 45) return false;
            }//for c
            return true;
        }
        private bool boxCheck() {
            for (int i = 0; i < 9; i += 3) {
                for (int j = 0; j < 9; j += 3) {
                        byte sum = 0;
                        for (int x = i; x < i + 3; x++)
                            for (int y = j; y < j + 3; y++)
                                sum += grid[x,y];
                        if (sum != 45) return false;
                }//for j
            }//for i
            return true;
        }
        public bool dupeCheck() {
            for (int i = 0; i < 9; i++) {
                bool[] row = new bool[10]; //0 to 9
                bool[] col = new bool[10]; //no need for +1
                
                for (int j = 0; j < 9; j++) {
                    //Rows
                    if (row[grid[i,j]] && grid[i,j] > 0) return false;
                    row[grid[i,j]] = true;
                    //Columns
                    if (col[grid[j,i]] && grid[j,i] > 0) return false;
                    col[grid[j,i]] = true;
                    //Squares
                    if (i % 3 == 0 && j % 3 == 0) {
                        bool[] box = new bool[10];
                        for (int x = i; x < i + 3; x++)
                            for (int y = j; y < j + 3; y++) {
                                if (box[grid[x,y]] && grid[x,y] > 0) return false;
                                box[grid[x,y]] = true;
                            }//for y
                    }//if
                    
                }//for j
            }//for i
            return true;
        }
        
        //OnButtonPress (DrawingArea)
        public bool Move(int x, int y) {
            this.currX = x;
            this.currY = y;
            changed();
            return true;
        }
        
        //On PadPress
        public bool Move(string s) {
            if (!given[currY, currX]) {
                if (s == "Clear") grid[currY, currX] = 0;
                else grid[currY, currX] = Convert.ToByte(int.Parse(s));
                changed();
            }
            return true;
        }
        
    }//Board
    
    
    //Toolbox class
    static class Util {
        public static Color parse(string name) {
            Color c = new Color(0, 0, 0);
            if (!Color.Parse(name, ref c))
              throw new Exception("bad color");
            return c;
        }
        // extension methods on Context
        public static void setColor(this Context c, Color color) {
            CairoHelper.SetSourceColor(c, color);
        }
        public static void drawLine(this Context c, double x1, double y1, double x2, double y2) {
            c.MoveTo(x1, y1);
            c.LineTo(x2, y2);
            c.Stroke();
        }
    }//Util
    
    
    //GUI class
    class View : DrawingArea {
        const int SquareSize = 50;
        const int Margin = SquareSize / 2;
        public static Board board;
        
        void init(int z = 0) {
            board = new Board(z);
            QueueDraw();
            board.changed += QueueDraw;
        }
        public View(int z = 0) {
            init(z);
            AddEvents((int) EventMask.ButtonPressMask);  // ask to receive button press events
            ModifyBg(StateType.Normal, Util.parse("white"));
        }
        
        protected override bool OnButtonPressEvent(EventButton ev) {
            int x = ((int) (ev.X - Margin)) / SquareSize;
            int y = ((int) (ev.Y - Margin)) / SquareSize;
            board.Move(x, y);
            return true;
        }
        
        protected override bool OnExposeEvent(EventExpose ev) {
            Context c = CairoHelper.Create(GdkWindow);
            c.Translate(Margin, Margin);
            c.Scale(SquareSize, SquareSize);
            
            //Color in situations
            if (board.win) {
                c.setColor(Util.parse("light green"));
                c.Rectangle(0, 0, 9, 9);
            } else if (board.currX < 9 && board.currY < 9) {
                c.setColor(Util.parse("light blue"));
                c.Rectangle(board.currX, board.currY, 1, 1);
            }
            c.Fill();
            c.setColor(Util.parse("black"));
            
            //Draw Board
            for (int i = 0; i <= 9; ++i) {
                if (i % 3 == 0) c.LineWidth = 0.05; //Box/Outer lines
                else { //Row/Col lines
                    c.LineWidth = 0.01;
                    c.setColor(Util.parse("steel blue"));
                }//else
                
                c.drawLine(i, 0, i, 9);
                c.drawLine(0, i, 9, i);
                c.setColor(Util.parse("black"));
            }//for i
            
            //Draw board values
            c.SetFontSize(0.7);
            for (int i = 0; i < 9; i++) {
                for (int j = 0; j < 9; j++) {
                    c.MoveTo(i + 0.3, j + 0.8); //These were the values that worked \_('>')_/
                    if (board[j,i] == 0) continue;
                    else if (board.given[j,i]) c.SetFontSize(0.9);
                    else c.setColor(Util.parse("gray"));
                    
                    c.ShowText(Convert.ToString(board[j,i]));
                    c.setColor(Util.parse("black"));
                    c.SetFontSize(0.7);
                }//for j
            }//for i
            
            c.GetTarget().Dispose();
            c.Dispose();
            return true;
        }
    }//View
    
    
    //New Game Menu
    class Bank : Window {
        public Bank() : base("Sudoku Puzzles") {
            Resize(600, 400);
            
            Table bank = new Table(9, 3, true);
            
            Label labelEasy = new Label("Easy"); //Category: Easy
            Label labelMed = new Label("Medium"); //Category: Medium
            Label labelHard = new Label("Hard"); //Category: Hard
            Label labelNotFun = new Label("Not Fun"); //Category: Not Fun
            bank.Attach(labelEasy, 0, 1, 0, 1);
            bank.Attach(labelMed, 1, 2, 0, 1);
            bank.Attach(labelHard, 2, 3, 0, 1);
            bank.Attach(labelNotFun, 1, 2, 8, 9); //Towards bottom of window
            
            Button[] bankButtons = new Button[22];
            uint x = 0, y = 1, a = 1, b = 2;
            for (int i = 1; i <= 22; i++) {
                bankButtons[i-1] = new Button((i).ToString());
                bankButtons[i-1].Clicked += OnBankPress;
                
                if (i == 8 || i == 15) { x++; y++; a = 1; b = 2; } //Splits into 3 columns
                
                if (i == 22) bank.Attach(bankButtons[i-1], 1, 2, 9, 10); //Special Case
                else bank.Attach(bankButtons[i-1], x, y, a++, b++); //Most Buttons
            }//for i
            
            Button sample = new Button("Sample");
            sample.Clicked += OnBankPress;
            bank.Attach(sample, 2, 3, 9, 10);
            
            Add(bank);
            ShowAll();
        }
        
        void OnBankPress(object sender, EventArgs args) {
            string s;
            if ((sender as Button).Label == "Sample") s = "0"; //default switch case
            else s = (sender as Button).Label;
            
            new Frame(int.Parse(s)).ShowAll();
            this.Destroy();
        }
        protected override bool OnDeleteEvent(Event ev) {
            Application.Quit();
            return true;
        }
    }
    
    
    //Main Window class
    class Frame : Window {
        public Frame(int z = 0) : base("Sudoku#") {
            SetPosition(WindowPosition.Center);
            Resize(1000, 550);
            
            VBox vbox = new VBox(false, 2);
            HBox hbox = new HBox(true, 1);
            
            //Menu bar
            MenuBar mb = new MenuBar();
            Menu filemenu = new Menu();
            MenuItem file = new MenuItem("Game");
            file.Submenu = filemenu;
            
            AccelGroup agr = new AccelGroup();
            AddAccelGroup(agr);
            
            ImageMenuItem newgame = new ImageMenuItem("New");
            newgame.AddAccelerator("activate", agr, new AccelKey(
                Gdk.Key.n, Gdk.ModifierType.ControlMask, AccelFlags.Visible));
            newgame.Activated += OnNewPress;
            filemenu.Append(newgame);
            
            SeparatorMenuItem sep = new SeparatorMenuItem();
            filemenu.Append(sep);
            
            MenuItem exit = new MenuItem("Exit");
            exit.AddAccelerator("activate", agr, new AccelKey(
                Gdk.Key.q, Gdk.ModifierType.ControlMask, AccelFlags.Visible));
            exit.Activated += OnExitPress;
            filemenu.Append(exit);
            
            mb.Append(file);
            
            //Number Pad
            Table pad = new Table(4, 3, true); //Table for # buttons
            
            Button[] padButtons = new Button[10];
            uint x = 0, y = 1, a = 0, b = 1;
            for (int i = 1; i <= 10; i++) {
                if (i == 10) padButtons[i-1] = new Button("Clear"); //Special "Clear" case
                else padButtons[i-1] = new Button((i).ToString()); //Most Buttons
                padButtons[i-1].Clicked += OnPadPress;
                
                if (i == 4 || i == 7) { x = 0; y = 1; a++; b++; }; //Split into 3 columns
                if (i == 10) pad.Attach(padButtons[i-1], 1, 2, 3, 4); //Special "Clear" case
                else pad.Attach(padButtons[i-1], x++, y++, a, b); //Most Buttons
            }
            
            //Check Validity Button
            Button check = new Button("Check");
            check.Clicked += OnCheckPress;
            check.SetSizeRequest(100, 30);
            
            //Packing
            hbox.PackStart(new View(z), true, true, 0);
            hbox.PackEnd(pad, false, true, 0);
            
            vbox.PackStart(mb, false, false, 0);
            vbox.PackStart(hbox, true, true, 0);
            vbox.PackEnd(check, false, false, 0);
            
            Add(vbox);
            ShowAll();
        }
        
        void OnNewPress(object sender, EventArgs args) {
            Bank Choose = new Bank();
            new Bank().ShowAll();
            Choose.Destroy();
            this.Destroy();
        }
        void OnExitPress(object sender, EventArgs args) {
            WriteLine("Goodbye.");
            Application.Quit();
        }
        void OnCheckPress(object sender, EventArgs args) {
            View.board.win = View.board.Check();
            WriteLine(View.board.win);
            View.board.Refresh();
        }
        void OnPadPress(object sender, EventArgs args) {
            string s = (sender as Button).Label;
            View.board.Move(s);
        }
        protected override bool OnDeleteEvent(Event ev) {
            Application.Quit();
            return true;
        }
        
        
        static void Main() {
            Application.Init();
            new Bank().ShowAll();
            Application.Run();
        }
    }//Frame
    
}//namespace
