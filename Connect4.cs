//CONNECT 4
using Cairo;
using Gdk;
using Gtk;
using System;
using static System.Console;
using static System.Math;

using Color = Gdk.Color;
using Rectangle = Cairo.Rectangle;
using Window = Gtk.Window;

delegate void Notify();

class Board {
    // [6,7] works but is off;  [7,6] crashes on start
    int[,] square = new int[7, 6];  // 0 = empty, 1 = player 1, 2 = player 2
    public int player = 1;  // whose turn it is
    public int winner = 0;  // who has won
    public int win_x, win_y, win_dx, win_dy;  // vector indicating winning squares
       
    public event Notify changed;  // fires whenever the board changes
    public int this[int x, int y] { get => square[x, y]; }
    //all
    bool all(int x, int y, int dx, int dy) {
        for (int i = 0 ; i < 4 ; ++i)
            if (square[x + i * dx, y + i * dy] != player)
                return false;
        win_x = x; win_y = y; win_dx = dx; win_dy = dy;
        return true;
    }
    //bool checkWin
    bool checkWin() {
        //ROW
        for (int row = 5; row >= 0; --row)
            for (int i = 0; i < 4; i++)
                if (all(i, row, 1, 0)) return true;
        //COL
        for (int i = 0; i < 7; i++)
            for (int col = 5; col >= 3; --col)
                if (all(i, col, 0, -1)) return true;
        
        //DIAGONAL
        for (int i = 0; i < 4; i++)  // lo-left/hi-right
            for (int j = 5; j >= 3; --j)
                if (all(i, j, 1, -1)) return true;
        for (int i = 6; i >= 3; --i)
            for (int j = 5; j >= 3; --j)
                if (all(i, j, -1, -1)) return true;
        
        return false;
    }
    //bool move
    public bool move(int x, int y) {
        if (square[x, y] > 0) return false;  //occupied
        
        //square[x, y] = player;  //No-falling
        for (y = 5; y >= 0; --y) {
            if (square[x, y] == 0) {
                square[x, y] = player;
                break;
            }//if
        }//for y
        
        if (checkWin()) winner = player;
        else player = 3 - player;
        
        changed();
        return true;
    }
}//Board

static class Util {
  public static Color parse(string name) {
    Color c = new Color(0, 0, 0);
    if (!Color.Parse(name, ref c))
      throw new Exception("bad color");
    return c;
  }
  public static void setColor(this Context c, Color color) {
    CairoHelper.SetSourceColor(c, color);
  }
  public static void drawLine(this Context c, double x1, double y1, double x2, double y2) {
    c.MoveTo(x1, y1);
    c.LineTo(x2, y2);
    c.Stroke();
  }
}//Util

class View : DrawingArea {
    const int SquareSize = 100;
    const int Margin = SquareSize / 2;
    Board board;
    
    void init() {
        board = new Board();
        QueueDraw();   // draw the initial position
        board.changed += QueueDraw;
    }
    public View() {
        init();
        AddEvents((int) EventMask.ButtonPressMask);  // ask to receive button press events
        ModifyBg(StateType.Normal, Util.parse("white"));
    }//constructor
    
    void drawCircle(Context c, Rectangle r, string shade) {
        c.setColor(Util.parse(shade));
        c.Arc(r.X + r.Width / 2, r.Y + r.Height / 2, r.Width / 2, 0.0, 2 * PI);
        //c.Stroke();
        c.StrokePreserve();
        c.Fill();
    }
    
    protected override bool OnButtonPressEvent(EventButton ev) {
        if (board.winner > 0) init();   // start a new game
        else {
            int x = ((int) (ev.X - Margin)) / SquareSize;
            int y = ((int) (ev.Y - Margin)) / SquareSize;
            board.move(x, y);
        }//else
        return true;
    }
    protected override bool OnExposeEvent(EventExpose ev) {
        Context c = CairoHelper.Create(GdkWindow);
        
        // Set a transformation matrix so that squares are 1 unit tall/wide, with a
        // margin around the drawing area.
        c.Translate(Margin, Margin);
        c.Scale(SquareSize, SquareSize);
        // Adjust the line width to compensate for the scaling transformation.
        c.LineWidth /= SquareSize;
        
        //draw grid
        //for (int i = 0; i <= 7; i++) c.drawLine(i, 0, i, 6);
        //for (int i = 0; i <= 6; i++) c.drawLine(0, i, 7, i);
        
        //draw board
        for (int x = 0; x < 7; ++x) {  // x = col
            for (int y = 0; y < 6; ++y) {  // y = row
                Rectangle rect = new Rectangle(x + 0.1, y + 0.1, 0.8, 0.8);
                c.setColor(Util.parse("Slate Gray"));  //BOARD
                c.Rectangle(x, y, 1, 1);
                c.Fill();
                drawCircle(c, rect, "white");
            }
        }
        
        if (board.winner > 0) {
            c.setColor(Util.parse("light green"));  //Win
            for (int i = 0; i < 4; ++i) {
                c.Rectangle(board.win_x + i * board.win_dx,
                            board.win_y + i * board.win_dy,
                            1, 1);
                c.StrokePreserve();
                c.Fill();
            }//for i
            c.setColor(Util.parse("black"));
        }//if
        
        //draw circles
        for (int x = 0; x < 7; ++x)
            for (int y = 0; y < 6; ++y) {
                Rectangle rect = new Rectangle(x + 0.1, y + 0.1, 0.8, 0.8);
                switch (board[x, y]) {
                    case 1: drawCircle(c, rect, "Pale Violet Red");  break;  //PLAYER 1
                    case 2: drawCircle(c, rect, "Deep Sky Blue");  break;  //PLAYER 2
                }
            }
        
        c.GetTarget().Dispose();
        c.Dispose();
        return true;
    }
}//View

class Frame : Window {
  Frame() : base("Connect4") {
    Resize(800, 700);
    Add(new View());
  }//constructor
  
  protected override bool OnDeleteEvent(Event ev) {
    Application.Quit();
    return true;
  }
  
  static void Main() {
    Application.Init();
    new Frame().ShowAll();
    Application.Run();
  }//MAIN
}//Frame

