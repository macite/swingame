package swingame.emulator;

import java.awt.Image;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Frame;
import java.awt.Insets;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyAdapter;

import swingame.MouseButton;
import swingame.KeyCode;

/**
 * <<Class summary>>
 *
 * @author   &lt;&gt;
 * @version $Rev$
 */
public final class EmulatedWindow extends Frame
{
	static final long serialVersionUID = -2221879713450231845L;
	
    private Point _mouse = new Point(0,0);
    private Image _backImage;
    private Image _nextImage;
    
    public EmulatedWindow(String caption, int width, int height) 
    {
        Insets insets = getInsets();
        
        setTitle(caption);
        setResizable(false);
        
        int totalWidth = width + (insets.left + insets.right);
        int totalHeight = height + (insets.top + insets.bottom);
        setSize(totalWidth, totalHeight);
        
        _backImage = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR);
        _nextImage = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR);
                
        addWindowListener ( new  WindowAdapter() {
                public void windowClosing(WindowEvent e) 
                {
                    System.exit(0);
                }
            });
        addMouseMotionListener( new MouseMotionAdapter() {
                public void mouseMoved(MouseEvent e)
                {
                    setMousePoint(e.getPoint());
                }
                public void mouseDragged(MouseEvent e)
                {
                    setMousePoint(e.getPoint());
                }
            });
        addMouseListener( new MouseAdapter() {
                public void mouseClicked(MouseEvent e)
                {
                    //System.out.println("Click " + mapButton(e.getButton()) + " " + MouseButton.RIGHT_BUTTON);
                    _next.buttonClicked[mapButton(e.getButton()) - 1] = true;
                }
                
                public void mousePressed(MouseEvent e) 
                {
                   _next.buttonDown[mapButton(e.getButton()) - 1] = true;
                }

                public void mouseReleased(MouseEvent e) 
                {
                    _next.buttonDown[mapButton(e.getButton()) - 1] = false;
                }
            });
        addKeyListener( new KeyAdapter(){
                public void keyPressed(KeyEvent e)
                {
                    _next.keyState[mapKey(e.getKeyCode())] |= EventStateData.KEY_DOWN;
                    //System.out.println("Type " + e.getKeyCode() + " " + KeyCode.VK_SPACE + " " + _next.keyState[mapKey(KeyCode.VK_SPACE)]);
                }
                public void keyReleased(KeyEvent e)
                {
                    _next.keyState[mapKey(e.getKeyCode())] = EventStateData.KEY_PRESSED;
                    //System.out.println("Type " + e.getKeyCode() + " " + KeyCode.VK_SPACE + " " + _next.keyState[mapKey(KeyCode.VK_SPACE)]);
                }
            });
    }
    
    private void setMousePoint(Point p)
    {
        Insets insets = getInsets();
        _mouse.setLocation(p.getX() - insets.left, p.getY() - insets.top);
    }
    
    protected Graphics getScreenGraphics()
    {
        return _nextImage.getGraphics();
    }
    
    protected Point getMousePoint()
    {
        return _mouse;
    }
    
    public void refreshScreen()
    {
        Graphics g = _backImage.getGraphics();
        g.drawImage(_nextImage, 0, 0, null);
        repaint();
    }
    
    public void paint(Graphics g)
    {
        super.paint(g);
        Insets insets = getInsets();        
        g.drawImage(_backImage, insets.left, insets.top, null);
    }
    
    private class EventStateData
    {
        public static final int KEY_DOWN = 1;
        public static final int KEY_PRESSED = 2;
        
        public boolean[] buttonClicked;
        public boolean[] buttonDown;
        public int[] keyState;
        
        public EventStateData()
        {
            //7 mouse buttons
            buttonClicked = new boolean[7];
            buttonDown = new boolean[7];
            keyState = new int[320];
        }
        
        public EventStateData(EventStateData current)
        {
            //7 mouse buttons
            buttonClicked = new boolean[7];
            buttonDown = current.buttonDown;
            keyState = new int[320];
            
            for(int i = 0; i < current.keyState.length; i++)
            {
                //preserve key down
                keyState[i] = current.keyState[i] & EventStateData.KEY_DOWN;
            }
        }
    }
    
    private EventStateData _current = new EventStateData();
    private EventStateData _next = new EventStateData();
    
    protected void processEvents()
    {
        //EventStateData old = _current;
        
        _current = _next;
        _next = new EventStateData(_current);
    }
    
    public void update(Graphics g)
    {
        paint(g);
    }
    
    protected boolean mouseClicked(int btn)
    {
        return _current.buttonClicked[btn - 1];
    }
    
    protected boolean mouseDown(int btn)
    {
        return _current.buttonDown[btn - 1];
    }
    
    protected boolean keyTyped(int key)
    {
        return (_current.keyState[key] & EventStateData.KEY_PRESSED) == EventStateData.KEY_PRESSED;
    }

    protected boolean keyDown(int key)
    {
        return (_current.keyState[key] & EventStateData.KEY_DOWN) == EventStateData.KEY_DOWN;
    }
    
    private int mapButton(int awtButton)
    {
        switch(awtButton)
        {
            case MouseEvent.BUTTON1: return MouseButton.LEFT_BUTTON;
            case MouseEvent.BUTTON3: return MouseButton.RIGHT_BUTTON;
            case MouseEvent.BUTTON2: return MouseButton.MIDDLE_BUTTON;
        }
        return MouseButton.X1_BUTTON;
    }
    
    private int mapKey(int awtKey)
    {
        switch(awtKey)
        {
            case KeyEvent.VK_BACK_SPACE : return  KeyCode.VK_BACK;
            case KeyEvent.VK_TAB        : return  KeyCode.VK_TAB;
            case KeyEvent.VK_CLEAR      : return  KeyCode.VK_CLEAR;
            case KeyEvent.VK_ENTER     : return  KeyCode.VK_RETURN;
            case KeyEvent.VK_SHIFT      : return  KeyCode.VK_SHIFT;
            case KeyEvent.VK_CONTROL    : return  KeyCode.VK_CONTROL;
            case KeyEvent.VK_META       : return  KeyCode.VK_MENU;
            case KeyEvent.VK_ALT        : return  KeyCode.VK_ALT;
            case KeyEvent.VK_PAUSE  : return  KeyCode.VK_PAUSE;
            //case KeyEvent.VK_CAPITAL  : return  KeyCode.VK_CAPITAL;
            case KeyEvent.VK_ESCAPE  : return  KeyCode.VK_ESCAPE;
            case KeyEvent.VK_SPACE  : return  KeyCode.VK_SPACE;
            case KeyEvent.VK_PAGE_UP  : return  KeyCode.VK_PAGE_UP;
            case KeyEvent.VK_PAGE_DOWN  : return  KeyCode.VK_PAGE_DOWN;
            case KeyEvent.VK_END  : return  KeyCode.VK_END;
            case KeyEvent.VK_HOME  : return  KeyCode.VK_HOME;
            case KeyEvent.VK_LEFT  : return  KeyCode.VK_LEFT;
            case KeyEvent.VK_UP  : return  KeyCode.VK_UP;
            case KeyEvent.VK_RIGHT  : return  KeyCode.VK_RIGHT;
            case KeyEvent.VK_DOWN  : return  KeyCode.VK_DOWN;
            //case KeyEvent.VK_PRINT  : return  KeyCode.VK_PRINT;
            case KeyEvent.VK_INSERT  : return  KeyCode.VK_INSERT;
            case KeyEvent.VK_DELETE  : return  KeyCode.VK_DELETE;
            case KeyEvent.VK_HELP  : return  KeyCode.VK_HELP;
            case KeyEvent.VK_0  : return  KeyCode.VK_0;
            case KeyEvent.VK_1  : return  KeyCode.VK_1;
            case KeyEvent.VK_2  : return  KeyCode.VK_2;
            case KeyEvent.VK_3  : return  KeyCode.VK_3;
            case KeyEvent.VK_4  : return  KeyCode.VK_4;
            case KeyEvent.VK_5  : return  KeyCode.VK_5;
            case KeyEvent.VK_6  : return  KeyCode.VK_6;
            case KeyEvent.VK_7: return KeyCode.VK_7;
            case KeyEvent.VK_8: return KeyCode.VK_8;
            case KeyEvent.VK_9: return KeyCode.VK_9;
            case KeyEvent.VK_A: return KeyCode.VK_A;
            case KeyEvent.VK_B: return KeyCode.VK_B;
            case KeyEvent.VK_C: return KeyCode.VK_C;
            case KeyEvent.VK_D: return KeyCode.VK_D;
            case KeyEvent.VK_E: return KeyCode.VK_E;
            case KeyEvent.VK_F: return KeyCode.VK_F;
            case KeyEvent.VK_G: return KeyCode.VK_G;
            case KeyEvent.VK_H: return KeyCode.VK_H;
            case KeyEvent.VK_I: return KeyCode.VK_I;
            case KeyEvent.VK_J: return KeyCode.VK_J;
            case KeyEvent.VK_K: return KeyCode.VK_K;
            case KeyEvent.VK_L: return KeyCode.VK_L;
            case KeyEvent.VK_M: return KeyCode.VK_M;
            case KeyEvent.VK_N: return KeyCode.VK_N;
            case KeyEvent.VK_O: return KeyCode.VK_O;
            case KeyEvent.VK_P: return KeyCode.VK_P;
            case KeyEvent.VK_Q: return KeyCode.VK_Q;
            case KeyEvent.VK_R: return KeyCode.VK_R;
            case KeyEvent.VK_S: return KeyCode.VK_S;
            case KeyEvent.VK_T: return KeyCode.VK_T;
            case KeyEvent.VK_U: return KeyCode.VK_U;
            case KeyEvent.VK_V: return KeyCode.VK_V;
            case KeyEvent.VK_W: return KeyCode.VK_W;
            case KeyEvent.VK_X: return KeyCode.VK_X;
            case KeyEvent.VK_Y: return KeyCode.VK_Y;
            case KeyEvent.VK_Z: return KeyCode.VK_Z;
            //case KeyEvent.VK_LWIN: return KeyCode.VK_LWIN;
            //case KeyEvent.VK_RWIN: return KeyCode.VK_RWIN;
            //case KeyEvent.VK_APPS: return KeyCode.VK_APPS;
            //case KeyEvent.VK_SLEEP: return KeyCode.VK_SLEEP;
            case KeyEvent.VK_NUMPAD0: return KeyCode.VK_NUMPAD0;
            case KeyEvent.VK_NUMPAD1: return KeyCode.VK_NUMPAD1;
            case KeyEvent.VK_NUMPAD2: return KeyCode.VK_NUMPAD2;
            case KeyEvent.VK_NUMPAD3: return KeyCode.VK_NUMPAD3;
            case KeyEvent.VK_NUMPAD4: return KeyCode.VK_NUMPAD4;
            case KeyEvent.VK_NUMPAD5: return KeyCode.VK_NUMPAD5;
            case KeyEvent.VK_NUMPAD6: return KeyCode.VK_NUMPAD6;
            case KeyEvent.VK_NUMPAD7: return KeyCode.VK_NUMPAD7;
            case KeyEvent.VK_NUMPAD8: return KeyCode.VK_NUMPAD8;
            case KeyEvent.VK_NUMPAD9: return KeyCode.VK_NUMPAD9;
            case KeyEvent.VK_MULTIPLY: return KeyCode.VK_MULTIPLY;
            case KeyEvent.VK_ADD: return KeyCode.VK_ADD;
            case KeyEvent.VK_SUBTRACT: return KeyCode.VK_SUBTRACT;
            case KeyEvent.VK_DECIMAL: return KeyCode.VK_DECIMAL;
            case KeyEvent.VK_DIVIDE: return KeyCode.VK_DIVIDE;
            case KeyEvent.VK_F1: return KeyCode.VK_F1;
            case KeyEvent.VK_F2: return KeyCode.VK_F2;
            case KeyEvent.VK_F3: return KeyCode.VK_F3;
            case KeyEvent.VK_F4: return KeyCode.VK_F4;
            case KeyEvent.VK_F5: return KeyCode.VK_F5;
            case KeyEvent.VK_F6: return KeyCode.VK_F6;
            case KeyEvent.VK_F7: return KeyCode.VK_F7;
            case KeyEvent.VK_F8: return KeyCode.VK_F8;
            case KeyEvent.VK_F9: return KeyCode.VK_F9;
            case KeyEvent.VK_F10: return KeyCode.VK_F10;
            case KeyEvent.VK_F11: return KeyCode.VK_F11;
            case KeyEvent.VK_F12: return KeyCode.VK_F12;
            case KeyEvent.VK_F13: return KeyCode.VK_F13;
            case KeyEvent.VK_F14: return KeyCode.VK_F14;
            case KeyEvent.VK_F15: return KeyCode.VK_F15;
            //case KeyEvent.VK_NUMLOCK: return KeyCode.VK_NUMLOCK;
            //case KeyEvent.VK_SCROLL: return KeyCode.VK_SCROLL;
            //case KeyEvent.VK_LSHIFT: return KeyCode.VK_LSHIFT;
            //case KeyEvent.VK_RSHIFT: return KeyCode.VK_RSHIFT;
            //case KeyEvent.VK_LCONTROL: return KeyCode.VK_LCONTROL;
            //case KeyEvent.VK_RCONTROL: return KeyCode.VK_RCONTROL;
            //case KeyEvent.VK_LMENU: return KeyCode.VK_LMENU;
            //case KeyEvent.VK_LALT: return KeyCode.VK_LALT;
            //case KeyEvent.VK_RMENU: return KeyCode.VK_RMENU;
            //case KeyEvent.VK_RALT: return KeyCode.VK_RALT;
            case KeyEvent.VK_EQUALS: return KeyCode.VK_EQUALS;
            case KeyEvent.VK_COLON: return KeyCode.VK_COLON;
            case KeyEvent.VK_SEMICOLON: return KeyCode.VK_SEMICOLON;
            case KeyEvent.VK_LESS: return KeyCode.VK_LESS;
            case KeyEvent.VK_GREATER: return KeyCode.VK_GREATER;
            //case KeyEvent.VK_QUESTION: return KeyCode.VK_QUESTION;
            case KeyEvent.VK_AT: return KeyCode.VK_AT;
            case KeyEvent.VK_COMMA: return KeyCode.VK_COMMA;
            case KeyEvent.VK_PERIOD: return KeyCode.VK_PERIOD;
            case KeyEvent.VK_SLASH: return KeyCode.VK_SLASH;
        }
        
        return KeyCode.VK_NUMLOCK;
    }
}
