package swingame.emulator;

import java.awt.Graphics;
import java.awt.Color;

import swingame.GraphicsAdapter;

/**
 * <<Class summary>>
 *
 * @author   &lt;&gt;
 * @version $Rev$
 */
public final class EmulatedGraphics implements GraphicsAdapter
{
    private static EmulatedWindow _window; 
    
    public void openGraphicsWindow(String caption, int width, int height)
    {
        //Create the window
        _window = new EmulatedWindow(caption, width, height);
        //remove initial backcolor - make black
        swingame.Graphics.clearScreen(Color.BLACK);
        //send to front buffer
        refreshScreen();
        //draw window
        _window.setVisible(true);
    }
    
    protected static Graphics getGraphics()
    {
        return _window.getScreenGraphics();
    }
    
    protected static EmulatedWindow getWindow()
    {
        return _window;
    }
    
    public int screenHeight()
    {
        return _window.getHeight();
    }

    public int screenWidth()
    {
        return _window.getWidth();        
    }
    
    public void refreshScreen()
    {
        getWindow().refreshScreen();
    }

    public void refreshScreen(int fps)
    {
        getWindow().refreshScreen();
        try
        {
            java.lang.Thread.sleep(1000 / fps);
        } catch (Exception e){}
    }
    
    public void clearScreen(Color color)
    {
        Graphics g = getGraphics();
        g.setColor(color);
        g.fillRect(0, 0, screenWidth(), screenHeight());
    }
    
    public void fillRectangle(Color color, float x, float y, int w, int h)
    {
        Graphics g = getGraphics();
        g.setColor(color);
        g.fillRect((int)x, (int)y, w, h);
    }
    
    public void fillEllipse(Color color, float x, float y, int w, int h) 
    {
        Graphics g = getGraphics();
        g.setColor(color);
        g.fillOval((int)x, (int)y, w, h);        
    }
    
    public void drawPixel(Color color, float x, float y)
    {
        fillRectangle(color, x, y, 1, 1);        
    }
    
    public void drawLine(Color color, float x, float y, float xEnd, float yEnd)
    {
        Graphics g = getGraphics();
        g.setColor(color);
        g.drawLine((int)x, (int)y, (int)xEnd, (int)yEnd);                
    }
    
    public void close()
    {
        getWindow().dispose();
    }
}
