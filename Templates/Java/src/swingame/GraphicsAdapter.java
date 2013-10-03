package swingame;

import java.awt.Color;

/**
 * <<Class summary>>
 *
 * @author Andrew Cain &lt;&gt;
 * @version $Rev$
 */
public interface GraphicsAdapter 
{
    public void fillRectangle(Color color, float x, float y, int w, int h);
    public void fillEllipse(Color color, float x, float y, int w, int h);
    public void drawPixel(Color color, float x, float y);
    public void drawLine(Color color, float x, float y, float xEnd, float yEnd);
    public void clearScreen(Color color);
    
    public void openGraphicsWindow(String caption, int width, int height);
    public void refreshScreen(int frameRate);
    public int screenHeight();
    public int screenWidth();
    public void refreshScreen();
    public void close();

}
