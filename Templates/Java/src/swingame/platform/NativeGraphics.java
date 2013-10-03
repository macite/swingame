    package swingame.platform;

import java.awt.Color;

import swingame.GraphicsAdapter;

/**
 * <<Class summary>>
 *
 * @author Andrew Cain &lt;&gt;
 * @version $Rev$
 */
public final class NativeGraphics implements GraphicsAdapter
{
    static
    {        
        try
        {
            System.loadLibrary("JavaSwinGame");
        }
        finally { System.out.println("Loaded SwinGame native library"); }
    }

    private static native void n_openGraphicsWindow(String caption, int width, int height);
    private static native void n_refreshScreen(int frameRate);
    private static native int n_screenHeight();
    private static native int n_screenWidth();
    private static native void n_refreshScreen();

    public void openGraphicsWindow(String caption, int width, int height) { n_openGraphicsWindow(caption, width, height); }
    public void refreshScreen(int frameRate) { n_refreshScreen(frameRate); }
    public int screenHeight() { return n_screenHeight(); }
    public int screenWidth() { return n_screenWidth(); }
    public void refreshScreen() { n_refreshScreen(); }

    private static native void n_fillRectangle(int color, float x, float y, int w, int h);    
    public void fillRectangle(Color color, float x, float y, int w, int h) 
    {
        n_fillRectangle(color.getRGB(), x, y, w, h);
    }

    private static native void n_fillEllipse(int color, float x, float y, int w, int h);    
    public void fillEllipse(Color color, float x, float y, int w, int h) 
    {
        n_fillEllipse(color.getRGB(), x, y, w, h);
    }

    
    private static native void n_drawPixel(int color, float x, float y);    
    public void drawPixel(Color color, float x, float y)
    {
        n_drawPixel(color.getRGB(), x, y);
    }
    
    private static native void n_drawLine(int color, float x, float y, float x1, float y1);
    public void drawLine(Color color, float x, float y, float xEnd, float yEnd)
    {
        n_drawLine(color.getRGB(), x, y, xEnd, yEnd);
    }
    
    private static native void n_clearScreen(int color);    
    public void clearScreen(Color color)
    {
        n_clearScreen(color.getRGB());
    }
    
    public void close()
    {
        
    }
}
