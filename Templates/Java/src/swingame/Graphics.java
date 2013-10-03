/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package swingame;

import java.awt.Color;

import swingame.emulator.EmulatedGraphics;
import swingame.platform.NativeGraphics;

/**
 *
 * @author acain
 */
public class Graphics 
{
    private static GraphicsAdapter _ga = new EmulatedGraphics();
    
    protected static void useNative()
    {
        _ga = new NativeGraphics();
    }
    
    public static void openGraphicsWindow(String caption, int width, int height) { _ga.openGraphicsWindow(caption, width, height); }
    public static void refreshScreen(int frameRate) { _ga.refreshScreen(frameRate); }
    public static int screenHeight() { return _ga.screenHeight(); }
    public static int screenWidth() { return _ga.screenWidth(); }
    public static void refreshScreen() { _ga.refreshScreen(); }
    
    public static Color hSBColor(float hue, float saturation, float brightness)
    {
        return Color.getHSBColor(hue, saturation, brightness);
    }
    
    public static Color rGBAFloatColor(float r, float g, float b, float a)
    {
        return new Color(r, g, b, a);
    }
    
    /**
    * Returns the hue component of the passed in color.
    *
    * @param c  The color to get the Hue of
    * @return The hue of the color c
    */
    public static float hueOf(Color c)
    {
        float[] f;
        
        f = Color.RGBtoHSB(c.getRed(), c.getGreen(), c.getBlue(), null);
        return f[0];
    }
    
    /**
    * Returns the saturation component of the passed in color.
    *
    * @param c  The color to get the saturation of
    * @return The saturation of the color c
    */
    public static float saturationOf(Color c)
    {
        float[] f;
        
        f = Color.RGBtoHSB(c.getRed(), c.getGreen(), c.getBlue(), null);
        return f[1];
    }
    
    /**
    * Returns the brightness component of the passed in color.
    *
    * @param c  The color to get the Brightness of
    * @return The brightness of the color c
    */
    public static float brightnessOf(Color c)
    {
        float[] f;
        
        f = Color.RGBtoHSB(c.getRed(), c.getGreen(), c.getBlue(), null);
        return f[2];
    }
    
    
    public static void fillRectangle(Color color, float x, float y, int w, int h) 
    {
        _ga.fillRectangle(color, x, y, w, h);
    }

    public static void fillEllipse(Color color, float x, float y, int w, int h) 
    {
        _ga.fillEllipse(color, x, y, w, h);
    }
    
    public static void fillCircle(Color color, float x, float y, int r) 
    {
        _ga.fillEllipse(color, x - r, y - r, (int)x + r, (int)y + r);
    }
    
    public static void drawPixel(Color color, float x, float y)
    {
        _ga.drawPixel(color, x, y);
    }
    
    public static void drawLine(Color color, float x, float y, float xEnd, float yEnd)
    {
        _ga.drawLine(color, x, y, xEnd, yEnd);
    }
    
    public static void clearScreen(Color color)
    {
        _ga.clearScreen(color);
    }
    
    public static void clearScreen()
    {
        clearScreen(Color.BLACK);
    }
    
    public static void close()
    {
        _ga.close();
    }
}
